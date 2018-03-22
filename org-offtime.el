;;; org-offtime.el --- off-time tracking with org mode -*- lexical-binding: t -*-

;; Copyright (C) 2018 by Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "25.1"))
;; URL: https://github.com/akirak/org-offtime

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This library provides functions for running offtime on an org mode entry:

;; - `org-offtime' lets you clock in an entry in a file specified as
;;   `org-offtime-file'.
;; - `org-offtime-clock-in' lets you clock in any entry in org mode.
;;
;; Optionally, you can use `counsel-org-offtime' as a replacement for
;; `org-offtime' if you prefer an Ivy interface.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'offtime)
(require 'org)

(defcustom org-offtime-file
  (expand-file-name "offtime.org" org-directory)
  "File to track off-time activities."
  :group 'org-offtime
  :type 'string)

(cl-defun org-offtime--get-headings (&key format-with-schedule)
  "Generate an alist of headings in `org-offtime-file'.

Each item of the result is a cons cell of its outline path and a marker.
Scheduled items come first, and then non-scheduled items.
If FORMAT-WITH-SCHEDULE is non-nil, the schedule is prepended to the path.

Archived items, comment entries, and entries with one of the done states are
omitted."
  (with-current-buffer (or (find-buffer-visiting org-offtime-file)
                           (find-file-noselect org-offtime-file))
    (setq org-outline-path-cache nil)
    (mapcar 'cdr
            (seq-sort (lambda (x y)
                        (let ((a (car x))
                              (b (car y)))
                          (cond
                           ((and a b) (< (float-time a) (float-time b)))
                           (a t)
                           (b nil))))
                      (org-with-wide-buffer
                       (org-map-entries
                        (lambda ()
                          (let ((time (org-get-scheduled-time nil)))
                            (cons time
                                  (cons (concat (when (and format-with-schedule
                                                           time)
                                                  (format-time-string "%F %R: " time))
                                                (org-offtime--format-headline))
                                        (point-marker)))))
                        nil nil
                        'archive 'comment '(org-entry-is-done-p)))))))

(defun org-offtime--format-headline ()
  "Format the outline path of the current heading."
  (org-format-outline-path (org-get-outline-path t t) nil nil "/"))

(defcustom org-offtime-default-action
  'offtime-suspend
  "Function to run after clocking in to an entry with org-offtime."
  :group 'org-offtime
  :type 'function)

(defun org-offtime--clock-in (cand &optional func)
  "Start an off-time clock at MARKER and run the default action.

FUNC is called after clocking in."
  (let ((marker (cond
                 ((markerp cand) cand)
                 ((consp cand) (cdr cand))
                 ((stringp cand) (org-offtime--create-headline cand)))))
    (with-current-buffer (marker-buffer marker)
      (org-with-wide-buffer
       (goto-char marker)
       (org-clock-in)))
    (let ((action (or func org-offtime-default-action)))
      (when action
        (funcall action)
        (when org-offtime-clock-out
          (org-clock-out))))))

;;;###autoload
(defun org-offtime-clock-in ()
  "Start an off-time clock on the current headline in org-mode."
  (interactive)
  (unless (eq major-mode 'org-mode)
    (error "not in org-mode"))
  (org-offtime--clock-in (point-marker)))

(defcustom org-offtime-clock-out nil
  "Automatically clock out after running an action.

If you use systemctl to suspend the computer, you may have to set this value to
nil."
  :type 'symbol
  :group 'org-offtime)

;;;###autoload
(defun org-offtime (&optional func)
  "Choose an off-time state from org-offtime-file.

FUNC is a function used to enter the off-time state, e.g. `offtime-suspend'."
  (interactive)
  (let* ((cands (org-offtime--get-headings :format-with-schedule t))
         (cand (completing-read "off-time: " cands)))
    (when cand
      (org-offtime--clock-in (or (assoc cand cands) cand) func))))

;;;###autoload
(defun counsel-org-offtime ()
  "Use Ivy to enter off-time."
  (interactive)
  (require 'ivy)
  (ivy-read "off-time: " (org-offtime--get-headings :format-with-schedule t)
            :require-match nil
            :caller 'counsel-org-offtime
            :action '(1 ("o" org-offtime--clock-in "default")
                        ("s"
                         (lambda (cand)
                           (org-offtime--clock-in cand 'offtime-suspend))
                         "suspend")
                        ("l"
                         (lambda (cand)
                           (org-offtime--clock-in cand 'offtime-lock))
                         "lock"))))

(defun org-offtime--create-headline (path)
  "Create a headline from PATH."
  (with-current-buffer (or (find-buffer-visiting org-offtime-file)
                           (find-file-noselect org-offtime-file))
    (org-with-wide-buffer
     (goto-char (point-min))
     (cl-loop for heading in (split-string path "/")
              do (let* ((level (if (= (point) (point-min))
                                   0
                                 (nth 0 (org-heading-components))))
                        (next-level (org-get-valid-level (1+ level)))
                        (subtree-end (if (= level 0)
                                         (point-max)
                                       (save-excursion (org-end-of-subtree)))))
                   (while (or (= (point) (point-min))
                              (not (= next-level (nth 0 (org-heading-components)))))
                     (unless (re-search-forward
                              (format org-complex-heading-regexp-format
                                      (regexp-quote heading))
                              subtree-end t)
                       (goto-char subtree-end)
                       (insert "\n" (make-string next-level ?\*) " " heading))))
              finally return (point-marker)))))

(provide 'org-offtime)
;;; org-offtime.el ends here
