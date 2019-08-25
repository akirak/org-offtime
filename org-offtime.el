;;; org-offtime.el --- off-time tracking with org mode -*- lexical-binding: t -*-

;; Copyright (C) 2018 by Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 0.2
;; Package-Requires: ((emacs "25.1") (org-ql "0.2"))
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
(require 'subr-x)
(require 'offtime)
(require 'org)
(require 'org-ql)
(require 'org-element)

(defgroup org-offtime nil
  "Offtime tracking with Org files."
  :group 'offtime)

(defcustom org-offtime-files nil
  "List of file names that contain offtime entries."
  :group 'org-offtime
  :type '(repeat string))

(defcustom org-offtime-query '(tag "offtime")
  "Query to get offtime entries, as supported by org-ql."
  :group 'org-offtime
  :type 'sexp)

(defcustom org-offtime-default-action
  'offtime-suspend
  "Function to run after clocking in to an entry with org-offtime."
  :group 'org-offtime
  :type 'function)

(defcustom org-offtime-clock-out nil
  "Automatically clock out after running an action.

If you use systemctl to suspend the computer, you may have to set this value to
nil."
  :type 'symbol
  :group 'org-offtime)

(defvar org-offtime-history nil)

(defun org-offtime-get-candidates ()
  "Get offtime entries."
  (org-ql-select
    org-offtime-files
    org-offtime-query
    :action (lambda ()
              (propertize (org-offtime--format-entry)
                          'org-hd-marker (point-marker)))))

(defun org-offtime--format-entry ()
  "Format the current entry."
  (format "%s: %s"
          (buffer-name (org-base-buffer (current-buffer)))
          (org-format-outline-path (org-get-outline-path t t))))

(defun org-offtime--select ()
  "Return a marker to an entry selected by `org-offtime-get-candidates'."
  (let ((ent (completing-read "Offtime entry: "
                              (org-offtime-get-candidates)
                              nil t nil org-offtime-history)))
    (get-text-property 0 'org-hd-marker ent)))

;;;###autoload
(cl-defun org-offtime-clock-in (marker &optional (func org-offtime-default-action))
  "Clock in to an entry at MARKER with FUNC called afterwards."
  (interactive (list (org-offtime--select)))
  (with-current-buffer (marker-buffer marker)
    (unless (derived-mode-p 'org-mode)
      (error "Not in org mode"))
    (org-with-wide-buffer
     (message "org-offtime: Clock in at %s to %s"
              (org-timestamp-format
               (org-timestamp-from-time (current-time) t t)
               (org-time-stamp-format t t))
              (org-offtime--format-entry))
     (goto-char marker)
     (org-clock-in)))
  (when func
    ;; Await for the function to finish
    (funcall func)
    (when org-offtime-clock-out
      (message "org-offtime: Clock out at %s"
               (org-timestamp-format
                (org-timestamp-from-time (current-time) t t)
                (org-time-stamp-format t t)))
      (org-clock-out))))

;;;###autoload
(defun org-offtime-clock-in-this-entry ()
  "Start an off-time clock on the current headline in org mode."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (error "Not in org-mode"))
  (org-offtime-clock-in (point-marker)))

;; (defun org-offtime--create-headline (path)
;;   "Create a headline from PATH."
;;   (with-current-buffer (or (find-buffer-visiting org-offtime-file)
;;                            (find-file-noselect org-offtime-file))
;;     (org-with-wide-buffer
;;      (goto-char (point-min))
;;      (cl-loop for heading in (split-string path "/")
;;               do (let* ((level (if (= (point) (point-min))
;;                                    0
;;                                  (nth 0 (org-heading-components))))
;;                         (next-level (org-get-valid-level (1+ level)))
;;                         (subtree-end (if (= level 0)
;;                                          (point-max)
;;                                        (save-excursion (org-end-of-subtree)))))
;;                    (while (or (= (point) (point-min))
;;                               (not (= next-level (nth 0 (org-heading-components)))))
;;                      (unless (re-search-forward
;;                               (format org-complex-heading-regexp-format
;;                                       (regexp-quote heading))
;;                               subtree-end t)
;;                        (goto-char subtree-end)
;;                        (insert "\n" (make-string next-level ?\*) " " heading))))
;;               finally return (point-marker)))))

;;;###autoload
(with-eval-after-load 'ivy
  ;; TODO: Might need to declare ivy-read
  (defun org-offtime-ivy ()
    "Use Ivy to enter off-time."
    (interactive)
    (ivy-read "off-time: " (org-offtime--select)
              :require-match t
              :caller 'counsel-org-offtime
              :action '(1 ("o" org-offtime-clock-in "default")
                          ("s"
                           (lambda (marker)
                             (org-offtime-clock-in marker 'offtime-suspend))
                           "suspend")
                          ("l"
                           (lambda (marker)
                             (org-offtime-clock-in marker 'offtime-lock))
                           "lock")
                          ("g" #'org-goto-marker-or-bmk "go to the heading")))))

(provide 'org-offtime)
;;; org-offtime.el ends here
