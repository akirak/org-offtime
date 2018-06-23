;;; counsel-org-offtime.el --- Ivy interface for org-offtime -*- lexical-binding: t -*-

;; Copyright (C) 2018 by Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "25.1") (ivy "0.10.0"))
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

;; This library provides a function `counsel-org-offtime', which is an Ivy
;; interface to org-offtime.el.

;;; Code:

(require 'org-offtime)
(require 'offtime)
(require 'ivy)

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
                         "lock")
                        ("g" counsel-org-offtime--jump "go to the heading"))))

(defun counsel-org-offtime--jump (cand)
  "Jump to CAND."
  (org-goto-marker-or-bmk (cond
                           ((markerp cand) cand)
                           ((consp cand) (cdr cand)))))

(provide 'counsel-org-offtime)
;;; counsel-org-offtime.el ends here
