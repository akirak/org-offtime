;;; offtime.el --- Basic facilities for off-time tracking -*- lexical-binding: t -*-

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

;; This library provides basic facilities for off-time tracking used by
;; org-offtime.el.

;;; Code:

(require 'cl-lib)

(defvar offtime-pre-hook
  '((lambda () (save-some-buffers t)))
  "List of functions to run before an off-time command is run.")

(defvar offtime-post-hook nil
  "List of functions to run after an off-time command is run.")

(defun offtime--shell-command (cmd)
  "Run a shell command with proper hooks for an off-time event."
  (run-hooks 'offtime-pre-hook)
  (shell-command-to-string cmd)
  (run-hooks 'offtime-post-hook))

(defun offtime-suspend ()
  "Suspend the computer."
  (interactive)
  (offtime--shell-command offtime-suspend-command))

(defcustom offtime-suspend-command "systemctl suspend"
  "Shell command used to suspend the computer."
  :group 'offtime
  :type 'string)

(defun offtime-lock ()
  "Lock screen."
  (interactive)
  (offtime--shell-command offtime-lock-command))

(defcustom offtime-lock-commands
  '("physlock" "slock")
  "List of shell commands that can be used to lock the screen."
  :type '(list string))

(defcustom offtime-lock-command
  (car (cl-remove-if-not 'executable-find offtime-lock-commands))
  "Shell command used to lock the screen."
  :group 'offtime
  :type 'string)

(provide 'offtime)
;;; offtime.el ends here
