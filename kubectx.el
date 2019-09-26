;;; kubectx.el --- Display current kubernetes context and namespace in Emacs mode line

;; Copyright (C) 2018-2019 Terje Sannum

;; Author: Terje Sannum <terje@offpiste.org>
;; Created: 19 Oct 2018
;; Keywords: mode-line kubernetes
;; Homepage: https://github.com/terjesannum/emacs-kubectx-mode-line

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; See https://github.com/terjesannum/emacs-kubectx-mode-line/blob/master/README.md

;;; Code:
(defvar kubectx-kubectl-command "kubectl" "Kubectl binary")
(defvar kubectx-timer nil)
(defvar kubectx-string "")
(defvar kubectx-update-interval 10 "Number of seconds between background mode-line updates")
(defvar kubectx-string-format " %C %N" "String to display in mode-line (%C = context, %N = namespace)")

(defun kubectx-run-kubectl (&rest args)
  "Run kubectl command"
  (with-temp-buffer
    (let ((default-directory "~"))
      (if (and (executable-find kubectx-kubectl-command)
               (= (apply 'call-process kubectx-kubectl-command nil t nil args) 0))
          (replace-regexp-in-string "\n\\'" "" (buffer-string))
        "n/a"))))

(defun make-kubectx-string (context namespace)
  "Create kubectx string to display"
  (replace-regexp-in-string "%C" context (replace-regexp-in-string "%N" namespace kubectx-string-format t) t))

(defun kubectx-update ()
  "Update kubectx mode-line string with current context and namespace"
  (interactive)
  (let ((ctx (kubectx-run-kubectl "config" "current-context"))
        (ns (kubectx-run-kubectl "config" "view" "--minify" "--output" "jsonpath={..namespace}")))
    (setq kubectx-string (make-kubectx-string ctx ns))))

(define-minor-mode kubectx-mode
  "Add kubectx and namespace info to the mode line"
  :global t
  (when (not global-mode-string) (setq global-mode-string '("")))
  (when kubectx-timer (cancel-timer kubectx-timer))
  (if (not kubectx-mode)
      (setq global-mode-string
            (delq 'kubectx-string global-mode-string))
    (add-to-list 'global-mode-string 'kubectx-string t)
    (when (> kubectx-update-interval 0)
      (setq kubectx-timer
            (run-at-time nil kubectx-update-interval
                         'kubectx-update)))
    (kubectx-update)))

(provide 'kubectx)

;;; kubectx.el ends here
