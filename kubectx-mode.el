;;; kubectx-mode.el --- Global minor-mode to change kubectl context and namespace and display in Emacs mode line

;; Copyright (C) 2018-2019 Terje Sannum

;; Author: Terje Sannum <terje@offpiste.org>
;; Created: 19 Oct 2018
;; Keywords: mode-line kubernetes
;; Homepage: https://github.com/terjesannum/emacs-kubectx-mode

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

;; See https://github.com/terjesannum/emacs-kubectx-mode/blob/master/README.md

;;; Code:
(defvar kubectx-mode-kubectl-command "kubectl" "Kubectl binary")
(defvar kubectx-mode-timer nil)
(defvar kubectx-mode-mode-line-string "")
(defvar kubectx-mode-mode-line-update-interval 10 "Number of seconds between background mode-line updates")
(defvar kubectx-mode-mode-line-string-format " [kube:%C %N]" "String to display in mode-line (%C = context, %N = namespace)")

(defun kubectx-mode-run-kubectl (&rest args)
  "Run kubectl command"
  (with-temp-buffer
    (let ((default-directory "~"))
      (if (and (executable-find kubectx-mode-kubectl-command)
               (= (apply 'call-process kubectx-mode-kubectl-command nil t nil args) 0))
          (replace-regexp-in-string "\n\\'" "" (buffer-string))
        "n/a"))))

(defun kubectx-mode-namespaces ()
  "Get list of namespaces"
  (split-string (kubectx-mode-run-kubectl "get" "namespaces" "--output" "jsonpath={.items[*].metadata.name}")))

(defun kubectx-mode-set-namespace (namespace)
  "Set current kubectl namespace"
  (interactive
   (list
    (completing-read "Namespace: " (kubectx-mode-namespaces) nil t)))
  (kubectx-mode-run-kubectl "config" "set-context" "--current" (format "--namespace=%s" namespace))
  (kubectx-mode-mode-line-update))

(defun kubectx-mode-contexts ()
  "Get list of contexts"
  (split-string (kubectx-mode-run-kubectl "config" "get-contexts" "--output" "name")))

(defun kubectx-mode-set-context (context)
  "set current kubectl context"
  (interactive
   (list
    (completing-read "Context: " (kubectx-mode-contexts) nil t)))
  (kubectx-mode-run-kubectl "config" "use-context" context)
  (kubectx-mode-mode-line-update))

(defun kubectx-mode-kubectx-string (context namespace)
  "Create kubectx string to display"
  (replace-regexp-in-string "%C" context (replace-regexp-in-string "%N" namespace kubectx-mode-mode-line-string-format t) t))

(defun kubectx-mode-mode-line-update ()
  "Update kubectx mode-line string with current context and namespace"
  (interactive)
  (let ((ctx (kubectx-mode-run-kubectl "config" "current-context"))
        (ns (kubectx-mode-run-kubectl "config" "view" "--minify" "--output" "jsonpath={..namespace}")))
    (setq kubectx-mode-mode-line-string (kubectx-mode-kubectx-string ctx ns))
    (force-mode-line-update t)))

(define-minor-mode kubectx-mode
  "Add kubectx and namespace info to the mode line"
  :global t
  (when (not global-mode-string) (setq global-mode-string '("")))
  (when kubectx-mode-timer (cancel-timer kubectx-mode-timer))
  (if (not kubectx-mode)
      (setq global-mode-string
            (delq 'kubectx-mode-mode-line-string global-mode-string))
    (add-to-list 'global-mode-string 'kubectx-mode-mode-line-string t)
    (when (> kubectx-mode-mode-line-update-interval 0)
      (setq kubectx-mode-timer
            (run-at-time nil kubectx-mode-mode-line-update-interval
                         'kubectx-mode-mode-line-update)))
    (kubectx-mode-mode-line-update)))

(provide 'kubectx-mode)

;;; kubectx-mode.el ends here
