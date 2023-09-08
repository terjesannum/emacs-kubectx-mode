;;; kubectx-mode.el --- Change kubectl context/namespace and show in mode line -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2019 Terje Sannum

;; Author: Terje Sannum <terje@offpiste.org>
;; Keywords: tools kubernetes
;; Version: 1.2.0
;; Package-Requires: ((emacs "24"))
;; URL: https://github.com/terjesannum/emacs-kubectx-mode

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; See https://github.com/terjesannum/emacs-kubectx-mode/blob/master/README.md

;;; Code:

(defvar kubectx-kubectl-command "kubectl" "Kubectl executable.")
(defvar kubectx-mode-line-update-timer nil)
(defvar kubectx-mode-line-string "")
(defvar kubectx-mode-line-update-interval 5 "Number of seconds between background mode-line updates.")
(defvar kubectx-mode-line-string-format " [kube:%C %N]" "String to display in mode-line (%C = context, %N = namespace).")
(defvar kubectx-mode-map
  (let ((km (make-sparse-keymap)))
    (define-key km (kbd "C-c C-k c") #'kubectx-set-context)
    (define-key km (kbd "C-c C-k n") #'kubectx-set-namespace)
    km)
   "Keymap for `kubectx-mode'.")

(defun kubectx-run-kubectl-command (&rest args)
  "Run kubectl command with ARGS."
  (with-temp-buffer
    (let ((default-directory "~"))
      (if (and (executable-find kubectx-kubectl-command)
               (= (apply #'call-process kubectx-kubectl-command nil t nil args) 0))
          (replace-regexp-in-string "\n\\'" "" (buffer-string))
        "n/a"))))

(defun kubectx-namespaces ()
  "Get list of namespaces."
  (split-string (kubectx-run-kubectl-command "get" "namespaces" "--output" "jsonpath={.items[*].metadata.name}")))

(defun kubectx-set-namespace (namespace)
  "Set current kubectl namespace to NAMESPACE."
  (interactive
   (list
    (completing-read "Namespace: " (kubectx-namespaces) nil t)))
  (kubectx-run-kubectl-command "config" "set-context" "--current" (format "--namespace=%s" namespace))
  (kubectx-mode-line-update))

(defun kubectx-contexts ()
  "Get list of contexts."
  (split-string (kubectx-run-kubectl-command "config" "get-contexts" "--output" "name")))

(defun kubectx-set-context (context)
  "Set current kubectl context to CONTEXT."
  (interactive
   (list
    (completing-read "Context: " (kubectx-contexts) nil t)))
  (kubectx-run-kubectl-command "config" "use-context" context)
  (kubectx-mode-line-update))

(defun kubectx-mode-line-string (context namespace)
  "Create kubectx string with CONTEXT and NAMESPACE to display in mode-line."
  (replace-regexp-in-string "%C" context (replace-regexp-in-string "%N" namespace kubectx-mode-line-string-format t) t))

(defun kubectx-mode-line-update ()
  "Update kubectx mode-line string with current context and namespace."
  (interactive)
  (let ((ctx (split-string (kubectx-run-kubectl-command "config" "view" "--minify" "--output" "jsonpath={.contexts[0].context.cluster} {...contexts[0].context.namespace}"))))
    (setq kubectx-mode-line-string (kubectx-mode-line-string (car ctx) (cadr ctx)))
    (force-mode-line-update t)))

;;;###autoload
(define-minor-mode kubectx-mode
  "Switch kubernetes context and show info in the mode line."
  :global t
  :keymap kubectx-mode-map
  (when kubectx-mode-line-update-timer (cancel-timer kubectx-mode-line-update-timer))
  (if kubectx-mode
      (progn
        (unless global-mode-string (setq global-mode-string '("")))
        (add-to-list 'global-mode-string 'kubectx-mode-line-string t)
        (when (> kubectx-mode-line-update-interval 0)
          (setq kubectx-mode-line-update-timer
                (run-at-time nil kubectx-mode-line-update-interval
                             #'kubectx-mode-line-update)))
        (kubectx-mode-line-update))
    (setq global-mode-string
          (delq 'kubectx-mode-line-string global-mode-string))))

(provide 'kubectx-mode)

;;; kubectx-mode.el ends here
