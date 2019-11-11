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
(defvar kubectx-kubectl-command "kubectl" "Kubectl executable")
(defvar kubectx-mode-line-update-timer nil)
(defvar kubectx-mode-line-string "")
(defvar kubectx-mode-line-update-interval 10 "Number of seconds between background mode-line updates")
(defvar kubectx-mode-line-string-format " [kube:%C %N]" "String to display in mode-line (%C = context, %N = namespace)")
(defvar kubectx-mode-submap)
(define-prefix-command 'kubectx-mode-submap)
(define-key kubectx-mode-submap "c" 'kubectx-set-context)
(define-key kubectx-mode-submap "n" 'kubectx-set-namespace)
(defvar kubectx-mode-keybind (kbd "C-c C-k") "Keybind where kubectx-mode-submap is assigned")

(defun kubectx-run-kubectl-command (&rest args)
  "Run kubectl command"
  (with-temp-buffer
    (let ((default-directory "~"))
      (if (and (executable-find kubectx-kubectl-command)
               (= (apply 'call-process kubectx-kubectl-command nil t nil args) 0))
          (replace-regexp-in-string "\n\\'" "" (buffer-string))
        "n/a"))))

(defun kubectx-namespaces ()
  "Get list of namespaces"
  (split-string (kubectx-run-kubectl-command "get" "namespaces" "--output" "jsonpath={.items[*].metadata.name}")))

(defun kubectx-set-namespace (namespace)
  "Set current kubectl namespace"
  (interactive
   (list
    (completing-read "Namespace: " (kubectx-namespaces) nil t)))
  (kubectx-run-kubectl-command "config" "set-context" "--current" (format "--namespace=%s" namespace))
  (kubectx-mode-line-update))

(defun kubectx-contexts ()
  "Get list of contexts"
  (split-string (kubectx-run-kubectl-command "config" "get-contexts" "--output" "name")))

(defun kubectx-set-context (context)
  "set current kubectl context"
  (interactive
   (list
    (completing-read "Context: " (kubectx-contexts) nil t)))
  (kubectx-run-kubectl-command "config" "use-context" context)
  (kubectx-mode-line-update))

(defun kubectx-mode-line-string (context namespace)
  "Create kubectx string to display"
  (replace-regexp-in-string "%C" context (replace-regexp-in-string "%N" namespace kubectx-mode-line-string-format t) t))

(defun kubectx-mode-line-update ()
  "Update kubectx mode-line string with current context and namespace"
  (interactive)
  (let ((ctx (kubectx-run-kubectl-command "config" "current-context"))
        (ns (kubectx-run-kubectl-command "config" "view" "--minify" "--output" "jsonpath={..namespace}")))
    (setq kubectx-mode-line-string (kubectx-mode-line-string ctx ns))
    (force-mode-line-update t)))

(define-minor-mode kubectx-mode
  "Switch kubernetes context and show info in the mode line"
  :global t
  :keymap `((,kubectx-mode-keybind . ,kubectx-mode-submap))
  (when (not global-mode-string) (setq global-mode-string '("")))
  (when kubectx-mode-line-update-timer (cancel-timer kubectx-mode-line-update-timer))
  (if (not kubectx-mode)
      (setq global-mode-string
            (delq 'kubectx-mode-line-string global-mode-string))
    (add-to-list 'global-mode-string 'kubectx-mode-line-string t)
    (when (> kubectx-mode-line-update-interval 0)
      (setq kubectx-mode-line-update-timer
            (run-at-time nil kubectx-mode-line-update-interval
                         'kubectx-mode-line-update)))
    (kubectx-mode-line-update)))

(provide 'kubectx-mode)

;;; kubectx-mode.el ends here
