# emacs-kubectx-mode-line

Display current kubectl context and namespace in Emacs mode line.

```lisp
(add-to-list 'load-path "~/.emacs.d/emacs-kubectx-mode-line")
(require 'kubectx)
```

Enable it with `M-x kubectx-mode` or `(kubectx-mode 1)`

The mode line will be updated every 10 seconds or what is defined in
`kubectx-update-interval`. 

If you run your shells in emacs you can also trigger the update with
`(kubectx-update)`, e.g like a prompt command:

```lisp
(setq kubectx-update-interval 0)
(add-hook 'comint-output-filter-functions (lambda (string) (kubectx-update)))
(kubectx-mode 1)
```


