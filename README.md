# emacs-kubectx-mode-line

Display current kubectl context and namespace in Emacs mode line.

```lisp
(add-to-list 'load-path "~/.emacs.d/emacs-kubectx-mode-line")
(require 'kubectx)
```

Enable it with `M-x kubectx-mode` or `(kubectx-mode 1)`

The mode line will be updated every 10 seconds or what is defined in
`kubectx-update-interval`. 

You can also trigger the update with emacsclient as a PROMPT_COMMAND:

```bash
export PROMPT_COMMAND="emacsclient -e '(kubectx-update)' &>/dev/null"
```
