# kubectx-mode

Switch [kubectl](https://kubernetes.io/docs/reference/kubectl/overview/) context and namespace and display current settings in Emacs mode line.

## Install

Clone this repository and add to `load-path`:

```lisp
(add-to-list 'load-path "~/.emacs.d/emacs-kubectx-mode")
(require 'kubectx-mode)
(kubectx-mode 1)
```

### Keybindings

| Keys        | Description               |
| ---------   | ------------------------- |
| `C-c C-k c` | Set kubectl context       |
| `C-c C-k n` | Set kubectl namespace     |

### Mode line

The variable `kubectx-mode-line-string-format` defines what to display in the mode line,
and mode line will be updated every 5 seconds or as often defined in `kubectx-mode-line-update-interval`.

## Acknowledgements

`kubectx-mode` is inspired by [kubectx](https://github.com/ahmetb/kubectx/) by [ahmetb](https://github.com/ahmetb/) and provides the same functionality in Emacs.

If you also change `kubectl` context or namespace in a shell, you can trigger a mode line update in Emacs with e.g. `emacsclient` as a `PROMPT_COMMAND`:

```bash
export PROMPT_COMMAND="emacsclient -e '(kubectx-mode-line-update)' &>/dev/null"
```
