# kubectx-mode

Switch kubectl context and namespace and display current setting in Emacs mode line.

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

The variable `kubectx-mode-line-string-format` defines what to display in the mode line, and
mode line will be updated every 5 seconds or what is defined in
`kubectx-mode-line-update-interval`.

You can also trigger the update with `emacsclient` as a `PROMPT_COMMAND`:

```bash
export PROMPT_COMMAND="emacsclient -e '(kubectx-mode-line-update)' &>/dev/null"
```
