---
name: emax
description: Generate and run elisp in the running Emacs instance via emacsclient. Use for opening files in dired, showing magit diffs/logs, or any Emacs action. Supports --scratch to insert into *scratch* instead of direct eval.
---

Arguments: $ARGUMENTS
Current directory: !`pwd`

## Modes

Parse $ARGUMENTS for a leading flag:
- `--scratch` — insert the generated elisp into `*scratch*` for the user to eval manually
- `--eval` (default, or no flag) — eval directly via emacsclient, targeting `pvr/agent-shell-frame`

Strip the flag before interpreting the rest of $ARGUMENTS as the intent.

## Direct eval (default)

```bash
emacsclient -e '(with-selected-frame pvr/agent-shell-frame ELISP)'
```

`pvr/agent-shell-frame` is set by the agent-shell mode hook — it's always the frame where agent-shell launched.

## Scratch buffer

```bash
emacsclient -e '(with-current-buffer (get-buffer-create "*scratch*")
  (goto-char (point-max))
  (insert "\n;; /emax\nELISP\n")
  (display-buffer (current-buffer)))'
```

The user then evals with `C-x C-e` in their chosen frame.

## Generating the elisp

Use the intent from $ARGUMENTS to produce the right elisp. Examples:

| Intent | Elisp |
|---|---|
| open dired here | `(dired "/abs/path")` |
| open file | `(find-file "/abs/path/file")` |
| magit status | `(magit-status "/abs/path")` |
| magit diff unstaged | `(magit-diff-unstaged)` |
| magit diff staged | `(magit-diff-staged)` |
| magit log last hour | `(magit-log-all (list "--since=1 hour ago") nil)` |
| magit log all | `(magit-log-all nil nil)` |

Always use absolute paths. Use the captured `pwd` above as the base.
