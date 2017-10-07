# Readline

## learn 

- `\e*` insert-completions
- `\e\\` delete-horizontal-space
- `\e\C-]` character-search-backward
- `\e\C-e` shell-expand-line
- `\e.` insert-last-argument
- `\C-x\C-x` exchange-point-and-mark
- `\C-t` transpose-chars
- `\ec` capitalize-word
- `\C-]` character-search
- `\e ` set-mark
- `\er` revert-line

- `\e#` insert-comment

## macros !!!

- `\C-x(` start-kbd-macro
- `\C-x)` end-kbd-macro
- `\C-xe` call-last-kbd-macro

- `\e\C-g` abort

## completion !!!

- `\eg` glob-complete-word
- `\C-x*` glob-expand-word
- `\C-xg` glob-list-expansions

- `\e!` complete-command
- `\e/` complete-filename
- `\e@` complete-hostname
- `\e{` complete-into-braces
- `\e~` complete-username
- `\e$` complete-variable

- `\C-x!` possible-command-completions
- `\C-x$` possible-variable-completions
- `\C-x~` possible-username-completions
- `\C-x@` possible-hostname-completions
- `\C-x/` possible-filename-completions

- `\e\C-i` dynamic-complete-history

## yank

- `\e.` yank-last-arg
- `\e\C-y` yank-nth-arg
- `\ey` yank-pop

## ???

- `\C-q` quoted-insert
- `\C-v` quoted-insert

- `\e[200~` bracketed-paste-begin

<!-- vim foldmethod=marker foldmarker={,} nospell
