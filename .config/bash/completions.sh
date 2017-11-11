
# ~/.config/bash/completions.sh completions to be soured by bash(1)
# Can also be sourced by zsh if you autoload bashcompinit, see zshcompsys(1)

[[ -x $(command which pandoc 2>/dev/null) ]] && eval "$(pandoc --bash-completion)"
[[ -x $(command which stack 2>/dev/null) ]] && eval "$(stack --bash-completion-script stack)"
