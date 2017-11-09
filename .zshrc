
# ~/.zshrc

fpath+=~/.zfunc

# Load generic shell configuration (aliases, variables)
for i in ~/.shells/*.sh; do
	[[ -f $i ]] && source $i 
done 

# If ~/.home or ~/.pc detected, don't source this script!
[[ -e ~/.nozsh ]] && echo -e "~/.nozsh detected - .zshrc not sourced" && return 0

 # don't nice background tasks
setopt NO_BG_NICE
setopt NO_HUP
setopt NO_LIST_BEEP

# allow functions to have local options
setopt LOCAL_OPTIONS

 # allow functions to have local traps
setopt LOCAL_TRAPS
setopt HIST_IGNORE_ALL_DUPS
setopt HIST_REDUCE_BLANKS
setopt COMPLETE_IN_WORD
setopt NO_BEEP

# {a-c} -> a b c
setopt BRACE_CCL
setopt LIST_TYPES

# Compact completion
setopt AUTO_LIST
setopt AUTO_MENU
setopt AUTO_PARAM_SLASH
setopt AUTO_PARAM_KEYS
setopt LIST_PACKED
setopt LIST_ROWS_FIRST
setopt AUTO_PUSHD
setopt PUSHD_MINUS
setopt PUSHD_IGNORE_DUPS

setopt GLOB_DOTS

# Write the history file in the ':start:elapsed;command' format.
setopt EXTENDED_HISTORY

 # Write to the history file immediately, not when the shell exits.
setopt INC_APPEND_HISTORY

 # Share history between all sessions.
setopt SHARE_HISTORY

 # Expire a duplicate event first when trimming history.
setopt HIST_EXPIRE_DUPS_FIRST

 # Do not record an event that was just recorded again.
setopt HIST_IGNORE_DUPS

 # Delete an old recorded event if a new event is a duplicate.
setopt HIST_IGNORE_ALL_DUPS

 # Do not display a previously found event.
setopt HIST_FIND_NO_DUPS

 # Do not record an event starting with a space.
setopt HIST_IGNORE_SPACE

 # Do not write a duplicate event to the history file.
setopt HIST_SAVE_NO_DUPS

 # Do not execute immediately upon history expansion.
setopt HIST_VERIFY

# Check original command in alias completion
setopt COMPLETE_ALIASES

# Expand globs when completion
# setopt GLOB_COMPLETE
# Add "/" if completes directory
setopt MARK_DIRS

unsetopt CORRECT_ALL

setopt COMPLETE_ALIASES

# unfortunately this is necessary
setopt MENU_COMPLETE

# enable completion
zmodload zsh/complist
autoload -Uz compinit && compinit

zstyle ':completion:::::' completer _complete _approximate
zstyle ':completion:*:descriptions' format "- %d -"
# zstyle ':completion:*:corrections' format "- %d - (errors %e})"
zstyle ':completion:*:default' list-prompt '%S%M matches%s'
zstyle ':completion:*' group-name ''
zstyle ':completion:*:manuals' separate-sections true
zstyle ':completion:*:manuals.(^1*)' insert-sections true
# zstyle ':completion:*' verbose yes
zstyle -e ':completion:*:approximate:*' max-errors 'reply=( $(( ($#PREFIX + $#SUFFIX) / 3 )) )'
zstyle ':completion::approximate*:*' prefix-needed false
zstyle ':completion:*' matcher-list '' 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=*' 'l:|=* r:|=*'
#zstyle ':completion:*' menu select

if [[ "${terminfo[kcbt]}" != "" ]]; then
  bindkey "${terminfo[kcbt]}" reverse-menu-complete   # [Shift-Tab] - move through the completion menu backwards
else
	bindkey '^[[Z' reverse-menu-complete
fi

autoload edit-command-line
zle -N edit-command-line
bindkey '^x^e' edit-command-line

export WORDCHARS='*?_-.[]~=/&;!#$%^(){}<>'

backward-kill-dir () {
	local WORDCHARS=${WORDCHARS/\/}
	zle backward-kill-word
}

zle -N backward-kill-dir
bindkey '^[^?' backward-kill-dir

tcsh-backward-word() {
  local WORDCHARS="${WORDCHARS:s@/@}"
  zle backward-word
}

zle -N tcsh-backward-word

# this is necessary because the string "vim" is present in $EDITOR zsh will attempt to set ZLE to use vi mode

bindkey -e
# vim: foldmethod=marker sw=2 ts=2 nowrap 
