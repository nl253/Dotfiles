
# ~/.zshrc 
# refer to zshoptions(1)

[[ -f ~/.config/sh/init.sh ]] && source ~/.config/sh/init.sh

export PS1=" %d ~> " # normalise prompt in case somthing goes wrong
export WORDCHARS='*?_-.[]~=/&;!#$%^(){}<>'

# this is necessary because the string "vim" is present in $EDITOR zsh will attempt to set ZLE to use vi mode
setopt EMACS

fpath+=~/.config/zsh/zfunc

# # setopt LIST_PACKED
# # setopt LIST_ROWS_FIRST
# setopt ALWAYS_TO_END
# setopt AUTO_CONTINUE
# setopt AUTO_PARAM_KEYS
# setopt AUTO_PUSHD
# setopt BAD_PATTERN
# setopt BANG_HIST
setopt BRACE_CCL
setopt CDABLE_VARS
# setopt CHASE_DOTS
# setopt CHASE_LINKS
# setopt COMBINING_CHARS
setopt COMPLETE_ALIASES
setopt COMPLETE_IN_WORD
# setopt EXTENDED_HISTORY
# setopt GLOB_ASSIGN
setopt GLOB_DOTS
# setopt GLOB_STAR_SHORT
# setopt GLOB_SUBST
setopt HIST_EXPIRE_DUPS_FIRST
setopt HIST_FCNTL_LOCK
setopt HIST_FIND_NO_DUPS
setopt HIST_IGNORE_ALL_DUPS
setopt HIST_IGNORE_DUPS
setopt HIST_IGNORE_SPACE
setopt HIST_NO_FUNCTIONS
setopt HIST_NO_STORE
setopt HIST_REDUCE_BLANKS
setopt SHARE_HISTORY
setopt HIST_SAVE_NO_DUPS
setopt HIST_SUBST_PATTERN
setopt HIST_VERIFY
setopt INC_APPEND_HISTORY
# setopt KSH_GLOB
# setopt LOCAL_OPTIONS
# setopt LOCAL_TRAPS
# setopt LONG_LIST_JOBS
# setopt MARK_DIRS
# setopt MENU_COMPLETE
setopt NO_BEEP
setopt NO_BG_NICE
setopt NO_HIST_BEEP
setopt NO_HUP
setopt NO_LIST_BEEP
# setopt POSIX_IDENTIFIERS
# setopt PRINT_EXIT_VALUE
# setopt PUSHD_IGNORE_DUPS
# setopt PUSHD_MINUS
# setopt PUSHD_TO_HOME
# setopt RC_EXPAND_PARAM
# setopt WARN_CREATE_GLOBAL
autoload -Uz compinit && compinit
zmodload zsh/computil
zmodload zsh/complete
zmodload zsh/complist
zmodload zsh/regex
# zmodload zsh/zutil

zstyle ':completion:::::' completer _complete _approximate
zstyle ':completion:*:descriptions' format " >> %d"
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

# vim: foldmethod=marker sw=2 ts=2 nowrap 
