zstyle ':completion:*' menu select
bindkey '^[[Z' reverse-menu-complete

zstyle ':completion:*' matcher-list '' 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=*' 'l:|=* r:|=*'

bindkey "${terminfo[khome]}" beginning-of-line
bindkey "${terminfo[kend]}" end-of-line

# Enable Ctrl-x-e to edit command line
autoload edit-command-line
zle -N edit-command-line
bindkey '^x^e' edit-command-line

backward-kill-dir () {
	local WORDCHARS=${WORDCHARS/\/}
	zle backward-kill-word
}

zle -N backward-kill-dir
bindkey '^[^?' backward-kill-dir

# by default: export WORDCHARS='*?_-.[]~=/&;!#$%^(){}<>'
# we take out the slash, period, angle brackets, dash here.

export WORDCHARS='*?_[]~=&;!#$%^(){}'

tcsh-backward-word() {
  local WORDCHARS="${WORDCHARS:s@/@}"
  zle backward-word
}

zle -N tcsh-backward-word
