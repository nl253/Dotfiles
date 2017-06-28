
# PYENV 
export PYENV_ROOT=~/.zplug/repos/pyenv/pyenv 
export PATH=$PYENV_ROOT/bin:$PATH 
eval "$(pyenv init -)" 
eval "$(pyenv virtualenv-init -)"

export HISTFILE=~/.zsh_history
export SAVEHIST=10000
# Enable Ctrl-x-e to edit command line

autoload -U edit-command-line
# Emacs style
zle -N edit-command-line
bindkey '^xe' edit-command-line
bindkey '^x^e' edit-command-linexport 
