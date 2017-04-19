#!/usr/bin/env bash

# ~/.bash_profile
 
# Uncomment both lines to change shell to zsh
# export SHELL=/bin/zsh 
# exec /bin/zsh -l

echo "~/.bash_profile loaded" # indicate

[[ -f ~/.extend.bash_profile ]] && . ~/.extend.bash_profile

[[ -f ~/.bashrc ]] && . ~/.bashrc

# bash-completion
[[ -f /opt/local/etc/profile.d/bash_completion.sh ]] && . /opt/local/etc/profile.d/bash_completion.sh

if [ -x /usr/bin/pyvenv ]; then
  export PYENV_ROOT="$HOME/.pyenv"
  export PATH="$PYENV_ROOT/bin:$PATH"
  eval "$(pyenv init -)"
fi

source_bash_completion() {
  local f
  [[ $BASH_COMPLETION ]] && return 0
  for f in /{etc,usr/share/bash-completion}/bash_completion; do
    if [[ -r $f ]]; then
      . "$f"
      return 0
    fi
  done
}

source_bash_completion

# UTF-8 all the way.
export LC_ALL='en_GB.UTF-8'
export LANG='en_GB'
