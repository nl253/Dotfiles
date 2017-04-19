#!/usr/bin/env bash

# ~/.bash_profile

echo "~/.bash_profile loaded"

[[ -f ~/.extend.bash_profile ]] && . ~/.extend.bash_profile

[[ -f ~/.bashrc ]] && . ~/.bashrc

# bash-completion
if [ -f /opt/local/etc/profile.d/bash_completion.sh ]; then
  . /opt/local/etc/profile.d/bash_completion.sh
fi

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

# enable bash completion in interactive shells
if ! shopt -oq posix; then
  if [ -f /usr/share/bash-completion/bash_completion ]; then
    . /usr/share/bash-completion/bash_completion
  elif [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
  fi
fi

# UTF-8 all the way.
export LC_ALL='en_GB.UTF-8'
export LANG='en_GB'

source ~/.xsh
