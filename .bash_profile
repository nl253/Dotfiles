#!/usr/bin/env bash

# ~/.bash_profile
 
echo "${HOME}/.bash_profile loaded" # indicate

[[ -f ~/.extend.bash_profile ]] && . ~/.extend.bash_profile

[[ -f ~/.bashrc ]] && . ~/.bashrc

#export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'
# bash-completion {{{

source_bash_completion() {
  local f
  [[ $BASH_COMPLETION ]] && return 0
  for f in /{etc,usr/share/bash-completion}/bash_completion; do
    if [[ -r $f ]]; then
      . "$f"
      return 0
    fi
  done
  [[ -f /opt/local/etc/profile.d/bash_completion.sh ]] && . /opt/local/etc/profile.d/bash_completion.sh
}

source_bash_completion 

 # }}}

# Python virtual env manager  {{{
if [[ -x $(which pyenv) ]]; then 
  export PYENV_ROOT="$HOME/.pyenv"
  export PATH="$PYENV_ROOT/bin:$PATH"
  eval "$(pyenv init -)"
  source "$(pyenv root)/completions/pyenv.bash"
  eval "$(pyenv virtualenv-init -)"
fi

# UTF-8 {{{
export LC_ALL='en_GB.UTF-8'
export LANG='en_GB' # }}}

