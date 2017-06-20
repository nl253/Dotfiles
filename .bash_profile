
# ~/.bash_profile
 
echo "${HOME}/.bash_profile loaded" # indicate

[[ -f ~/.bashrc ]] && . ~/.bashrc

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
