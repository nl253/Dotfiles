# if ! shopt -oq posix; then
  # if [[ -f /usr/share/bash-completion/bash_completion ]]; then
    # . /usr/share/bash-completion/bash_completion
  # elif [[ -f /etc/bash_completion ]]; then
    # . /etc/bash_completion
  # fi
# fi 

[[ -x $(command which pandoc 2>/dev/null) ]] && eval "$(pandoc --bash-completion)"
[[ -x $(command which stack 2>/dev/null) ]] && eval "$(stack --bash-completion-script stack)"
