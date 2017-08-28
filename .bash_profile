
# ~/.bash_profile
 
echo "${HOME}/.bash_profile loaded" # indicate

[[ -f ~/.bashrc ]] && . ~/.bashrc

# bash-completion {{{

#source_bash_completion 

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).

if ! shopt -oq posix; then
  if [ -f /usr/share/bash-completion/bash_completion ]; then
    . /usr/share/bash-completion/bash_completion
  elif [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
  fi
fi


 # }}}

export PATH="$HOME/.cargo/bin:$PATH"
