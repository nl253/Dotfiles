#
# ~/.bash_profile
#

echo "~/.bash_profile loaded"

[[ -f ~/.extend.bash_profile ]] && . ~/.extend.bash_profile

[[ -f ~/.bashrc ]] && . ~/.bashrc

# bash-completion
if [ -f /opt/local/etc/profile.d/bash_completion.sh ]; then
    . /opt/local/etc/profile.d/bash_completion.sh
fi

export PYENV_ROOT="$HOME/.pyenv"
export PATH="$PYENV_ROOT/bin:$PATH"
eval "$(pyenv init -)"
