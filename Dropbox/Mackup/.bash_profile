#
# ~/.bash_profile
#

echo "~/.bash_profile loaded"

[[ -f ~/.extend.bash_profile ]] && . ~/.extend.bash_profile
[[ -f ~/.bashrc ]] && . ~/.bashrc

export PYENV_ROOT="$HOME/.pyenv"
export PATH="$PYENV_ROOT/bin:$PATH"
eval "$(pyenv init -)"
