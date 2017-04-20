

export PS1="[%* - %D] %d %% "

############### #!/usr/bin/env zsh

export PATH="/usr/local/sbin:/usr/local/bin:/usr/bin/site_perl:/usr/bin/vendor_perl:/usr/bin/core_perl:/usr/bin/:$PATH:"
[[ -e ~/miniconda3 ]] && export PATH="${PATH}:${HOME}/miniconda3/bin"

for file in ~/.shells/* ; do  # Custom dirs with general shell configuration
  [[ -f $file ]] && source $file
done

for file in ~/.zsh/* ; do      # Custom dirs with zsh specific configuration
  [[ -f $file ]] && source $file
done

#if [[ -x /usr/bin/pyenv ]]; then # repladed temporarily by conda
#export PYENV_ROOT="$HOME/.pyenv"
#export PATH="$PYENV_ROOT/bin:$PATH"
#eval "$(pyenv init -)"
#source "$(pyenv root)/completions/pyenv.zsh"
#fi
