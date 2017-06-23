
# THIS FILE MUST USE POSIX COMPLIANT SYNTAX
# IT IS SOURCED BY BOTH `zsh` AND `bash`

# SETUP 

# automatically link /tmp to ~/Downloads  {{{
[[ -d ~/Downloads ]] && [[ ! -L ~/Downloads ]] && rmdir ~/Downloads && ln -s /tmp ~/Downloads || [[ ! -L ~/Downloads ]] && echo '~/Downloads not empty - symlinking to /tmp failed.'
# }}}

# if nvim, link to ~/.vimrc
[[ -x $(which nvim) ]] && [[ ! -e ~/.config/nvim/init.vim ]] && ln -s ~/.vimrc ~/.config/nvim/init.vim

# PYENV {{{
#if [[ ! -e ~/.pyenv ]]; then 
  #curl -L https://raw.githubusercontent.com/pyenv/pyenv-installer/master/bin/pyenv-installer | bash 
  #exec $SHELL 
  #pyenv update 
  #pyenv install 3.6.1 
  #git clone https://github.com/pyenv/pyenv-virtualenv.git $(pyenv root)/plugins/pyenv-virtualenv 
  #cd 
  #[[ ! -f ~/.python-version ]] && pyenv global 3.6.1 system
#fi
# }}}
