[[ -f ~/.profile ]] && source ~/.profile

# not run with -i (interactive) but with tmux so make interactive anyway
[[ ! $- =~ i ]] && [[ -n $TMUX ]] && [[ -f $ZDOTDIR/.zshrc ]] && source $ZDOTDIR/.zshrc
