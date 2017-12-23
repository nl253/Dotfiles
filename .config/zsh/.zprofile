
# $ZDOTDIR/.profile read by zsh(1) when run as login shell

[[ -f ~/.profile ]] && source ~/.profile

# not run with -i (interactive) but with TMUX so make interactive anyway
[[ ! $- =~ i ]] && [[ -n $TMUX ]] && [[ -f $ZDOTDIR/.zshrc ]] && source $ZDOTDIR/.zshrc
