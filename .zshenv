
# ~/.zshenv sourced by zsh(1) 

if ((SHLVL = 0)) || ([[ -n TMUX ]] && ((SHELL = 1))); then
    [[ -f ~/.profile ]] && source ~/.profile
fi
