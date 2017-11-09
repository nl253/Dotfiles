
if ((SHLVL = 0)) || ([[ -n TMUX ]] && ((SHELL = 1))); then
	[[ -f ~/.profile ]] && source ~/.profile
	[[ -f ~/.bashrc ]] && source ~/.bashrc
fi
