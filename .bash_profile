
if ((SHLVL <= 1)); then
	[[ -f ~/.profile ]] && source ~/.profile
	[[ -f ~/.bashrc ]] && source ~/.bashrc
fi
