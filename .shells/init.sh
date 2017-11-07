# make working directories

shell-init() {

	# make working directories
	for i in ~/Documents/{Go,Python,Rust,Computer-Science,Java,JavaScript,Maths}; do
		[[ ! -e $i ]] && (mkdir -p $i) &
	done

	# link nvim config file (in a subshell) to vim config file if missing
	if [[ -x $(which nvim 2>/dev/null) ]] && [[ ! -e ~/.config/nvim/init.vim ]]; then
		if [[ -e ~/.vimrc ]]; then
			(
				cd
				mkdir -p ~/.config/nvim ~/.local/share/nvim/site/autoload
				ln -s ~/.vim/autoload/plug.vim ~/.local/share/nvim/site/autoload/
				ln -s ~/.vimrc ~/.config/nvim/init.vim
			) &
		fi
	fi
}
