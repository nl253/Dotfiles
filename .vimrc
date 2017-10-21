" vim: nospell foldmethod=marker foldlevel=1 formatoptions=o 

" " Exit if not UNIX
" if !has('unix') 
     " !echo "You need to be running a UNIX-like system for this script to work."
    " finish
" endif 

" VARIABLES:
let g:VIMDIR = expand('~/.vim')
let $MYVIMRC = expand('~/.vimrc')

if !filereadable(g:VIMDIR.'/plugins.vim')
    echo system('mkdir -p '.g:VIMDIR.' && curl -fLo '.g:VIMDIR.'/plugins.vim https://raw.githubusercontent.com/nl253/Dotfiles/master/.vim/plugins.vim')
endif

" KEYBINDINGS:
let mapleader = " "
let maplocalleader = ","

" PLUG:
let g:PLUG_FILE = expand('~/.vim/autoload/plug.vim')

" Plug - Download if missing:
if !filereadable(g:PLUG_FILE) && executable('curl')
	echo system('mkdir -p $(dirname '.g:PLUG_FILE.') && curl -flo '.g:PLUG_FILE.' https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim')
	execute 'source '.g:PLUG_FILE
	PlugInstall
elseif !filereadable(g:PLUG_FILE) 
	finish
endif

call plug#begin('~/.vim/plugged')

" Add to $PATH bin dirs for package managers in case they aren't in $PATH already
for i in ['~/node_modules/.bin', '~/.gem/ruby/*/bin'] + split(expand('~/.{fzf,cargo,local,stack,cabal}/bin'))
	let bin_dir = expand(i)
	if !empty(bin_dir) && !($PATH =~ bin_dir)
		let $PATH = expand(i).':'.$PATH
	endif
endfor

unlet bin_dir

exec 'source '.g:VIMDIR.'/plugins.vim' 
