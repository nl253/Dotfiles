" vim: nospell foldmethod=marker foldlevel=1 formatoptions=o

if has('win32') | finish | endif
	
let s:vimdir = expand('~/.vim')

if !filereadable(s:vimdir.'/plugins.vim') && executable('curl')
    silent call mkdir(s:vimdir, 'p')
    echo system('curl -fLo '.s:vimdir.'/plugins.vim https://raw.githubusercontent.com/nl253/Dotfiles/master/.vim/plugins.vim')
elseif !filereadable(s:vimdir.'/plugins.vim')
    finish
endif

let mapleader = " "
let maplocalleader = ","
let s:plug_file = expand('~/.vim/autoload/plug.vim')

" Plug - Download if missing

" GNU/Linux
if has('unix') && !filereadable(s:plug_file) && executable('curl')
    silent call mkdir(fnamemodify(s:plug_file, ':p:h'), 'p')
    echo system('curl -flo '.s:plug_file.' https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim')
	execute 'source '.s:plug_file
	PlugInstall
elseif !filereadable(s:plug_file)
	finish
endif

" Init Vim-Plug
call plug#begin('~/.vim/plugged')

" Add to $PATH bin dirs for package managers in case they aren't in $PATH already
function! g:AddToPATH(items)
	for i in filter(map(a:items, 'expand(v:val)'), 'isdirectory(v:val) && !($PATH =~? v:val)')
		let $PATH = i.':'.$PATH
	endfor
endfunction

silent call g:AddToPATH([
			\ '~/.gem/ruby/*/bin',
			\ '~/.fzf/bin',
			\ '~/.cargo/bin',
			\ '~/.local/bin',
			\ '~/.stack/bin',
			\ '~/.cabal/bin',
			\ '~/.local/share/fzf/bin',
			\ '~/.yarn/bin'
			\ ])

if filereadable(s:vimdir.'/plugins.vim') | exec 'source '.s:vimdir.'/plugins.vim' | endif
