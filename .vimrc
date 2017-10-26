" vim: nospell foldmethod=marker foldlevel=1 formatoptions=o 

let s:vimdir = expand('~/.vim')

if !filereadable(s:vimdir.'/plugins.vim') && executable('curl')
	silent call mkdir(s:vimdir, 'p')
    echo system('curl -fLo '.s:vimdir.'/plugins.vim https://raw.githubusercontent.com/nl253/Dotfiles/master/.vim/plugins.vim')
elseif !filereadable(s:vimdir.'/plugins.vim')
	finish
endif

let mapleader = " "
let maplocalleader = ","

" PLUG:
let s:plug_file = expand('~/.vim/autoload/plug.vim')

" Plug - Download if missing

" GNU/Linux
if has('unix') && !filereadable(s:plug_file) && executable('curl')
	silent call mkdir(expand(s:plug_file.':p:h'), 'p')
	echo system('curl -flo '.s:plug_file.' https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim')
	execute 'source '.s:plug_file
	PlugInstall

" Windows
elseif !filereadable(s:plug_file) && has('win32')
	if !(&shell =~# 'powershell')
		set shell=powershell.exe
	endif
	silent call system('md ~\vimfiles\autoload; $uri = "https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim"; (New-Object Net.WebClient).DownloadFile($uri, $ExecutionContext.SessionState.Path.GetUnresolvedProviderPathFromPSPath("~\vimfiles\autoload\plug.vim"))')

" All other
elseif !filereadable(s:plug_file) 
	finish
endif

" Init Vim-Plug 

call plug#begin('~/.vim/plugged')

" Add to $PATH bin dirs for package managers in case they aren't in $PATH already

for i in filter(map([
		\ '~/node_modules/.bin', 
		\ '~/.gem/ruby/*/bin', 
		\ '~/.rbenv/bin', 
		\ '~/.nvm/versions/node/*/bin', 
		\ '~/.fzf/bin', 
		\ '~/.cargo/bin', 
		\ '~/.local/bin', 
		\ '~/.stack/bin', 
		\ '~/.cabal/bin', 
		\ '~/.config/composer/bin', 
		\ '~/.local/share/fzf/bin', 
		\ '~/.config/yarn/global/node_modules/.bin', 
		\ '~/.yarn/bin', 
		\ '~/.gvm/bin'], 'expand(v:val)'), '!empty(v:val) && !($PATH =~ v:val)')
	let $PATH = expand(i).':'.$PATH
endfor

if filereadable(s:vimdir.'/plugins.vim')
	exec 'source '.s:vimdir.'/plugins.vim' 
endif
