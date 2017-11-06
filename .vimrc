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

function! g:AddToPATH(items)
	for i in filter(map(a:items, 'expand(v:val)'), '!empty(v:val) && !($PATH =~ v:val)')
		let $PATH = expand(i).':'.$PATH
	endfor
endfunction

silent call g:AddToPATH(
			\ [
			\ '~/node_modules/.bin', 
			\ '~/.gem/ruby/*/bin', 
			\ '~/.fzf/bin', 
			\ '~/.cargo/bin', 
			\ '~/.local/bin', 
			\ '~/.stack/bin', 
			\ '~/.cabal/bin', 
			\ '~/go/bin', 
			\ '~/.anaconda3/bin', 
			\ '~/anaconda3/bin', 
			\ '~/.config/composer/bin', 
			\ '~/.local/share/fzf/bin', 
			\ '~/.yarn/bin'
			\ ])

function! g:InstallPackages(package_mangager, install_command, query_command, packages)

	for i in filter([a:package_mangager, a:install_command], '!type(v:val) == 2')
		echom '[ERROR] Bad type of '.i
		return
	endfor

	if !executable(a:package_mangager) 
		echom '[ERROR] Package manger '.a:package_mangager.' not executable!'
		return
	endif

	if !type(a:packages) == 3 
		echom '[ERROR] Bad type of packages expected List<String> got '.a:packages
		return
	endif

	if len(a:packages) == 0 
		echom '[ERROR] no packages passed, got an empty list!'
		return
	endif

	if !executable('bash')
		echom '[ERROR] bash not executable!'
		return
	endif

	let s:packages = system(a:query_command)

	for i in filter(a:packages, '!(s:packages =~? v:val)')
		echom '[INFO] installing '.i
		silent call system('bash -c "'.a:package_mangager.' '.a:install_command.' '.i.' &"')
	endfor
endfunction


if filereadable(s:vimdir.'/plugins.vim')
	exec 'source '.s:vimdir.'/plugins.vim' 
endif
