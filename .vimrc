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
		\ '~/.yarn/bin'], 'expand(v:val)'), '!empty(v:val) && !($PATH =~ v:val)')
	let $PATH = expand(i).':'.$PATH
endfor

function! g:InstallPackages(package_mangager, install_command, search_path)
	let s:search_path = expand(a:search_path)

	if exists(execute('g:'.a:package_mangager.'_packages'))
		let s:package_mangager_packages = execute('g:'.a:package_mangager.'_packages')
	else 
		return
	endif
	for i in [a:package_mangager, a:search_path, a:install_command]
		if !type(i) == 2
			return
		endif
	endfor
	if !executable(a:package_mangager) 
		return
	endif
	let s:search_path = split(expand(a:search_path))[0]
	if !type(s:package_mangager_packages) == 3 
		return
	endif
	if !len(s:package_mangager_packages) > 0 
		return
	endif
	if !executable('bash')
		return
	endif

	let s:packages = join(map(split(expand(join([s:search_path, '*'], '/'))), 'fnamemodify(v:val, ":t")'))

	for i in filter(s:package_mangager_packages, '!(s:packages =~ v:val)')
		silent call system('bash -c "'.a:package_mangager.' '.a:install_command.' '.i.' &"')
	endfor
endfunction


if filereadable(s:vimdir.'/plugins.vim')
	exec 'source '.s:vimdir.'/plugins.vim' 
endif
