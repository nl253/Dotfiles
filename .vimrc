
" Exit if not UNIX 
if ! has('unix') 
    !echo "You need to be running a UNIX-like system for this script to work."
    exit 
endif 

" VIMDIR - NVIM/VIM 
if has('nvim')
    let g:VIMDIR = expand('~/.config/nvim/')
else " if vim
    let g:VIMDIR = expand('~/.vim/')
    let $MYVIMRC = expand('~/.vimrc')  " set automatically in nvim
    syntax enable  " enable sane-defaults (already present in nvim)
    filetype plugin indent on
endif

let g:CORE_DIR = g:VIMDIR.'core/'

for i in split(expand('{options,variables,init,functions,autocommands}.vim')) 
    execute 'source '.g:CORE_DIR.i
endfor

let g:EXTRAS_DIR = g:VIMDIR.'extras/'

for file in split(glob(g:EXTRAS_DIR.'*.vim'))
    if filereadable(file)
        execute 'source '.file
    endif
endfor

" vim: nospell foldmethod=marker foldlevel=1 formatoptions=o 
