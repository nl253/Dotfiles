
" Exit if not UNIX TODO
if ! has('unix') 
    !echo "You need to be running a UNIX-like system for this script to work."
    exit 
endif 

" VARIABLES  
" VIMDIR - NVIM/VIM 
let g:VIMDIR = expand('~/.vim/')

" MARKUP languages you actively use  
let g:MARKUP = [ 'markdown', 'vimwiki', 'rst' ]

" PROGRAMMING LANGUAGES you code in 
let g:PROGRAMMING =  [ 'xhtml', 'html', 'css', 'javascript', 'python', 'php', 'sql', 'sh', 'zsh' ]

if ! has('nvim')
    let $MYVIMRC = expand('~/.vimrc')  " set automatically in nvim
    syntax enable  " enable sane-defaults (already present in nvim)
    filetype plugin indent on
endif

if ! filereadable(g:VIMDIR.'plugins.vim')
    echo system('mkdir -p '.g:VIMDIR)
    echo system('curl -fLo '.g:VIMDIR.'plugins.vim https://raw.githubusercontent.com/nl253/Dotfiles/master/.vim/plugins.vim')
endif

exec 'source '.g:VIMDIR.'plugins.vim'

" vim: nospell foldmethod=marker foldlevel=1 formatoptions=o 

