"vim: nospell foldmethod=marker foldlevel=1 formatoptions=o 

" Exit if not UNIX 
if ! has('unix') 
     !echo "You need to be running a UNIX-like system for this script to work."
    finish
endif 

" VARIABLES  
let g:VIMDIR = !has('nvim') ? expand('~/.vim/') : expand('~/.config/nvim/') 

if ! has('nvim')
    "this is set automatically in `Neovim`
    let $MYVIMRC = expand('~/.vimrc')
endif

if ! filereadable(g:VIMDIR.'plugins.vim')
    echo system('mkdir -p '.g:VIMDIR.' && curl -fLo '.g:VIMDIR.'plugins.vim https://raw.githubusercontent.com/nl253/Dotfiles/master/.vim/plugins.vim')
endif

" KEYBINDINGS 
let mapleader = " "
let maplocalleader = ","

exec 'source '.g:VIMDIR.'plugins.vim'
