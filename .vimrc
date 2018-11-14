" vim: nospell foldmethod=marker foldlevel=1 formatoptions=o
if !has('unix') || !has('nvim') 
    setg runtimepath-=~/.vim
    finish 
endif

exe 'let $MYVIMRC = '.string(expand('<sfile>'))

setg runtimepath-=~/.vim
exe 'setg runtimepath-='.expand('~/.vim')
setg runtimepath^=~/.vim

setg runtimepath-=~/.vim/after
exe 'setg runtimepath-='.expand('~/.vim/after')
setg runtimepath+=~/.vim/after

let &packpath = &runtimepath

" Variables: (these need to be global)
let g:mapleader      = ' '
let g:maplocalleader = ','
let g:markup_langs   = ['markdown', 'tex', 'gitcommit', 'mail']

let g:config_ftypes  = [
            \ 'yaml',
            \ 'gitconfig',
            \ 'cfg',
            \ 'dosini',
            \ 'conf',
            \ 'json',
            \ 'config'
            \ ]

let g:prog_langs = [
            \ 'sql',
            \ 'sh',
            \ 'rust',
            \ 'javascript',
            \ 'typescript',
            \ 'haskell',
            \ ]

" Only source after the above vars have been set!
so ~/.vim/plugin/plugins.vim

if has('termguicolors') && $TERM !=? 'linux'
    setg termguicolors
    "               Jn  Fb  Mr  Ap  Jn  Jl  Ag  St  Ot  Nv   Dc
    let s:months = [16, 16, 17, 18, 18, 18, 18, 17, 16, 15, 15]
    let s:this_month = strftime('%m')
    " remove leading spaces and leading zeros eg '  09' -> 9
    while (s:this_month[0] == ' ') || (s:this_month[0] == '0')
        let s:this_month = s:this_month[1:]
    endwhile
    " index from 0
    let s:this_month -= 1
    if (strftime('%H') <= (24 - s:months[s:this_month])) || (strftime('%H') >= s:months[s:this_month])
        colorscheme fabulous_dark
    else
        colorscheme fabulous
    endif
else
    colorscheme delek
endif
