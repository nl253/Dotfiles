" vim: nospell foldmethod=marker foldlevel=1 formatoptions=o
if !has('unix') | finish | endif

let $MYVIMRC = expand('~/.vimrc')

setg runtimepath-=~/.vim
exe 'setg runtimepath-='.expand('~/.vim')
setg runtimepath^=~/.vim

setg runtimepath-=~/.vim/after
exe 'setg runtimepath-='.expand('~/.vim/after')
setg runtimepath+=~/.vim/after

let &packpath = &runtimepath

setg termguicolors

" Variables: (these need to be global)
let g:mapleader      = ' '
let g:maplocalleader = ','
let g:markup_langs   = ['markdown', 'tex']

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
            " \ 'vim',

let g:markdown_fenced_languages = g:prog_langs + ['java']
let g:rst_syntax_code_list      = g:markdown_fenced_languages

" Only source after the above vars have been set!
so ~/.vim/plugin/plugins.vim
