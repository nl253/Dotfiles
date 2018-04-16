" vim: nospell foldmethod=marker foldlevel=1 formatoptions=o

if !has('unix') | finish | endif

" Variables: (these need to be global)
let mapleader = " "
let maplocalleader = ","
let g:markup_langs = ['markdown', 'rst', 'tex']
let g:config_ftypes = [
            \ 'yaml',
            \ 'gitconfig',
            \ 'cfg',
            \ 'dosini',
            \ 'conf',
            \ 'json',
            \ 'config']
let g:prog_langs = [
            \ 'sql',
            \ 'python',
            \ 'sh',
            \ 'erlang',
            \ 'haskell',
            \ 'vim']

" Init Vim-Plug
call plug#begin('~/.vim/plugged')

" Add to $PATH bin dirs for package managers in case they aren't in $PATH already
fu! s:append_to_path(items)
    for i in filter(map(a:items, 'split(expand(v:val))[0]'), 'isdirectory(v:val) && !($PATH =~? v:val)')
        let $PATH = i.':'.$PATH
    endfor
endfu

silent call s:append_to_path([
            \ '~/.gem/ruby/*/bin',
            \ '~/.fzf/bin',
            \ '~/.cargo/bin',
            \ '~/.local/bin',
            \ '~/.stack/bin',
            \ '~/.cabal/bin',
            \ '~/.config/yarn/global/node_modules/.bin',
            \ '~/.local/share/fzf/bin',
            \ '~/.yarn/bin'
            \ ])

exec 'source '.expand('~/.vim').'/plugins.vim'
