" vim: nospell foldmethod=marker foldlevel=1 formatoptions=o
if !has('unix') | finish | endif

setl termguicolors

" Variables: (these need to be global)
let mapleader = " "
let maplocalleader = ","
let g:markup_langs = ['markdown', 'tex']

let g:config_ftypes = [
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
            \ 'haskell',
            \ 'vim',
            \ ]

if has('nvim') | exe 'setl rtp=~/.vim,'.&rtp | endif

so ~/.vim/plugin/plugins.vim

silent call opts#append_to_path([
            \ '~/.gem/ruby/*/bin',
            \ '~/.fzf/bin',
            \ '~/go/bin',
            \ '~/.cargo/bin',
            \ '~/.local/bin',
            \ '~/.stack/bin',
            \ '~/.cabal/bin',
            \ '~/.config/yarn/global/node_modules/.bin',
            \ '~/.local/share/fzf/bin',
            \ '~/.yarn/bin'
            \ ])

silent call repl#set_repl({
            \ "haskell"   : "ghci", 
            \ "python"    : "ipython", 
            \ "erlang"    : "erl", 
            \ "java"      : "jshell", 
            \ "javascript": "node", 
            \ "html"      : "node",
            \ })
