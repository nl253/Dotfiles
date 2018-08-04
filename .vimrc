" vim: nospell foldmethod=marker foldlevel=1 formatoptions=o
if !has('unix') | finish | endif

setl termguicolors

" Variables: (these need to be global)
let g:mapleader = ' '
let g:maplocalleader = ','
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
            \ 'rust',
            \ 'javascript',
            \ 'typescript',
            \ 'haskell',
            \ 'vim',
            \ ]


let g:markdown_fenced_languages = g:prog_langs + ['java']
let g:rst_syntax_code_list = g:markdown_fenced_languages

if has('nvim') | exe 'setl rtp=~/.vim,'.&runtimepath | endif

so ~/.vim/plugin/plugins.vim

call utils#make_missing_dirs(map([
            \ 'undo',
            \ 'backup',
            \ 'swap',
            \ 'views',
            \ 'sessions',
            \ 'templates',
            \ 'snips'
            \ ], '"~/.vim/".v:val'))

call opts#append_to_path([
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
