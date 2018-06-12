 " vim::foldmethod=indent:

" Init Vim-Plug
call plug#begin('~/.vim/plugged')

Plug 'tpope/vim-rsi'
Plug 'nikvdp/ejs-syntax'
Plug 'tmux-plugins/vim-tmux-focus-events'
Plug 'vim-scripts/SyntaxAttr.vim'
Plug 'konfekt/fastfold'
Plug 'wellle/targets.vim'
Plug 'scrooloose/nerdcommenter'
Plug 'tpope/vim-fugitive'
Plug 'scrooloose/nerdtree'
Plug 'Xuyuanp/nerdtree-git-plugin'
Plug 'tpope/vim-eunuch', {'on' : ['Delete', 'Find', 'Mkdir', 'Move', 'Rename', 'SudoEdit', 'SudoWrite']}

Plug 'dkarter/bullets.vim', {'for': g:markup_langs + g:config_ftypes}
Plug 'reedes/vim-wordy', {'on': ['Wordy', 'WordyWordy'], 'for': g:markup_langs + ['gitcommit']}

Plug 'othree/html5.vim', {'for': ['markdown', 'html']}
Plug 'othree/csscomplete.vim', {'for': ['css', 'html']}
" Plug 'pangloss/vim-javascript', {'for': 'javascript'}

Plug 'lervag/vimtex', {'for': 'tex'}

Plug 'vim-erlang/vim-erlang-omnicomplete', {'for': 'erlang'}

Plug 'justmao945/vim-clang', {'for': 'c'}

Plug 'fatih/vim-go', {'for': 'go'}

Plug 'racer-rust/vim-racer', {'for': 'rust'}

if has('python3') || has('python')
    Plug 'davidhalter/jedi-vim', {'for': 'python'}
    Plug 'mattn/emmet-vim', {'for': ['xml', 'html', 'php', 'ejs']}
    Plug 'SirVer/ultisnips'
en

if has('nvim') || has('patch8') | Plug 'w0rp/ale', {'on': ['ALEEnable', 'ALEEnableBuffer', 'ALEToggle', 'ALEToggleBuffer']} | el | Plug 'vim-syntastic/syntastic' | en

let g:my_plug_dir = expand('~/Documents/Vim')

if !isdirectory(g:my_plug_dir) && hostname() =~? "Chummy-Laptop"
    call mkdir(g:my_plug_dir, 'p')
en

Plug 'flazz/vim-colorschemes/'

if isdirectory(g:my_plug_dir)
    Plug 'nl252/fabulous', {'frozen': 1, 'dir': g:my_plug_dir.'/fabulous'}
    Plug 'nl253/vim-saner', {'frozen': 1, 'dir': g:my_plug_dir.'/vim-saner'}
    Plug 'nl253/vim-markup', {'frozen': 1, 'dir': g:my_plug_dir.'/vim-markup'}
    Plug 'nl253/vim-programming', {'frozen': 1, 'dir': g:my_plug_dir.'/vim-programming'}
    Plug 'nl253/vim-webdev', {'frozen': 1, 'dir': g:my_plug_dir.'/vim-webdev'}
en
call plug#end()

" these need to be called after plug#end()
" ----------------------------------------
colorscheme fabulous
hi link javaScriptRegexpString Define
hi Function guifg=Orange3
hi PreProc guibg=Grey15 guifg=Magenta1
hi Normal guibg=Grey10
" hi Statement guifg=DarkMagenta
hi Statement guifg=MediumOrchid
hi Repeat guifg=Orange3
hi Conditional guifg=Purple1
hi Keyword guifg=SlateBlue
hi Operator guifg=Grey50 gui=Bold
hi Folded guifg=MediumPurple4 guibg=Grey13
hi Boolean guifg=DarkOrange3
hi Number guifg=Yellow3
hi SpecialChar guifg=DeepPink4
hi Comment guifg=DeepSkyBlue4
hi String guifg=SeaGreen
