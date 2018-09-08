 " vim::foldmethod=indent:

" Init Vim-Plug
call plug#begin('~/.vim/plugged')

Plug 'konfekt/fastfold'
Plug 'wlangstroth/vim-racket'
Plug 'wellle/targets.vim'
Plug 'scrooloose/nerdcommenter'
Plug 'tpope/vim-fugitive'
Plug 'scrooloose/nerdtree', {'on': ['NERDTreeToggle']}
Plug 'Xuyuanp/nerdtree-git-plugin', {'on': ['NERDTreeToggle']}
Plug 'tpope/vim-eunuch', {'on' : ['Delete', 'Find', 'Mkdir', 'Move', 'Rename', 'SudoEdit', 'SudoWrite']}

Plug 'dkarter/bullets.vim', {'for': g:markup_langs + g:config_ftypes}
Plug 'reedes/vim-wordy', {'on': ['Wordy', 'WordyWordy'], 'for': g:markup_langs + ['gitcommit']}

Plug 'othree/html5.vim', {'for': ['markdown', 'html']}
Plug 'othree/csscomplete.vim', {'for': ['css', 'html']}

Plug 'lervag/vimtex', {'for': 'tex'}

Plug 'vim-erlang/vim-erlang-omnicomplete', {'for': 'erlang'}

Plug 'autozimu/LanguageClient-neovim', {
            \ 'branch': 'next',
            \ 'do': 'bash install.sh',
            \ }

Plug 'fatih/vim-go', {'for': 'go'}

if has('python3') || has('python')
    " Plug 'davidhalter/jedi-vim', {'for': 'python'}
    Plug 'mattn/emmet-vim', {'for': ['xml', 'html', 'php', 'ejs']}
    Plug 'SirVer/ultisnips'
en

if has('nvim') || has('patch8') | Plug 'w0rp/ale', {'on': ['ALEEnable', 'ALEEnableBuffer', 'ALEToggle', 'ALEToggleBuffer']} | el | Plug 'vim-syntastic/syntastic' | en

call plug#end()

" these need to be called after plug#end()
" ----------------------------------------
colorscheme fabulous
hi link        javaScriptRegexpString Define