 " vim:foldmethod=indent:
exe 'let $PLUGINS = '.string(expand('<sfile>'))

" Init Vim-Plug
call plug#begin('~/.vim/plugged')

Plug 'junegunn/fzf', {'dir': '~/.local/fzf', 'do': './install --all'}
Plug 'wellle/targets.vim'
Plug 'scrooloose/nerdcommenter'
Plug 'tpope/vim-fugitive'

Plug 'dkarter/bullets.vim', {'for': g:markup_langs + g:config_ftypes}
Plug 'reedes/vim-wordy', {'on': ['Wordy', 'WordyWordy'], 'for': g:markup_langs}

Plug 'othree/html5.vim', {'for': ['markdown', 'html']}
Plug 'othree/csscomplete.vim', {'for': ['css', 'scss', 'html']}

Plug 'fatih/vim-go', {'for': 'go', 'do': ':GoUpdateBinaries'}

Plug 'autozimu/LanguageClient-neovim', {
            \ 'on': 'LanguageClientStart',
            \ 'branch': 'next',
            \ 'do': 'bash install.sh',
            \ }

if has('python3') || has('python')
    Plug 'mattn/emmet-vim', {'for': ['xml', 'html', 'php', 'htmldjango'], 'on': 'Emmet'}
    Plug 'SirVer/ultisnips'
en

if has('nvim') || has('patch8') | Plug 'w0rp/ale', {'on': ['ALEEnable', 'ALEEnableBuffer', 'ALEToggle', 'ALEToggleBuffer']} | el | Plug 'vim-syntastic/syntastic' | en

call plug#end()
