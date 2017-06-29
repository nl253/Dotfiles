
" PLUG {{{
if has('nvim')
    let g:PLUG_FILE = expand('~/.local/share/nvim/site/autoload/plug.vim')
else " if vim
    let g:PLUG_FILE = expand('~/.vim/autoload/plug.vim')
endif

" Plug - Download if missing 
if ! filereadable(g:PLUG_FILE) && executable('curl')
    echo system('mkdir -p $(dirname '.g:PLUG_FILE.') && curl -flo '.g:PLUG_FILE.' https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim')
    execute 'source '.g:PLUG_FILE
    PlugInstall
elseif ! filereadable(g:PLUG_FILE) 
    finish
endif

if has('nvim')
    call plug#begin('~/.local/share/nvim/plugged/')
else
    call plug#begin('~/.vim/plugged')
endif
" }}}

" VARIABLES {{{
if ! exists('g:MARKUP')
    " MARKUP languages you actively use  
    let g:MARKUP = [ 'markdown', 'vimwiki', 'rst' ]
endif

if ! exists('g:PROGRAMMING')
    " PROGRAMMING LANGUAGES you code in 
    let g:PROGRAMMING =  [ 'xhtml', 'html', 'css', 'javascript', 'python', 'php', 'sql', 'sh', 'zsh' ]
endif
" }}}

" Place plugins here
" ==================

" GENERAL {{{
Plug 'tpope/vim-sleuth' 
Plug 'tpope/vim-speeddating' 
Plug 'tpope/vim-repeat'

" A must have if you work with tmux
if executable('tmux') | Plug 'tmux-plugins/vim-tmux-focus-events' | endif

Plug 'tpope/vim-fugitive' 
Plug 'junegunn/gv.vim'

set statusline=%<%f\ %r\ %{fugitive#statusline()}%m\ %=%-14.(%q\ %w\ %y\ %p\ of\ %l%)\ \ 

Plug 'konfekt/fastfold' 
Plug 'wellle/targets.vim'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-eunuch', { 'on' : [ 'Move', 'Remove', 'Find', 
            \'Mkdir', 'Wall', 'SudoEdit', 'Chmod',
            \'SudoWrite', 'Unlink', 'Rename' ]}

Plug 'junegunn/fzf.vim'

let g:fzf_layout = { 'up': '~40%' }

let g:fzf_action = {
            \ 'ctrl-t': 'tab split',
            \ 'ctrl-s': 'split',
            \ 'ctrl-v': 'vsplit' }

" }}}

" COMPLETION {{{
if (has('python') || has('python3')) && ((has('lambda') && has('job') && has('timers')) || has('nvim'))
    Plug 'SirVer/ultisnips'
    Plug 'honza/vim-snippets'
    let g:UltiSnipsEditSplit = 'vertical'
    let g:ultisnipsexpandtrigger="<tab>"
    let g:UltiSnipsJumpForwardTrigger="<C-j>"
    let g:UltiSnipsJumpBackwardTrigger="<C-k>"
    let g:UltiSnipsListSnippets = '<LocalLeader><Tab>'                
    Plug 'maralla/completor.vim'
    let g:completor_blacklist = [ 'tagbar', 'sql',
                \'qf', 'netrw', 'unite', 'vim', 
                \'help' ]
    let g:completor_python_binary = 'python3'
    Plug 'davidhalter/jedi-vim', {'for': 'python'}
    let g:jedi#completions_enabled = 0
    let g:jedi#goto_command = "<C-]>"
    let g:jedi#goto_assignments_command = ",a"
    let g:jedi#goto_definitions_command = ",d"
    let g:jedi#documentation_command = ",D"
    let g:jedi#usages_command = ",u"
    let g:jedi#rename_command = ",r"
    let g:jedi#use_splits_not_buffers = "right"
    Plug 'nvie/vim-flake8', {'for': 'python'}
    Plug 'tmhedberg/SimpylFold', {'for': 'python'}
    let g:flake8_show_in_file = 1  
    let g:flake8_show_in_gutter=1  
endif

" }}}

" MARKUP {{{ 

Plug 'dkarter/bullets.vim' 
let g:bullets_enabled_file_types = [ 'markdown' ]

Plug 'dbmrq/vim-ditto', { 'on': [ 'ToggleDitto', 'DittoOn' ]}
let g:ditto_mode = "paragraph"

Plug 'reedes/vim-wordy', { 'on': [ 'Wordy', 'WordyWordy' ], 'for': 'vimwiki' }

" TABLE MODE 
Plug 'dhruvasagar/vim-table-mode', { 'on': [ 'TableModeEnable' ] }
let g:table_mode_disable_mappings = 1
let g:table_mode_verbose = 0 | let g:loaded_table_mode = 1
let g:table_mode_syntax = 1 | let g:table_mode_update_time = 800
au! BufEnter *.md let g:table_mode_corner = '|'
au! BufEnter *.rst let g:table_mode_corner_corner='+' | let g:table_mode_header_fillchar='='

" VIMWIKI 
Plug 'vimwiki/vimwiki'
let g:vimwiki_table_mappings = 0
let g:vimwiki_html_header_numbering = 2
let g:vimwiki_hl_headers = 1
let g:vimwiki_use_calendar = 0
let g:vimwiki_dir_link = 'index'
let g:vimwiki_hl_cb_checked = 1
let g:vimwiki_valid_html_tags = 'b,i,s,u,sub,sup,kbd,br,hr,h1,h2,h3,h4,h5,h6,pre,code'
let g:vimwiki_list = [{ 'path': '~/Notes/',
            \ 'auto_toc': 1,
            \ 'syntax': 'default',
            \ 'ext': '.wiki',
            \ 'path_html': '~/.Notes_html/' }]


" MARKDOWN 
Plug 'mzlogin/vim-markdown-toc', { 'for' : 'markdown' }
Plug 'rhysd/vim-gfm-syntax', { 'for' : 'markdown' }
Plug 'nelstrom/vim-markdown-folding', { 'for' : 'markdown' }

" }}} 

" WEB DEV {{{
"
" HTML
Plug 'othree/html5.vim', { 'for': [ 'html', 'xhtml', 'php' ] }
Plug 'othree/html5-syntax.vim', { 'for': [ 'html', 'xhtml', 'php' ] }
Plug 'mattn/emmet-vim', { 'for': [ 'xml', 'html', 'xhtml', 'css', 'php' ] }
"let g:xml_syntax_folding = 1
let g:emmet_html5 = 1 

" PHP
Plug 'shawncplus/phpcomplete.vim', { 'for': 'php' }

" }}}

" FOR NVIM {{{
if has('nvim')
    Plug 'kassio/neoterm', { 'on' : [
                \'TREPLSendSelection', 
                \'TREPLSendLine', 
                \'TREPLSendFile' ]}
    let g:neoterm_position = 'vertical'
    let g:neoterm_keep_term_open = 0
    let g:neoterm_size = 50
endif
" }}} 

" MY PLUGINS
" ==========
Plug 'nl253/vim-saner'
Plug 'nl253/vim-dicts'
Plug 'nl253/vim-python'
Plug 'nl253/fabulous'
Plug 'nl253/vim-vim', { 'for': 'vim' }
Plug 'nl253/vim-fzf-extensions'
Plug 'nl253/vim-fugative-extensions'
Plug 'nl253/vim-vimwiki-extensions'
Plug 'nl253/vim-scratchpads'
Plug 'nl253/vim-templates'
Plug 'nl253/vim-chunks'
Plug 'nl253/vim-sh'
Plug 'nl253/vim-licenses'
Plug 'nl253/vim-gitignore'

let g:dicts_markup = [ 'computer-science', 'unix-programmers' ]

call plug#end()

colorscheme fabulous

" vim: foldlevel=0 foldmarker={{{,}}} foldmethod=marker
