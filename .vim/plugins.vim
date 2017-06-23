
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

" BUILT-IN PLUGINS 
" ================
" {{{
if v:version >= 800
    packadd! matchit
else
    runtime macros/matchit.vim
endif

" NETRW 
let g:netrw_scpport = "-P 22" | let g:netrw_sshport = "-p 22"
let g:netrw_preview = 1 | let g:netrw_mousemaps = 0

" SQL
let g:sql_type_default = 'mysql' | let msql_sql_query = 1
let g:ftPlugin_sql_omni_key = ',' " shadows localleader
Plug 'alcesleo/vim-uppercase-sql', { 'for': 'sql' }

" SHELL 
let readline_has_bash = 1 | let g:is_bash = 1
let g:sh_fold_enabled = 4

" ZSH 
let g:zsh_fold_enable = 1

" YAML
let g:yaml_schema = 'pyyaml'

" }}}

" Place plugins here
" ==================
" GENERAL {{{
Plug 'tpope/vim-sleuth' | Plug 'tpope/vim-speeddating' | Plug 'tpope/vim-repeat'
if executable('tmux')
    Plug 'tmux-plugins/vim-tmux-focus-events' " a must have if you work with tmux
endif

Plug 'tpope/vim-fugitive'

set statusline=%<%f\ %r\ %{fugitive#statusline()}%m\ %=%-14.(%q\ %w\ %y\ %p\ of\ %l%)\ \
Plug 'konfekt/fastfold' 
Plug 'scrooloose/nerdcommenter' | Plug 'wellle/targets.vim'
Plug 'tpope/vim-eunuch', { 'on' : [ 'Move', 'Remove', 'Find', 
            \'Mkdir', 'Wall', 'SudoEdit', 'Chmod',
            \'SudoWrite', 'Unlink', 'Rename' ]}
" }}}

" COMPLETION {{{
if (has('python') || has('python3')) && ((has('lambda') && has('job') && has('timers')) || has('nvim'))
    Plug 'sirver/ultisnips' | Plug 'honza/vim-snippets'
    let g:ultisnipsexpandtrigger="<tab>"
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
    let g:flake8_show_in_file = 1  
    let g:flake8_show_in_gutter=1  
endif

" }}}
"
" MARKUP {{{ {{{

Plug 'dkarter/bullets.vim' | Plug 'reedes/vim-textobj-sentence'

let g:bullets_enabled_file_types = [ 'markdown' ]

Plug 'dbmrq/vim-ditto', { 'on': [ 'ToggleDitto', 'DittoOn', 'DittoSent','DittoSentOn' ]}
Plug 'reedes/vim-wordy', { 'on': [ 'Wordy', 'WordyWordy' ] }

" TABLE MODE {{{
Plug 'dhruvasagar/vim-table-mode', { 'on': [ 'TableModeEnable' ] }
let g:table_mode_disable_mappings = 1
let g:table_mode_verbose = 0 | let g:loaded_table_mode = 1
let g:table_mode_syntax = 1 | let g:table_mode_update_time = 800
au! BufEnter *.md let g:table_mode_corner = '|'
au! BufEnter *.rst let g:table_mode_corner_corner='+' | let g:table_mode_header_fillchar='='
" }}}

" VIMWIKI {{{
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
" }}}

" MARKDOWN {{{
let g:markdown_fenced_languages = g:PROGRAMMING 
Plug 'mzlogin/vim-markdown-toc', { 'for' : 'markdown' }
Plug 'rhysd/vim-gfm-syntax', { 'for' : 'markdown' }
Plug 'nelstrom/vim-markdown-folding', { 'for' : 'markdown' }
" }}}

" }}} 

" WEB DEV {{{
"
" HTML
Plug 'othree/html5.vim', { 'for': [ 'html', 'xhtml', 'php' ]}
Plug 'othree/html5-syntax.vim', { 'for': [ 'html', 'xhtml', 'php' ]}
Plug 'mattn/emmet-vim', { 'for': [ 'xml', 'html', 'xhtml', 'css', 'php' ]}
"let g:xml_syntax_folding = 1
let g:emmet_html5 = 1 | let g:html_hover_unfold = 1
let g:html_font = ["Sans Serif", "DejaVu Sans Mono", 'Consolas', 'monospace']
let g:html_use_xhtml = 1 | let g:html_dynamic_folds = 1
let g:html_no_foldcolumn = 1 | let g:html_use_encoding = "UTF-8"
let html_wrong_comments=1

" PHP
Plug 'shawncplus/phpcomplete.vim', { 'for': 'php' }

" FZF {{{
if empty(expand('~/.applications'))
    !mkdir -p ~/.applications/
endif

Plug 'junegunn/fzf', { 'dir': '~/.applications/fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'

let g:fzf_layout = { 'up': '~40%' }

let g:fzf_action = {
            \ 'ctrl-t': 'tab split',
            \ 'ctrl-s': 'split',
            \ 'ctrl-v': 'vsplit' }
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

" }}}

" }}}

" MY PLUGINS
" ==========
Plug 'nl253/vim-saner'
Plug 'nl253/vim-dicts'
Plug 'nl253/vim-colors'
Plug 'nl253/vim-vim', {'for': 'vim'}
Plug 'nl253/vim-fzf-extensions'
Plug 'nl253/vim-fugative-extensions'
Plug 'nl253/vim-scratchpads'
Plug 'nl253/vim-templates'

call plug#end()
