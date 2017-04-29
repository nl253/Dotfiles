
" VARIABLES and UTILS {{{
let g:MARKUP = [ 'markdown', 'rst' ]
let g:PROGRAMMING =  [ 'vim', 'xhtml', 'html', 'css',
            \'javascript', 'python', 'php', 'sh', 'zsh' ]
let g:REPL = ['php', 'python', 'sh', 'zsh', 'javascript']
let g:DICT_DIR = glob('~/.dicts/')
let g:TEMPLATE_DIR = glob('~/.templates/')
let g:SCRATCHPAD_DIR = glob('~/.scratchpads/')

let g:WORKING_DIRS = [ 'Scripts', 'Notes','.scratchpads', 
            \'.templates', 'Projects', '.bin']

if empty(g:DICT_DIR)
    call system('!mkdir -p '.g:DICT_DIR)
endif

if empty(g:SCRATCHPAD_DIR)
    call system('!mkdir -p '.g:SCRATCHPAD_DIR)
endif

if empty(g:TEMPLATE_DIR)
    call system('!mkdir -p '.g:TEMPLATE_DIR)
endif

let loaded_matchit = 1
let mapleader = " "
let maplocalleader = ","
" }}}

" DOWNLOAD PLUG {{{
if has('nvim')
    let g:VIMDIR = glob('~/.config/nvim/')
    let g:PLUG_FILE = glob('~/.local/share/nvim/site/autoload/plug.vim')
    if empty(g:VIMDIR)
        call system('!mkdir -p '.g:VIMDIR)
    endif
    tnoremap <Esc> <C-\><C-n>
    set inccommand=nosplit
else
    let g:VIMDIR = glob('~/.vim/')
    let g:PLUG_FILE = glob('~/.vim/autoload/plug.vim')
    let $MYVIMRC = glob('~/.vimrc')
    syntax enable
    filetype plugin indent on
    set encoding=utf8 syntax=on autoindent nocompatible magic incsearch ttyfast
    set display=lastline nrformats=bin,hex complete+=i hlsearch
    if has('tags')
        set tags
    endif
endif

if ! filereadable(g:PLUG_FILE)
    call system('curl -fLo ' . g:PLUG_FILE . ' --create-dirs ' . 'https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim')
    PlugInstall
    source $MYVIMRC
endif

" }}}
"
" PLUG INIT :: SET VARIABLES {{{
execute 'set thesaurus=' . g:DICT_DIR . 'thesaurus.txt'
execute 'set dictionary=' .  g:DICT_DIR . 'frequent.dict'
if empty(g:DICT_DIR)
    call system('mkdir -p ' . g:DICT_DIR)
endif

if has('nvim')
    call plug#begin('~/.local/share/nvim/plugged/')
else
    call plug#begin('~/.vim/plugged')
endif
" }}}

" OPTIONS {{{
set ignorecase smartcase foldmethod=marker autochdir
set sessionoptions+=resize sessionoptions-=blank
set completeopt=menuone,longest,noinsert diffopt=filler,vertical,iwhite
set mouse= complete=.,w,t,k noswapfile mps+=<:> pumheight=12
set sessionoptions-=options bufhidden=hide wildignorecase
set shiftwidth=4 autowrite undofile formatoptions=tcqjonl1
set autoread fileignorecase hidden clipboard=unnamed,unnamedplus
set wildignore+=*cache*,*chrome*,*/.dropbox/*,*intellij*,*fonts*,*libreoffice*,*.png
set wildignore+=tags,*~,.vim,*sessio*,*swap*,*.git,*.class,*.svn,*.jpg,*.jpeg
set nostartofline " Don't reset cursor to start of line when moving around
set splitbelow virtualedit=all
set shortmess=atI " Don't show the intro message when starting vim
set path=

" THESE NEED!!! TO BE RELATIVE TO $HOME
for dir in g:WORKING_DIRS
    execute 'set path+=' . glob('~/') . dir . '/**'
endfor

" }}}

" PLUGINS {{{
" Place plugins here
" -------------------
"
" GENERAL {{{
Plug 'Haron-Prime/Antares' " colorscheme
Plug 'tpope/vim-sleuth' " auto set buffer options
Plug 'tpope/vim-speeddating'
Plug 'tmux-plugins/vim-tmux-focus-events' " a must have if you work with tmux
Plug 'tpope/vim-fugitive' " git
set statusline=%<%f\ %r\ %{fugitive#statusline()}%m\ %=%-14.(%q\ %w\ %y\ %P\ of\ %L%)\ \
Plug 'gregsexton/gitv', { 'on': 'Gitv' }
Plug 'junegunn/vim-easy-align', { 'on' : 'EasyAlign' }
Plug 'Konfekt/FastFold' " more efficient folds
Plug 'scrooloose/nerdcommenter'
Plug 'wellle/targets.vim'

Plug 'tpope/vim-eunuch', {'on' : [ 'Move', 'Remove', 'Find', 'Mkdir', 'Wall',
            \'SudoWrite', 'SudoEdit', 'Unlink', 'Chmod', 'Rename', ]}
" }}}

" SESSION {{{
Plug 'xolox/vim-misc'
Plug 'xolox/vim-session' " enhanced session management
let g:session_autosave = 'yes'
let g:session_autosave_silent = 1
let g:session_autosave_to = 'default'
let g:session_autosave_periodic = 3
let g:session_lock_enabled = 0
let g:session_autoload = 'no'
let g:session_default_to_last = 1
let g:session_persist_globals = [ '&foldmethod', '&foldcolumn', '&scrolloff',
            \'&spell', '&wrap', '&scrollbind', '&number',
            \'&relativenumber', '&foldmarker', '&background']

if has('nvim') | let g:session_directory = '~/.config/nvim/session'| else | let g:session_directory = '~/.vim/session' | endif
" }}}

" COMPLETION {{{
if has('python') || has('python3')
    Plug 'SirVer/ultisnips' " Track the engine.
    " Snippets are separated from the engine. Add this if you want them:
    Plug 'honza/vim-snippets' 
    let g:UltiSnipsExpandTrigger="<Tab>"
    let g:UltiSnipsEditSplit="vertical"
endif
" }}}

Plug 'neomake/neomake', {'on' : [ 'Neomake', 'NeomakeProject', 'NeomakeFile' ]}

" SYNTASTIC {{{
if ! has('nvim')
    Plug 'vim-syntastic/syntastic', { 'on' : [ 'SyntasticInfo', 'SyntasticCheck', 'SyntasticToggleMode']}
    let g:syntastic_error_symbol = '✗'
    let g:syntastic_warning_symbol = '⚠'
    let g:syntastic_enable_signs = 1
    let g:syntastic_mode_map = { 'mode': 'passive', 'active_filetypes': []}
    let g:syntastic_vim_checkers = []
    let g:syntastic_html_checkers = ['w3', 'validator', 'tidy', 'jshint', 'eslint']
    let g:syntastic_xhtml_checkers = ['w3', 'validator', 'tidy', 'jshint', 'eslint']
    let g:syntastic_markdown_checkers = ['proselint', 'mdl', 'textlint']
    let g:syntastic_rst_checkers = ['proselint', 'mdl', 'textlint']
    let g:syntastic_python_checkers = ['flake8', 'pylint', 'pycodestyle']
    let g:syntastic_sh_checkers = ['bashate', 'sh', 'shellcheck']
    let g:syntastic_javascript_checkers = ['jshint', 'eslint']
    let g:syntastic_css_checkers = ['stylelint', 'csslint', 'phpcs']
endif
" }}}

Plug 'Chiel92/vim-autoformat', { 'on': 'Autoformat' }

" TAGS {{{
Plug 'xolox/vim-easytags', {'for' : g:PROGRAMMING}
let b:easytags_auto_highlight = 1
let g:easytags_events = ['BufReadPost']
let g:easytags_always_enabled = 1
let g:easytags_resolve_links = 1
" }}}

" MARKUP {{{ {{{
Plug 'reedes/vim-wordy', { 'on': ['Wordy', 'WordyWordy']}
Plug 'reedes/vim-textobj-sentence'
Plug 'dbmrq/vim-ditto', { 'on': [ 'ToggleDitto', 'DittoOn', 'DittoSent',
            \'DittoSentOn', 'DittoFile', 'DittoFileOn', 'DittoPar', 'DittoParOn'],
            \'for' : g:MARKUP}
let g:neomake_markdown_enabled_makers = ['writegood', 'proselint']
let g:neomake_rst_enabled_makers = ['writegood', 'proselint']
" }}}
"
" TABLE MODE {{{
Plug 'dhruvasagar/vim-table-mode', { 'for': ['mardown', 'rst'],
            \'on': ['TableModeEnable'] }
let g:table_mode_disable_mappings = 1
let g:table_mode_verbose = 0 " stops from indicating that it has loaded
let g:table_mode_syntax = 1
let g:table_mode_update_time = 800
au! BufEnter *.md let g:table_mode_corner = '|'
au! BufEnter *.rst let g:table_mode_corner_corner='+' | let g:table_mode_header_fillchar='='

" }}}

"{{{ RST
Plug 'Rykka/riv.vim'
"}}}

" MARKDOWN {{{
Plug 'chrisbra/Colorizer', { 'for': [ 'css', 'html', 
            \'javascript', 'json', 'markdown',
            \'rst' ,'xhtml', 'yaml']}
Plug 'godlygeek/tabular', { 'for': ['markdown', 'rst'], 'on' : 'Tabularize' }
Plug 'plasticboy/vim-markdown', { 'for': ['markdown']}
let g:vim_markdown_autowrite = 1
let g:vim_markdown_emphasis_multiline = 1
let g:vim_markdown_new_list_item_indent = 4
let g:vim_markdown_fenced_languages = [
            \'sh=shell', 'java', 'python=py', 'zsh=zshell',
            \'html=xhtml', 'css', 'php', 'javascript=js']
let g:vim_markdown_no_default_key_mappings = 1
let g:vim_markdown_folding_style_pythonic = 1
let g:vim_markdown_folding_level = 1
" }}}  }}}

" PYTHON {{{
Plug 'klen/python-mode', { 'for': 'python' }

let g:pymode_breakpoint_bind = '<localleader>b'
let g:pymode_doc = 1
let g:pymode_doc_bind = ',h'
let g:pymode_indent = 1
let g:pymode_lint = 1 " Neomake is better
let g:pymode_motion = 1
let g:pymode_options = 1
let g:pymode_options_colorcolumn = 1
let g:pymode_paths = [glob('~/Scripts/'), glob('~/Projects/')]
let g:pymode_python = 'python3'
let g:pymode_rope = 1
let g:pymode_rope_autoimport = 1
let g:pymode_rope_autoimport_import_after_complete = 1
let g:pymode_rope_change_signature_bind = '<localleader>cs'
let g:pymode_rope_complete_on_dot = 1
let g:pymode_rope_completion = 1
let g:pymode_rope_goto_definition_bind = '<localleader>D'
let g:pymode_rope_regenerate_on_write = 0
let g:pymode_rope_show_doc_bind = '<localleader>d'
let g:pymode_run_bind = '<leader>me'
let g:pymode_syntax = 1
let g:pymode_syntax_all = 1
let g:pymode_syntax_print_as_function = 1
let g:pymode_trim_whitespaces = 1
" }}}

" SHELL {{{
Plug 'vim-scripts/bats.vim', {'for' : 'sh'}
let readline_has_bash = 1
let g:is_bash         = 1
let g:sh_fold_enabled = 4
" }}}

" SQL {{{
let g:sql_type_default = 'mysql'
let g:ftplugin_sql_omni_key = ',' " shadows localleader
" }}}

" WEB DEV {{{
Plug 'othree/html5.vim', { 'for': ['html', 'xhtml']}
Plug 'othree/html5-syntax.vim'
Plug 'mattn/emmet-vim', { 'for': ['xml', 'html', 'xhtml', 'css' ]}
let g:emmet_html5 = 1
Plug 'pangloss/vim-javascript', { 'for': ['javascript'] }
Plug 'maksimr/vim-jsbeautify', { 'for': [ 'javascript', 'json', 'html',
            \'xhtml', 'xml', 'css'] }
" }}}
"
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'

" FOR NVIM {{{
if has('nvim')
    Plug 'kassio/neoterm', {'on' : ['TREPLSendSelection', 'TREPSendLine', 'TREPLSendFile']}
    let g:neoterm_position = 'vertical'
    let g:neoterm_keep_term_open = 0
    let g:neoterm_size = 50
    Plug 'sbdchd/neoformat', {'on' : 'Neoformat'}
    if executable('ranger') | Plug 'airodactyl/neovim-ranger' | endif
endif
" }}}

call plug#end()
" }}}

" Scratchpad {{{
function! Scratch()
    if index(['netrw', 'terminal','gitcommit'], &filetype) >= 0  " blacklist
        return 1
    endif
    if index(g:PROGRAMMING, &filetype) >= 0 && expand('%:r') != expand('%:e') && len(expand('%:e')) > 0 || index(g:MARKUP, &filetype) >= 0 && expand('%:r') != expand('%:e') && len(expand('%:e')) > 0
        execute 'vnew ' . '~/.scratchpads/scratch.' . expand('%:e')
        execute 'setl ft=' . &filetype
    elseif &filetype == 'help'
        vnew ~/.scratchpads/scratch.vim
        setl ft=vim
    elseif &filetype == 'man'
        vnew ~/.scratchpads/scratch.sh
        setl ft=sh
    else
        vnew ~/.scratchpads/scratch
        setl ft=scratch
    endif
    vertical resize 60
    write
    nnoremap <buffer> <M-BS> :close!<CR>
    nnoremap <buffer> q :close!<CR>
    vnoremap <buffer> <M-BS> <Nop>
endfunction
command! Scratch call Scratch()
" Alt-BackSpace in normal mode to quickly open a scratch buffer with the same
"
" filetype. It will be saved in ~/Notes/scratch{.py,.java,.go} etc
nnoremap <M-BS> :silent Scratch<CR>
vnoremap <M-BS> :yank<CR>:Scratch<CR>p
" }}}

" INIT {{{
" Init() execute for all buffers on filetype {{{
function! Init()
    if index(g:MARKUP, &filetype) >= 0
        setl spell complete=.,w,k,s
        for dir in g:WORKING_DIRS
            if expand('%:e') != ''
                execute 'setl complete+=k~/'.dir.'/**.'.expand('%:e')
                execute 'setl complete+=k~/'.dir.'/**/**.'.expand('%:e')
                execute 'setl complete+=k~/'.dir.'/**/**/**.'.expand('%:e')
            endif
        endfor
        setl conceallevel=3 formatoptions=tcrqjonl1 foldlevel=1 sw=4
        if ! exists('b:table_mode_on') || (exists('b:table_mode_on') && b:table_mode_on == 0)
            TableModeEnable
        endif
        if executable('wn') 
            nnoremap <buffer> <CR> :execute '!wn '.expand('<cword>').' -over'<CR>
        endif
        nnoremap <expr> <buffer> <Leader>mS has('nvim') ? ':WordyWordy\<CR>:setl spell\<CR>:Neomake\<CR>:DittoSentOn\<CR>' : ':WordyWordy\<CR>:setl spell\<CR>:SyntasticCheck\<CR>:DittoSentOn\<CR>'
        nnoremap <buffer> <M-Tab> :TableModeRealign<CR>
        nnoremap <buffer> gx vF:FhoEy:execute '!'. $BROWSER . ' ' . @+ <CR>
    else
        setl nospell
        if index(g:PROGRAMMING, &filetype) >= 0 
            setl complete=.,w,t,k
        endif
    endif
    " if completion / omnifunction is not provided fall back on default
    if exists("+omnifunc") && &omnifunc == ""
        setlocal omnifunc=syntaxcomplete#Complete
    endif
    if exists("+completefunc") && &completefunc == ""
        setlocal completefunc=syntaxcomplete#Complete
    endif
    if filereadable(g:DICT_DIR . &filetype . '.dict')
        let g:to_exe = 'setl dictionary='. g:DICT_DIR . &filetype . '.dict'
        execute g:to_exe
    endif
endfunction
" }}}

" GitcommitInit() {{{
function! GitcommitInit()
    setl virtualedit=block 
    setl spell complete=.,w,k,s
endfunction
" }}}
"
" ManInit() {{{
function! ManInit()
    nnoremap <buffer> <CR> :execute 'Man ' . expand('<cword>')<CR>
    " make it more like less
    nnoremap <buffer> q :bd!<CR>
    nnoremap <buffer> d <C-d>
    nnoremap <buffer> u <C-u>
    nnoremap <buffer> <M-j> /\v\w+\(\d+\)<CR>
    nnoremap <buffer> <M-k> ?\v\w+\(\d+\)<CR>
    nnoremap <buffer> ] /\v^[A-Z]{3,}\n<CR>
    nnoremap <buffer> [ ?\v^[A-Z]{3,}\n<CR>
    setl nowrap
endfunction
" }}}

" ShInit() {{{
function! ShInit()
    nnoremap <buffer> <CR> :execute 'Man ' . expand('<cword>')<CR>
endfunction
" }}}

" HelpInit() {{{
function! HelpInit()
    " on enter follow that `tag`
    nnoremap <buffer> <CR> <C-]>
    nnoremap <buffer> K <C-]>
    " make it more like less
    nnoremap <buffer> q :bd!<CR>
    nnoremap <buffer> d <C-d>
    nnoremap <buffer> u <C-u>
endfunction
" }}}

" QfInit() {{{
function! QfInit()
    " easir preview after grepping (use emacs' C-p C-n)
    nnoremap <buffer> <C-n> j<CR><C-w><C-w>
    nnoremap <buffer> <C-p> k<CR><C-w><C-w>
    " quick exit
    nnoremap <buffer> q :cclose<CR>
    setl nospell
endfunction
" }}}

" VimInit() {{{
function! VimInit()
    nnoremap <buffer> K :execute 'help ' . expand('<cword>')<CR>
    setl foldmethod=marker
endfunction
" }}}

" MarkdownInit() {{{
function! MarkdownInit()
    nmap <buffer> [[ <Plug>Markdown_MoveToPreviousHeader
    nmap <buffer> ]] <Plug>Markdown_MoveToNextHeader
    nmap gx <buffer> <Plug>Markdown_OpenUrlUnderCursor
    command! PandocMarkdownPreview execute '!pandoc -s -o /tmp/' . expand('%:r') . '.html  -f markdown_github -t html ' . expand('%:p') . ' ; ' . $BROWSER . ' /tmp/' . expand('%:r') . '.html'
    nnoremap <buffer> <expr> <Leader>me executable('pandoc') && executable($BROWSER) ? ":PandocMarkdownPreview\<CR>" : "\<Leader>me"

endfunction
" }}}
"
" RstInit() {{{
function! RstInit()
    command! PandocRstPreview execute '!pandoc -s -o /tmp/' . expand('%:r') . '.html  -f rst -t html ' . expand('%:p') . ' ; ' . $BROWSER . ' /tmp/' . expand('%:r') . '.html'
    nnoremap <buffer> <expr> <Leader>me executable('pandoc') && executable($BROWSER) ? ":PandocRstPreview\<CR>" : "\<Leader>me"
endfunction
"}}}

" HTMLInit() {{{
function! HTMLInit()
    nnoremap <buffer> <Leader>me :execute '!$BROWSER ' . expand('%:p')<CR>
    setl foldmethod=indent
    setl complete=.,w
endfunction
"}}}
"}}}

" Template {{{
function! Template()
    if filereadable(g:TEMPLATE_DIR."template.".expand("%:e"))
        call execute('read '.g:TEMPLATE_DIR."template.".expand("%:e"))
        write
    endif
endfunction
command! Template call Template()
"}}}

" AUTOCOMMANDS {{{
aug VIMENTER
    " go back to where you left off
    au BufReadPost * if line("'\"") > 0 && line("'\"") <= line("$") | exe "normal! g`\"" | endif
    " automatically change dir to the file you are editing
    au BufEnter * try | lchdir %:p:h | catch /.*/ | endtry
    " automatically reload external changes NOTE: doesn't always work properly
    au CursorHold  * silent!  checktime
    au BufEnter * if &filetype == "" | setl ft=markdown | endif
    au FocusLost   * silent!  wall
    au CmdwinEnter * setlocal updatetime=2000
    au CmdwinLeave * setlocal updatetime=200
    au BufNewFile * call Template()
    au FileType * call Init()
    au FileType markdown call MarkdownInit()
    au FileType rst call RstInit()
    au FileType man call ManInit()
    au FileType sh call ShInit()
    au FileType vim call VimInit()
    au FileType help call HelpInit()
    au FileType xhtml,html call HTMLInit()
    au FileType gitcommit call GitcommitInit()
    au FileType qf call QfInit()
aug END
" }}}

" download dictionaries from GitHub if missing {{{
let g:DICTS = ['frequent.dict', 'thesaurus.txt', 'css.dict', 'sql.dict', 'sh.dict', 'javascript.dict']
" let g:DICTS += ['erlang.dict', 'php.dict', 'haskell.dict', 'perl.dict', 'java.dict'] " UNCOMMENT IN NEED
for dict in g:DICTS
    if ! filereadable(g:DICT_DIR . dict)
        execute '!curl -o ' . g:DICT_DIR . dict . ' https://raw.githubusercontent.com/nl253/Dictionaries/master/' . dict
    endif
endfor
" }}}

colorscheme antares

" COMMANDS {{{
" markup conversion, recommended {{{
if executable('pandoc')
    command! TOman execute '!pandoc -s -o ' expand('%:p:r') . '.1  -t man ' . expand('%:p') | sleep 250ms | execute 'vs  ' . expand('%:p:r') . '.1'
    command! TOmarkdown execute '!pandoc -s -o ' expand('%:p:r') . '.md  -t markdown_github --atx-headers --ascii --toc ' . expand('%:p') | sleep 250ms | execute 'vs  ' . expand('%:p:r') . '.md'
    command! TOrst execute '!pandoc -s -o ' expand('%:p:r') . '.rst  -t rst --ascii ' . expand('%:p') | sleep 250ms | execute 'vs  ' . expand('%:p:r') . '.rst'
    command! TOtex execute '!pandoc -s -o ' expand('%:p:r') . '.tex  -t tex ' . expand('%:p') | sleep 250ms | execute 'vs  ' . expand('%:p:r') . '.tex'
    command! TOwordocx execute '!pandoc -s -o ' expand('%:p:r') . '.docx  -t docx ' . expand('%:p') | sleep 250ms | execute 'vs  ' . expand('%:p:r') . '.docx'
    command! TOhtml2 execute '!pandoc -s -o ' expand('%:p:r') . '.html  -t html --html-q-tags --self-contained ' . expand('%:p') | sleep 250ms | execute 'vs  ' . expand('%:p:r') . '.html'
endif
if executable('pdftotext')
    command! FROMpdfTOtxt execute '!pdftotext -eol unix "' . expand('%:p') . '"'
    au! BufRead *.pdf execute '!pdftotext -eol unix "' . expand('%:p') . '" | edit ' expand('%:r') . '.txt'
endif
" }}}
if executable('dos2unix')
    command! Dos2Unix !dos2unix -F -n %:p %:p | edit
endif
command! DeleteEmptyLines execute 'g/^\s*$/d'
command! CountOccurances execute printf('%%s/%s//gn', escape(expand('<cword>'), '/')) | normal! ``
command! Note -nargs=1 -bar -complete=file edit <args>
" }}}

" KEYBINDINGS {{{
inoremap <expr> <Tab> pumvisible() ? "\<C-n>" : "\<Tab>"
inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"
nnoremap <expr> <Leader>mS has('nvim') ? ":Neomake\<CR>": ":SyntasticCheck\<CR>"

" Move by screen lines instead of file lines.
" http://vim.wikia.com/wiki/Moving_by_screen_lines_instead_of_file_lines
noremap k gk
noremap j gj

nnoremap <expr> <M-CR> index(g:REPL, &filetype) >= 0 && has('nvim') ? ":TREPLSendLine\<CR>" : "\<M-CR>"
vnoremap <expr> <M-CR> index(g:REPL, &filetype) >= 0 && has('nvim') ? ":TREPLSendSelection\<CR>" : "\<M-CR>"

"automatically enable spell if you attempt to use correction
nnoremap <expr> z= &spell ? "z=" : ":setl spell\<CR>z="
nnoremap <expr> [s &spell ? "[s" : ":setl spell\<CR>[s"
nnoremap <expr> ]s &spell ? "]s" : ":setl spell\<CR>]s"

nnoremap <Leader>fed    :e $MYVIMRC<CR>
nnoremap <Leader>fer    :so $MYVIMRC<CR>
nnoremap <Leader>ga     :Git add %:p<Space>
nnoremap <Leader>gb     :Gblame<CR>
nnoremap <Leader>gd     :Gdiff<Space>
nnoremap <Leader>gc     :Gcommit<CR>
nnoremap <Leader>gW     :Gwrite<Space>
nnoremap <Leader>gD     :Gdiff<CR>
nnoremap <Leader>gm     :Gmove<Space>
if len($TMUX) > 1
    nnoremap <Leader>gp     :Gpush<CR>
    nnoremap <Leader>gf     :Gfetch<Space>
endif
nnoremap <Leader>gV     :Gitv!<CR>
nnoremap <Leader>gv     :Gitv<CR>
nnoremap <Leader>g?     :Gitv?<CR>
vnoremap <Leader>gv     :Gitv<CR>
vnoremap <Leader>g?     :Gitv?<CR>
nnoremap <Leader>gs     :Gstatus<CR>

nnoremap <C-s>s         :SaveSession<Space>
nnoremap <C-s><C-s>     :SaveSession<CR>
nnoremap <C-s>o         :OpenSession<Space>
nnoremap <C-s><C-o>     :OpenSession<CR>
nnoremap <C-s>d         :DeleteSession<Space>
nnoremap <C-s><C-d>     :DeleteSession!<CR>
nnoremap <C-s><C-c>     :CloseSession!<CR>
nnoremap <C-s>c         :CloseSession<CR>

nnoremap <C-v> :FZFFileAnchor<CR>
nnoremap <leader>fh :FZFRecFilesHome<CR>
nnoremap <Leader>fr :FZFMru<CR>

nnoremap <Leader>sg :execute 'grep ' . expand('<cword>') . " ./* ~/Notes/** ~/* ~/.templates/* ~/Scripts/** ~/Projects/**"<CR>:cw<CR>
nnoremap <M-k> :silent cp<CR>
nnoremap <M-j> :silent cn<CR>
nnoremap <Leader>sa :Ag!<CR>
nnoremap <Leader>sl :Lines!<CR>

" {{{ SHAME
cno w!!<CR> %!sudo tee > /dev/null %<CR>
cno W!<CR> w!<CR>
cno W<CR> w<CR>
cno Wa<CR> wa<CR>
cno WA<CR> wa<CR>
cno Wa!<CR> wa!<CR>
cno WA!<CR> wa!<CR>
cno Wqa<CR> wqa<CR>
cno wQa<CR> wqa<CR>
cno WQA<CR> wqa<CR>
cno wqA<CR> wqa<CR>
cno wqA<CR> wqa<CR>
cno qwa<CR> wqa<CR>
cno Qwa<CR> wqa<CR>
cno qWa<CR> wqa<CR>
cno qwA<CR> wqa<CR>
cno qWA<CR> wqa<CR>
cno QWA<CR> wqa<CR>
cno QwA<CR> wqa<CR>
" }}}
" }}}
