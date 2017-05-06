
" VARIABLES and UTILS {{{
let g:MARKUP = [ 'markdown', 'vimwiki' ]

let g:MARKUP_EXT = ['md', 'wiki', 'txt']

let g:PROGRAMMING =  [ 'vim', 'xhtml', 'html', 'css',
            \'javascript', 'python', 'php', 'sh', 'zsh' ]

let g:REPL = ['php', 'python', 'sh', 'zsh', 'javascript']

let g:DICT_DIR = glob('~/.dicts/')

let g:TEMPLATE_DIR = glob('~/.templates/')

let g:SCRATCHPAD_DIR = glob('~/.scratchpads/')

" THESE NEED!!! TO BE RELATIVE TO $HOME
let g:WORKING_DIRS = [ 'Scripts', '.scratchpads', 'vimwiki',
            \'.templates', 'Projects', '.bin', '.dicts']

for d in [g:TEMPLATE_DIR, g:SCRATCHPAD_DIR, g:DICT_DIR]
    if empty(d)
        call system('!mkdir -p '.d)
    endif
endfor

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
    set inccommand=nosplit termguicolors
else " if vim
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

if ! filereadable(g:PLUG_FILE) && executable('curl')
    call system('curl -fLo ' . g:PLUG_FILE . ' --create-dirs ' . 'https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim')
    PlugInstall
    source $MYVIMRC
endif

" }}}
"
" PLUG INIT :: SET VARIABLES {{{
execute 'set thesaurus=' . g:DICT_DIR . 'thesaurus.txt'
execute 'set dictionary=' .  g:DICT_DIR . 'frequent.dict'

if has('nvim')
    call plug#begin('~/.local/share/nvim/plugged/')
else
    call plug#begin('~/.vim/plugged')
endif
" }}}

" OPTIONS {{{
set ignorecase smartcase foldmethod=marker autochdir
set sessionoptions+=resize sessionoptions-=blank nospell
set completeopt=menuone,longest,noinsert diffopt+=vertical,iwhite
set mouse= complete=.,w,t,k noswapfile mps+=<:> pumheight=12
set sessionoptions-=options bufhidden=hide wildignorecase
set shiftwidth=4 autowrite undofile formatoptions=tcqjonl1
set autoread fileignorecase hidden clipboard=unnamed,unnamedplus
set wildignore+=*cache*,*chrome*,*/.dropbox/*,*intellij*,*fonts*,*libreoffice*,*.png
set wildignore+=tags,*~,.vim,*sessio*,*swap*,*.git,*.class,*.svn,*.jpg,*.jpeg,.rope*
set nostartofline " Don't reset cursor to start of line when moving around
set splitbelow virtualedit=all
set shortmess=atI " Don't show the intro message when starting vim
set path=~/.*

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
Plug 'junegunn/vim-easy-align', { 'on' : 'EasyAlign' }
Plug 'Konfekt/FastFold' " more efficient folds
Plug 'scrooloose/nerdcommenter'
Plug 'wellle/targets.vim'
Plug 'tpope/vim-eunuch', {'on' : [ 'Move', 'Remove', 'Find', 'Mkdir', 'Wall',
            \'SudoWrite', 'SudoEdit', 'Unlink', 'Chmod', 'Rename', ]}
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

Plug 'neomake/neomake', {'on' : [ 'Neomake']}

Plug 'vimwiki/vimwiki'

" MARKDOWN {{{
let g:markdown_fenced_languages = [
            \'html', 'python', 'zsh', 'javascript',
            \'php', 'css', 'java', 'vim', 'sh']

Plug 'mzlogin/vim-markdown-toc', {'for' : 'markdown'}
Plug 'rhysd/vim-gfm-syntax', {'for' : 'markdown'}
" }}}

Plug 'dkarter/bullets.vim'

" TAGS {{{
Plug 'xolox/vim-misc'
Plug 'xolox/vim-easytags', {'for' : g:PROGRAMMING}
let b:easytags_auto_highlight = 1
let g:easytags_events = ['BufNewFile']
let g:easytags_always_enabled = 1
let g:easytags_resolve_links = 1
" }}}

" MARKUP {{{ {{{
Plug 'reedes/vim-wordy', { 'on': ['Wordy', 'WordyWordy'] }
Plug 'reedes/vim-textobj-sentence'
Plug 'dbmrq/vim-ditto', { 'on': [ 'ToggleDitto',
            \'DittoOn', 'DittoSent','DittoSentOn']}
" }}}
"
" TABLE MODE {{{
Plug 'dhruvasagar/vim-table-mode', { 'on': ['TableModeEnable'] }
let g:loaded_table_mode = 1
let g:table_mode_disable_mappings = 1
let g:table_mode_verbose = 0 " stops from indicating that it has loaded
let g:table_mode_syntax = 1
let g:table_mode_update_time = 800
au! BufEnter *.md let g:table_mode_corner = '|'
au! BufEnter *.rst let g:table_mode_corner_corner='+' | let g:table_mode_header_fillchar='='
" }}}

Plug 'chrisbra/Colorizer', { 'for': [ 'css', 'html',
            \'javascript', 'json', 
            \'xhtml', 'yaml']}

Plug 'godlygeek/tabular', { 'for': g:MARKUP, 'on' : 'Tabularize' }
" }}}  }}}

" PYTHON {{{
Plug 'klen/python-mode', { 'for': 'python' }
let g:pymode_lint_on_write = 0
let g:pymode_lint_options_pep8 = { 'max_line_length': 150 }
let g:pymode_lint_ignore = "E116,W"
let g:pymode_breakpoint_bind = '<localleader>b'
let g:pymode_doc = 1
let g:pymode_doc_bind = ',h'
let g:pymode_indent = 1
let g:pymode_lint = 1
let g:pymode_motion = 1
let g:pymode_options = 1
let g:pymode_options_colorcolumn = 1
let g:pymode_paths = [glob('~/Scripts/'), glob('~/Projects/')]
let g:pymode_rope_autoimport_modules = [
            \ 'os', 'shutil', 'PythonUtils', 're',
            \ 'pathlib', 'subprocess', 'shlex' ]
let g:pymode_python = 'python3'
let g:pymode_rope = 1
let g:pymode_rope_autoimport = 1
let g:pymode_rope_autoimport_import_after_complete = 1
let g:pymode_rope_change_signature_bind = '<localleader>cs'
let g:pymode_rope_complete_on_dot = 1
let g:pymode_rope_completion = 1
let g:pymode_rope_goto_definition_bind = '<localleader>D'
let g:pymode_rope_regenerate_on_write = 1
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
Plug 'othree/html5-syntax.vim', { 'for': ['html', 'xhtml']}
Plug 'mattn/emmet-vim', { 'for': ['xml', 'html', 'xhtml', 'css' ]}
let g:emmet_html5 = 1
" }}}
"
" FZF {{{
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'

let g:fzf_action = {
            \ 'ctrl-t': 'tab split',
            \ 'ctrl-s': 'split',
            \ 'ctrl-v': 'vsplit' }
" }}}

" FOR NVIM {{{
if has('nvim')
    Plug 'kassio/neoterm', {'on' : ['TREPLSendSelection', 'TREPSendLine', 'TREPLSendFile']}
    let g:neoterm_position = 'vertical'
    let g:neoterm_keep_term_open = 0
    let g:neoterm_size = 50
    if executable('ranger') | Plug 'airodactyl/neovim-ranger' | endif
else
    Plug 'francoiscabrol/ranger.vim'
    let g:ranger_map_keys = 0
endif

" }}}

call plug#end()

" Scratchpad {{{
function! Scratch()
    if index(['netrw', 'terminal','gitcommit', 'qf', 'gitcommit', 'git', 'netrc'], &filetype) >= 0  " blacklist
        return 1
    endif
    if &filetype == 'help'
        vnew ~/.scratchpads/scratch.vim
        setl ft=vim
    elseif &filetype == 'man'
        vnew ~/.scratchpads/scratch.sh
        setl ft=sh
    elseif (index(g:MARKUP, &filetype) >= 0) && expand('%:r') != expand('%:e') && len(expand('%:e')) > 0 && expand('%:p') != '~/vimwiki/diary/diary.wiki'
        vnew ~/vimwiki/diary/diary.wiki
        setl ft=vimwiki
    elseif (index(g:PROGRAMMING, &filetype) >= 0) && expand('%:r') != expand('%:e') && len(expand('%:e')) > 0 
        let g:_SCRATCH_FILETYPE = &filetype
        vnew ~/.scratchpads/scratch.%:e
        execute 'setl ft=' . g:_SCRATCH_FILETYPE
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
nnoremap <M-BS> :silent Scratch<CR>
vnoremap <M-BS> :yank<CR>:Scratch<CR>p
" }}}

" INIT  {{{
" Init() execute for all buffers on filetype {{{
"
function! Markup()
    setl complete=.,w,k,s conceallevel=3 makeprg=write-good
    setl formatoptions=tcrqjonl1 foldlevel=1 sw=4
    for dir in g:WORKING_DIRS  " this actually isn't recursive
        for extension in g:MARKUP_EXT " 2 levels of depth ...
            execute 'setl complete+=k~/'.dir.'/**.'.extension
            " uncomment to get 2 levels of depth
            execute 'setl complete+=k~/'.dir.'/**/**.'.extension
        endfor
    endfor
    nnoremap <buffer> <Leader>mS :setl spell<CR>:WordyWordy<CR>:Neomake<CR>:DittoSentOn<CR>
    nnoremap <expr> <buffer> <M-Tab> exists('b:table_mode_on') && b:table_mode_on == 1 ? ":TableModeRealign\<CR>" : "\<M-Tab>"
    nnoremap <buffer> gx vF:FhoEy:execute '!'. $BROWSER . ' ' . @+ <CR>
    if executable('wn')
        nnoremap <buffer> K :execute('Capture wn ' . expand('<cword>') . ' -over')<CR>
    endif
endfunction

function! Programming()
    setl nospell complete=.,w,t 
    if expand('%:e') != ''     " if there is an extension (needed)
        for dir in g:WORKING_DIRS
            execute 'setl complete+=k~/'.dir.'/**.'.expand('%:e')
            " uncomment to get 2 levels of depth
            "execute 'setl complete+=k~/'.dir.'/**/**.'.expand('%:e')
        endfor
    endif
    if filereadable(g:DICT_DIR . &filetype . '.dict')
        let g:to_exe = 'setl dictionary='. g:DICT_DIR . &filetype . '.dict' "
        execute g:to_exe
        setl complete+=k
    endif
endfunction

function! Init()
    if index(g:MARKUP, &filetype) >= 0
        call Markup()
    elseif index(g:PROGRAMMING, &filetype) >= 0
        call Programming()
    endif
    if exists("+omnifunc") && &omnifunc == ""
        setlocal omnifunc=syntaxcomplete#Complete
    endif
    if exists("+completefunc") && &completefunc == ""
        setlocal completefunc=syntaxcomplete#Complete
    endif
    if filereadable(g:DICT_DIR . &filetype . '.dict')
        call execute('setl dictionary='. g:DICT_DIR . &filetype . '.dict')
    endif
    nnoremap <Leader>* :execute('grep '.expand('<cword>').' '.substitute(&path,',',' ','g'))<CR>
endfunction
" }}}

" GitcommitInit() {{{
function! GitcommitInit()
    setl virtualedit=block
    setl spell complete=.,w,k,s
    if executable('markdown-formatter')
        setl formatprg=markdown-formatter
    endif
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
    setl complete+=k~/.bashrc,k~/.shells/**.sh,k~/.profile,k~/.bash_profile
    if executable('shfmt')
        setl formatprg=shfmt
    endif
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
    nnoremap <buffer> q :cclose<CR>:lclose<CR>
    setl nospell
endfunction
" }}}

" VimInit() {{{
function! VimInit()
    nnoremap <buffer> K :execute 'help ' . expand('<cword>')<CR>
    setl foldmethod=marker
    setl complete+=k~/.vimrc
endfunction
" }}}

" VimWikiInit() {{{      
function! VimWikiInit()
    nmap <F13> <Plug>VimwikiNextLink
    nmap <F14> <Plug>VimwikiPrevLink
    nmap <F15> <Plug>VimwikiAddHeaderLevel
    nnoremap <Leader>me :Vimwiki2HTMLBrowse<CR>
    hi VimwikiHeader1 guifg=#FF9999
    hi VimwikiHeader2 guifg=#FF9900
    hi VimwikiHeader3 guifg=#CCCC00
    hi VimwikiHeader4 guifg=#00CC66
    hi VimwikiHeader5 guifg=#3399FF
    hi VimwikiHeader6 guifg=#CC66FF
    if executable('markdown-formatter') 
        setl formatprg=markdown-formatter
    endif                                  
    nnoremap <buffer> <M-CR> :VimwikiTabnewLink<CR>
endfunction
" }}}

" MarkdownInit() {{{
function! MarkdownInit()
    if executable('markdown-formatter')
        setl formatprg=markdown-formatter
    endif
    if executable('markdown-preview')
        nnoremap <Leader>me :!markdown-preview %<CR>
    endif
endfunction
" }}}

"PythonInit() {{{
function! PythonInit()
    nnoremap <Leader>mS :PymodeLint<CR>
    if executable('autopep8') && executable('pycodestyle')
        setl formatprg=autopep8\ -
    endif
endfunction
" }}}
"
" JavascriptInit() {{{
function! JavascriptInit()
    setl foldmethod=marker
    setl foldmarker={,}
    if executable('js-beautify')
        setl formatprg=js-beautify
    endif
endfunction
" }}}
"
" CSSInit() {{{
function! CSSInit()
    setl foldmethod=marker
    setl foldmarker={,}
    if executable('js-beautify')
        setl formatprg=js-beautify\ --type\ css
    endif
endfunction
" }}}
"
" HTMLInit() {{{
function! HTMLInit()
    nnoremap <buffer> <Leader>me :!$BROWSER %:p<CR>
    setl foldmethod=indent
    setl complete=.,w
    if executable('js-beautify')
        setl formatprg=js-beautify\ --type\ html
    endif
endfunction
"}}} "}}}

" Template {{{
function! Template()
    if filereadable(g:TEMPLATE_DIR."template.".expand("%:e"))
        normal gg
        call execute('read '.g:TEMPLATE_DIR."template.".expand("%:e"))
        normal gg
        normal dd
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
    " by default blank files are rst notes
    au BufEnter * if &filetype == "" | setl ft=markdown | endif
    au FocusLost   * silent!  wall
    au CmdwinEnter * setlocal updatetime=2000
    au CmdwinLeave * setlocal updatetime=200
    au BufNewFile * call Template()
    au FileType * call Init()
    au FileType man call ManInit()
    au FileType sh call ShInit()
    au FileType vim call VimInit()
    au FileType help call HelpInit()
    au FileType xhtml,html,xml call HTMLInit()
    au FileType gitcommit call GitcommitInit()
    au FileType vimwiki call VimWikiInit()
    au FileType qf call QfInit()
    au FileType python call PythonInit()
    au FileType json call JsonInit()
    au FileType javascript,json call JavascriptInit()
    au FileType css call CssInit()
    au FileType markdown call MarkdownInit()
    au BufNewFile,BufRead *.txt setl ft=asciidoc
aug END
" }}}

" download dictionaries from GitHub if missing {{{
let g:DICTS = ['frequent.dict', 'thesaurus.txt', 'css.dict', 'sql.dict', 'sh.dict', 'javascript.dict']
" let g:DICTS += ['erlang.dict', 'php.dict', 'haskell.dict', 'perl.dict', 'java.dict'] " UNCOMMENT IN NEED
for dict in g:DICTS
    if ! filereadable(g:DICT_DIR . dict) && executable('curl')
        execute '!curl -fLo ' . g:DICT_DIR . dict . ' https://raw.githubusercontent.com/nl253/Dictionaries/master/' . dict
    endif
endfor
" }}}

colorscheme antares

" COMMANDS {{{
" markup conversion, recommended {{{
if executable('pandoc')
    command! TOmarkdown call system('pandoc -s -o '.expand('%:p:r').'.md -t markdown_github --atx-headers --ascii --toc '.expand('%:p')) | sleep 250ms | vs %:p:r.md
    command! TOrst call system('pandoc -s -o '.expand('%:p:r').'.rst -t rst --ascii '.expand('%:p')) | sleep 250ms | vs %:p:r.rst
    command! TOtex call system('pandoc -s -o '.expand('%:p:r').'.tex  -t tex '.expand('%:p')) | sleep 250ms | vs %:p:r.tex
    command! TOwordocx call system('pandoc -s -o '.expand('%:p:r').'.docx -t docx '.expand('%:p')) | sleep 250ms | vs %:p:r.docx
    command! TOhtml2 call system('pandoc -s -o '.expand('%:p:r').'html -t html --html-q-tags --self-contained '.expand('%:p')) | sleep 250ms | vs %:p:r.html
    command! TOmediawiki call system('pandoc -s -o '.expand('%:p:r').'wiki -t mediawiki --self-contained '.expand('%:p')) | sleep 250ms | vs %:p:r.wiki
endif
if executable('pdftotext')
    function! PdfTOtxt()
        !pdftotext -eol unix %:p /tmp/%:r.txt
        edit /tmp/%:r.txt
    endfunction
    command! PdfTOtxt call PdfTOtxt()
    function! PdfInit()
        if matchstr(expand('%'),' ') == ""
            PdfTOtxt
        endif
    endfunction
    au! FileType pdf call PdfInit()
endif
" }}}
if executable('dos2unix')
    command! Dos2Unix !dos2unix %:p | edit
endif
command! CountOccurances execute printf('%%s/%s//gn', escape(expand('<cword>'), '/')) | normal! ``
command! -bang -nargs=* GGrep call fzf#vim#grep('git grep --line-number '.shellescape(<q-args>), 0, <bang>0)
command! -complete=shellcmd -nargs=+ Capture lexpr(system(expand(<q-args>))) | topleft lopen
" }}}

" KEYBINDINGS {{{
inoremap <expr> <Tab> pumvisible() ? "\<C-n>" : "\<Tab>"
inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"
nnoremap <Leader>mS :Neomake<CR>

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
nnoremap <C-s><C-s>     :SaveSession default<CR>
nnoremap <C-s>o         :OpenSession<Space>
nnoremap <C-s><C-o>     :OpenSession default<CR>
nnoremap <C-s>d         :DeleteSession<Space>
nnoremap <C-s><C-d>     :DeleteSession!<CR>
nnoremap <C-s><C-c>     :CloseSession!<CR>
nnoremap <C-s>c         :CloseSession<CR>

nnoremap <M-k> :silent cp<CR>
nnoremap <M-j> :silent cn<CR>
nnoremap <LocalLeader>* :lgrep <cword> %:p<CR>:lopen<CR>
nnoremap <C-k> :silent lp<CR>
nnoremap <C-j> :silent lne<CR>
nnoremap <Leader>a<Leader> :Ag!<CR>
nnoremap <Leader>g<Leader> :GGrep!<CR>
nnoremap <Leader>l<Leader> :Lines!<CR>
nnoremap <Leader>/ :History/<CR>
nnoremap <Leader>: :History:<CR>
nnoremap <Leader><Leader> :Commands!<CR>

cno w!!<CR> %!sudo tee > /dev/null %<CR>

"vim:set foldlevel=0
