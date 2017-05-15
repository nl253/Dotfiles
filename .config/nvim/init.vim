
" VARIABLES and UTILS {{{
let g:MARKUP = [ 'markdown', 'vimwiki' ]

let g:MARKUP_EXT = ['md', 'wiki']

let g:PROGRAMMING =  ['xhtml', 'html', 
            \'css', 'javascript', 
            \'python', 'php', 
            \'sh', 'zsh']

let g:REPL = ['php', 'python', 'sh', 'zsh', 'javascript']

let g:DICT_DIR = glob('~/.dicts/')

let g:TEMPLATE_DIR = glob('~/.templates/')

let g:SCRATCHPAD_DIR = glob('~/.scratchpads/')

" THESE NEED!!! TO BE RELATIVE TO $HOME
let g:WORKING_DIRS = ['Scripts', 'vimwiki', 'Projects']

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
call execute('set thesaurus=' . g:DICT_DIR . 'thesaurus.txt')
call execute('set dictionary=' .  g:DICT_DIR . 'frequent.dict')

if has('nvim')
    call plug#begin('~/.local/share/nvim/plugged/')
else
    call plug#begin('~/.vim/plugged')
endif
" }}}

" OPTIONS {{{
set ignorecase smartcase foldmethod=marker autochdir
set sessionoptions+=resize sessionoptions-=blank 
set completeopt=menuone,longest,noinsert diffopt+=vertical,iwhite
set mouse= complete=.,w,t,k noswapfile mps+=<:> pumheight=12
set sessionoptions-=options bufhidden=hide wildignorecase
set shiftwidth=4 autowrite undofile formatoptions=tcqjonl1 formatprg=fmt\ -s\ -u
set autoread fileignorecase hidden clipboard=unnamed,unnamedplus
set wildignore+=*cache*,*chrome*,*/.dropbox/*,*intellij*,*fonts*,*libreoffice*,*.png
set wildignore+=tags,*~,.vim,*sessio*,*swap*,*.git,*.class,*.svn,*.jpg,*.jpeg,.rope*
set nostartofline " Don't reset cursor to start of line when moving around
set shortmess=atI " Don't show the intro message when starting vim
set splitbelow virtualedit=all path=~/.*

for dir in g:WORKING_DIRS
    call execute('set path+=' . glob('~/') . dir . '/**,')
endfor

" }}}

" PLUGINS {{{
" Place plugins here
" -------------------

" GENERAL {{{
Plug 'tpope/vim-sleuth' | Plug 'tpope/vim-speeddating'
Plug 'tmux-plugins/vim-tmux-focus-events' " a must have if you work with tmux
Plug 'Haron-Prime/Antares' | Plug 'tpope/vim-fugitive'
set statusline=%<%f\ %r\ %{fugitive#statusline()}%m\ %=%-14.(%q\ %w\ %y\ %P\ of\ %L%)\ \
Plug 'junegunn/vim-easy-align', { 'on' : 'EasyAlign' }
Plug 'Konfekt/FastFold' " more efficient folds
Plug 'scrooloose/nerdcommenter' | Plug 'wellle/targets.vim'
Plug 'tpope/vim-eunuch', {'on' : [ 'Move', 'Remove', 'Find', 'Mkdir', 'Wall',
            \'SudoWrite', 'SudoEdit', 'Unlink', 'Chmod', 'Rename', ]}
" }}}

" COMPLETION {{{
if has('python') || has('python3')
    Plug 'SirVer/ultisnips' | Plug 'honza/vim-snippets'
    let g:UltiSnipsExpandTrigger="<Tab>"
    let g:UltiSnipsEditSplit="vertical"
endif
" }}}

Plug 'neomake/neomake', {'on' : [ 'Neomake']}

" MARKUP {{{ {{{
Plug 'vimwiki/vimwiki'

" MARKDOWN {{{
let g:markdown_fenced_languages = [
            \'html', 'python', 'zsh', 'javascript',
            \'php', 'css', 'java', 'vim', 'sh']

Plug 'mzlogin/vim-markdown-toc', {'for' : 'markdown'}
Plug 'rhysd/vim-gfm-syntax', {'for' : 'markdown'}
Plug 'nelstrom/vim-markdown-folding', {'for' : 'markdown'}
" }}}
Plug 'rhysd/vim-grammarous', {'on': 'GrammarousCheck'}
Plug 'reedes/vim-wordy', { 'on': ['Wordy', 'WordyWordy'] }
Plug 'dkarter/bullets.vim' | Plug 'reedes/vim-textobj-sentence'
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

Plug 'chrisbra/Colorizer', { 'for': [ 
            \'css', 'html',
            \'javascript', 
            \'json', 'php', 
            \'xhtml', 'yaml']}

Plug 'godlygeek/tabular', { 'for': g:MARKUP, 'on' : 'Tabularize' }
" }}}  }}}

" HASKELL {{{
Plug 'Shougo/vimproc.vim', {'do' : 'make', 'for' : ['haskell']}
Plug 'eagletmt/ghcmod-vim', {'for' : 'haskell'}
Plug 'eagletmt/neco-ghc', {'for' : 'haskell'}
let g:haskellmode_completion_ghc = 0   " Disable haskell-vim omnifunc
" }}}
"
" PYTHON {{{
Plug 'klen/python-mode', { 'for': 'python' }
let g:pymode_lint_on_write = 0
let g:pymode_lint_options_pep8 = { 'max_line_length': 150 }
"let g:pymode_lint_ignore = "E303,W"
"let g:pymode_breakpoint_bind = '<localleader>b'
let g:pymode_lint_checkers = ['pyflakes']
let g:pymode_doc = 1 | let g:pymode_doc_bind = 'K'
let g:pymode_rope_show_doc_bind = '' | let g:pymode_lint = 1
let g:pymode_paths = [glob('~/Scripts/'), glob('~/Projects/')]
let g:pymode_python = 'python3'
let g:pymode_quickfix_minheight = 4
let g:pymode_rope_change_signature_bind = '<localleader>cs'
let g:pymode_rope_goto_definition_cmd = 'rightbelow vs'
let g:pymode_rope_goto_definition_bind = '<CR>'
let g:pymode_rope_regenerate_on_write = 1
let g:pymode_run_bind = '<leader>me'
let g:pymode_syntax_print_as_function = 1
let g:pymode_lint_todo_symbol = 'DO'
let g:pymode_lint_comment_symbol = 'C'
let g:pymode_lint_visual_symbol = 'V'
let g:pymode_lint_error_symbol = 'E'
let g:pymode_lint_info_symbol = 'I'
let g:pymode_lint_pyflakes_symbol = 'F'
" }}}

" SHELL {{{
Plug 'vim-scripts/bats.vim', {'for' : 'sh'}
let readline_has_bash = 1
let g:is_bash         = 1
let g:sh_fold_enabled = 4
" }}}

" WEB DEV {{{
"
" HTML {{{
Plug 'othree/html5.vim', { 'for': ['html', 'xhtml', 'php']}
Plug 'othree/html5-syntax.vim', { 'for': ['html', 'xhtml', 'php']}
Plug 'mattn/emmet-vim', { 'for': ['xml', 'html', 'xhtml', 'css', 'php' ]}
let g:emmet_html5 = 1
" }}}

" SQL {{{
let g:sql_type_default = 'mysql'
let g:ftplugin_sql_omni_key = ',' " shadows localleader
" }}}

" PHP{{{
Plug 'shawncplus/phpcomplete.vim', {'for': 'php'}
" }}} }}}
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
    Plug 'kassio/neoterm', {'on' : [
                \'TREPLSendSelection', 
                \'TREPLSendLine', 
                \'TREPLSendFile']}
    let g:neoterm_position = 'vertical'
    let g:neoterm_keep_term_open = 0
    let g:neoterm_size = 50
    "if executable('ranger') | Plug 'airodactyl/neovim-ranger' | endif
else
    "if executable('ranger') | Plug 'francoiscabrol/ranger.vim' | endif
    let g:ranger_map_keys = 0
endif

let g:netrw_scpport	= "-P 21"
let g:netrw_sshport	= "-p 21"

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
    elseif (index(g:MARKUP, &filetype) >= 0) && expand('%:r') != expand('%:e') && len(expand('%:e')) > 0 
        if expand('%:p') != '~/vimwiki/diary/diary.wiki'
            vnew ~/vimwiki/diary/diary.wiki
            setl ft=vimwiki
        else
            return 0
        endif
    elseif (index(g:PROGRAMMING, &filetype) >= 0) && expand('%:r') != expand('%:e') && len(expand('%:e')) > 0 
        vnew ~/.scratchpads/scratch.%:e
        call execute('setl ft='.&filetype)
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
    set complete=.,w, conceallevel=3 makeprg=write-good 
    setl spell formatoptions=tcrqjonl1 foldlevel=1 sw=4
    if &filetype == 'vimwiki'
        nnoremap <buffer> <Leader>x :VimwikiToggleListItem<CR>
    else
        nnoremap <buffer> <Leader>x :ToggleCheckbox<CR>
    endif
    call execute('set complete+=k'.glob('~/vimwiki').'/*/*/*.wiki')
    call execute('set complete+=k'.glob('~/vimwiki').'/*/*.wiki')
    call execute('set complete+=k'.glob('~/vimwiki').'/*.wiki')
    set complete+=k*.md
    nnoremap <buffer> <Leader>mS :setl spell<CR>:WordyWordy<CR>:Neomake<CR>:DittoSentOn<CR>:GrammarousCheck<CR>
    nnoremap <expr> <buffer> <M-Tab> exists('b:table_mode_on') && b:table_mode_on == 1 ? ":TableModeRealign\<CR>" : "\<M-Tab>"
    " nnoremap <buffer> gx vF:FhoEy:execute '!'. $BROWSER . ' ' . @+ <CR>
    if executable('wn')
        nnoremap <buffer> K :execute('Capture wn ' . expand('<cword>') . ' -over')<CR>
    endif
endfunction

function! Programming()
    setl nospell 
    set complete=.,w,t 
    if expand('%:e') != ''     " if there is an extension (needed)
        call execute('set complete+=k'.glob('~/Projects').'/*/*.'.expand('%:e').",")
        call execute('set complete+=k'.glob('~/Scripts').'/*/*.'.expand('%:e').",")
    endif
endfunction

function! Init()
    if exists("+omnifunc") && &omnifunc == ""
        setlocal omnifunc=syntaxcomplete#Complete
    endif
    if exists("+completefunc") && &completefunc == ""
        setlocal completefunc=syntaxcomplete#Complete
    endif
    if filereadable(g:DICT_DIR . &filetype . '.dict')
        call execute('setl dictionary='. g:DICT_DIR . &filetype . '.dict')
        set complete+=k,
    endif
endfunction
" }}}

" GitcommitInit() {{{
function! GitcommitInit()
    setl virtualedit=block
    set complete=.,kspell,
    setl spell 
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
    nnoremap <buffer> <CR> :call execute('Man ' . expand('<cword>'))<CR>
    set complete+=k~/.bashrc,k~/.shells/**.sh,k~/.profile,k~/.bash_profile
    set complete+=k~/Scripts/*.sh
    if executable('shfmt')
        setl formatprg=shfmt
    endif
endfunction
" }}}

" PhpInit() {{{
function! PhpInit()
    nnoremap <buffer> <Leader>me :!php % > /tmp/php-converted.html<CR>:!google-chrome-stable /tmp/php-converted.html<CR>
    if executable('js-beautify')
        setl formatprg=js-beautify\ --type\ html
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

" HaskellInit() {{{
function! HaskellInit()
     setlocal omnifunc=necoghc#omnifunc 
     nnoremap <M-CR> :TREPLSendLine<CR> 
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
    nnoremap <buffer> K :call execute('help ' . expand('<cword>'))<CR>
    setl foldmethod=marker
    set complete=.,w,
    if expand('%:t') != 'init.vim' && expand('%:t') != '.vimrc'
        if has('nvim')
            set complete+=k~/.config/nvim/init.vim,
        else
            set complete+=k~/.vimrc,
        endif
    endif
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
    nnoremap <buffer> <M-CR> :VimwikiTabnewLink<CR>
endfunction
" }}}

" MarkdownInit() {{{
function! MarkdownInit()
    if executable('markdown-preview')
        nnoremap <Leader>me :!markdown-preview %<CR>
    endif
    syn region markdownBold start="\S\@<=\*\*\|\*\*\S\@=" end="\S\@<=\*\*\|\*\*\S\@=" keepend contains=markdownLineStart
    syn region markdownBold start="\S\@<=__\|__\S\@=" end="\S\@<=__\|__\S\@=" keepend contains=markdownLineStart
    syn region markdownBoldItalic start="\S\@<=\*\*\*\|\*\*\*\S\@=" end="\S\@<=\*\*\*\|\*\*\*\S\@=" keepend contains=markdownLineStart
    syn region markdownBoldItalic start="\S\@<=___\|___\S\@=" end="\S\@<=___\|___\S\@=" keepend contains=markdownLineStart
endfunction
" }}}

" PythonInit() {{{
function! PythonInit()
    nnoremap <Leader>mS :PymodeLint<CR>
    if executable('autopep8') && executable('pycodestyle')
        setl formatprg=autopep8\ -
    elseif executable('yapf') 
        call execute('setl formatprg=yapf\ '.expand('%:p'))
    endif
    setl complete-=k
    nnoremap q :pclose<CR>
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
" CssInit() {{{
function! CssInit()
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
    set complete=.,w
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

" CTags() {{{
function! Ctags()
    if expand('%') == ''
        return 0
    endif
    let s:working_dirs = g:WORKING_DIRS
    for i in range(len(s:working_dirs))
        let s:working_dirs[i] = s:working_dirs[i].'/**.'.expand('%:e')
    endfor
    let s:working_dirs = join(s:working_dirs)
    call system('ctags -R '.s:working_dirs.' **.'.expand('%:e'))
endfunction
" }}}

" AUTOCOMMANDS {{{
aug VIMENTER
    " go back to where you left off
    au!
    au BufReadPost * if line("'\"") > 0 && line("'\"") <= line("$") | exe "normal! g`\"" | endif
    " automatically change dir to the file you are editing
    au BufEnter * try | lchdir %:p:h | catch /.*/ | endtry
    " automatically reload external changes NOTE: doesn't always work properly
    au CursorHold  * silent!  checktime
    " by default blank files are markdown notes
    au VimEnter * if &filetype == "" | setl ft=markdown | endif
    au FocusLost   * silent!  wall
    au CmdwinEnter * setlocal updatetime=2000
    au CmdwinLeave * setlocal updatetime=200
    au BufNewFile * call Template()
    au FileType * call Init()
    call execute('au! FileType '.join(g:PROGRAMMING, ',').' call Programming()')
    call execute('au! FileType '.join(g:MARKUP, ',').' call Markup()')
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
    au FileType php call PhpInit()
    au FileType markdown call MarkdownInit()
    au BufNewFile,BufRead *.txt setl ft=asciidoc
    au BufNewFile call Ctags()
    au FileType haskell call HaskellInit()
aug END
" }}}

" download dictionaries from GitHub if missing {{{
let g:DICTS = ['frequent.dict', 'haskell.dict' , 'thesaurus.txt', 'php.dict', 'css.dict', 'sql.dict', 'sh.dict', 'javascript.dict']
" let g:DICTS += ['erlang.dict', 'php.dict', 'haskell.dict', 'perl.dict', 'java.dict'] " UNCOMMENT IN NEED
for dict in g:DICTS
    if ! filereadable(g:DICT_DIR . dict) && executable('curl')
        call execute('!curl -fLo ' . g:DICT_DIR . dict . ' https://raw.githubusercontent.com/nl253/Dictionaries/master/' . dict)
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
command! CountOccurances call execute printf('%%s/%s//gn', escape(expand('<cword>'), '/')) | normal! ``
command! -bang -nargs=* GGrep call fzf#vim#grep('git grep --line-number '.shellescape(<q-args>), 0, <bang>0)
command! -complete=shellcmd -nargs=+ Capture lexpr(system(expand(<q-args>))) | topleft lopen
" }}}

" KEYBINDINGS {{{
" inoremap <expr> <Tab> pumvisible() ? "\<C-n>" : "\<Tab>"
" inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"
nnoremap <expr> <Leader>mS &filetype != 'python' ? ":Neomake\<CR>" : ":PymodeLint\<CR>"

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

"move visually highlighted lines
"vnoremap <S-k> :m '<-2<CR>gv=gv
"vnoremap <S-j> :m '>+1<CR>gv=gv
vnoremap > >gv
vnoremap < <gv

nnoremap <Leader>fed    :e $MYVIMRC<CR>
nnoremap <Leader>fer    :so $MYVIMRC<CR>
nnoremap <Leader>ga     :Git add %:p<Space>
nnoremap <Leader>gb     :Gblame<CR>
nnoremap <Leader>gd     :Gdiff<Space>
nnoremap <Leader>gc     :Gcommit<CR>
nnoremap <Leader>gs     :Gstatus<CR>
nnoremap <Leader>gW     :Gwrite<Space>
nnoremap <Leader>gD     :Gdiff<CR>
nnoremap <Leader>gm     :Gmove<Space>
if len($TMUX) > 1
    nnoremap <Leader>gp     :Gpush<CR>
    nnoremap <Leader>gf     :Gfetch<Space>
endif

nnoremap <M-k> :silent cp<CR>
nnoremap <M-j> :silent cn<CR>
nnoremap <LocalLeader>* :lgrep <cword> %:p<CR>:lopen<CR>
nnoremap <Leader>* :call execute('grep '.expand('<cword>').' '.substitute(&path,',',' ','g'))<CR>
nnoremap <C-k> :silent lp<CR>
nnoremap <C-j> :silent lne<CR>
nnoremap <Leader>a<Leader> :Ag!<CR>
nnoremap <Leader>g<Leader> :GGrep!<CR>
nnoremap <Leader>l<Leader> :Lines!<CR>
nnoremap <Leader>t<Leader> :Tags!<CR>
nnoremap <Leader>/ :History/<CR>
nnoremap <Leader>: :History:<CR>
nnoremap <Leader><Leader> :Commands!<CR>

cno w!!<CR> %!sudo tee > /dev/null %<CR>

"vim:set foldlevel=0
syntax on
