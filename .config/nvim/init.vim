
let g:MARKUP = [ 'markdown', 'ghmarkdown', 'org',
            \'gitcommit', 'markdown.ghmardown', 'ghmarkdown.markdown']
let g:PROGRAMMING =  [ 'vim', 'xhtml', 'html', 'css', 'javascript', 'python', 'php', 'sh', 'zsh' ]
let g:REPL = ['php', 'python', 'sh', 'zsh', 'javascript']

let loaded_matchit = 1
let mapleader = " "
let maplocalleader = ","

set smartcase foldmethod=marker autochdir sessionoptions-=blank completeopt=menuone,longest,preview,noinsert diffopt=filler,vertical,iwhite
set mouse= complete=.,w,t noswapfile mps+=<:> bufhidden=hide wildignorecase shiftwidth=4 autowrite undofile hidden clipboard=unnamed,unnamedplus
set wildignore+=*cache*,*chrome*,*/.dropbox/*,*intellij*,*fonts*,*libreoffice*,*.png,*.jpg,*.jpeg,tags,*~,.vim,*sessio*,*swap*,*.git,*.class,*.svn
execute 'set path+=' . glob('~') . '/**'

if has('nvim') && ! filereadable(glob('~/.local/share/nvim/site/autoload/plug.vim'))
    !curl -fLo ~/.local/share/nvim/site/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
elseif ! has('nvim') && ! filereadable(glob('~/.vim/autoload/plug.vim'))
    !curl -fLo ~/.vim/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
endif

if has('nvim')
    let g:VIMDIR = glob('~/.config/nvim/')
    let g:DICTDIR = glob('~/.config/nvim/dicts/')
    tnoremap <Esc> <C-\><C-n>
    if empty(g:DICTDIR)
        !mkdir -p ~/.config/nvim/dicts
    endif
    call plug#begin('~/.local/share/nvim/plugged/')
else
    let $MYVIMRC = glob('~/.vimrc')
    let g:VIMDIR = glob('~/.vim/')
    let g:DICTDIR = glob('~/.vim/dicts/')
    if empty(g:DICTDIR)
        !mkdir -p ~/.vim/dicts
    endif
    syntax enable
    filetype plugin indent on
    set encoding=utf8 syntax=on filetype=on autoindent nocompatible magic incsearch ttyfast
    set display=lastline formatoptions=tcqj nrformats=bin,hex complete+=i hlsearch
    if has('tags')
        set tags
    endif
    call plug#begin('~/.vim/plugged')
endif

" Place plugins here
Plug 'Haron-Prime/Antares'
Plug 'tpope/vim-dispatch', {'on' : ['Make','Dispatch','Copen','Start','Spawn']}
Plug 'tpope/vim-sleuth' " auto set buffer options
Plug 'xolox/vim-misc'
Plug 'xolox/vim-session' " enhanced session management
let g:session_autosave = 'yes'
let g:session_autosave_to = 'default'
let g:session_autosave_periodic = 3
let g:session_lock_enabled = 0
let g:session_autoload = 'yes'
let g:session_default_to_last = 1
let g:session_persist_globals = [ '&foldmethod', '&foldcolumn', '&scrolloff', '&spell', '&wrap',
            \'&scrollbind', '&number', '&relativenumber', '&foldmarker', '&background']

if has('nvim') | let g:session_directory = '~/.config/nvim/session'| else | let g:session_directory = '~/.vim/session' | endif

Plug 'tpope/vim-fugitive'
Plug 'gregsexton/gitv', { 'on': 'Gitv' }

let g:gitgutter_map_keys = 0
let g:gitgutter_diff_args = '-w'

Plug 'junegunn/vim-easy-align', { 'on' : 'EasyAlign' }
Plug 'Konfekt/FastFold' " more efficient folds
Plug 'scrooloose/nerdcommenter'

Plug 'tpope/vim-eunuch', {'on' : [ 'Move', 'Remove', 'Find', 'Mkdir', 'Wall',
            \'SudoWrite', 'SudoEdit', 'Unlink', 'Chmod', 'Rename', ]}

Plug 'reedes/vim-wordy', { 'on': ['Wordy', 'WordyWordy']}
Plug 'reedes/vim-textobj-sentence'
Plug 'neomake/neomake', {'on' : [ 'Neomake', 'NeomakeProject', 'NeomakeFile' ]}
let g:neomake_ghmarkdown_enabled_makers = ['writegood', 'proselint']
let g:neomake_markdown_enabled_makers = ['writegood', 'proselint']


" if executable('tidy') | let g:syntastic_html_checkers = ['tidy'] | endif
if executable('jsonlint') | let g:syntastic_json_checkers = ['jsonlint'] | endif
Plug 'dbmrq/vim-ditto', { 'on': [ 'ToggleDitto', 'DittoOn', 'DittoSent',
            \'DittoSentOn', 'DittoFile', 'DittoFileOn', 'DittoPar', 'DittoParOn']}

Plug 'Chiel92/vim-autoformat', { 'on': 'Autoformat' , 'do' : 'npm install -g writegood proselint'}

Plug 'xolox/vim-easytags', {'for' : g:PROGRAMMING}
let b:easytags_auto_highlight = 1
let g:easytags_events = ['BufReadPost']
let g:easytags_always_enabled = 1
let g:easytags_resolve_links = 1

Plug 'vim-scripts/utl.vim'

Plug 'dhruvasagar/vim-table-mode', { 'for': ['markdown', 'ghmardown', 'org'] }
let g:table_mode_disable_mappings = 1

Plug 'vim-scripts/utl.vim'
Plug 'jceb/vim-orgmode', {'for' : 'org'}
let g:org_todo_keywords = ['TODO', '|', 'DONE', 'PENDING', 'URGENT', 'SOON']
let g:org_heading_shade_leading_stars = 0

Plug 'xolox/vim-notes'

if empty(glob("~/nl253/vim-notes"))
    !mkdir -p ~/nl253/vim-notes
endif

let g:notes_directories = ['~/nl253/vim-notes/']

" Markdown
" ==========
Plug 'tpope/vim-markdown', { 'for': ['markdown', 'ghmarkdown']}
Plug 'jtratner/vim-flavored-markdown', { 'for': ['markdown', 'ghmarkdown']}
Plug 'plasticboy/vim-markdown', { 'for': ['markdown', 'ghmarkdown']}

let g:vim_markdown_fenced_languages = [
            \'sh', 'java', 'python',
            \'html', 'css', 'php',
            \'javascript', 'haskell']
<

let g:vim_markdown_no_default_key_mappings = 1
let g:vim_markdown_folding_style_pythonic = 1
let g:vim_markdown_folding_level = 2

Plug 'klen/python-mode', { 'for': 'python' } " Python mode

let g:pymode_breakpoint_bind = '<localleader>b'
let g:pymode_doc = 1
let g:pymode_doc_bind = '<C-q><C-q>'
let g:pymode_indent = 1
let g:pymode_lint = 0 " Neomake is better
let g:pymode_motion = 1
let g:pymode_options = 1
let g:pymode_options_colorcolumn = 1
let g:pymode_paths = ['~/Scripts/']
let g:pymode_python = 'python3'
let g:pymode_rope = 1
let g:pymode_rope_autoimport = 1
let g:pymode_rope_autoimport_import_after_complete = 1
let g:pymode_rope_change_signature_bind = '<localleader>cs'
let g:pymode_rope_complete_on_dot = 1
let g:pymode_rope_completion = 1
let g:pymode_rope_extract_method_bind = '<localleader>em'
let g:pymode_rope_extract_variable_bind = '<localleader>ev'
let g:pymode_rope_goto_definition_bind = '<C-e><C-e>'
let g:pymode_rope_module_to_package_bind = '<localleade>rmp'
let g:pymode_rope_move_bind = '<localleader>mm'
let g:pymode_rope_organize_imports_bind = '<localleader>i'
let g:pymode_rope_regenerate_on_write = 0
let g:pymode_rope_rename_bind = '<localleader>rr'
let g:pymode_rope_rename_module_bind = '<localleader>rm'
let g:pymode_rope_show_doc_bind = '<localleader>d'
let g:pymode_rope_use_function_bind = '<localleader>uf'
let g:pymode_run_bind = '<leader>me'
let g:pymode_syntax = 1
let g:pymode_syntax_all = 1
let g:pymode_syntax_print_as_function = 1
let g:pymode_trim_whitespaces = 1

if ((has('python') || has('python3')) && has('lambda') && has('timers') && has('job')) || has('nvim')
    Plug 'maralla/completor.vim'
    if filereadable(glob('~/.pyenv/versions/3.5.0/bin/python3.5'))
        let g:completor_python_binary = glob('~/.pyenv/versions/3.5.0/bin/python3.5')
    else
        let g:completor_python_binary = '/usr/bin/python'
    endif
    let g:completor_completion_delay = 1
endif

if has('python') || has('python3')
    Plug 'SirVer/ultisnips' " Track the engine.
    Plug 'honza/vim-snippets' " Snippets are separated from the engine. Add this if you want them:
    let g:UltiSnipsExpandTrigger="<tab>"
    let g:UltiSnipsEditSplit="vertical"
endif

Plug 'othree/html5.vim', { 'for': ['html', 'xhtml']}
Plug 'othree/html5-syntax.vim'
Plug 'mattn/emmet-vim', { 'for': ['xml', 'html', 'xhtml', 'css' ]}

Plug 'chrisbra/Colorizer', { 'for': [
            \ 'css', 'html', 'javascript', 'json', 'markdown',
            \ 'org', 'ghmarkdown', 'xhtml', 'yaml']}

let g:emmet_html5 = 1

Plug 'pangloss/vim-javascript', { 'for': ['javascript'] }

Plug 'maksimr/vim-jsbeautify', { 'for': [ 'javascript', 'json', 'html',
            \'xhtml', 'xml', 'css'] }

Plug 'wellle/targets.vim'

function! Scratch()
    if index(['netrw', 'terminal','gitcommit'], &filetype) >= 0  " blacklist
        return 1
    endif
    if index(g:PROGRAMMING, &filetype) >= 0 && expand('%:r') != expand('%:e') && len(expand('%:e')) > 0
        execute 'vnew ' . '~/nl253/scratch.' . expand('%:e')
        execute 'setl ft=' . &filetype
    elseif index(g:MARKUP, &filetype) >= 0
        vnew ~/nl253/scratch
        setl ft=note
    elseif &filetype == 'help'
        vnew ~/nl253/scratch.vim
        setl ft=vim
    elseif &filetype == 'man'
        vnew ~/nl253/scratch.sh
        setl ft=sh
    else
        vnew ~/nl253/scratch
        setl ft=note
    endif
    vertical resize 60
    write
    nnoremap <buffer> <BS> :close!<CR>
    nnoremap <buffer> q :close!<CR>
    vnoremap <buffer> <BS> <Nop>
endfunction
command! Scratch call Scratch()
nnoremap <BS> :Scratch<CR>
vnoremap <BS> :yank<CR>:Scratch<CR>p
nnoremap <Leader><BS> :edit ~/nl253/todo.org<CR>``

aug VIMENTER
    au FileType * if exists("+omnifunc") && &omnifunc == "" | setlocal omnifunc=syntaxcomplete#Complete | endif
    au FileType * if exists("+completefunc") && &completefunc == "" | setlocal completefunc=syntaxcomplete#Complete | endif
    au FileType * if index(g:MARKUP, &filetype) < 0 | setlocal nospell | endif
    au BufReadPost * if line("'\"") > 0 && line("'\"") <= line("$") | exe "normal! g`\"" | endif
    au BufEnter * try | lchdir %:p:h | catch /.*/ | endtry
    au CursorHold  * silent!  checktime
    au FocusLost   * silent!  wall
    au CmdwinEnter * setlocal updatetime=2000
    au CmdwinLeave * setlocal updatetime=200
    au FileType * if index(g:MARKUP, &filetype) < 0 | setl complete=.,w,t, | else | setl complete=.,w,k,s | endif
    au Filetype markdown setlocal ft=ghmarkdown | let g:table_mode_corner = '|'
    au BufReadPost,BufNew *.org let g:table_mode_corner = '+'
    au BufReadPost,BufNew *.org,*.md,*.mmd nnoremap <buffer> <M-Tab> :TableModeRealign<CR>
    au FileType xhtml,html nnoremap <buffer> <Leader>me :execute '!$BROWSER ' . expand('%:p')<CR>
au FileType markdown,ghmardown nnoremap <buffer> <Leader>me :execute '!pandoc -f markdown_github -t html -o /tmp/' . expand('%') . '.html'<CR>:execute '!$BROWSER /tmp/' . expand('%') . '.html'<CR>
    au FileType gitcommit setl spell
    au FileType man setl nowrap
    au FileType org setlocal foldlevel=2
    au FileType help nnoremap <buffer> q :bd!<CR> | nnoremap <buffer> <CR> <C-]> | nnoremap <buffer> <BS> <C-o> | nnoremap <buffer> d <C-d> | nnoremap <buffer> u <C-u>
    au FileType qf nnoremap <buffer> <C-n> j<CR><C-w><C-w> | nnoremap <buffer> <C-p> k<CR><C-w><C-w> | nnoremap q :cclose<CR>
aug END

if executable('pdftotext')
    au! BufRead *.pdf execute '!pdftotext ' . expand('%:p') . ' ' . expand('%:p:r') .   '.txt'
    au! BufReadPost *.pdf execute 'enew ' . expand('%:p:r') . '.txt'
endif

if has('nvim')
    Plug 'roxma/python-support.nvim', {'on' : ['PythonSupportInitPython2', 'PythonSupportInitPython3']}
    let g:python_support_python3_requirements = add(get(g:,'python_support_python3_requirements',[]),'jedi')
    let g:python_support_python3_requirements = add(get(g:,'python_support_python3_requirements',[]),'yapf')
    let g:python_support_python3_requirements = add(get(g:,'python_support_python3_requirements',[]),'pep8')
    let g:python_support_python3_requirements = add(get(g:,'python_support_python3_requirements',[]),'flake8')
    if ! has('python')
        PythonSupportInitPython2
    endif
    if ! has('python3')
        PythonSupportInitPython3
    endif
    Plug 'kassio/neoterm', {'on' : ['TREPLSendSelection', 'TREPSendLine', 'TREPLSendFile']}
    nnoremap <expr> <M-CR> index(g:REPL, &filetype) >= 0 ? ":TREPLSendLine\<CR>" : "\<M-CR>"
    vnoremap <expr> <M-CR> index(g:REPL, &filetype) >= 0 ? ":TREPLSendSelection\<CR>" : "\<M-CR>"
    let g:neoterm_position = 'vertical'
    let g:neoterm_keep_term_open = 0
    let g:neoterm_size = 50
    Plug 'sbdchd/neoformat', {'on' : 'Neoformat'}
    if executable('ranger') | Plug 'airodactyl/neovim-ranger' | endif
endif

if executable('fzf')
    Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
    Plug 'junegunn/fzf.vim'
endif

call plug#end()

if ! filereadable(g:DICTDIR . 'frequent.dict')
    execute '!curl -o ' . g:DICTDIR . 'frequent.dict https://raw.githubusercontent.com/nl253/VimScript/master/dicts/frequent.dict'
    execute 'set dictionary=' .  g:DICTDIR . 'frequent.dict'
endif
if ! filereadable(g:VIMDIR .'thesaurus.txt')
    execute '!curl -o ' . g:VIMDIR . 'thesaurus.txt https://raw.githubusercontent.com/nl253/VimScript/master/thesaurus.txt'
endif
execute 'set thesaurus=' . g:VIMDIR . 'thesaurus.txt'
if ! filereadable(g:DICTDIR .'css.dict')
    execute '!curl -o ' . g:DICTDIR . 'css.dict https://raw.githubusercontent.com/nl253/VimScript/master/dicts/css.dict'
endif
au! FileType css execute 'setlocal dictionary=' . g:DICTDIR . 'css.dict'
if ! filereadable(g:DICTDIR .'mysql.txt')
    execute '!curl -o ' . g:DICTDIR . 'mysql.txt https://raw.githubusercontent.com/nl253/VimScript/master/dicts/mysql.txt'
endif
au! FileType sql,mysql execute 'setlocal dictionary=' . g:DICTDIR . 'mysql.txt'

colorscheme antares

if executable('pandoc')
    command! TOman execute '!pandoc -s -o ' expand('%:p:r') . '.1  -t man ' . expand('%:p') | sleep 250ms | execute 'vs  ' . expand('%:p:r') . '.1'
    command! TOmarkdown execute '!pandoc -s -o ' expand('%:p:r') . '.md  -t markdown_github ' . expand('%:p') | sleep 250ms | execute 'vs  ' . expand('%:p:r') . '.md'
    command! TOrst execute '!pandoc -s -o ' expand('%:p:r') . '.rst  -t rst ' . expand('%:p') | sleep 250ms | execute 'vs  ' . expand('%:p:r') . '.rst'
    command! TOtex execute '!pandoc -s -o ' expand('%:p:r') . '.tex  -t tex ' . expand('%:p') | sleep 250ms | execute 'vs  ' . expand('%:p:r') . '.tex'
    command! TOwordocx execute '!pandoc -s -o ' expand('%:p:r') . '.docx  -t docx ' . expand('%:p') | sleep 250ms | execute 'vs  ' . expand('%:p:r') . '.docx'
endif

if executable('dos2unix')
    command! Dos2Unix !dos2unix -F -n %:p %:p | edit
endif

command! DeleteEmptyLines execute 'g/^\s*$/d'

command! CountOccurances execute printf('%%s/%s//gn', escape(expand('<cword>'), '/')) | normal! ``

inoremap <expr> <Tab> pumvisible() ? "\<C-y>\<Space>" : "\<Tab>"
inoremap <expr> <CR> pumvisible() ? "\<C-y>" : "\<CR>"

nnoremap <Leader>fed    :e $MYVIMRC<CR>
nnoremap <Leader>fer    :so $MYVIMRC<CR>
nnoremap <Leader>gA     :GHActivity<CR>
nnoremap <Leader>ga     :GHActivity<Space>
nnoremap <Leader>gb     :Gblame<CR>
nnoremap <Leader>gB     :Gbrowse<CR>
nnoremap <Leader>gc     :Gcommit<CR>
nnoremap <Leader>gd     :Gdiff<Space>
nnoremap <Leader>gD     :Gdiff<CR>
nnoremap <Leader>gf     :Gfetch<Space>
nnoremap <Leader>gm     :Gmove<Space>
nnoremap <Leader>gh     :GHDashboard<CR>
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

if executable('fzf')
    let g:PREVIEW = ' --preview "head -n 20 {} " '
    let g:IGNORE_REGEXP = "grep -P -v \"(\d{4,}$)|(~$)|".
                \"(.*(c|C)ache.*)|(.*\.git.*)|(.*\.(png)|(jpeg)|(bluej)|(ctxt)|(hg)|(svn)".
                \"|(bak)|(jpg)|(so)|(pyc)|(obj)|(out)|(class)|(swp)|(xz)|(svn)|(swp)|(ri))\""
    let g:DIR_IGNORE_REGEXP = 'grep -P -v "^/(dev)|(tmp)|(mnt)|(root)"'

    command! FZFMru call fzf#run(fzf#wrap({
                \'source':  v:oldfiles,
                \'options': g:FZF_COLORS . '--multi -x +s --preview "head -n 30 {}"',
                \'up':    '50%'
                \}))

    command! FZFFileAnchor call fzf#run(fzf#wrap({
                \'source': ' (git ls-tree -r --name-only HEAD || find . -path "*/\.*" -prune -o -type f -print -o -type l -print | ' . g:IGNORE_REGEXP . ' | sed s/^..//) 2> /dev/null',
                \'options': '-x +s --reverse --preview "head -n 38 {}"',
                \'up': '80%',
                \}))

    command! FZFRecFilesHome execute 'lcd ' . glob("~") | call fzf#run(fzf#wrap({
\'source': 'find ~ 2>/dev/null | grep -P -v "(\[0-9]{4,}$)|(~$)|(\.(png)|(jpeg)|(bluej)|(ctxt)|(hg)|(svn)|(bak)|(jpg)|(so)|(pyc)|(obj)|(out)|(class)|(swp)|(xz)|(svn)|(swp)|(ri)$)" | grep -v "%" | grep -v chrome | grep -v ".git" | grep -v -i cache | sed -E "s/^\/home\/\w+\///"',
                \'options': '-x +s --reverse --preview "head -n 38 {}"',
                \'up': '80%',
                \}))
    nnoremap <C-v> :FZFFileAnchor<CR>
    nnoremap <C-x><C-f> :FZFRecFilesHome<CR>
    nnoremap <C-x><C-r> :FZFMru<CR>
endif

nnoremap <C-x><C-a> :execute 'Ggrep ' . expand('<cword>') . " * "<CR>

