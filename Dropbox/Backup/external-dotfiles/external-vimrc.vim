let g:VIMDIR = glob('~/.vim') . '/'
let g:SCRATCHDIR = g:VIMDIR . 'scratchpads/'
let g:NOTESDIR = glob('~') . '/nl253/'
let g:SHELLESCAPE = '!'
"
" EXTERNAL VIMRC
" --------------
" UTILS
function! BashExecuteLine()
    let s:line = getline('.')
    let s:line = escape(s:line, '"')
    execute '!bash -c "' . s:line . '"'
endfunction
function! PythonExecuteLine()
    let s:line = getline(".")
    let s:line =  substitute(s:line, "\<Space>*", "", "")
    let s:line = escape(s:line, '"')
    execute '!python3 -c ' . '"' .  s:line . '"'
endfunction
function! MatchingQuotes()
    inoremap <buffer> ( ()<left>
    inoremap <buffer> [ []<left>
    inoremap <buffer> < <><left>
    inoremap <buffer> { {}<left>
    inoremap <buffer> " ""<left>
    inoremap <buffer> ' ''<left>
    inoremap <buffer> ` ``<left>
endfunction
function! Replace()
    let s:word = input('Replace ' . expand('<cword>') . ' with:')
    execute '%s/\<' . expand('<cword>') . '\>/' . s:word . '/ge'
    unlet! s:word
endfun
function! CompileRun()
    exec 'w'
    if &filetype == 'c'
	if has("win32") || has("win64")
	    exec '!gcc -Wall -std=c11 -o %:r %:p && %:r.exe'
	    " exec "AsyncRun! gcc % -o %<; time ./%<"
	else
	    exec '!clang -Wall -std=c11 -o %:r %:p && ./%:r'
	endif
    elseif &filetype == 'cpp'
	if has("win32") || has("win64")
	    exec '!g++ -Wall -std=c++14 -o %:r %:p && %:r.exe'
	    "exec "AsyncRun! g++ -std=c++11 % -o %<; time ./%<"
	else
	    exec '!clang++ -Wall -std=c++14 -o %:r %:p && ./%:r'
	endif
    elseif &filetype == 'objc'
	if has("macunix")
	    exec '!clang -fobjc-arc -framework Foundation %:p -o %:r && ./%:r'
	endif
    elseif &filetype == 'swift'
	if has("macunix")
	    exec '!swift %:p'
	endif
    elseif &filetype == 'd'
	if has("win32") || has("win64")
	    exec '!dmd -wi %:p && del %:r.obj && %:r.exe'
	else
	    exec '!dmd -wi %:p && rm %:r.o && ./%:r'
	endif
    elseif &filetype == 'rust'
	if has("win32") || has("win64")
	    exec '!rustc %:p && %:r.exe'
	else
	    exec '!rustc %:p && ./%:r'
	endif
    elseif &filetype == 'go'
	if has("win32") || has("win64")
	    exec '!go build %:p && %:r.exe'
	else
	    exec '!go build %:p && ./%:r'
	endif
    elseif &filetype == 'nim'
	if has("win32") || has("win64")
	    exec '!nim c %:p && %:r.exe'
	else
	    exec '!nim c %:p && ./%:r'
	endif
    elseif &filetype == 'crystal'
	if has("win32") || has("win64")
	    exec '!crystal build %:p && %:r.exe'
	else
	    exec '!crystal build %:p && ./%:r'
	endif
    elseif &filetype == 'vala'
	if has("win32") || has("win64")
	    exec '!valac %:p && %:r.exe'
	else
	    exec '!valac %:p && ./%:r'
	endif
    elseif &filetype == 'java'
	exec '!javac %:p && java %:r'
	"exec "AsyncRun! javac %; time java %<"
    elseif &filetype == 'groovy'
	exec '!groovy %:p'
    elseif &filetype == 'kotlin'
	exec '!kotlinc %:p -include-runtime -d %:r.jar && kotlin %:r.jar'
    elseif &filetype == 'scala'
	exec '!scala %:p'
    elseif &filetype == 'clojure'
	exec '!lein exec %:p'
    elseif &filetype == 'cs'
	if has("win32") || has("win64")
	    exec '!csc %:p && %:r.exe'
	else
	    exec '!mcs %:p && mono %:r.exe'
	endif
    elseif &filetype == 'fsharp'
	if has("win32") || has("win64")
	    exec '!fsc %:p && %:r.exe'
	else
	    exec '!fsharpc %:p && mono %:r.exe'
	endif
    elseif &filetype == 'erlang'
	exec '!escript %:p'
    elseif &filetype == 'elixir'
	exec '!elixir %:p'
    elseif &filetype == 'lfe'
	exec '!lfe %:p'
    elseif &filetype == 'scheme' || &filetype == 'racket'
	exec '!racket -fi %:p'
    elseif &filetype == 'newlisp'
	exec '!newlisp %:p'
    elseif &filetype == 'lisp'
	exec '!sbcl --load %:p'
    elseif &filetype == 'ocaml'
	if has("win32") || has("win64")
	    exec '!ocamlc -o %:r.exe %:p && %:r.exe'
	else
	    exec '!ocamlc -o %:r %:p && ./%:r'
	endif
    elseif &filetype == 'haskell'
	if has("win32") || has("win64")
	    exec '!ghc -o %:r %:p && %:r.exe'
	else
	    exec '!ghc -o %:r %:p && ./%:r'
	endif
    elseif &filetype == 'lua'
	exec '!lua %:p'
    elseif &filetype == 'perl'
	exec '!perl %:p'
    elseif &filetype == 'perl6'
	exec '!perl6 %:p'
    elseif &filetype == 'php'
	exec '!php %:p'
    elseif &filetype == 'python'
	exec "AsyncRun! time python %:p"
    elseif &filetype == 'ruby'
	exec "AsyncRun! time ruby %"
    elseif &filetype == 'julia'
	exec '!julia %:p'
    elseif &filetype == 'dart'
	exec '!dart %:p'
    elseif &filetype == 'elm'
	exec '!elm make %:p'
    elseif &filetype == 'haxe'
	exec '!haxe -main %:r --interp'
    elseif &filetype == 'javascript'
	exec '!node %:p'
    elseif &filetype == 'coffee'
	exec '!coffee -c %:p && node %:r.js'
    elseif &filetype == 'typescript'
	exec '!tsc %:p && node %:r.js'
    elseif &filetype == 'ls'
	exec '!lsc -c %:p && node %:r.js'
    elseif &filetype == 'r'
	exec '!Rscript %:p'
    elseif &filetype == 'sh'
	exec '!bash %:p'
	"exec "AsyncRun! time bash %"
    elseif &filetype == 'scss'
	exec '!scss %:p > %:r.css'
    elseif &filetype == 'less'
	exec '!lessc %:p > %:r.css'
    elseif &filetype == 'html' || &filetype == 'xhtml'
	execute 'AsyncRun google-chrome-stable %:p'
    endif
endfunc
function! Scratchpad()
    if (&filetype != 'help') || (&filetype != 'man')
	let s:scratchname = g:SCRATCHDIR . "scratchpad." . expand("%:e")
    elseif (&filetype == 'help') || (&filetype == 'man')
	let s:scratchname = g:SCRATCHDIR . "scratchpad.vim"
    endif
    if !filereadable(s:scratchname)
	silent execute '!touch s:scratchname'
    else
    endif
    let s:toexecute = 'vnew '
    let s:toexecute = s:toexecute . s:scratchname
    try
	silent execute s:toexecute
    catch /.*/
    endtry
endfunction
function! ApplyCword(command)
    let s:word = expand('<cword>')
    let s:command = a:command . ' ' . s:word
    execute s:command
endfunction
function! TryChangeFiletype(type)
    if &filetype != a:type
	execute 'setlocal filetype=' . a:type
    endif
endfunction
function! TrimWhitespace()
    let l:save = winsaveview()
    %s/\s\+$//e
    call winrestview(l:save)
endfunction
function! ToggleLineLenghtGuide()
    let s:ccmn = &colorcolumn
    " Inversion
    if s:ccmn > 0
	set colorcolumn=0
	silent echomsg 'LINE LEN GUIDE OFF'
    else
	set colorcolumn=80
	silent echomsg 'LINE LEN GUIDE ON'
    endif
endfunction
function! ToggleBG()
    if &background == "dark"
	set background=light
	silent echomsg 'BACKGROUND MADE LIGHT'
    elseif &background == "light"
	set background=dark
	silent echomsg 'BACKGROUND MADE DARK'
    else
	set background=dark
	silent echomsg 'BACKGROUND MADE DARK'
    endif
endfunction
function! ToggleScrollOff()
    if &scrolloff < 30
	set scrolloff=50
	silent echomsg 'SCROLLOFF ON'
    else
	set scrolloff=3
	silent echomsg 'SCROLLOFF OFF'
    endif
endfunction
function! CopyToTmp()
    normal ggyG
    execute 'e /tmp/test_' . expand('%:t')
    normal p
    normal w
endfunction
function! CheckIfInList(item, listToBeChecked)
    for i in a:listToBeChecked
	if i == a:item
	    return 1
	endif
    endfor
    return 0
endfunction
function! PrependSomething(x)
    let s:currline=getline('.')
    let s:currline=a:x . ' ' . s:currline
    call setline('.',s:currline)
endfunction
function! BufOnly(buffer, bang)
    if a:buffer == ''
	" No buffer provided, use the current buffer.
	let buffer = bufnr('%')
    elseif (a:buffer + 0) > 0
	" A buffer number was provided.
	let buffer = bufnr(a:buffer + 0)
    else
	" A buffer name was provided.
	let buffer = bufnr(a:buffer)
    endif
    if buffer == -1
	echohl ErrorMsg
	echomsg "No matching buffer for" a:buffer
	echohl None
	return
    endif
    let last_buffer = bufnr('$')
    let delete_count = 0
    let n = 1
    while n <= last_buffer
	if n != buffer && buflisted(n)
	    if a:bang == '' && getbufvar(n, '&modified')
		echohl ErrorMsg
		echomsg 'No write since last change for buffer'
			    \ n '(add ! to override)'
		echohl None
	    else
		silent exe 'bdel' . a:bang . ' ' . n
		if ! buflisted(n)
		    let delete_count = delete_count+1
		endif
	    endif
	endif
	let n = n+1
    endwhile
    if delete_count == 1
	echomsg delete_count "buffer deleted"
    elseif delete_count > 1
	echomsg delete_count "buffers deleted"
    endif
endfunction

" MODES
function! BashMode()
    command! BashRun execute '!bash %:p'
    nnoremap <buffer> <Leader>me :BashRun<CR>
    nnoremap <buffer> <CR> :call BashExecuteLine()<CR>
    nnoremap <buffer> <localleader>s :set filetype=sh<CR>
    nnoremap <buffer> <localleader>p :set filetype=python<CR>
endfunction
function! PythonMode()
    command! -buffer PythonRun execute '!python %:p'
    nnoremap <buffer> <Leader>me :PythonRun<CR>
    nnoremap <buffer> <CR> :call ExecuteLine()<CR>
    setlocal wrap
    setlocal omnifunc=pythoncomplete#Complete
    setlocal foldlevel=1
    setlocal noshowcmd
    setlocal comments+=:#
    setlocal textwidth=80
    setlocal commentstring=#%s
    setlocal define=^\s*\\(def\\\\|class\\)
endfunction
function! VimMode()
    setlocal foldmethod=indent
    nnoremap <buffer> <CR> :call ExecutionLine()<CR>
    nnoremap <buffer> <Leader>me :source %<CR>
endfunction
function! JavascriptMode()
    setlocal conceallevel=3
    setlocal tabstop=4
    setlocal shiftwidth=4
    setlocal expandtab
    setlocal softtabstop=4
    setlocal cindent
    setlocal smartindent
    setlocal complete=.,w,t,
    setlocal foldlevel=1
endfunction
function! MarkdownMode()
    setlocal foldmethod=expr
    setlocal foldlevel=2
    setlocal conceallevel=3
    setlocal foldmethod=expr
    setlocal synmaxcol=1000
    "
    setlocal comments+=:>
    setlocal comments+=:-
    setlocal comments+=:+
    setlocal comments+=:*
    setlocal comments+=::
    setlocal comments+=n:>
    setlocal comments+=n:-
    setlocal comments+=n:+
    setlocal comments+=n:*
    setlocal comments+=n::
endfunction
function! HTMLMode()
    setlocal expandtab
    setlocal shiftwidth=2
    setlocal tabstop=2
    setlocal softtabstop=2
    setlocal expandtab
    setlocal complete=.,w,t,
    setlocal foldmethod=indent
    setlocal foldlevel=3
    setlocal omnifunc=htmlcomplete#CompleteTags
endfunction
function! CSSMode()
    setlocal omnifunc=csscomplete#CompleteCSS noci
    setlocal complete=.,w,b,
    setlocal foldmethod=marker
    setlocal foldmarker=/*,*/
endfunction
"
let g:mapleader = ' '
let g:maplocalleader = ','
"
set shell=/bin/bash
set statusline=\ %{HasPaste()}%F%m%r%h\ %w\ \ CWD:\ %r%{getcwd()}%h\ \ \ Line:\ %l\ \ Column:\ %c
set wildchar=<TAB>
"
set lispwords+=defroutes " Compojure
set lispwords+=defpartial,defpage " Noir core
set lispwords+=defaction,deffilter,defview,defsection " Ciste core
set lispwords+=describe,it " Speclj TDD/BDD
"
if has("wildmenu")
    set wildmode=full
    set wildmenu
endif

set nocompatible
set encoding=utf8
set autoindent
set display=lastline
if has("extra_search")
    set incsearch
endif
set ttyfast
set tabpagemax=50
set nrformats=bin,hex
if has("reltime")
    set hlsearch
endif
set complete-=i
"
" search
set grepformat=%f:%l:%c:%m,%f:%l:%m
set magic " for regular expressions
set gdefault
set ignorecase
set timeoutlen=1500
"
" insert mode completion
if has("spell")
    set nospell
    set spelllang=en_gb
endif
if has("insert_expand")
    set complete=.,w,
    set completeopt=menuone,longest,preview,
    set completeopt+=noinsert
    "set completeopt+=noselect
endif
"
if has("wildmenu") && has("wildignore")
    set wildignore+=*/.git/*,*/.hg/*,*/.svn/*
    set wildignore+=*.o,*.out,*.obj,.git,*.pyc,*.class
    set wildignore+=*.swp,*~,._*
    set wildignore+=*.so,*.swp
    set wildignore+=*.bak,*.swo,*.a
    set wildignore+=*.jpg,*.jpeg,*.gif,*.png,*.pdf
    set wildignore+=*/.git*
    set wildignore+=*.cscopeout
    set wildignore+=*/bower_components/*,*/node_modules/*
    set wildignore+=*/nginx_runtime/*,*/build/*,*/logs/*
endif
"
" UI
set showmode
set background=dark
set fillchars=fold:\ 
set noruler
set nonumber
set nocursorline
set scrolloff=50
set mouse=
"
set backspace=indent,eol,start
"
"sessions
if has('mksession')
    set sessionoptions+=tabpages,globals
    set sessionoptions-=buffers
    set sessionoptions-=options
    set sessionoptions-=blank "recommended by SyntasticCheck
endif
set lazyredraw " reduce unnecessary refreshing
if has("clipboard")
    set clipboard=unnamed,unnamedplus
endif
"
set filetype=on
if has('syntax')
    syntax enable
endif
filetype plugin indent on
set syntax=on

"Editing
set pastetoggle="<C-p>"
set expandtab
set foldmethod=marker
set foldminlines=1
set foldnestmax=5 " Set max fold nesting level
set foldcolumn=3
set autochdir
set novisualbell
set nostartofline
set scroll=10
set matchtime=10
set noshowmatch
set shiftwidth=4
set tabstop=4
set undolevels=100
set wrap
set updatecount=100
set path+=**
"
" Quickformat
set formatoptions=
"setlocal formatoptions+=a  " Automatic Formatting :: text is formatted automatically when inserting text or deleting text.  
"set formatoptions+=r " Continue comments by default
"set formatoptions+=t " Format text
"set formatoptions+=w  " Trailing white space indicates a paragraph continues in the next line.
set formatoptions+=1 " Break before 1-letter words
set formatoptions+=2 " Use indent from 2nd line of a paragraph
set formatoptions+=c " Format comments
set formatoptions+=j  "Where it makes sense, remove a comment leader when joining lines.  For
set formatoptions+=l " Don't break lines that are already long in insert mode
set formatoptions+=n " Recognize numbered lists
set formatoptions+=o " Make comment when using o or O from comment line
set formatoptions+=q " Format comments with gq
"
" buffers
set switchbuf=usetab,
set autowrite
set autowriteall
set autoread
set bufhidden=hide
set hidden
"
" diff
set diffopt=filler,vertical
set diffopt+=iwhite " Ignore whitespace changes (focus on code changes)
"
" formatting
set matchpairs+=<:>
set synmaxcol=333
"
" windows
set splitbelow
set splitright
"
set noerrorbells
set bomb
set updatetime=200
set fileformats=unix
"
if (has("termguicolors"))
    set termguicolors
endif
colorscheme desert
"
"backup
set nobackup
set noswapfile
if has('persistent_undo')
    silent !mkdir ~/.vim/backups > /dev/null 2>&1
    set undodir=~/.vim/backups
    set undofile
endif
"set pastetoggle=<F9>
"set modelines=2
let &grepprg = 'grep -rn $* *'
command! Bashrc e ~/.bashrc
command! CopyToTmp call CopyToTmp()
"
" PROGRAMMINNG
" ------------
command! Scratchpad call Scratchpad()
command! CompileRun call CompileRun()
"
" SEARCH
" ------
command! -nargs=1 -bar Grep execute 'silent! grep! <q-args>' | redraw! | copen
command! -nargs=1 RecurGrep lvimgrep /<args>/gj ./**/*.* | lopen | set nowrap
"
" TOGGLABLES
"------------
command! ToggleLineLenghtGuide call ToggleLineLenghtGuide()
command! ToggRelLineNum setlocal relativenumber!
command! ToggLineNumber setlocal number!
command! ToggFollow windo setlocal scrollbind!
command! ToggleBG call ToggleBG()
command! ToggleScrollOff call ToggleScrollOff()
command! ToggleCursorLine setlocal cursorline!
command! ToggleSpell setlocal spell!
"
command! -bar -nargs=1 -complete=file E :exe 'edit ' . substitute(<q-args>,'\(.*\):\(\d\+\):\=$','+\2 \1','')
"
" UTILS
"-------
command! CurrentTimeAndDate echomsg strftime('%c') | let @+=strftime('%c')
command! YankAll execute '%yank'
command! Replace call Replace()
command! CDToParent execute 'cd ' . expand('%:p:h')
command! RemoveDuplicateLines execute 'normal %s/^\(.*\)\n\1$/\1/'
" Yank buffer's absolute path to X11 clipboard
command! YankPath execute 'let @+="' . expand("%:p") . '"' | echo 'Path yanked'
command! DeleteEmptyLines execute 'g/^\s*$/d'
command! Chomp %s/\s\+$// | normal! ``
command! -nargs=1 CountOccurances execute printf('%%s/%s//gn', escape(<q-args>, '/')) | normal! ``
command! TrimWhitespace call TrimWhitespace()
command! AlignAll normal gg=G
command! -nargs=? -complete=buffer -bang BufOnly call BufOnly('<args>', '<bang>')
"
"  KEYMAP
" --------
" comma always followed by space
inoremap  ,  ,<Space>
"change the behavior of the <Enter> key when the popup menu is visible.
"In that case the Enter key will simply select the highlighted menu item, 
"just as <C-Y> does.
inoremap <expr> <CR> pumvisible() ? "\<C-Y>" : "\<CR>"
" tab will do the same but also insert a space
inoremap <expr> <Tab> pumvisible() ? "\<C-Y>\<Space>" : "\<Tab>"
" go down / up the popup menu list if it is present
" otherwise go 1 line down / up
imap <expr><C-j>   pumvisible() ? "\<C-n>" : "\<Down>"
" Ctrl-k just like in Bash will delete till end of line in insert mode
imap <expr><C-k>   pumvisible() ? "\<C-p>" : "\<C-o>D"
" Unmap conflicting keybindings
try
    nunmap <C-x>
catch /.*/
endtry
nnoremap <S-q> <Nop>
nnoremap <C-q> <Nop>
nnoremap <S-y> y$:echo 'Yanked until the end of line'<CR>
"
" Faster split resizing (+,-)
if bufwinnr(1)
    map + <C-W>+
    map _ <C-W>-
endif
" }}
nnoremap <M--> :vertical resize -15<CR>
nnoremap <M-=> :vertical resize +15<CR>
set ttimeout
if &ttimeoutlen == -1
    set ttimeoutlen=50
endif

" normal map
" -----------
" if accidentally pressed in normal mode 
" these will put you in insert
" and delete word forward
nnoremap <M-BS> i<C-w>
" back and forward a word
" if accidentally pressed in normal mode 
" these will put you in insert
nnoremap <M-f> :startinsert<CR><C-o>w
nnoremap <M-b> :startinsert<CR><C-o>b

" command mode map
" -----------
" jump beggining of line
cnoremap <C-A> <Home>
" jump to the end of line
cnoremap <C-E> <End>
" move back by a letter
cnoremap <C-b> <Left>
" move by a letter forwards
cnoremap <C-f> <Right>
"cnoremap <C-k> <C-U>
" jump in history
cnoremap <C-N> <Down>
cnoremap <C-P> <Up>
" delete a letter backwords
cnoremap <C-d> <Del>
" jump beteen words
cnoremap <M-b> <S-Left>
cnoremap <M-f> <S-Right>

cnoremap <expr> <C-D> getcmdpos()>strlen(getcmdline())?"\<Lt>C-D>":"\<Lt>Del>"
cnoremap <expr> <C-F> getcmdpos()>strlen(getcmdline())?&cedit:"\<Lt>Right>"

" insert map
" -----------
" line up and down
inoremap <C-n> <Down>
inoremap <C-p> <Up>
" jump beggining of line
inoremap <C-a> <Home>
" jump to the end of line
inoremap <C-e> <End>
"inoremap <C-X><C-A> <C-A>
" when exit with C-c leave a mark
inoremap <C-C> <Esc>`^
" move the cursor right
inoremap <C-l> <C-o>a
" bash - undo
inoremap <C-x><C-u> <C-o>u
inoremap <M-BS> <C-w>
"
inoremap <expr> <C-B> getline('.')=~'^\s*$'&&col('.')>strlen(getline('.'))?"0\<Lt>C-D>\<Lt>Esc>kJs":"\<Lt>Left>"
inoremap <expr> <C-D> col('.')>strlen(getline('.'))?"\<Lt>C-D>":"\<Lt>Del>"
inoremap <expr> <C-E> col('.')>strlen(getline('.'))<bar><bar>pumvisible()?"\<Lt>C-E>":"\<Lt>End>"
inoremap <expr> <C-F> col('.')>strlen(getline('.'))?"\<Lt>C-F>":"\<Lt>Right>"
inoremap <C-C> <Esc>`^

" to be needs recursive
imap <M-c> <Esc>gciwea
inoremap <M-l> <C-o>guiw<C-o>e<C-o>l
inoremap <M-u> <C-o>gUiw<C-o>e<C-o>l
"
" emacs page up and down
inoremap <M-v> <PageUp>
inoremap <C-v> <PageDown>
"
if (&tildeop)
    nmap gcw guw~l
    nmap gcW guW~l
    nmap gciw guiw~l
    nmap gciW guiW~l
    nmap gcis guis~l
    nmap gc$ gu$~l
    nmap gcgc guu~l
    nmap gcc guu~l
    vmap gc gu~l
else
    nmap gcw guw~h
    nmap gcW guW~h
    nmap gciw guiw~h
    nmap gciW guiW~h
    nmap gcis guis~h
    nmap gc$ gu$~h
    nmap gcgc guu~h
    nmap gcc guu~h
    vmap gc gu~h
endif
"
" TAB
" ----
nnoremap <S-j> gT
nnoremap <S-k> gt
nnoremap <Leader>tt :tabs<CR>
nnoremap <C-t>n     :tabnew<CR>
nnoremap <C-t>d     :tabclose<CR>
nnoremap <C-t><C-d> :tabclose!<CR>
nnoremap <C-t>k     :tabclose<CR>
nnoremap <C-t><C-k> :tabclose<CR>
nnoremap <C-t>o     :tabonly<CR>
nnoremap <C-t><C-o> :tabonly!<CR>
nnoremap <C-t><C-b> :tab ball<CR>
nnoremap <Leader>1  :normal 1gt<CR>
nnoremap <Leader>2  :normal 2gt<CR>
nnoremap <Leader>3  :normal 3gt<CR>
nnoremap <Leader>4  :normal 4gt<CR>
nnoremap <Leader>5  :normal 5gt<CR>
nnoremap <Leader>6  :normal 6gt<CR>
nnoremap <Leader>7  :normal 7gt<CR>
nnoremap <Leader>8  :normal 8gt<CR>
nnoremap <Leader>9  :normal 9gt<CR>
"
" TOGGLE
" ------
nnoremap <Leader>ts    :set spell!<CR>
" toggle cursorline from spacemacs
nnoremap <Leader>thh    :ToggleCursorLine<CR>:silent echomsg 'CURSOR LINE TOGGLED'<CR>
" bg toggle between dark and light
nnoremap <Leader>tb     :ToggleBG<CR>
" toggle Relative Numbers on the side
nnoremap <Leader>tr     :ToggRelLineNum<CR>:silent echomsg 'RELATIVE NUMBERS TOGGLED'<CR>
" toggle Numbers on the side
nnoremap <Leader>tn     :ToggLineNumber<CR>:echo 'LINE NUMBERS TOGGLED'<CR>
" disabled by def because it slows things down
nnoremap <Leader>t<Tab> :IndentLinesToggle<CR>
" center cursor - from spacemacs
nnoremap <Leader>t-     :ToggleScrollOff<CR>
" from spacemacs tells u when u exceeded 80 chars
nnoremap <Leader>tf     :ToggleLineLenghtGuide<CR>
"
" EDIT
"------
nnoremap <Leader>p :Scratchpad<CR>
nnoremap <buffer> <C-down> g,
nnoremap <buffer> <C-up> g;
nnoremap <Tab> :silent normal za<CR>
nnoremap <Leader>jj :jumps<CR>
nnoremap <Leader>jc :changes<CR>
"
" TEXT
" ----
nnoremap <C-x><C-o> :DeleteEmptyLines<CR>:silent echomsg 'EMPTY LINES DELETED'<CR>

nnoremap <Leader>rr :registers<CR>
nnoremap <Leader>hf :function<CR>
nnoremap <Leader>mm :marks<CR>
"
"SEARCH
"-------
nnoremap <Leader>me       :CompileRun<CR>
"
" RELOAD .vimrc
nnoremap <Leader>fer      :source ~/.vimrc<CR>:e<CR>:echo 'SOURCED VIMRC AND REFRESHED'<CR>
nnoremap <Leader>fed      :e ~/.vimrc<CR>
"
" BUFFER
"--------
nnoremap <Leader>bm :BufOnly!<CR>
nnoremap <Leader>bn :bnext<CR>
nnoremap <Leader>bp :bprevious<CR>
nnoremap <C-w><C-q> :bd!<CR>
"
" WINDOW
"--------
" window follow mode
nnoremap <Leader>wf :ToggFollow<CR>
nnoremap <C-w><C-b> :ball<CR>
" from spacemacs
nnoremap <Leader>b<S-n> :enew<CR>
" open a new windows in a vertical split
nnoremap <C-w><C-n> :vnew<CR>
nnoremap <C-l> :vsplit<CR>
" force close
nnoremap <C-w><C-c> :close!<CR>
" supplement to default <C-w>o =>> force
nnoremap <C-w><C-o> :only!<CR>
" Normalize widths
nnoremap <Leader>= :wincmd =<CR>
"
" MARKS
"-------
" delete all
nnoremap <Leader>m<S-d> :delmarks a-z<CR>:delmarks A-Z<CR>:delmarks 0-9<CR>
"
" from Shougo
" ------------
" Better x
nnoremap x "_x
" Smart <C-f>, <C-b>.
noremap <expr> <C-f> max([winheight(0) - 2, 1])
	    \ . "\<C-d>" . (line('w$') >= line('$') ? "L" : "M")
noremap <expr> <C-b> max([winheight(0) - 2, 1])
	    \ . "\<C-u>" . (line('w0') <= 1 ? "H" : "M")
"
" a>, i], etc... "{{{
" <angle>
onoremap aa  a>
xnoremap aa  a>
onoremap ia  i>
xnoremap ia  i>
" [rectangle]
onoremap ar  a]
xnoremap ar  a]
onoremap ir  i]
xnoremap ir  i]
"}}}
"
"" Enable undo <C-w> and <C-u>.
inoremap <C-w>  <C-g>u<C-w>
inoremap <C-u>  <C-g>u<C-u>
"
" Change selected word and repeatable
vnoremap <expr> cn "y/\\V\<C-r>=escape(@\", '/')\<CR>\<CR>" . "``cgn"
vnoremap <expr> cN "y/\\V\<C-r>=escape(@\", '/')\<CR>\<CR>" . "``cgN"

" Quit / exit
" ------------
try
    nmap <C-x> <Nop>
catch /.*/
endtry
nnoremap <C-x><C-c> :wqa!<CR>
"---------
"
" from Junegunn
" --------------
" <Leader>c Close quickfix/location window
" ----------------------------------------------------------------------------
nnoremap <leader>; :cclose<bar>lclose<cr>
nnoremap <leader>o :copen<CR>
" ----------------------------------------------------------------------------
" ?il | inner line
" ----------------------------------------------------------------------------
xnoremap <silent> il <Esc>^vg_
onoremap <silent> il :<C-U>normal! ^vg_<CR>
" ----------------------------------------------------------------------------
" Rescursive Maps
" ----------------------------------------------------------------------------
imap <C-p> <C-k>
imap <C-n> <C-j>
" CMD Line
"-------
ca w!!<CR> %!sudo tee > /dev/null %<CR>
"
ca W! w!
"
ca Wa! wa!
ca WA! wa!
"
ca Wqa wqa
ca wQa wqa
ca WQA wqa
ca wqA wqa
ca wqA wqa
"
ca qwa wqa
ca Qwa wqa
ca qWa wqa
ca qwA wqa
ca qWA wqa
ca QWA wqa
ca QwA wqa
"
aug GLOBALINIT
    au!
    " Turn off highlighting when opening a new file
    au BufNew,BufNewFile,BufReadPost,FileReadPost * noh
    " Change the working directory to the directory containing the current file
    au BufEnter * try | lchdir %:p:h | catch /.*/ | endtry
    au FileType * if exists("+omnifunc") && &omnifunc == "" | setlocal omnifunc=syntaxcomplete#Complete | endif
    au FileType * if exists("+completefunc") && &completefunc == "" | setlocal completefunc=syntaxcomplete#Complete | endif
    au VimResized,BufDelete,WinEnter,BufWinLeave,BufReadPost,FileReadPost * if winnr() == winnr('$') | setlocal foldcolumn=6 | else | setlocal foldcolumn=3 |endif
    au BufReadPost *
		\ if line("'\"") > 0 && line("'\"") <= line("$") |
		\   exe "normal! g`\"" |
		\ endif
    au filetype gitcommit setlocal scrolloff=40 cursorline nonumber norelativenumber cursorline | nnoremap <buffer> q :bd!<CR>
    au Filetype qf nmap <buffer> q :cclose<CR>
    au FileType css,scss,less  setlocal iskeyword+=.
    au FileType css,scss,less  setlocal iskeyword+=#
    au FileType css,scss,less  setlocal iskeyword+=-
    au BufNewFile,BufRead Tmuxfile,tmux/config  setlocal filetype=tmux
    au BufNewFile,BufRead *.coffeekup,*.ck,*._coffee,*.cson,*.coffee,*Cakefile setlocal filetype=coffee
    au BufRead,BufNewFile *.phpt,*.inc setlocal filetype=php
    au BufRead,BufNewFile *.sql setlocal filetype=mysql
    au BufNewFile,BufReadPost *.pug,*.jade setlocal filetype=pug
    au BufNewFile,BufRead *.rb,*.rbw,*.gemspec setlocal filetype=ruby
    au BufNewFile,BufRead *.el,.spacemacs setlocal syntax=lisp
    au BufRead,BufNewFile *.h,*.i setlocal filetype=c
    au BufRead,BufNewFile *.org setlocal filetype=org
    au BufRead,BufNewFile *.yml.example,*.yml.bak,*.yml.default setlocal filetype=yaml
    au BufNewFile,BufRead *.txt setlocal filetype=text
    au BufNewFile,BufRead *gitignore setlocal filetype=config
    au FileType git,gitcommit setlocal foldmethod=syntax foldlevel=1 spell
    au BufNewFile,BufRead *.snip,*.snippets,*.snippet           setlocal filetype=snippet
    au BufRead,BufNewFile,BufReadPost _*doc*_                   setlocal nospell
    au BufNewFile,BufRead *.rj					setf rj
    au BufNewFile,BufRead *.jsm					setf javascript
    au BufNewFile,BufRead *.json				setf json
    au BufNewFile,BufRead *.rfc					setf rfc
    au BufNewFile,BufRead *.aspx,*.ascx				setf html

    au BufNewFile,BufRead *.md					setf markdown
    au BufNewFile,BufRead $HOME/.vim/dict/*.txt,$VIM/vimfiles/dict/*.txt	setf dict
    au BufNewFile,BufRead fcitx_skin.conf,*/fcitx*.{conf,desc}*,*/fcitx/profile	setf dosini
    au BufNewFile,BufRead mimeapps.list				setf desktop
    au BufNewFile,BufRead	*/xorg.conf.d/*			setf xf86conf
    au BufNewFile,BufRead hg-editor-*.txt			setf hgcommit
    au BufNewFile,BufRead *openvpn*/*.conf,*.ovpn		setf openvpn
    au BufNewFile,BufRead	*.pxi				setf pyrex
    au BufNewFile,BufRead *.stp					setf stap
    au BufNewFile,BufRead supervisor*.conf                      setf dosini
    au BufNewFile,BufRead *.i					setf swig
    au BufRead		*tmux.conf				setf tmux
    au BufRead		*access[._]log*,*/nginx/*[._]log*	setf httplog
    au BufRead		*/.getmail/*rc				setf getmailrc
    au BufRead		.msmtprc				setf msmtp
    au BufRead		/var/log/*.log*				setf messages
    au BufRead		$HOME/temp/mb				setf mb
    au BufRead		PKGBUIL,rc.conf,grub.cfg,*.install,install,.INSTALL	setf sh
    au BufRead		accels					setf lisp
    au BufRead		$HOME/.cabal/config			setf cabal
    au FileType html  call HTMLMode()
    au FileType vim  call VimMode()
    au FileType css  call CSSMode()
    au FileType markdown  call MarkdownMode()
    au FileType javascript  call JavascriptMode()
    au FileType python  call PythonMode()
augroup END
