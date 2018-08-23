setg rtp^=~/.vim/after errorformat+=%f tabline=%!opts#my_tabline()

call repl#set_repl({
            \ 'haskell'   : 'ghci', 
            \ 'python'    : 'ipython', 
            \ 'erlang'    : 'erl', 
            \ 'java'      : 'jshell', 
            \ 'javascript': 'node', 
            \ 'html'      : 'node',
            \ })

let g:template_vars = {
            \ 'author':      join(systemlist("git config user.name"), ' '),
            \ 'year':        strftime("%Y"),
            \ 'now':         strftime("%c"),
            \ 'description': '',
            \ 'keywords':    '',
            \ }

call opts#set_if_executable('grepprg', {
            \ 'rg':   'rg --hidden --maxdepth=5 --color=never --threads=4 --vimgrep $*', 
            \ },  1) 
" \ 'git': 'git --no-pager grep --max-depth=5 --extended-regexp --threads=4 --color=never --line-number $*'

call opts#dict()
call opts#thesaurus()
call opts#safe_setg([
            \ 'autochdir',
            \ 'autoread',
            \ 'autowriteall',
            \ 'backup',
            \ 'backupdir=~/.vim/backup',
            \ 'breakat= .,:;!?',
            \ 'clipboard=unnamed,unnamedplus',
            \ 'cm=blowfish2',
            \ 'cmdwinheight=3',
            \ 'completeopt=menuone,longest',
            \ 'cpoptions=aABceFsW',
            \ 'diffopt+=vertical,iwhite',
            \ 'directory=~/.vim/swap',
            \ 'encoding=utf8',
            \ 'fileignorecase',
            \ 'foldclose=all',
            \ 'foldlevelstart=99',
            \ 'formatprg=fmt -s -u --width=79',
            \ 'gdefault',
            \ 'winwidth=20',
            \ 'winminwidth=20',
            \ 'errorfile=.errors.log',
            \ 'makeef=.make-output.log',
            \ 'hidden',
            \ 'nohlsearch',
            \ 'inccommand=nosplit',
            \ 'incsearch',
            \ 'noignorecase',
            \ 'laststatus=2',
            \ 'magic',
            \ 'maxmempattern=200000',
            \ 'mouse=',
            \ 'nocompatible',
            \ 'noshowcmd',
            \ 'nostartofline',
            \ 'path='.join(['./', '../', './*'], ','),
            \ 'pumheight=12',
            \ 'scrolloff=11',
            \ 'sessionoptions+=resize',
            \ 'sessionoptions-=blank',
            \ 'sessionoptions-=options',
            \ 'shiftround',
            \ 'shortmess=stTAIcoOWF',
            \ 'showbreak= ~> ',
            \ 'sidescroll=1',
            \ 'sidescrolloff=30',
            \ 'spellsuggest=best,12,',
            \ 'splitbelow',
            \ 'splitright',
            \ 'switchbuf=usetab,newtab,',
            \ 'tagcase=ignore',
            \ 'tagcase=match',
            \ 'taglength=20',
            \ 'tagrelative',
            \ 'tagstack',
            \ 'ttyfast',
            \ 'undodir=~/.vim/undo',
            \ 'undolevels=9999',
            \ 'updatetime=200',
            \ 'viewdir=~/.vim/views',
            \ 'viewoptions=folds,options,curdir,cursor',
            \ 'virtualedit=all',
            \ 'wildignorecase',
            \ 'wildmenu',
            \ 'wildoptions=tagfile',
            \ 'writebackup',
            \ ])

call opts#wildignore([
            \ '*.bac',
            \ '*.bac', 
            \ '*.beam',
            \ '*.bk',
            \ '*.bk', 
            \ '*.class',
            \ '*.ctxt', 
            \ '*.db', 
            \ '*.hi',
            \ '*.iml',
            \ '*.iml', 
            \ '*.lock',
            \ '*.lock', 
            \ '*.o',
            \ '*.obj',
            \ '*.so',
            \ '*.sqlite*',
            \ '*.ipynb',
            \ '*.sqlite*', 
            \ '*.swp',
            \ '*.swp', 
            \ '*_', 
            \ '*~',
            \ '*~', 
            \ '.egg', 
            \ '.eggs',
            \ '.git',
            \ '.git', 
            \ '.idea', 
            \ '.rope*',
            \ '.svn',
            \ '.svn', 
            \ '_*', 
            \ 'build',
            \ 'build', 
            \ 'dist', 
            \ 'node_modules',
            \ 'out', 
            \ 'tags', 
            \ 'target', 
            \ ])

let s:ignore_paths = map([
            \ 'sessions',
            \ 'tmp',
            \ 'swap'
            \ ], '"**/".v:val."/**"')

let s:ignore_dirs = map(filter(split(system('bash -c "echo /{usr/,usr/local/,usr/local/share/,}{lost+found,run,srv,opt,var,sys,boot,dev,lib,lib32,lib64,man,bin,root,sbin,proc,mnt,include}"'), ' '), 'isdirectory(v:val)'), 'v:val."/*"')

let s:ignore_exts = map([
            \ 'tab',
            \ 'webm',
            \ 'log',
            \ 'swp',
            \ 'pdf',
            \ 'jpg',
            \ 'png',
            \ 'lock',
            \ 'docx',
            \ 'tmp',
            \ 'bak',
            \ 'info',
            \ 'pptx',
            \ 'mp3',
            \ 'mp4',
            \ 'toc',
            \ 'out',
            \ 'fls',
            \ 'aux',
            \ 'fdb_latexmk',
            \ 'tab',
            \ 'webm'
            \ ], '"*.".v:val')

let s:ignore_exact = [
            \ 'tags',
            \ '*~',
            \ 'swapfile'
            \ ]

let s:ignore_phrases = map([
            \ 'history',
            \ 'cache',
            \ 'chrome',
            \ 'firefox',
            \ '%'
            \ ], "'**'.v:val.'**'")

call opts#wildignore(s:ignore_paths + s:ignore_dirs + s:ignore_exts + s:ignore_exact + s:ignore_phrases)

exe 'setg shell='.system('/usr/bin/env which '.(executable("dash") ? "dash" : "bash"))

if has('gui') | call opts#gui() | endif

if !exists('$MYVIMRC') && exists(expand("~/.vimrc"))
    let $MYVIMRC = expand('~/.vimrc')  " set automatically in nvim
endif

" Auto-Enable built-in 'Man' plugin
if !exists(":Man") && filereadable($VIMRUNTIME.'/ftplugin/man.vim') 
    execute 'source '.$VIMRUNTIME.'/ftplugin/man.vim'
endif

if has('nvim') 
    setg shada=!,'20,<50,s10,h,:50,f10
    call opts#comma_opt('runtimepath', ['~/.vim', '~/.vim/after'])
    let &packpath = &runtimepath
endif

" enable matchit (part of vim nowadays)
if v:version >= 800 && !has('nvim')
    packadd! matchit
else
    runtime macros/matchit.vim
endif
