" This file contains options that need to be set globally (ie only once on Vim start-up).

if has('gui_running') | call opts#gui() | endif
if !exists(":Man") | runtime! ftplugin/man.vim | endif
packadd justify

call utils#make_missing_dirs(map([
            \ 'undo',
            \ 'backup',
            \ 'swap',
            \ 'views',
            \ 'sessions',
            \ 'templates',
            \ 'snips'
            \ ], '"~/.vim/".v:val'))

call opts#append_to_path([
            \ '~/.gem/ruby/*/bin',
            \ '~/.fzf/bin',
            \ '~/go/bin',
            \ '~/.cargo/bin',
            \ '~/.local/bin',
            \ '~/.stack/bin',
            \ '~/.cabal/bin',
            \ '~/.config/yarn/global/node_modules/.bin',
            \ '~/.local/share/fzf/bin',
            \ '~/.yarn/bin'
            \ ])

call repl#set_repl({
            \ 'haskell'   : 'ghci', 
            \ 'python'    : 'ptipython', 
            \ 'erlang'    : 'erl', 
            \ 'java'      : 'jshell', 
            \ 'javascript': 'node -i', 
            \ 'lisp'      : 'clisp -i', 
            \ 'css'       : 'node -i', 
            \ 'html'      : 'node -i',
            \ })

" Lazy evaluation (shell is called for `author` and `email` which may slow down start-up)
fu! GetTemplateVars()
    if exists('g:template_vars')
        return g:template_vars
    else    
        let g:template_vars = {
                    \ 'author':      join(systemlist("git config user.name"), ' '),
                    \ 'date':        strftime("%c"),
                    \ 'description': '',
                    \ 'email':       join(systemlist("git config user.email"), ' '),
                    \ 'github':      'https://github.com/'.join(systemlist("git config user.name"), ' '),
                    \ 'keywords':    join([], ', '),
                    \ 'now':         strftime("%c"),
                    \ 'summary':     '',
                    \ 'year':        strftime("%Y"),
                    \ }
        return GetTemplateVars()
    endif
endf

call opts#set_if_executable('grepprg', {
            \ 'rg':   'rg --hidden --maxdepth=5 --color=never --threads=4 --vimgrep $*', 
            \ 'grep': 'grepprg=grep -n -r $*',
            \ },  1) 

call opts#set_if_executable('shell', {
            \ 'dash': 'dash',
            \ 'bash': 'bash',
            \ 'zsh':  'zsh',
            \ }, 1)

let s:dict_dir = expand('~/.vim/dicts/')

let s:thesaurus = s:dict_dir.'thesaurus.dict'

if empty(&thesaurus) && filereadable(s:thesaurus)
    call opts#safe_setg(['thesaurus='.s:thesaurus])
endif

let s:dict = s:dict_dir.'frequent.dict'

if filereadable(s:dict)
    call opts#safe_setg(['dictionary='.s:dict])
    call opts#comma_opt('complete', ['k'])
endif

call opts#safe_setg([
            \ "shada=!,'20,<50,s10,h,:50,f10",
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
            \ 'errorfile=.errors.log',
            \ 'errorformat+=%f',
            \ 'fileignorecase',
            \ 'foldclose=all',
            \ 'foldlevelstart=99',
            \ 'formatprg=fmt -s -u --width=79',
            \ 'gdefault',
            \ 'hidden',
            \ 'inccommand=nosplit',
            \ 'incsearch',
            \ 'laststatus=2',
            \ 'magic',
            \ 'makeef=.make-output.log',
            \ 'maxmempattern=200000',
            \ 'mouse=',
            \ 'nocompatible',
            \ 'nohlsearch',
            \ 'noignorecase',
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
            \ 'showbreak= >> ',
            \ 'sidescroll=1',
            \ 'sidescrolloff=30',
            \ 'spellsuggest=best,12,',
            \ 'splitbelow',
            \ 'splitright',
            \ 'switchbuf=usetab,newtab,',
            \ 'tabline=%!utils#my_tabline()',
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
            \ 'viminfo=NONE',
            \ 'virtualedit=all',
            \ 'wildignorecase',
            \ 'wildmenu',
            \ 'wildoptions=tagfile',
            \ 'winminwidth=20',
            \ 'winwidth=20',
            \ 'writebackup',
            \ ])

" dirs
let s:wildignore_patterns = map([
            \ 'sessions',
            \ 'tmp',
            \ 'swap',
            \ '.git', 
            \ '.idea', 
            \ '.svn',
            \ 'node_modules',
            \ 'build', 
            \ 'dist', 
            \ 'out', 
            \ 'target', 
            \ ], 'v:val."/**"')

call extend(s:wildignore_patterns, map(filter(split(system('bash -c "echo /{usr/,usr/local/,usr/local/share/,}{lost+found,run,srv,opt,var,sys,boot,dev,lib,lib32,lib64,man,bin,root,sbin,proc,mnt,include}"'), ' '), 'isdirectory(v:val)'), 'v:val."/*"'))

" exts
call extend(s:wildignore_patterns, map([
            \ 'aux',
            \ 'ba[kc]',
            \ 'beam',
            \ 'bk',
            \ 'class',
            \ 'ctxt',
            \ 'db',
            \ 'docx',
            \ 'egg', 
            \ 'eggs',
            \ 'fdb_latexmk',
            \ 'fls',
            \ 'hi',
            \ 'iml',
            \ 'info',
            \ 'ipynb',
            \ 'jpg',
            \ 'lock',
            \ 'log',
            \ 'mp[34]',
            \ 'o',
            \ 'obj',
            \ 'out',
            \ 'pdf',
            \ 'png',
            \ 'pptx',
            \ 'rope*',
            \ 'so',
            \ 'sqlite*',
            \ 'swp',
            \ 'tab',
            \ 'tmp',
            \ 'toc',
            \ 'webm',
            \ ], '"*.".v:val'))

" files
call extend(s:wildignore_patterns, [
            \ 'tags',
            \ '*_', 
            \ '*~',
            \ 'swapfile',
            \ ])

" contains 
call extend(s:wildignore_patterns, map([
            \ 'history',
            \ 'cache',
            \ 'chrome',
            \ 'firefox',
            \ '%',
            \ ], "'**'.v:val.'**'"))

call opts#wildignore(s:wildignore_patterns)
