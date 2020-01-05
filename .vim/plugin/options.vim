" Options that need to be set globally (i.e. only once on Vim start-up).

if !has('win32') && !exists(":Man") | ru! ftplugin/man.vim | endif
if has('gui_running') | call opts#gui()      | endif
pa justify

for s:dir in filter(map([
            \ '.undo',
            \ '.bak',
            \ '.swp',
            \ 'views',
            \ 'sessions',
            \ 'templates',
            \ 'snips',
            \ ], 'expand("~/.vim/".v:val)'), '!isdirectory(v:val)')
    call mkdir(s:dir, 'p')
endfor


aug ReplFtypes
    au!
    for entry in items({
            \ 'haskell'   : 'ghci', 
            \ 'python'    : 'ptipython', 
            \ 'erlang'    : 'erl', 
            \ 'java'      : 'jshell', 
            \ 'javascript': 'node -i', 
            \ 'lisp'      : 'clisp -i', 
            \ 'css'       : 'node -i', 
            \ 'html'      : 'node -i',
            \ })
        exe "au Filetype ".entry[0]." nn <buffer> <Leader>' :silent call repl#open_repl(".string(entry[1]).")<CR>"
    endfor
aug END

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

for s:pair in [
			\ [ 'rg',   'rg --hidden --maxdepth=5 --color=never --threads=4 --vimgrep $*'], 
			\ [ 'grep', 'grepprg=grep -n -r $*'],
			\ ]
    if executable(s:pair[0])
        exe 'setg grepprg='.escape(s:pair[1], ' ')
		break
    endif
endfor

for s:pair in [['dash', 'dash'], ['bash', 'bash'], ['zsh', 'zsh']]
    if executable(s:pair[0])
        exe 'setg shell='.s:pair[1]
        break
    endif
endfor

setg thesaurus=~/.vim/dicts/thesaurus.dict dictionary=~/.vim/dicts/frequent.dict path+=. autochdir autoread autowriteall backup backupdir=~/.vim/.bak breakat=\ .,:;!? clipboard=unnamed,unnamedplus cmdwinheight=3 completeopt=menuone,longest cpoptions=aABceFsW diffopt+=vertical,iwhite directory=~/.vim/swap encoding=utf8 errorfile=.errors.log errorformat+=%f fileignorecase foldclose=all foldlevelstart=99 formatprg=fmt\ -s\ -u\ --width=79 gdefault hidden inccommand=nosplit incsearch laststatus=2 magic makeef=.make-output.log maxmempattern=200000 mouse= nocompatible hlsearch noignorecase noshowcmd nostartofline pumheight=12 scrolloff=11 sessionoptions+=resize sessionoptions-=blank sessionoptions-=options shiftround shortmess=stTAIcoOWF showbreak=\ >> sidescroll=1 sidescrolloff=30 spellsuggest=best,12, splitbelow splitright switchbuf=usetab,newtab, tabline=%!utils#my_tabline() tagcase=ignore tagcase=match taglength=20 tagrelative tagstack ttyfast undodir=~/.vim/.undo undolevels=9999 updatetime=200 viewdir=~/.vim/views viewoptions=folds,options,curdir,cursor virtualedit=all wildignorecase wildmenu wildoptions=tagfile winminwidth=20 winwidth=20 writebackup
" setg shada=!,20,<50,s10,h,:50,f10

" dirs
let s:wildignore_patterns = map([
            \ 'sessions',
            \ 'tmp',
            \ 'swap',
            \ 'doc',
            \ 'docs',
            \ '.git', 
            \ '.idea', 
            \ '.svn',
            \ 'node_modules',
            \ 'build', 
            \ 'dist', 
            \ 'out', 
            \ 'target', 
            \ ], '"**/".v:val."/**"')

call extend(s:wildignore_patterns, map(filter(split(system('bash -c "echo /{usr/,usr/local/,usr/local/share/,}{lost+found,run,srv,opt,var,sys,boot,dev,lib,lib32,lib64,man,bin,root,sbin,proc,mnt,include}"'), ' '), 'isdirectory(v:val)'), 'v:val."/*"'))

" exts
call extend(s:wildignore_patterns, map([
            \ '*.map',
            \ 'aux',
            \ 'ba[kc]',
            \ 'beam',
            \ 'bk',
            \ 'class',
            \ 'ctxt',
            \ 'db',
            \ 'doc',
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
            \ 'min.*',
            \ 'mp[34]',
            \ 'o',
            \ 'obj',
            \ 'out',
            \ 'pdf',
            \ 'png',
            \ 'ppt',
            \ 'pptx',
            \ 'rope*',
            \ 'so',
            \ 'sqlite*',
            \ 'swp',
            \ 'synctex.gz',
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
            \ '*-lock.*',
            \ ])

" contains 
call extend(s:wildignore_patterns, map([
            \ 'history',
            \ 'cache',
            \ 'chrome',
            \ 'firefox',
            \ '%',
            \ ], "'**'.v:val.'**'"))

exec 'setg wildignore+='.join(s:wildignore_patterns, ',')
