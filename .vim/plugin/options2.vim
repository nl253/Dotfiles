if exists('g:loaded_vim_saner_options') || !executable('bash') || !has('unix')
    finish
endif

let s:vimdir = expand('~/.vim')

let s:sub_vimdirs = [
            \ 'undo',
            \ 'backup',
            \ 'swap',
            \ 'views',
            \ 'sessions',
            \ 'templates',
            \ 'snips'
            \ ]

" Make Missing Dirs:
for s:dir in filter(map(s:sub_vimdirs, 's:vimdir."/".v:val'), '!isdirectory(v:val)')
    silent call mkdir(s:dir, 'p')
endfor

" Options:
if has('wildignore')
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

    exe 'silent set wildignore+='.join(
                \ s:ignore_paths +
                \ s:ignore_dirs +
                \ s:ignore_exts +
                \ s:ignore_exact +
                \ s:ignore_phrases, ',')
endif

let s:globals = [
            \ 'autochdir',
            \ 'autoread',
            \ 'autowriteall',
            \ 'backup',
            \ 'backupdir='.s:vimdir.'/backup',
            \ 'breakat=\ .,:;!?',
            \ 'clipboard=unnamed,unnamedplus',
            \ 'cm=blowfish2',
            \ 'cmdwinheight=3',
            \ 'completeopt=menuone,longest',
            \ 'cpoptions=aABceFsW',
            \ 'diffopt+=vertical,iwhite',
            \ 'directory='.s:vimdir.'/swap',
            \ 'encoding=utf8',
            \ 'fileignorecase',
            \ 'foldclose=all',
            \ 'foldlevelstart=99',
            \ 'formatprg=fmt\ -s\ -u\ --width=79',
            \ 'gdefault',
            \ 'hidden',
            \ 'nohlsearch',
            \ 'inccommand=nosplit',
            \ 'incsearch',
            \ 'ignorecase',
            \ 'laststatus=2',
            \ 'magic',
            \ 'maxmempattern=200000',
            \ 'mouse=',
            \ 'nocompatible',
            \ 'noshowcmd',
            \ 'nostartofline',
            \ 'path='.join(['./.', './..', './*'] + filter(split($CDPATH, ':'), 'v:val != ""'), ','),
            \ 'pumheight=12',
            \ 'scrolloff=11',
            \ 'sessionoptions+=resize',
            \ 'sessionoptions-=blank',
            \ 'sessionoptions-=options',
            \ 'shiftround',
            \ 'shortmess=stTAIcoOWF',
            \ 'showbreak=\ ~>\ ',
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
            \ 'undodir='.s:vimdir.'/undo',
            \ 'undolevels=9999',
            \ 'updatetime=200',
            \ 'viewdir='.s:vimdir.'/views',
            \ 'viewoptions=folds,options,curdir,cursor',
            \ 'virtualedit=all',
            \ 'wildignorecase',
            \ 'wildmenu',
            \ 'wildoptions=tagfile',
            \ 'writebackup',
            \ ]

let s:locals = [
            \ 'autoindent',
            \ 'breakindent',
            \ 'bufhidden=hide',
            \ 'complete-=b',
            \ 'complete-=u',
            \ 'conceallevel=3',
            \ 'copyindent',
            \ 'expandtab',
            \ 'foldminlines=4',
            \ 'infercase',
            \ 'matchpairs=(:),<:>,{:},[:]',
            \ 'nowrap',
            \ 'nrformats+=alpha',
            \ 'nrformats=bin,hex',
            \ 'smartindent',
            \ 'spelllang=en_gb',
            \ 'undofile',
            \ 'spellfile=~/.vim/spell/en.utf-8.add,~/.config/nvim/spell/en.utf-8.add',
            \ ]

fu! s:set_locals()
    for l:opt in s:locals
        try
            execute 'silent setl '.l:opt
        catch /\vE(518)/
            silent call add(v:errors, '[vim-saner] could not set local option "'.l:opt.'"')
        endtry
    endfor
endf

for s:opt in s:globals
    try
        execute 'silent setg '.s:opt
    catch /\vE(518)/
        silent call add(v:errors, '[vim-saner] could not set global option "'.s:opt.'"')
    endtry
endfor

if executable('rg')
    exe 'setg grepprg='.escape('rg --hidden --maxdepth=5 --color=never --threads=4 --vimgrep $*', ' ')
else
    exe 'setg grepprg='.escape('git --no-pager grep --max-depth=5 --extended-regexp --threads=4 --color=never --line-number $*', ' ')
endif

if has('nvim') 
    set shada=!,'20,<50,s10,h,:50,f10
endif

if has('win32')
    setg shell=powershell
elseif has('unix')
    exe 'setg shell='.system('/usr/bin/env which '.(executable("dash") ? "dash" : "bash"))
endif

if has('gui')
    exe 'set guifont='.(has('win32') ? 'consolas:h11.4:w5.8:qPROOF' : 'Monospace\ 13')
    for i in filter(['T', 'm', 'l', 'L', 'b', 'R', 'r', 'g'], '&guioptions =~# v:val')
        exec 'setg guioptions-='.i
    endfor
    for i in filter(['i', 'p', 'h', 'M', 'a'], '!(&guioptions =~# v:val)')
        exec 'setg guioptions+='.i
    endfor
endif

aug SanerOptsAutoCmds
    au!
    au FileType * silent call s:set_locals()
    if !executable('rg')
        au BufReadPost ~/** if !isdirectory(system('git rev-parse --show-toplevel')) | exe 'setl grepprg='.escape('grep -n -r $*', ' ') | endif
    endif
aug END

let g:loaded_vim_saner_options = 1
" vim: foldmethod=indent
