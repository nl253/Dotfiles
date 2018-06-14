if exists('g:loaded_saner') | finish | endif

if !exists('$MYVIMRC') && exists(expand("~/.vimrc"))
    let $MYVIMRC = expand('~/.vimrc')  " set automatically in nvim
endif

" Auto-Enable built-in 'Man' plugin
if !exists(":Man") && filereadable($VIMRUNTIME.'/ftplugin/man.vim') 
    execute 'source '.$VIMRUNTIME.'/ftplugin/man.vim'
endif

if has('nvim') && !(&runtimepath =~ expand('~/.vim'))
    setg runtimepath^=~/.vim runtimepath+=~/.vim/after
    let &packpath = &runtimepath
endif

" enable matchit (part of vim nowadays)
if v:version >= 800 && !has('nvim')
    packadd! matchit
else
    runtime macros/matchit.vim
endif

" NETRW - built-in
let g:netrw_scpport = '-P 22' 
let g:netrw_sshport = '-p 22'
let g:netrw_preview = 1 
let g:netrw_mousemaps = 0

if has('gui')
    for i in filter(['no_buffers_menu', 'did_install_default_menus', 'did_install_syntax_menu'], '!exists(v:val)')
        exec 'let '.i.' = 1'
    endfor
endif

" YAML - built-in
let g:yaml_schema = 'pyyaml'
let g:loaded_saner = 1
