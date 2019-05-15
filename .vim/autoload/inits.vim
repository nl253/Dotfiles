" This file contains config that needs to be re-run. For cfg that is run once
" on startup see ./opts.vim

" to be triggered for all buffers by BufReadPost
fu! inits#all() abort
    if !(expand('%:p') =~# $HOME) | return 0 | endif

    " use git grep when in git repo and rg not availible
    if !executable('rg') && isdirectory(systemlist("git rev-parse --show-toplevel")[0]) 
        setl grepprg=git\ grep\ -n\ -r\ $*
    endif

    if empty(&omnifunc)
        setl omnifunc=syntaxcomplete#Complete
    endif

    " words from all loaded buffers 
    if empty(&completefunc)
        setl completefunc=complete#AllBufsWords
    endif


    " Looks in ~/.vim/dicts for a matching dict. 
    " If it finds one thent it sets locally dict to it.
    for l:name in [&filetype, expand('%:e')]
        let l:maybe_dict = expand('~').'/.vim/dicts/'.l:name.'.dict'
        if filereadable(l:maybe_dict)
            exe 'setl dictionary='.l:maybe_dict
            setl complete+=k
            break
        endif
    endfor

    setl autoindent breakindent bufhidden=hide complete-=b complete+=.,k,t complete-=u conceallevel=3 copyindent expandtab foldminlines=4 infercase matchpairs=(:),<:>,{:},[:] nowrap nrformats+=alpha nrformats=bin,hex smartindent spellfile=~/.vim/spell/en.utf-8.add,~/.config/nvim/spell/en.utf-8.add spelllang=en_gb undofile suffixesadd+=erl,hs,ini,java,js,md,py,rs,rst,ts,vim,yaml,yml cinwords+=match,loop,foreach,until,case,select 
    exe 'setl suffixesadd+='.expand('%:e')

    " go back to where you left off
    if line("'\"") > 0 && line("'\"") <= line("$") | exe "normal! g`\"" | endif
endf

fu! inits#programming() abort
    if !(expand('%:p') =~# $HOME) | return 0 | endif
    setl nospell complete+=.,w,t
    let l:ext = expand('%:e')
    let l:root = utils#proj_root('.git')
    if !empty(l:ext)
        exe 'setl complete+=k*.'.l:ext.',k'.l:root.'/*.'.l:ext
    endif
    exe 'setl path+='.l:root.'/**4/'
endf

fu! inits#markup() abort
    if !(expand('%:p') =~# $HOME) | return 0 | endif
    setl conceallevel=3 expandtab ignorecase iskeyword+=- iskeyword-=_ linebreak nowrap spell tagcase=ignore textwidth=79 formatoptions+=torcnqjl1
    exe 'setl complete+=.,w,k*.'.expand('%:e')
    call iabbrs#iab_init(&filetype)
endf

fu! inits#lang_server() abort
    if !(expand('%:p') =~# $HOME) | return 0 | endif
    LanguageClientStart
    nn <buffer> <silent> <LocalLeader>h :call LanguageClient#textDocument_hover()<CR>
    nn <buffer> <silent> K              :call LanguageClient#textDocument_definition()<CR>
    nn <buffer> <silent> <LocalLeader>t :call LanguageClient#textDocument_typeDefinition()<CR>
    nn <buffer> <silent> <LocalLeader>i :call LanguageClient#textDocument_implementation()<CR>
    nn <buffer> <silent> <LocalLeader>r :call LanguageClient#textDocument_rename()<CR>
    nn <buffer> <silent> <LocalLeader><LocalLeader> :call LanguageClient#textDocument_codeAction()<CR>
    nn <buffer> <silent> <LocalLeader>f :call LanguageClient#textDocument_formatting()<CR>
    nn <buffer> <silent> <LocalLeader>* :call LanguageClient#textDocument_documentHighlight()<CR>
    nn <buffer> <silent> <LocalLeader>c :call LanguageClient#clearDocumentHighlight()<CR>
    setl omnifunc=LanguageClient#complete
endf

fu! inits#non_home() abort
    setl nomodifiable readonly
endf

" NOTE: autocmd for this needs to be TermOpen (which requires feature check `has('nvim')`).
fu! inits#term() abort
    setl nomodifiable readonly nospell
endf

fu! inits#emmet() abort
    if !(expand('%:p') =~# $HOME) | return 0 | endif
    if exists(':EmmetInstall')
        EmmetInstall
        im <buffer> <Tab> <plug>(emmet-expand-abbr)
    endif
endf

fu! inits#gui() abort
    exe 'set guifont='.(has('win32') ? 'consolas:h11.4:w5.8:qPROOF' : 'Monospace\ 13')
    setg guioptions-=TmlLbRrg guioptions+=iphMa
    for i in filter(['no_buffers_menu', 'did_install_default_menus', 'did_install_syntax_menu'], '!exists("g:".v:val)')
        exec 'let g:'.i.' = 1'
    endfor
endf
