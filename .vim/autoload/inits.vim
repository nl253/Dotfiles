" This file contains config that needs to be re-run. For cfg that is run once
" on startup see ./opts.vim

" to be triggered by BufReadPost
fu! inits#all() abort
    if !(expand('%:p') =~# $HOME) | return 0 | endif
    call opts#comma_opt('cinwords', ['match', 'loop', 'foreach', 'until', 'case', 'select'])

    if !executable('rg') && isdirectory(systemlist("git rev-parse --show-toplevel")[0]) 
        call opts#safe_setl(["grepprg=git grep -n -r $*"])
    endif

    if empty(&omnifunc)
        call opts#safe_setl(['omnifunc=syntaxcomplete#Complete'])
    endif

    " words from all loaded buffers 
    setl completefunc=complete#AllBufsWords

    call opts#comma_opt('complete', ['.', 'k', 't'])

    " Looks in ~/.vim/dicts for a matching dict. 
    " If it finds one thent it sets locally dict to it.
    fu! GetDict(root) closure
        return expand('~').'/.vim/dicts/'.a:root.'.dict'
    endfu

    for l:candidate in map([&filetype, expand('%:e')], 'GetDict(v:val)') 
        if filereadable(l:candidate)
            call opts#safe_setl(['dictionary='.l:candidate])
            call opts#comma_opt('complete', ['k'])
            break
        endif
    endfor

    call opts#safe_setl([
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
                \ ])
    call opts#comma_opt('suffixesadd', [
                \ expand('%:e'), 
                \ 'erl', 
                \ 'hs', 
                \ 'ini', 
                \ 'java', 
                \ 'md', 
                \ 'js', 
                \ 'ts', 
                \ 'py', 
                \ 'rs', 
                \ 'rst', 
                \ 'vim', 
                \ 'yaml', 
                \ 'yml',
                \ ])

    if !empty(&makeprg) && !(&makeprg =~# 'make')
        nn <buffer> <LocalLeader>m :make!<CR>
    endif

    nn <buffer> <LocalLeader>e megg=G'ezz
    nn <buffer> <LocalLeader>f mfgggqG'fzz

    " go back to where you left off
    if line("'\"") > 0 && line("'\"") <= line("$") | exe "normal! g`\"" | endif
endf

fu! inits#programming() abort
    if !(expand('%:p') =~# $HOME) | return 0 | endif
    "call tags#project(l:anchors, 0)
    call opts#safe_setl(['nospell'])
    call opts#comma_opt('complete', ['.', 'w', 't'])
    let l:ext = expand('%:e')
    let l:root = utils#proj_root('.git')
    if !empty(l:ext)
        call opts#comma_opt('complete', [
                    \ 'k*.'.l:ext, 
                    \ 'k'.l:root.'/*.'.l:ext,
                    \ ])
    endif
        call opts#comma_opt('path', [l:root.'/**4/'])
    call utils#add_project_files('.git')
endf

fu! inits#markup() abort
    if !(expand('%:p') =~# $HOME) | return 0 | endif

    call opts#safe_setl([
                \ 'conceallevel=3',
                \ 'iskeyword+=\-',
                \ 'iskeyword-=_',
                \ 'spell',
                \ 'tagcase=ignore',
                \ 'ignorecase',
                \ 'expandtab',
                \ 'textwidth=79',
                \ 'linebreak',
                \ 'nowrap',
                \ ])
    call opts#letter_opt('formatoptions', ['t', 'o', 'r', 'c', 'n', 'q', 'j', 'l', '1'])
    call opts#comma_opt('complete', ['.', 'w', 'k*.'.expand('%:e')])
    call iabbrs#iab_init(&filetype)
endf

fu! inits#lang_server() abort
    if !(expand('%:p') =~# $HOME) | return 0 | endif
    LanguageClientStart
    nn <buffer> <silent> <LocalLeader>R :call LanguageClient#textDocument_references()<CR>
    nn <buffer> <silent> K              :call LanguageClient#textDocument_definition()<CR>
    nn <buffer> <silent> <LocalLeader>h :call LanguageClient#textDocument_hover()<CR>
    nn <buffer> <silent> <LocalLeader>i :call LanguageClient#textDocument_implementation()<CR>
    nn <buffer> <silent> <LocalLeader>r :call LanguageClient#textDocument_rename()<CR>
    nn <buffer> <silent> <LocalLeader>s :call LanguageClient#textDocument_documentSymbol()<CR>
    call opts#omni(['LanguageClient#complete'])
    " if &filetype != 'javascript' && &filetype != 'typescript'
        " setl omnifunc=LanguageClient#complete
    " endif
endf

fu! inits#non_home() abort
    call opts#safe_setl(['nomodifiable', 'readonly'])
endf

" Note: autocmd for this needs to be TermOpen (which requires feature check `has('nvim')`).
fu! inits#term() abort
    call opts#safe_setl(['nomodifiable', 'readonly', 'nospell'])
    nn <buffer> <Leader>' :close<CR>
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
    call opts#letter_opt_unset('guioptions', ['T', 'm', 'l', 'L', 'b', 'R', 'r', 'g'])
    call opts#letter_opt('guioptions', ['i', 'p', 'h', 'M', 'a'])
    call opts#letg_all('no_buffers_menu', 'did_install_default_menus', 'did_install_syntax_menu')
endf
