" This file contains config that needs to be re-run. For cfg that is run once
" on startup see ./opts.vim

" to be triggered by BufReadPost
fu! inits#all()
    call opts#grepprg()
    call opts#completion()
    call opts#dict_local()
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

    if empty(&makeprg) && !(makeprg =~# 'make')
        nn <buffer> <Space>me :make<CR>
    endif

    " go back to where you left off
    if line("'\"") > 0 && line("'\"") <= line("$") | exe "normal! g`\"" | endif
endf

fu! inits#programming()
    let l:anchors = ['.git']
    "call tags#project(l:anchors, 0)
    call opts#safe_setl(['nospell'])
    call opts#comma_opt('complete', ['.', 'w', 't'])
    let l:ext = expand('%:e')
    let l:root = utils#proj_root(l:anchors)
    if !empty(l:ext)
        call opts#comma_opt('complete', [
                    \ 'k*.'.l:ext, 
                    \ 'k'.l:root.'/*.'.l:ext,
                    \ ])
    endif
    call opts#comma_opt('path', [l:root.'/**4/'])
    call utils#add_project_files(l:anchors)
endf

fu! inits#markup()
    call opts#safe_setl([
                \ 'conceallevel=3',
                \ 'spell',
                \ 'tagcase=ignore',
                \ 'expandtab',
                \ 'textwidth=79',
                \ 'linebreak',
                \ 'nowrap',
                \ ])
    call opts#letter_opt('formatoptions', ['t', 'o', 'r', 'c', 'n', 'q', 'j', 'l', '1'])
    call opts#comma_opt('complete', ['.', 'w', 'k*.'.expand('%:e')])
    call iabbrs#iab_init(&filetype)
endf

fu! inits#lang_server()
    nn <buffer> <silent> <LocalLeader>R :call LanguageClient#textDocument_references()<CR>
    nn <buffer> <silent> K              :call LanguageClient#textDocument_definition()<CR>
    nn <buffer> <silent> <LocalLeader>h :call LanguageClient#textDocument_hover()<CR>
    nn <buffer> <silent> <LocalLeader>i :call LanguageClient#textDocument_implementation()<CR>
    nn <buffer> <silent> <LocalLeader>r :call LanguageClient#textDocument_rename()<CR>
    nn <buffer> <silent> <LocalLeader>s :call LanguageClient#textDocument_documentSymbol()<CR>
    setl omnifunc=LanguageClient#complete formatexpr=LanguageClient_textDocument_rangeFormatting()
endf

fu! inits#non_home()
    setl nomodifiable readonly
endf

fu! inits#term()
    setl modifiable nospell noreadonly 
    nn <buffer> <Leader>' :close<CR>
endf

fu! inits#emmet()
    if exists(':EmmetInstall')
        EmmetInstall
        imap <buffer> <Tab> <plug>(emmet-expand-abbr)
    endif
endf

fu! inits#gui()
    exe 'set guifont='.(has('win32') ? 'consolas:h11.4:w5.8:qPROOF' : 'Monospace\ 13')
    call opts#letter_opt_unset('guioptions', ['T', 'm', 'l', 'L', 'b', 'R', 'r', 'g'])
    call opts#letter_opt('guioptions', ['i', 'p', 'h', 'M', 'a'])
    call opts#letg_all(['no_buffers_menu', 'did_install_default_menus', 'did_install_syntax_menu'])
endf
