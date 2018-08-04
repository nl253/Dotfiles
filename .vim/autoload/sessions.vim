fu! sessions#list(A, L, P)
    let l:a = map(systemlist('ls ~/.vim/sessions/*.vim'), 'fnamemodify(v:val, ":t:r")')
    if !empty(l:a) && len(l:a[0]) < 2
        let l:a = []
    endif
    return l:a
endf

fu! sessions#read(file)
    if len(a:file) > 0
        let l:f = join([g:session_dir, substitute(a:file, '.vim', '', 'g').'.vim'], '/')
        echom '[vim-saner] loading session from '.l:f
        exe 'source '.l:f
    elseif filereadable(g:default_session_file)
        echom '[vim-saner] loading session from '.g:default_session_file
        exe 'source '.g:default_session_file
    else
        echoerr '[vim-saner] no session file in '.g:default_session_file
    en
endf

fu! sessions#save(file)
    if len(a:file) > 0 
        let l:f = join([g:session_dir, substitute(a:file, '.vim', '', 'g').'.vim'], '/')
        exe 'mksession! '.l:f
        echom '[vim-saner] created a session file in '.l:f
    else
        exe 'mksession! '.g:default_session_file
        echom '[vim-saner] created a session file in '.g:default_session_file
    en
endf

fun! sessions#delete(file, bang)
    if len(a:file) > 0
        let l:f = join([g:session_dir, a:file.'.vim'], '/')
        if delete(l:f, 'rf') < 0
            let l:f = join([g:session_dir, a:file], '/')
            echo delete(l:f, 'rf')
        endif
        echom '[vim-saner] removed '.l:f
    elseif a:bang
        for i in systemlist('ls '.g:session_dir.'/*')
            silent call delete(i)
        endfor
        echom '[vim-saner] removed all session files'
    else
        silent call delete(g:default_session_file, 'rf')
        echom '[vim-saner] removed '.g:default_session_file
    endif
endf
