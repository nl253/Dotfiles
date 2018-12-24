call opts#letg_default('session_dir',          expand('~/').'.vim/sessions')
call opts#letg_default('default_session_file', join([g:session_dir, $USER.'.vim'], '/'))

fu! sessions#err(msg, ...)
    let l:msg = '[sessions] '.a:msg.' '.join(a:000, ' ')
    call add(v:errors, l:msg)
    echoerr l:msg
endf

fu! sessions#msg(msg, ...)
    echom '[sessions] '.a:msg.' '.join(a:000, ' ')
endf

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
        call sessions#msg('loading session from', l:f)
        exe 'so '.l:f
    elseif filereadable(g:default_session_file)
        call sessions#msg('loading session from', g:default_session_file)
        exe 'so '.g:default_session_file
    else
        call sessions#err('no session file in', g:default_session_file)
    en
endf

fu! sessions#save(file)
    if len(a:file) > 0 
        let l:f = join([g:session_dir, substitute(a:file, '.vim', '', 'g').'.vim'], '/')
        exe 'mks! '.l:f
        call sessions#msg('created a session file in ', l:f)
    else
        exe 'mksession! '.g:default_session_file
        call sessions#err('created a session file in', g:default_session_file)
    en
endf

fun! sessions#delete(file, bang)
    if len(a:file) > 0
        let l:f = join([g:session_dir, a:file.'.vim'], '/')
        if delete(l:f, 'rf') < 0
            let l:f = join([g:session_dir, a:file], '/')
            echo delete(l:f, 'rf')
        endif
        call sessions#msg('removed', l:f)
    elseif a:bang
        for l:i in systemlist('ls '.g:session_dir.'/*')
            sil call delete(l:i)
        endfor
        call sessions#msg('removed all session files')
    else
        sil call delete(g:default_session_file, 'rf')
        call sessions#msg('removed', g:default_session_file)
    endif
endf
