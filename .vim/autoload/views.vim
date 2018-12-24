call opts#letg_default('view_dir',             expand('~/').'.vim/views')
call opts#letg_default('default_view_file',    join([g:view_dir, $USER.'.vim'], '/'))

fu! views#err(msg, ...)
    let l:msg = '[views] '.a:msg.' '.join(a:000, ' ')
    call add(v:errors, l:msg)
    echoerr l:msg
endf

fu! views#msg(msg, ...)
    echom '[views] '.a:msg.' '.join(a:000, ' ')
endf

fun! views#list(A, L, P)
    let l:a = map(systemlist('ls ~/.vim/views/*.vim'), 'fnamemodify(v:val, ":t:r")')
    if !empty(l:a) && len(l:a[0]) < 2
        let l:a = []
    endif
    return l:a
endfun

fun! views#read(file)
    if len(a:file) > 0
        let l:f = join([g:view_dir, substitute(a:file, '.vim', '', 'g').'.vim'], '/')
        call views#msg('loading view from', l:f)
        exe 'so '.l:f
    elseif filereadable(g:default_view_file)
        call views#msg('loading view from', g:default_view_file)
        exe 'so '.g:default_view_file
    else
        call views#err('no view file in ', g:default_view_file)
    endif
endfun

fun! views#save(file)
    if len(a:file) > 0 
        let l:f = join([g:view_dir, substitute(a:file, '.vim', '', 'g').'.vim'], '/')
        exe 'mkvie! '.l:f
        call views#msg('created a view file in', l:f)
    else
        exe 'mkvie! '.g:default_view_file
        call views#msg('created a view file in', g:default_view_file)
    endif
endfun

fun! views#delete(file, bang)
    if len(a:file) > 0
        let l:f = join([g:view_dir, a:file.'.vim'], '/')
        if delete(l:f, 'rf') < 0
            let l:f = join([g:view_dir, a:file], '/')
            echo delete(l:f, 'rf')
        endif
        call views#msg('removed', l:f)
    elseif a:bang
        for i in systemlist('ls '.g:view_dir.'/*')
            silent call delete(i)
        endfor
        call views#msg('removed all view files')
    else
        silent call delete(g:default_view_file, 'rf')
        call views#msg('removed', g:default_view_file)
    endif
endfun
