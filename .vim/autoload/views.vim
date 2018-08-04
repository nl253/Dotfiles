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
        echom '[vim-saner] loading view from '.l:f
        exe 'so '.l:f
    elseif filereadable(g:default_view_file)
        echom '[vim-saner] loading view from '.g:default_view_file
        exe 'so '.g:default_view_file
    else
        echoerr '[vim-saner] no view file in '.g:default_view_file
    endif
endfun

fun! views#save(file)
    if len(a:file) > 0 
        let l:f = join([g:view_dir, substitute(a:file, '.vim', '', 'g').'.vim'], '/')
        exe 'mkview! '.l:f
        echom '[vim-saner] created a view file in '.l:f
    else
        exe 'mkview! '.g:default_view_file
        echom '[vim-saner] created a view file in '.g:default_view_file
    endif
endfun

fun! views#delete(file, bang)
    if len(a:file) > 0
        let l:f = join([g:view_dir, a:file.'.vim'], '/')
        if delete(l:f, 'rf') < 0
            let l:f = join([g:view_dir, a:file], '/')
            echo delete(l:f, 'rf')
        endif
        echom '[vim-saner] removed '.l:f
    elseif a:bang
        for i in systemlist('ls '.g:view_dir.'/*')
            silent call delete(i)
        endfor
        echom '[vim-saner] removed all view files'
    else
        silent call delete(g:default_view_file, 'rf')
        echom '[vim-saner] removed '.g:default_view_file
    endif
endfun
