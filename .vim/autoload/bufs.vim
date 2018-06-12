fu! bufs#buffer_wipeout()
    for l:b in range(1, bufnr('$'), 1)
        let l:info = getbufinfo(l:b)
        if !empty(l:info)
            silent call bufs#_buffer_wipeout_helper(l:info['name']['name'], l:b)
        endif
    endfor
endf

fu! bufs#_buffer_wipeout_helper(path, bufno)
    if getbufvar(a:bufno, '&modified')
        " all good -- valid buffer
        return 1
    endif

    if !filereadable(a:path) || (a:path ==# '') || !(a:path =~# $HOME) || (a:path =~# 'NetrwTreeListing|^man:') || (a:path =~ $VIMRUNTIME && (getbufvar(a:bufno, '&filetype') =~# 'vim|help'))
        " either empty scratch but not modified
        " or     nvim terminal
        " or     man page
        " or     unmodified file not at home
        " or     vim help file
        exe 'bw '.a:bufno
    endif
endf
