" NOTE: this is a command completion function not omnifunct
fu! complete#buffer_words(ArgLead, CmdLine, CursorPos)
    return systemlist("command grep -oE ".shellescape(!empty(a:ArgLead) ? a:ArgLead."[-a-zA-Z0-9_]*" : '[-a-zA-Z0-9_]{3,}')." < ".shellescape(expand('%')).' | command sort -d | command uniq -u | command tr "[:upper:]" "[:lower:]"')
endf

" NOTE: this is a command completion function not omnifunct
fu! complete#buffers_words(ArgLead, CmdLine, CursorPos)
    let l:fst_idx = 1
    let l:lst_idx = buffer_number("$")
    let l:words = []
    for l:buf_no in range(l:fst_idx, l:lst_idx)
        if bufexists(l:buf_no) && bufloaded(l:buf_no) && buflisted(l:buf_no) && line('$') > 2
            call extend(l:words, systemlist("command grep -oE ".shellescape(!empty(a:ArgLead) ? a:ArgLead."[-a-zA-Z0-9_]*" : '[-a-zA-Z0-9_]{3,}')." < ".shellescape(buffer_name(l:buf_no)).' | command sort -d | command uniq -u | command tr "[:upper:]" "[:lower:]"'))
        endif
    endfor
    return l:words
endf

" Usage: `setl completefunc=complete#BufWords`
" NOTE: it requirest grep(1)
fu! complete#TabWords(findstart, base)
    if a:findstart
        " locate the start of the word
        let l:line = getline('.')
        let l:start = col('.') - 1
        while l:start > 0 && l:line[l:start - 1] =~# '\v[-a-zA-Z0-9_]'
            let l:start -= 1
        endwhile
        return l:start
    " min len = 1
    elseif empty(a:base[:1])
        return -1
    else
        let l:buf_nums = []
        for l:tab_no in range(1, tabpagenr("$"))
            call extend(l:buf_nums, tabpagebuflist(l:tab_no))
        endfor
        return systemlist('command grep --no-filename -E -o "\b'.a:base.'[-a-zA-Z0-9_]+\b" '.join(filter(map(l:buf_nums, 'fnameescape(fnamemodify(bufname(v:val), ":p"))'), 'filereadable(v:val)'), ' '))
    endif
endfun

" Usage: `setl completefunc=complete#BufWords`
" NOTE: it requirest grep(1)
fu! complete#AllBufsWords(findstart, base)
    if a:findstart
        " locate the start of the word
        let l:line = getline('.')
        let l:start = col('.') - 1
        while l:start > 0 && l:line[l:start - 1] =~# '\v[-a-zA-Z0-9_]'
            let l:start -= 1
        endwhile
        return l:start
    " min len = 1
    elseif empty(a:base[:1])
        return -1
    else
        let l:buf_nums = []
        for l:buf_no in range(1, bufnr("$"))
            if bufexists(l:buf_no)
                let l:buf_nums = add(l:buf_nums, l:buf_no)
            endif
        endfor
        return systemlist('command grep --no-filename -E -o "\b'.a:base.'[-a-zA-Z0-9_]+\b" '.join(filter(map(l:buf_nums, 'fnameescape(fnamemodify(bufname(v:val), ":p"))'), 'filereadable(v:val)'), ' '))
    endif
endfun
