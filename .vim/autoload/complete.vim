fu! complete#buffer_words(ArgLead, CmdLine, CursorPos)
    return systemlist("command grep -oE ".shellescape(!empty(a:ArgLead) ? a:ArgLead."[-a-zA-Z0-9_]*" : '[-a-zA-Z0-9_]{3,}')." < ".shellescape(expand('%')).' | command sort -d | command uniq -u | command tr "[:upper:]" "[:lower:]"')
endf

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
