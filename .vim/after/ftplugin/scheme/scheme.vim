setl sw=2 ts=4 expandtab foldmethod=indent

for s:pair in items({'scmindent': 'scmindent'})
    if executable(s:pair[0])
        exe 'setl formatprg='.escape(s:pair[1], ' ')
        break
    endif
endfor
