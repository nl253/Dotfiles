setl shiftwidth=4 tabstop=4 expandtab foldmethod=indent complete-=k formatoptions=cqjonl1

if executable('isort')
    for s:pair in [
                \ ['black',    'isort - \| black - 2>/dev/null'],
                \ ['yapf',     'isort - \| yapf 2>/dev/null'],
                \ ['autopep8', 'isort - \| autopep8 - 2>/dev/null'],
                \ ]
        if executable(s:pair[0])
            exe 'setl formatprg='.escape(s:pair[1], ' ')
            break
        endif
    endfor
else
    for s:pair in [
                \ ['black',    'black - 2>/dev/null'],
                \ ['yapf',     'yapf 2>/dev/null'],
                \ ['autopep8', 'autopep8 - 2>/dev/null'],
                \ ]
        if executable(s:pair[0])
            exe 'setl formatprg='.escape(s:pair[1], ' ')
            break
        endif
    endfor
endif

let b:match_words = '\<if\>:\<elif\>:\<else\>'
