setl shiftwidth=4 tabstop=4 expandtab foldmethod=indent complete-=k formatoptions=cqjonl1

if executable('isort')
    for s:pair in items({ 
                \ 'black':    'isort - \| black - 2>/dev/null',
                \ 'yapf':     'isort - \| yapf 2>/dev/null',
                \ 'autopep8': 'isort - \| autopep8 - 2>/dev/null',
                \ })
        if executable(s:pair[0])
            exe 'setl formatprg='.escape(s:pair[1], ' ')
            break
        endif
    endfor
else
    for s:pair in items({ 
                \ 'black':    'black - 2>/dev/null',
                \ 'yapf':     'yapf 2>/dev/null',
                \ 'autopep8': 'autopep8 - 2>/dev/null',
                \ })
        if executable(s:pair[0])
            exe 'setl formatprg='.escape(s:pair[1], ' ')
            break
        endif
    endfor
endif

let b:match_words = '\<if\>:\<elif\>:\<else\>'


Ctags    Pipfile.lock setup.py requirements.txt .git
" let s:lib_dir_global = split(systemlist('echo ~/.local/lib/python3.?/site-packages')[0], ' ')[0]
" let s:lib_dir_local= split(systemlist('echo /usr/lib/python3.?')[0], ' ')[-1]
" exe join(['CtagsLib', s:lib_dir_global, s:lib_dir_local], ' ') 
