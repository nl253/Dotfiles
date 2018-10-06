call opts#safe_setl([
            \ 'shiftwidth=4', 
            \ 'tabstop=4', 
            \ 'expandtab', 
            \ 'foldmethod=indent', 
            \ 'complete-=k', 
            \ 'formatoptions=cqjonl1'
            \ ])

if executable('isort')
    call opts#formatprg({ 
                \ 'black':    'isort - \| black - 2>/dev/null',
                \ 'yapf':     'isort - \| yapf',
                \ 'autopep8': 'isort - \| autopep8 -',
                \ })
else
    call opts#formatprg({ 
                \ 'black':    'black - 2>/dev/null',
                \ 'yapf':     'yapf',
                \ 'autopep8': 'autopep8 -',
                \ })
endif

let b:match_words = '\<if\>:\<elif\>:\<else\>'

let s:lib_dir_global = split(systemlist('echo ~/.local/lib/python3.?/site-packages')[0], ' ')[0]
let s:lib_dir_local= split(systemlist('echo /usr/lib/python3.?')[0], ' ')[-1]

Ctags    Pipfile.lock setup.py requirements.txt .git
exe join(['CtagsLib', s:lib_dir_global, s:lib_dir_local], ' ') 
