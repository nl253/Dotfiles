call opts#safe_setl([
            \ 'shiftwidth=4', 
            \ 'tabstop=4', 
            \ 'expandtab', 
            \ 'foldmethod=indent', 
            \ 'complete-=k', 
            \ 'formatoptions=cqjonl1'
            \ ])

call opts#formatprg({ 
            \ 'black':    'black - 2>/dev/null',
            \ 'yapf':     'yapf',
            \ 'autopep8': 'autopep8 -',
            \ })

let b:match_words = '\<if\>:\<elif\>:\<else\>'

CtagsProject Pipfile.lock setup.py requirements.txt .git
CtagsLib     ~/.local/lib/python3.6/site-packages /usr/lib/python3.6
