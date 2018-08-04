setl shiftwidth=4 tabstop=4 expandtab foldmethod=expr complete-=k formatoptions=cqjonl1 

call opts#formatprg({ 
            \ 'autopep8': 'autopep8 -',
            \ 'yapf':    'yapf',
            \ 'black':    'black',
            \ })

let b:match_words = '\<if\>:\<elif\>:\<else\>'

let s:anchors = ['Pipfile.lock', 'setup.py', 'requirements.txt', '.git']
call utils#add_project_files(s:anchors)
call tags#project(s:anchors, 0)
call tags#lib(999, 0, '~/.local/lib/python3.6/site-packages', '/usr/lib/python3.6')

