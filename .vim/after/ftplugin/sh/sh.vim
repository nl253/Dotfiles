call opts#formatprg({'shfmt': 'shfmt -ci -i 2'})
call opts#makeprg({'bash': 'bash %'})
call opts#safe_setl([
            \ 'shiftwidth=2', 
            \ 'iskeyword+=\-',
            \ 'tabstop=2', 
            \ 'foldmethod=marker', 
            \ 'foldlevel=4', 
            \ 'foldmarker={,}'
            \ ])
call opts#omni(['BashOmniFunc'])
let g:readline_has_bash = 1 
let g:is_bash = 1
let b:match_words = '\<if\>:\<elif\>:\<else\>'
" let g:sh_fold_enabled = 4
