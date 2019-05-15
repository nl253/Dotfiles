for s:pair in items({'shfmt': 'shfmt -ci -i 2'})
    if executable(s:pair[0])
        exe 'setl formatprg='.escape(s:pair[1], ' ')
        break
    endif
endfor
for s:pair in items({'bash': 'bash %'})
    if executable(s:pair[0])
        exe 'setl formatprg='.escape(s:pair[1], ' ')
        break
    endif
endfor
setl shiftwidth=2 iskeyword+= tabstop=2 foldmethod=marker foldlevel=4 foldmarker={,}
let g:readline_has_bash = 1 
let g:is_bash = 1
let b:match_words = '\<if\>:\<elif\>:\<else\>'
" let g:sh_fold_enabled = 4
