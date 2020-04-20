setl foldmarker={,} foldmethod=marker shiftwidth=2 tabstop=2 expandtab formatprg=

for s:pair in [['prettier', 'prettier --parser typescript'], ['js-beautify', 'js-beautify']]
    if executable(s:pair[0])
        exe 'setl formatprg='.escape(s:pair[1], ' ')
        break
    endif
endfor

" I added plenty of syntax keywords so 'syntaxcomplete#Complete' is better than 'javascriptcomplete#CompleteJS'
" if empty(&omnifunc) || &omnifunc ==? 'javascriptcomplete#CompleteJS'
setl omnifunc=syntaxcomplete#Complete
" endif

