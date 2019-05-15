setl foldmarker={,} foldmethod=marker shiftwidth=2 tabstop=2 expandtab formatprg=

for s:pair in items({ 
            \ 'prettier':    'prettier --stdin --parser typescript',
            \ 'js-beautify': 'js-beautify',
            \ })
    if executable(s:pair[0])
        exe 'setl formatprg='.s:pair[1]
        break
    endif
endfor

if executable('standard') 
    exe 'setl '.escape('formatprg=standard --fix --stdin'.(empty(&formatprg) ? '' : ' \| '.&formatprg), ' \|')
endif

" I added plenty of syntax keywords so 'syntaxcomplete#Complete' is better than 'javascriptcomplete#CompleteJS'
" if empty(&omnifunc) || &omnifunc ==? 'javascriptcomplete#CompleteJS'
setl omnifunc=syntaxcomplete#Complete
" endif

