setl foldmarker={,} foldmethod=syntax shiftwidth=2 tabstop=2 expandtab formatprg=

call opts#formatprg({ 
            \ 'prettier':    'prettier --stdin --parser typescript',
            \ 'js-beautify': 'js-beautify',
            \ })

if executable('standard') 
    exe 'setl '.escape('formatprg=standard --fix --stdin'.(empty(&formatprg) ? '' : ' \| '.&formatprg), ' \|')
endif

" I added plenty of syntax keywords so 'syntaxcomplete#Complete' is better than 'javascriptcomplete#CompleteJS'
if empty(&omnifunc) || &omnifunc == 'javascriptcomplete#CompleteJS'
    setl omnifunc=syntaxcomplete#Complete
endif

