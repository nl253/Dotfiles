setl foldmarker={,} foldmethod=syntax
setl shiftwidth=2 tabstop=2 expandtab formatprg=

call opts#formatprg({ 
            \ 'js-beautify': 'js-beautify',
            \ 'prettier':    'prettier --stdin --parser typescript',
            \ })

if executable('standard') 
    exe 'setl '.escape('formatprg=standard --fix --stdin'.(empty(&formatprg) ? '' : ' \| '.&formatprg), ' \|')
endif

" I added plenty of syntax keywords so 'syntaxcomplete#Complete' is better than 'javascriptcomplete#CompleteJS'
if &omnifunc == "" || &omnifunc == 'javascriptcomplete#CompleteJS'
    setl omnifunc=syntaxcomplete#Complete
endif

