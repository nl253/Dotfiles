setl foldmarker={,} foldmethod=syntax
setl shiftwidth=2 tabstop=2 expandtab formatprg=

call setters#formatprg({ 
            \ 'js-beautify': 'js-beautify',
            \ 'prettier':    'prettier --stdin --parser typescript',
            \ })

if executable('standard') 
    exe 'setl '.escape('formatprg=standard --fix --stdin'.(empty(&formatprg) ? '' : ' \| '.&formatprg), ' \|')
endif

if &omnifunc == "" || &omnifunc == 'javascriptcomplete#CompleteJS'
    setl omnifunc=syntaxcomplete#Complete
endif

