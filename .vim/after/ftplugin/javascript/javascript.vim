setl foldmarker={,} foldmethod=syntax
setl shiftwidth=2 tabstop=2 expandtab formatprg=

if executable('prettier')
	setl formatprg=prettier\ --parser=typescript
elseif executable('js-beautify')
	setl formatprg=js-beautify
endif

if executable('standard') 
    exe 'setl '.escape('formatprg=standard --fix --stdin'.(empty(&formatprg) ? '' : ' \| '.&formatprg), ' \|')
endif

if &omnifunc == "" || &omnifunc == 'javascriptcomplete#CompleteJS'
    setl omnifunc=syntaxcomplete#Complete
endif

