setl foldmethod=marker foldmarker={,} shiftwidth=2 tabstop=4 expandtab foldlevel=0

if executable('js-beautify')
	setl formatprg=css-beautify
elseif executable('prettier')
	exec 'setl formatprg=prettier\ --stdin\ --parser\ css'
endif

if executable('stylelint')
	exec 'setl formatprg='.&formatprg.'\|stylelint\ --fix'
endif

if &omnifunc == ''
	setl omnifunc=csscomplete#CompleteCSS
endif
