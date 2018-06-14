exec 'so '.expand('<sfile>:p:h:h').'/css/css.vim'

if executable('prettier')
	exec 'setl formatprg=prettier\ --stdin\ --parser\ less\ --print-width='.&textwidth.'\ --single-quote\ --no-semi\ --arrow-parens\ avoid'
endif
