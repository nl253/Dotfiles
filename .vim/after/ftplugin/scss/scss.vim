exec 'so '.expand('<sfile>:p:h:h').'/css/css.vim'
" setl ft=scss.css
if executable('prettier')
	exec 'setl formatprg=prettier\ --stdin\ --parser\ scss\ --print-width='.&textwidth.'\ --single-quote\ --no-semi\ --arrow-parens\ avoid'
endif
