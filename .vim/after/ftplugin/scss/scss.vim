" source css runtime files
exec 'so '.expand('<sfile>:p:h:h').'/css/css.vim'

if executable('prettier')
	exec 'setl formatprg='.escape('prettier --stdin --parser scss --print-width='.&textwidth.' --single-quote --no-semi --arrow-parens avoid', ' ')
endif

if executable('node-sass')
    exe 'setl makeprg='.escape('node-sass --output %:p:h --source-map %:p:h --output-style compressed --indent-type space %:p', ' ') 
endif
