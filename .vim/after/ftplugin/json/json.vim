setl foldmarker={,}
setl foldmethod=syntax
setl shiftwidth=2 tabstop=2

if executable('jq')
	let &formatprg = "jq -S ."
elseif executable('js-beautify')
	setl formatprg=js-beautify
elseif executable('prettier')
	exec 'setl formatprg=prettier\ --stdin\ --parser\ json\ --print-width='.max([&textwidth, 80]).'\ --single-quote\ --no-semi\ --arrow-parens\ avoid'
endif
