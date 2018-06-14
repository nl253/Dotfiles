let g:xml_syntax_folding = 1 " might be computationally demanding
let g:xml_syntax_folding = 1 " might be computationally demanding

setl foldmethod=syntax

if exists(':EmmetInstall')
	EmmetInstall
	imap <buffer> <Tab> <plug>(emmet-expand-abbr)
endif

if executable('js-beautify')
    setl formatprg=html-beautify
endif
