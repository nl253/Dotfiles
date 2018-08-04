setl foldmethod=marker foldmarker={,} shiftwidth=2 tabstop=4 expandtab foldlevel=0

call opts#formatprg({ 
            \ 'js-beautify': 'css-beautify',
            \ 'prettier':    'prettier --stdin --parser css',
            \ 'stylelint':   'stylelint --fix',
            \ })

if &omnifunc == ''
	setl omnifunc=csscomplete#CompleteCSS
endif
