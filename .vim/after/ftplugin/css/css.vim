setl foldmethod=marker foldmarker={,} shiftwidth=2 tabstop=4 expandtab foldlevel=0

for s:pair in items({ 
            \ 'js-beautify': 'css-beautify',
            \ 'prettier':    'prettier --stdin --parser css',
            \ 'stylelint':   'stylelint --fix',
            \ })
    if executable(s:pair[0])
        exe 'setl formatprg='.s:pair[1]
        break
    endif
endfor

if empty(&omnifunc)
	setl omnifunc=csscomplete#CompleteCSS
endif
