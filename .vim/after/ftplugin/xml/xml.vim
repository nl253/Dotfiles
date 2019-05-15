setl foldmethod=indent

for s:pair in items({ 
            \ 'js-beautify': 'html-beautify',
            \ 'prettier':    'prettier --stdin --parser markdown',
            \ })
    if executable(s:pair[0])
        exe 'setl formatprg='.escape(s:pair[1], ' ')
        break
    endif
endfor
