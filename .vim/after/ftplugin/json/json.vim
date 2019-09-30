setl foldmarker={,} foldmethod=syntax shiftwidth=2 tabstop=2

for s:pair in items({ 
            \ 'jq':          'jq .',
            \ 'js-beautify': 'js-beautify',
            \ 'prettier':    'prettier --stdin --parser json --print-width='.max([&textwidth, 80]).' --single-quote --no-semi --arrow-parens avoid',
            \ })
    if executable(s:pair[0])
        exe 'setl formatprg='.escape(s:pair[1], ' ')
        break
    endif
endfor
