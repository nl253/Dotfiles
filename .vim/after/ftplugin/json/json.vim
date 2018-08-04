setl foldmarker={,}
setl foldmethod=syntax
setl shiftwidth=2 tabstop=2

call opts#formatprg({ 
            \ 'jq':          'jq -S .',
            \ 'js-beautify': 'js-beautify',
            \ 'prettier':    'prettier --stdin --parser json --print-width='.max([&textwidth, 80]).' --single-quote --no-semi --arrow-parens avoid',
            \ })
