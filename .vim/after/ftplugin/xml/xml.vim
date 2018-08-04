setl foldmethod=indent

call opts#formatprg({ 
            \ 'js-beautify': 'html-beautify',
            \ 'prettier':    'prettier --stdin --parser markdown',
            \ })
