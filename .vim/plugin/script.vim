
" dodgy comment: {{ some }}

let g:template_vars = {'some': 22}

com! TemplateSubstitute exe "python3 import vim, pystache; vim.current.buffer[:] = list(map(lambda x: pystache.render(x, ".string(g:template_vars)."), vim.current.buffer[:]))"
