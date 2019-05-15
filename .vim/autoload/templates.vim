let g:template_dir = expand("~/.vim/templates")

fu! templates#read_template()
    if exists('b:_template_read') || !(expand('%:p:h') =~# $HOME)
        return 1 
    elseif line('.') == line('$')  && getline('.') =~# '^\s*$'
        call templates#read_template_helper()
    endif
endfu

fu! templates#template_subst()
    exe "python3 import vim, pystache; vim.current.buffer[:] = list(map(lambda x: pystache.render(x, ".string(GetTemplateVars())."), vim.current.buffer[:]))"
endf

fu! templates#read_template_helper()
    let b:_template_read = 1
    let l:fname = expand('%:t')
    let l:ext = expand('%:e')

    for l:d in map(filter([&filetype, l:ext], 'len(v:val) > 0'), 'g:template_dir . "/" . v:val') + [g:template_dir]
        for l:f in [l:fname, tolower(l:fname), toupper(l:fname)] + map(filter([&filetype, l:ext], 'len(v:val) > 0'), '"template." . v:val')
            let l:t = expand(l:d . '/' . l:f)
            if filereadable(l:t)
                exe '0r '.l:t
                call templates#template_subst()
                return 1
            endif
        endfor
    endfor
    call add(v:errors, 'could not find template for ' . l:fname)
endfu
