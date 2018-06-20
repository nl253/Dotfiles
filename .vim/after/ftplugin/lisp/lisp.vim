for i in ['lisp_instring', 'lisp_rainbow']
    if !exists('g:'.i)
        exec 'let g:'.i.' = 1'
    endif
endfor

if executable('sbcl')
    setl makeprg=sbcl\ --script\ %
elseif executable('clisp')
    setl makeprg=clisp\ %\ 2>/dev/null
endif

