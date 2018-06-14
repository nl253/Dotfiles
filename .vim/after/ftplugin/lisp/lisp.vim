for i in ['lisp_instring', 'lisp_rainbow']
    if !exists('g:'.i)
	exec 'let g:'.i.' = 1'
    endif
endfor
