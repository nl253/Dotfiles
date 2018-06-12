fu! funct#let_global_with_default(var_name, val)
    if !exists('g:'.a:var_name)
        exe 'let g:'.a:var_name.' = '.string(a:val)
    endif
endf

fu! funct#colorize(group, what) 
    setg redrawtime=200
    for i in range(1, 255) 
        echom "Color nr ".i 
        exe 'hi '.a:group.' '.a:what.'='.i 
        redraw 
        sleep 300m
    endfor 
    setg redrawtime=2000
endf
