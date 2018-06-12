call funct#let_global_with_default('todo_slide_in_width', 65)
call funct#let_global_with_default('todo_file', expand('~/Documents/Notes/todo.md'))
call funct#let_global_with_default('netrw_slide_in_width', 25)

fu! splits#toggle_netrw() 
    if expand('%:t') =~# '^Netrw'
        wincmd c
    else
        windo if expand('%:t') =~# 'Netrw' | wincmd c | endif
        exe 'botright '.g:netrw_slide_in_width.'Vexplore'
        wincmd H
        exe 'vertical resize '.g:netrw_slide_in_width
    endif
endf

fu! splits#slider_toggle(file, position, size) 
    if expand('%:p') == fnamemodify(a:file, ':p')
        wincmd c
    else
        windo if expand('%:p') == fnamemodify(a:file, ':p') | wincmd c | endif

        exe 'botright split '.a:file

        if a:position =~? '^l'
            wincmd H
            exe 'vertical resize '.a:size
        elseif a:position =~? '^r'
            wincmd L
            exe 'vertical resize '.a:size
        elseif a:position =~? '^t'
            wincmd K
            exe 'resize '.a:size
        else 
            wincmd J
            exe 'resize '.a:size
        endif
    endif
endf

fu! splits#toggle_todo() 
    return splits#slider_toggle(g:todo_file, 'r', g:todo_slide_in_width)
endf
