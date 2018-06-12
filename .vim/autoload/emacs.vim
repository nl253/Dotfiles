fu! emacs#cmd_line_readline_K()
    if getcmdpos() == 1
        let g:__modifiled_cmdline = ''
    else
        let g:__modifiled_cmdline = getcmdline()[:getcmdpos() - 2]
    endif
endf

fu! emacs#transpose_words()
    normal! viWE"xdi=join(reverse(split(@x, ' ')), ' ')
endf
