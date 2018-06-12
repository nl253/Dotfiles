fu! repl#set_repl(dict)
    aug ReplFtypes
        au!
        for l:entry in items(a:dict)
            exe "au Filetype ".l:entry[0]." nn <buffer> <Leader>' :silent call repl#open_repl(".string(l:entry[1]).")<CR>"
        endfor
    aug END
endf

fu! repl#open_shell()
    let l:shell = executable('bash') ? 'bash' : executable('zsh') ? 'zsh' : executable('fish') ? 'fish' : executable('ksh') ? 'ksh' : 'sh'
    silent call repl#open_repl(join([l:shell, '-i', '&&', 'cd', getcwd()], ' '))
endf

fu! repl#open_repl(repl)

    let g:__windows = {}
    let l:height = 12

    " get all win numbers
    windo let g:__windows[bufname("%")] = winnr()

    " all term bufs
    " if not not empty -- term already open -- nothing to do
    let s:open_terms = filter(g:__windows, 'v:key =~# "term://"') 

    for i in values(s:open_terms)
        let s:old = winnr()
        " bottom-most window
        wincmd b
        " successfully changed windown
        if winnr() != s:old
            startinsert
        endif
        return 1
    endfor

    " term buffer exists but not in a split -- reopen split
    for i in range(1, bufnr("$") - 1, 1) 
        if bufname(i) =~# 'term://'
            exe 'botright '.l:height.'split'
            exe 'b + '.i
            startinsert " auto-enter insert in terminal
            " because of the return the loop will exit when found
            return 1
        endif 
    endfor

    " term not open -- make a new split and open term in it
    exe 'botright '.l:height.'split'
    exe 'terminal '.a:repl

    startinsert
endf

" Commands ---------------------------------

com! ShellREPL silent call repl#open_shell()  
