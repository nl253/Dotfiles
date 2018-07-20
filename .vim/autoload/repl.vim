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
"
"
fu! repl#list_repls(A, L, P)

    let l:ok = []
    
    let l:args = {
                \ 'ash': ['-i'], 
                \ 'bash': ['-i'], 
                \ 'clisp': ['-repl'],
                \ 'csh': ['-i'], 
                \ 'dash': ['-i'], 
                \ 'erl': [], 
                \ 'fish': [], 
                \ 'ghci': [], 
                \ 'stack ghci': [], 
                \ 'git add': ['--patch'], 
                \ 'git commit': ['--patch'], 
                \ 'git diff': ['--patch'], 
                \ 'git diff --staged': ['--patch'], 
                \ 'git reset': ['--patch'], 
                \ 'guile': ['--no-debug', '--'], 
                \ 'ipython': [' --pprint', '--autoindent', '--pylab'], 
                \ 'ipython3': ['--pprint', '--autoindent', '--pylab'], 
                \ 'iruby': [], 
                \ 'jshell': [], 
                \ 'ksh': ['-i'], 
                \ 'lua': ['-i'], 
                \ 'mongo': ['--shell'], 
                \ 'mysql': [], 
                \ 'node': ['-i'], 
                \ 'pdksh': ['-i'], 
                \ 'rebar3': ['shell'], 
                \ 'rebar': ['shell'], 
                \ 'psql': [], 
                \ 'psysh': [], 
                \ 'ptpython': [], 
                \ 'python': ['-i'], 
                \ 'python3': ['-i'], 
                \ 'ranger': [], 
                \ 'redis': [], 
                \ 'redis-cli': [], 
                \ 'ruby': [], 
                \ 'sbcl': [], 
                \ 'sh': [], 
                \ 'sqlite': [], 
                \ 'sqlite3': ['-interactive'], 
                \ 'ssh': ['nl253@raptor.kent.ac.uk'], 
                \ 'zsh': ['-i'],        
                \ }

    for l:bin in keys(l:args)
        if executable(substitute(l:bin, '\v^(\S+) .*', '\1', '')) && l:bin =~? a:A 
            call add(l:ok, l:bin . ' ' . join(l:args[l:bin]))
        endif
    endfor
    return l:ok
endf

fu! repl#shell_repl_cmd(args)
    if len(a:args) > 0 
        call repl#open_repl(a:args) 
    else 
        call repl#open_shell() 
    endif 
endf

com! -nargs=* -complete=customlist,repl#list_repls ShellREPL call repl#shell_repl_cmd(<q-args>)
