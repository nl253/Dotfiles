
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
        winc b
        " successfully changed windown
        if winnr() != s:old
            star
        endif
        return 1
    endfor

    " term buffer exists but not in a split -- reopen split
    for i in range(1, bufnr("$") - 1, 1) 
        if bufname(i) =~# 'term://'
            exe 'bo '.l:height.'sp'
            exe 'b + '.i
            star " auto-enter insert in terminal
            " because of the return the loop will exit when found
            return 1
        endif 
    endfor

    " term not open -- make a new split and open term in it
    exe 'bo '.l:height.'sp'
    exe 'te '.a:repl

    startinsert
endf

" Commands ---------------------------------
"
fu! repl#list_repls(A, L, P)

    let l:ok = []
    
    let l:map = {
                \ 'ash': ['-i'], 
                \ 'bash': ['-i'], 
                \ 'ccsh': [], 
                \ 'clisp': ['-repl'],
                \ 'csh': ['-i'], 
                \ 'dash': ['-i'], 
                \ 'erl': [], 
                \ 'fish': [], 
                \ 'ghci': [], 
                \ 'git add': ['--patch'], 
                \ 'git checkout': ['--patch'], 
                \ 'git commit': ['--patch'], 
                \ 'git diff --staged': ['--patch'], 
                \ 'git diff': ['--patch'], 
                \ 'git reset': ['--patch'], 
                \ 'gitsome': [], 
                \ 'guile': ['--no-debug', '--'], 
                \ 'ion': [], 
                \ 'ipython': ['--pprint', '--autoindent', '--pylab'], 
                \ 'ipython3': ['--pprint', '--autoindent', '--pylab'], 
                \ 'iruby': [], 
                \ 'jshell': [], 
                \ 'ksh': ['-i'], 
                \ 'lua': ['-i'], 
                \ 'mawk': [], 
                \ 'mksh': [], 
                \ 'mongo': ['--shell'], 
                \ 'mosh': [], 
                \ 'mysql': [], 
                \ 'node': ['-i'], 
                \ 'octave': [], 
                \ 'pdksh': ['-i'], 
                \ 'psql': [], 
                \ 'psysh': [], 
                \ 'ptipython': [], 
                \ 'ptipython3': [], 
                \ 'ptpython': [], 
                \ 'ptpython3': [], 
                \ 'pypy': ['-i'], 
                \ 'pypy3': ['-i'], 
                \ 'python': ['-i'], 
                \ 'python3': ['-i'], 
                \ 'racket': ['-i'],
                \ 'ranger': [], 
                \ 'rebar': ['shell'], 
                \ 'rebar3': ['shell'], 
                \ 'redis': [], 
                \ 'redis-cli': [], 
                \ 'rlwrap clojure': ['--repl'],
                \ 'rlwrap dash': ['-i'],
                \ 'rlwrap racket': ['-i'],
                \ 'ruby': [], 
                \ 'sbcl': [], 
                \ 'scsh': [], 
                \ 'sh': [], 
                \ 'sqlite': [], 
                \ 'sqlite3': ['-interactive'], 
                \ 'ssh': ['nl253@raptor.kent.ac.uk'], 
                \ 'stack ghci': [], 
                \ 'zsh': ['-i'],        
                \ }
    for l:bin in keys(l:map)
        " check if the first non-space word in each key is executable
        " optionally you can prefix commands with 'rlwrap' (adds line editing)
        " and this will respond to that by checking the second non-space word
        if (((l:bin =~# '^\s*rlwrap') && executable('rlwrap') && executable(substitute(l:bin, '\v^rlwrap\s+(\S+).*', '\1', ''))) || executable(substitute(l:bin, '\v^(\S+).*', '\1', ''))) && (l:bin =~? a:A)
            call add(l:ok, l:bin . ' ' . join(l:map[l:bin]))
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

com! -nargs=* -complete=customlist,repl#list_repls ShellREPL 
            \ call repl#shell_repl_cmd(<q-args>)
