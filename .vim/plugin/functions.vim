if exists('g:loaded_vim_saner_functions') || !has('unix') || !executable('bash')
    finish 
endif

fu! s:append_to_dict(word)
    if filereadable(&dictionary) 
        silent call writefile(split(a:word), expand(&dictionary), 'a') 
        echom '[vim-saner] appended '.a:word.' to '.&dictionary
    else 
        echoerr '[vim-saner] dictionary not set'
    endif
endfu

fu! s:async_run(cmd)
    let l:expanded = join(map(split(a:cmd), 'substitute(expand(v:val), "\n", " ", "g")'))
    silent call system("bash -c '".shellescape(l:expanded)."' &")
    echom '[vim-saner] "'.l:expanded.'" spawned in the background'
endfu

fu! s:hl_word()
    let @/ = ''
    if exists('#auto_highlight')
        au! auto_highlight
        aug! auto_highlight
        setl updatetime=4000
        echo 'Highlight current word: off'
        return 0
    else
        aug auto_highlight
            au!
            au CursorHold * let @/ = '\V\<'.escape(expand('<cword>'), '\').'\>'
        aug end
        setl updatetime=500
        echo 'Highlight current word: ON'
        return 1
    endif
endfu

fu! s:close_dup_tabs() 
    let cnt = 0
    let i = 1
    let tpbufflst = []
    let dups = []
    let tabpgbufflst = tabpagebuflist(i)
    while type(tabpagebuflist(i)) == 3
        if index(tpbufflst, tabpagebuflist(i)) >= 0
            call add(dups, i)
        else
            call add(tpbufflst, tabpagebuflist(i))
        endif
        let i += 1
        let cnt += 1
    endwhile
    call reverse(dups)
    for tb in dups
        exec "tabclose ".tb
    endfor
endfu

fu! s:safe_subst(l1, l2)
    let l:word = expand('<cword>')
    if l:word != ''
        let l:in = input('replace '.l:word.' with ... ')
        if l:in != ''
            exec a:l1.','.a:l2.'s/'.l:word.'/'.l:in.'/c'
        else
            echoerr '[vim-saner] empty string'
        endif
    endif
endf

fu! s:reformat_buffer()
    if (&formatprg =~# '^fmt') || (&formatprg == '')
        let l:col = virtcol('.')
        silent normal mzgg=G'z
        silent exec "normal l".l:col
        unlet l:col
    elseif &equalprg != ""
        silent exec ("%!".&equalprg)
    else
        silent exec ("%!".&formatprg)
    endif
endf

com! -nargs=1 -complete=tag      AppendToDict   call s:append_to_dict(<q-args>)
com! -range=%                    SubstituteWord silent call s:safe_subst("".<line1>, "".<line2>)
com! -nargs=+ -complete=shellcmd AsyncRun       call s:async_run(<q-args>)
com!                             CloseDupTabs   call s:close_dup_tabs()
com!                             HLCurrentWord  silent call s:hl_word()
com!                             ReformatBuffer silent call s:reformat_buffer()

com! HL    ec exists('*SyntaxAttr') ?  SyntaxAttr() : join(map(synstack(line('.'), col('.')), 'synIDattr(v:val, "name")'), '/')
com! GRoot exec 'silent lcd' fnameescape(fnamemodify(finddir('.git', escape(expand('%:p:h'), ' ') . ';'), ':h')) | echom '[vim-saner] changed to git root "'.$PWD.'"'
com! Ctags try | silent call system("cd ".fnameescape(fnamemodify(finddir('.git', escape(expand('%:p:h'), ' ') . ';'), ':h'))." && ctags -R") | catch /.*/ | silent call system('ctags -R')| endtry

com! -range=% SnakeToCamel  <line1>,<line2>s#_\(\l\)#\u\1#g
com! -range=% SnakeToPascal <line1>,<line2>s#\(\%(\<\l\+\)\%(_\)\@=\)\|_\(\l\)#\u\1\2#g
com! -range=% SNAKEToSnake  <line1>,<line2>SNAKEToPascal | <line1>,<line2>PascalToSnake
com! -range=% SNAKEToPascal <line1>,<line2>s#_*\(\u\)\(\u*\)#\1\L\2#g
com! -range=% SNAKEToCamel  <line1>,<line2>SNAKEToPascal | <line1>,<line2>PascalToCamel
com! -range=% CamelToSnake  <line1>,<line2>s/\v([a-z]+)([A-Z])([a-z]+)/\1_\L\2\3/g
com! -range=% CamelToPascal <line1>,<line2>CamelToSnake | <line1>,<line2>SnakeToPascal
com! -range=% PascalToSnake <line1>,<line2>s#\(\<\u\l\+\|\l\+\)\(\u\)#\l\1_\l\2#g
com! -range=% PascalToCamel <line1>,<line2>PascalToSnake | <line1>,<line2>SnakeToCamel

let g:loaded_vim_saner_functions = 1
