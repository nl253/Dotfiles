"                                string     string    bool      bool
fu! tags#generate_tags_from_sources(sources_dir, anchor, check_vc, do_remove)
    " save state
    let l:save_pwd = getcwd()
    let l:shell = &shell
    setg shell=/bin/bash

    let l:ext = expand('%:e')

    for l:node in ([a:anchor] + (a:check_vc ? ['.git', '.svn', '.hg'] : []) + [expand('%')])
        let l:try_find = finddir(l:node, '.;')
        if isdirectory(l:try_find)
            exe 'cd '.fnamemodify(l:try_find, ':p:h')
            let l:tags = getcwd().'/tags'
        else
            let l:try_find = findfile(l:node, '.;')
            if filereadable(l:try_find)
                exe 'cd '.fnamemodify(l:try_find, ':p:h')
                let l:tags = getcwd().'/tags'
            endif
        endif
    endfor

    " check if stale or bang
    if filereadable(l:tags) && (a:do_remove || ((localtime() - getftime(l:tags)) > (60 * 10)))
        " if so, remove
        sil call system("rm ".l:tags)
    endif

    if !filereadable(l:tags)
        echom 'creating fresh tags for ['.&filetype.'] in "'.getcwd().'"'
        if isdirectory(expand(a:sources_dir)) && executable('tar')
            for l:archive in split(expand(a:sources_dir.'/**/*.tar.gz'), '\n')
                " dir.tar.gz => dir
                let l:unpacked = fnamemodify(l:archive, ':r:r')
                if !isdirectory(l:unpacked)
                    " source files stored *.tar.gz archives
                    " this will extract them if needed
                    call system('tar xf '.l:archive.' -C '.fnamemodify(l:unpacked, ':h'))
                endif
            endfor
            " NOTE: it's important that the argv is not too long
            "       there is too many files to pass all of them to bash so `find` is
            "       used instead
            call system('ctags $(find . -name "*.'.l:ext.'") $(find '.a:sources_dir.' -name "*.'.l:ext.'")')
        else
            call system('ctags $(find . -name "*.'.l:ext.'")')
        endif
    endif

    " if this tag file is not seen by Vim
    if filereadable(l:tags) && !(&tags =~ l:tags)
        " add it 
        exe 'setl tags+='.l:tags 
    endif

    " restore state
    exe 'cd '.l:save_pwd
    exe 'setg shell='.l:shell
endf
