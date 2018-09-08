
fu! tags#lib(age_min, force, lib_path, ...) abort
 
    if empty(&filetype) 
        return 0
    endif

    for l:dir in a:000 + [a:lib_path]
        call assert_true(isdirectory(expand(l:dir)), "non-existent library dir")
    endfor

    let l:vim_tmp_dir = expand("~/.cache/vim")
    let l:ext = expand('%:e')
    let l:subdir = empty(&filetype) ? l:ext : &filetype
    let l:tmp_dir = l:vim_tmp_dir.'/'.l:subdir
    let l:tag_file = l:tmp_dir.'/tags'

    if empty(l:ext) || empty(l:subdir) || 
                \ (!a:force && !utils#is_stale(l:tag_file, a:age_min))
        return 0
    endif

    call mkdir(l:tmp_dir , "p")

    let l:libs = join(map([a:lib_path] + a:000, 'shellescape(v:val)'), ' ')

    echom "generating fresh lib tags from ".l:libs

    let l:save_shell = &shell
    let &shell = '/bin/bash'

    let l:cmd = 'command ctags -f '.shellescape(l:tag_file).' $(command find '.l:libs.' -readable -maxdepth 8 -type f -name '.shellescape('*.'.l:ext).')'

    echom l:cmd

    call system(l:cmd)

    if v:shell_error
        echoerr 'error '.v:shell_error.' occurred'
    endif

    let &shell = l:save_shell

    if filereadable(l:tag_file)
        call opts#comma_opt('tags', [l:tag_file])
    endif
endf


fu! tags#project(force, anchor, ...)

    let l:anchors = filter([a:anchor] + a:000, '!empty(v:val)') 

    if empty(l:anchors)
        return 0
    endif

    let l:ext = expand('%:e')
    let l:root = execute('echo utils#proj_root('.join(map(l:anchors, 'string(v:val)'), ',').')')
    let l:tag_file = l:root.'/tags'

    if !isdirectory(l:root) || empty(l:ext) || (!a:force && !utils#is_stale(l:tag_file, 10))
        return 0
    endif

    let l:save_shell = &shell
    let &shell = '/bin/bash'

    echom "generating fresh tags for ".string(&filetype)." in ".string(l:tag_file)

    let l:cmd = "ctags -f ".shellescape(l:tag_file)." $(find ".shellescape(l:root)." -maxdepth 8 -type f -name ".string('*.'.l:ext).")"

    echom l:cmd

    call system(l:cmd)

    if v:shell_error 
        echoerr 'error '.v:shell_error.' has occurred'
    endif

    let &shell = l:save_shell

    if filereadable(l:tag_file)
        call opts#comma_opt('tags', [l:tag_file, l:root.'/**3/tags'])
    endif
endf
