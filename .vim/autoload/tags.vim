fu! tags#project(force, ...) abort

    let l:anchors = filter(a:000 + [], '!empty(v:val)') + []

    let l:ext = expand('%:e')

    if empty(l:ext) 
        throw 'empty extension -- could not gather similar files'
    endif

    exe "let l:root = utils#proj_root(".join(map(l:anchors, 'string(v:val)'), ', ').")"

    if !isdirectory(l:root)
        throw 'could not locate project root (found: '.l:root.')'
    endif

    let l:tag_file = l:root.'/tags'

    if !a:force && !utils#is_stale(l:tag_file, 10)
        echomsg 'tags not stale (tip: force with a bang!)'
        return 0
    endif

    let l:save_shell = &shell
    let &shell = '/bin/bash'

    echom "generating fresh project tags for ".string(&filetype)." in ".string(l:tag_file)

    let l:cmd = "command ctags -f ".shellescape(l:tag_file)." $(command find ".shellescape(l:root)." -maxdepth 8 -type f -name ".string('*.'.l:ext).")"

    echom l:cmd

    call system(l:cmd)

    let &shell = l:save_shell

    if v:shell_error 
        throw 'error '.v:shell_error.' has occurred'
    endif

    if filereadable(l:tag_file)
        call opts#comma_opt('tags', [l:tag_file, l:root.'/**3/tags'])
    endif
endf

fu! tags#lib(age_min, force, lib_path, ...) abort
 
    if empty(&filetype) 
		throw 'could not determine filetype'
    endif

    for l:dir in a:000 + [a:lib_path]
        call assert_true(isdirectory(expand(l:dir)), "non-existent library dir")
    endfor

    let l:vim_tmp_dir = expand("~/.cache/vim")

    let l:ext = expand('%:e')

    if empty(l:ext) 
        throw 'empty extension -- could not gather similar files'
    endif

    let l:subdir = &filetype

    let l:tmp_dir = l:vim_tmp_dir.'/'.l:subdir

    let l:tag_file = l:tmp_dir.'/tags'

    if !a:force && !utils#is_stale(l:tag_file, 10)
        echomsg 'tags not stale (tip: force with a bang!)'
        return 0
    endif


    call mkdir(l:tmp_dir , "p")

    let l:libs = join(map([a:lib_path] + a:000, 'shellescape(expand(v:val))'), ' ')


    let l:save_shell = &shell
    let &shell = '/bin/bash'

    echom "generating fresh lib tags from ".l:libs

    let l:cmd = 'command ctags -f '.shellescape(l:tag_file).' $(command find '.l:libs.' -readable -maxdepth 8 -type f -name '.shellescape('*.'.l:ext).')'

    echom l:cmd

    call system(l:cmd)

    let &shell = l:save_shell

    if v:shell_error
        throw 'error '.v:shell_error.' has occurred'
    endif

    if filereadable(l:tag_file)
        call opts#comma_opt('tags', [l:tag_file])
    endif
endf
