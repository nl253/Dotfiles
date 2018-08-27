" Generate tags from lib_path/**/*.<extension> and place them in ~/.cache/vim/<filetype>
" Return path to the tag file
fu! tags#lib(age_min, force, lib_path, ...)

    let l:vim_tmp_dir = expand("~/.cache/vim")
    let l:ext = expand('%:e')
    let l:subdir = empty(&filetype) ? l:ext : &filetype
    let l:tmp_dir = l:vim_tmp_dir.'/'.l:subdir
    let l:tag_file = l:tmp_dir.'/tags'

    if empty(l:ext) || empty(l:subdir) || (!a:force && !utils#is_stale(l:tag_file, a:age_min))
        return 0
    endif

    call mkdir(l:tmp_dir , "p")

    let l:libs = join([a:lib_path] + a:000, ' ')

    echom "generating fresh lib tags from ".l:libs

    let l:cmd = 'ctags -f '.l:tag_file.' $(find '.l:libs.' -readable -maxdepth 8 -type f -name '.string('*.'.l:ext).')'

    echom l:cmd

    call system(l:cmd)

    if v:shell_error
        echoerr 'error '.v:shell_error.' occurred'
    endif

    call opts#comma_opt('tags', [l:tag_file])
endf

" Generate tags from <project_root>/**/*.<extension> and place them in ~/.cache/vim/<filetype>
" Return path to the tag file
fu! tags#project(anchors, force)

    let l:ext = expand('%:e')
    let l:root = utils#proj_root(a:anchors)
    let l:tag_file = l:root.'/tags'

    if empty(l:ext) || (!a:force && !utils#is_stale(l:tag_file, 10))
        return 0
    endif

    let l:save_shell = &shell
    let &shell = '/bin/bash'

    echom "generating fresh tags for ".string(&filetype)." in ".string(l:tag_file)

    let l:cmd = "ctags -f ".l:tag_file." $(find ".l:root." -maxdepth 8 -type f -name '*.".l:ext."')"

    echom l:cmd

    call system(l:cmd)

    if v:shell_error 
        echoerr 'error '.v:shell_error.' has occurred'
    endif

    let &shell = l:save_shell

    call opts#comma_opt('tags', [l:tag_file, l:root.'/**3/tags'])
endf
