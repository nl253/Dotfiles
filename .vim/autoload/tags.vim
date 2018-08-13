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

" "                                string     string    bool      bool
" fu! tags#generate_tags_from_sources(sources_dir, anchor, check_vc, do_remove)
" " save state
" let l:save_pwd = getcwd()
" let l:shell = &shell
" setg shell=/bin/bash

" let l:ext = expand('%:e')

" " check if stale or bang
" if filereadable(l:tags) && utils#is_stale(l:tags)
" " if so, remove
" sil call system("rm ".l:tags)
" endif

" if !filereadable(l:tags)
" echom 'creating fresh tags for ['.&filetype.'] in '.string(getcwd())
" if isdirectory(expand(a:sources_dir)) && executable('tar')
" for l:archive in split(expand(a:sources_dir.'/**/*.tar.gz'), '\n')
" " dir.tar.gz => dir
" let l:unpacked = fnamemodify(l:archive, ':r:r')
" if !isdirectory(l:unpacked)
" " source files stored *.tar.gz archives
" " this will extract them if needed
" call system('tar xf '.l:archive.' -C '.fnamemodify(l:unpacked, ':h'))
" endif
" endfor
" " NOTE: it's important that the argv is not too long
" "       there is too many files to pass all of them to bash so `find` is
" "       used instead
" call system('ctags $(find . -name "*.'.l:ext.'") $(find '.a:sources_dir.' -name "*.'.l:ext.'")')
" else
" call system('ctags $(find . -name "*.'.l:ext.'")')
" endif
" endif

" " if this tag file is not seen by Vim add it 
" if filereadable(l:tags) && index(tagfiles(), l:tags) < 0
" exe 'setl tags+='.l:tags 
" endif

" " restore state
" exe 'cd '.l:save_pwd
" exe 'setg shell='.l:shell
" endf
