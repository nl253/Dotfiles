fu! project#project_files_qf()

    " save $PWD
    let l:old_cwd = getcwd() 

    " cd to this file's dir 
    exe 'cd '.expand('%:p:h')

    " try to find git root
    let l:git_dir = systemlist("git rev-parse --show-toplevel")[0]

    " on success
    if isdirectory(l:git_dir)

        silent exe 'silent cd '.l:git_dir

        " list all git-managed files in qf win
        silent cgete systemlist('git ls-files')

    " on failure
    else
        " change to parent dir of the current file
        silent exe 'silent cd '.expand('%:p:h')

        " list all files with the same extension 
        silent cgete systemlist('find -readable -type f -maxdepth 5 -name '.string('*.'.expand('%:e')).' 2>/dev/null')

    endif
    " return back to $OLDPWD
    silent exe 'cd '.l:old_cwd
endf
