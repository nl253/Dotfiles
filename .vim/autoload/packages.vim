fu! packages#install_package(command, package, binary)
    try
        if !executable(a:binary)
            echom '['.expand('<sfile>').'] installing '.l:package
            echo system('cd && '.a:command.' '.a:package)
        endfor
    catch /.*/
        call add(v:errors, '['.expand('<sfile>')."] couldn't install ".l:package)
    endtry
endf

fu! packages#install_packages(manager, command, packages)
    if !executable(a:manager)
        return 0
    endif
    for l:package in filter(a:packages, '!executable(v:val)')
        try
            echom '['.expand('<sfile>').'] installing '.l:package
            echo system("cd && ".a:manager.' '.a:command.' '.l:package
        catch /.*/
            call add(v:errors, '['.expand('<sfile>')."] couldn't install ".l:package)
        endtry
    endfor
endf
