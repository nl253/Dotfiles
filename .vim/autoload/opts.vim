fu! opts#my_tabline()
    let s = ''
    for i in range(tabpagenr('$'))
        " select the highlighting
        if i + 1 == tabpagenr()
            let s .= '%#TabLineSel#'
        else
            let s .= '%#TabLine#'
        endif

        " set the tab page number (for mouse clicks)
        let s .= '%' . (i + 1) . 'T'

        " the label is made by opts#my_tab_label()
        let s .= ' %{opts#my_tab_label(' . (i + 1) . ')} '
    endfor

    " after the last tab fill with TabLineFill and reset tab page nr
    let s .= '%#TabLineFill#%T'

    return s
endf

fu! opts#git_status_summary()
    " see if in a git repository
    call system('command git status') 

    " not in a git repository
    if v:shell_error 
        return "not in git repo | "
    endif 

    " last commit message 
    let l:msg = systemlist("command git --no-pager log --pretty=format:\"%h %ce %cr '%s'\" -1")[0] . " "

    " modified since last commit
    if system("command git ls-files -m") =~# expand("%:t")
        let l:msg .= "[M]"

    " not modified
    elseif system("command git ls-files") =~# expand("%:t")
        let l:msg .= ""

    " not added 
    elseif system("git ls-files --others --exclude-standard") =~#  expand('%:p')
        let l:msg .= "[I]"

    " ignored
    else 
        let l:msg .= "[?]"
    endif

    return l:msg.' | '
endf

" Add to $PATH bin dirs for package managers in case they aren't in $PATH already
fu! opts#append_to_path(paths)
    for l:dir in filter(map(a:paths, 'split(expand(v:val))[0]'), 'isdirectory(v:val) && !($PATH =~# v:val)')
        let $PATH = l:dir.':'.$PATH
    endfor
endf

fu! opts#my_tab_label(n)
    let buflist = tabpagebuflist(a:n)
    let winnr = tabpagewinnr(a:n)
    let bname = bufname(buflist[winnr - 1])
    return '#'.a:n.' '.substitute(bname, $HOME, '\~', '')
endf
