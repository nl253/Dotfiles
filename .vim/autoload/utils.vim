fu! utils#is_regular_buffer()
    let l:this_file_path = expand("%:p")
    if !filewritable(l:this_file_path) || (fnamemodify(l:this_file_path, ':t') ==? '[Command Line]') || (&filetype ==# 'qf') || (&buftype ==# 'nofile') || empty(&filetype)
        return v:false
    else
        return v:true
    endif
endf

fu! utils#bsearch(list, val)

    if empty(a:list)
        return v:false
    endif

    let l:middle_idx = len(a:list) / 2

    let l:middle_val = a:list[l:middle_idx]

    " found !
    if l:middle_val == a:val 
        return v:true

    " NOTE: VimScript slices lists inclusively

    " search left half
    " exclude middle value (checked above)
    elseif a:val < l:middle_val
        if (l:middle_idx - 1) < 0 
            return v:false
        else
            return utils#bsearch(a:list[:(l:middle_idx - 1)], a:val)
        endif

    " search right half
    " exclude middle value (checked above)
    else 
        return utils#bsearch(a:list[(l:middle_idx + 1):], a:val)
    endif
endf

fu! utils#syntax_attr()
     let synid = ""
     let guifg = ""
     let guibg = ""
     let gui   = ""

     let id1  = synID(line("."), col("."), 1)
     let tid1 = synIDtrans(id1)

     if synIDattr(id1, "name") != ""
	  let synid = "group: " . synIDattr(id1, "name")
	  if (tid1 != id1)
	       let synid = synid . '->' . synIDattr(tid1, "name")
	  endif
	  let id0 = synID(line("."), col("."), 0)
	  if (synIDattr(id1, "name") != synIDattr(id0, "name"))
	       let synid = synid .  " (" . synIDattr(id0, "name")
	       let tid0 = synIDtrans(id0)
	       if (tid0 != id0)
		    let synid = synid . '->' . synIDattr(tid0, "name")
	       endif
	       let synid = synid . ")"
	  endif
     endif

     " Use the translated id for all the color & attribute lookups; the linked id yields blank values.
     if (synIDattr(tid1, "fg") != "" )
	  let guifg = " guifg=" . synIDattr(tid1, "fg") . "(" . synIDattr(tid1, "fg#") . ")"
     endif
     if (synIDattr(tid1, "bg") != "" )
	  let guibg = " guibg=" . synIDattr(tid1, "bg") . "(" . synIDattr(tid1, "bg#") . ")"
     endif
     if (synIDattr(tid1, "bold"     ))
	  let gui   = gui . ",bold"
     endif
     if (synIDattr(tid1, "italic"   ))
	  let gui   = gui . ",italic"
     endif
     if (synIDattr(tid1, "reverse"  ))
	  let gui   = gui . ",reverse"
     endif
     if (synIDattr(tid1, "inverse"  ))
	  let gui   = gui . ",inverse"
     endif
     if (synIDattr(tid1, "underline"))
	  let gui   = gui . ",underline"
     endif
     if (gui != ""                  )
	  let gui   = substitute(gui, "^,", " gui=", "")
     endif

     echohl MoreMsg
     let message = synid . guifg . guibg . gui
     if message == ""
	  echohl WarningMsg
	  let message = "<no syntax group here>"
     endif
     echo message
     echohl None
endfunction

fu! utils#make_missing_dirs(dir_list)
    for l:dir in filter(map(deepcopy(a:dir_list), 'expand(v:val)'), '!isdirectory(v:val)')
        call mkdir(l:dir, 'p')
    endfor
endf

fu! utils#cpu_cores()
    let l:try_get = systemlist('grep -c ^processor /proc/cpuinfo')
    return v:shell_error ? 1 : l:try_get[0]
endf

fu! utils#project_files_qf()

    let l:root = utils#proj_root()
    let l:ext = expand('%:e')

    " list all files with the same extension
    if empty(l:ext)
        echoerr "no extension - no way to collect similar files"
        return 1
    endif

    cgete systemlist('find '.l:root.' -readable -type f -maxdepth 8 -name '.string('*.'.l:ext).' 2>/dev/null')
    copen
    wincmd J
endf

fu! utils#git_files_qf()

    let l:root = utils#proj_root()
    let l:ext = expand('%:e')

    " list all files with the same extension
    if empty(l:ext)
        echoerr "no extension - no way to collect similar files"
        return 1
    endif

    let l:save_shell = &shell
    let &shell = '/bin/bash'

    cgete map(systemlist('git -C '.l:root.' ls-tree --full-name --name-only -r HEAD'), 'l:root."/".v:val')

    let &shell = l:save_shell
    copen
    wincmd J
endf

fu! utils#str_to_list(str)
    let l:list = []
    let l:i = 0
    let l:end = len(a:str)
    while l:i < l:end
        let l:list = add(l:list, a:str[l:i])
        let l:i += 1
    endwhile
    return l:list
endf

fu! utils#_buffer_wipeout_helper(path, bufno)
    if getbufvar(a:bufno, '&modified')
        " all good -- valid buffer
        return 1
    endif

    if !filereadable(a:path) || (a:path ==# '') || !(a:path =~# $HOME) || (a:path =~# 'NetrwTreeListing|^man:') || (a:path =~ $VIMRUNTIME && (getbufvar(a:bufno, '&filetype') =~# 'vim|help'))
        " either empty scratch but not modified
        " or     nvim terminal
        " or     man page
        " or     unmodified file not at home
        " or     vim help file
        exe 'bw '.a:bufno
    endif
endf

fu! utils#proj_root(...)

    let l:this_dir = expand('%:p:h')
    let l:anchors = filter(a:000 + [expand('%:t'), '.git'], '!empty(v:val)')

    for l:anchor in l:anchors

        let l:maybe_dir = fnamemodify(finddir(l:anchor, l:this_dir.';'.$HOME), ':p')

        if isdirectory(l:maybe_dir) && (l:maybe_dir != $HOME) && (l:maybe_dir =~# $HOME)
            return fnamemodify(l:maybe_dir, ':h')
        endif

        let l:maybe_file = fnamemodify(findfile(l:anchor, l:this_dir.';'.$HOME), ':p')

        if filereadable(l:maybe_file) && (l:maybe_file =~# $HOME)
            return fnamemodify(l:maybe_file, ':h')
        endif

    endfor

    return l:this_dir
endf

fu! utils#my_tab_label(n)
    let buflist = tabpagebuflist(a:n)
    let winnr = tabpagewinnr(a:n)
    let bname = bufname(buflist[winnr - 1])
    return '#'.a:n.' '.substitute(bname, $HOME, '\~', '')
endf

" Helper fu for statusline
fu! utils#my_tabline()
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

        " the label is made by utils#my_tab_label()
        let s .= ' %{utils#my_tab_label(' . (i + 1) . ')} '
    endfor

    " after the last tab fill with TabLineFill and reset tab page nr
    let s .= '%#TabLineFill#%T'

    return s
endf

" Helper fu for statusline
fu! utils#git_status_summary()
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

fu! utils#is_stale(file, min_limit)
    return !filereadable(a:file) || ((localtime() - getftime(a:file)) > (60 * a:min_limit))
endf

fu! utils#safe_subst(l1, l2)
    let l:word = expand('<cword>')
    if !empty(l:word)
        let l:in = input('replace '.l:word.' with ... ')
        if !empty(l:in)
            exec a:l1.','.a:l2.'s/\v<'.l:word.'>/'.l:in.'/c'
        else
            echoerr 'empty string'
        endif
    endif
endf
