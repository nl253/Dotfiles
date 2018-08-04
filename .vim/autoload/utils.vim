" These *need* to be set!
call opts#letg_default('todo_slide_in_width',  65)
call opts#letg_default('todo_file',            expand('~/Documents/Notes/todo.md'))
call opts#letg_default('netrw_slide_in_width', 25)
call opts#letg_default('session_dir',          expand('~/').'.vim/sessions')
call opts#letg_default('default_session_file', join([g:session_dir, expand('$USER').'.vim'], '/'))
call opts#letg_default('view_dir',             expand('~/').'.vim/views')
call opts#letg_default('default_view_file',    join([g:view_dir, expand('$USER').'.vim'], '/'))

fu! utils#toggle_netrw() 
    if expand('%:t') =~# '^Netrw'
        wincmd c
    else
        windo if expand('%:t') =~# 'Netrw' | wincmd c | endif
        exe 'botright '.g:netrw_slide_in_width.'Vexplore'
        wincmd H
        exe 'vert resize '.g:netrw_slide_in_width
    endif
endf

fu! utils#slider_toggle(file, position, size) 
    if expand('%:p') == fnamemodify(a:file, ':p')
        wincmd c
    else
        windo if expand('%:p') == fnamemodify(a:file, ':p') | wincmd c | endif

        exe 'botright split '.a:file

        if a:position =~? '^l'
            wincmd H
            exe 'vert res '.a:size
        elseif a:position =~? '^r'
            wincmd L
            exe 'vert res '.a:size
        elseif a:position =~? '^t'
            wincmd K
            exe 'res '.a:size
        else 
            wincmd J
            exe 'res '.a:size
        endif
    endif
endf

fu! utils#toggle_todo() 
    return utils#slider_toggle(g:todo_file, 'r', g:todo_slide_in_width)
endf

fu! utils#make_missing_dirs(dir_list)
    for l:dir in filter(map(deepcopy(a:dir_list), 'expand(v:val)'), '!isdirectory(v:val)')
        call mkdir(l:dir, 'p')
    endfor
endf

fu! utils#cpu_cores()
    let l:try_get = systemlist('grep -c ^processor /proc/cpuinfo')
    if v:shell_error 
        return 1
    else
        return l:try_get[0]
    endif
endf

fu! utils#project_files_qf()

    let l:root = utils#proj_root(['.git'])
    let l:ext = expand('%:e')

    " list all files with the same extension 
    if !empty(l:ext)
        silent cgete systemlist('find -readable -type f -maxdepth 7 -name '.string('*.'.l:ext).' 2>/dev/null')
        copen
        wincmd J
    else
        echoerr "no extension - no way to collect similar files"
    endif
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

fu! utils#buffer_wipeout()
    for l:b in range(1, bufnr('$'), 1)
        let l:info = getbufinfo(l:b)
        if !empty(l:info)
            silent call utils#_buffer_wipeout_helper(l:info['name']['name'], l:b)
        endif
    endfor
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

" Add to args all project files with the same extension starting " from the project root
fu! utils#add_project_files(anchors)

    " start depth *NEEDS* to be 1
    let l:lvl = 1
    let l:root = utils#proj_root(a:anchors)
    let l:ext = expand('%:e')
    let l:max_depth = 6
    " you always start with some loaded files
    let l:files = argv()
    " count collected files
    let l:i = len(l:files)

    while (l:lvl < l:max_depth) && (l:i < 50)
        let l:query = l:root.'/'.join(utils#str_to_list(repeat("*", l:lvl)), '/').'.'.l:ext
        let l:results = split(expand(l:query), '\n')
        " slices are safe even if the list is empty
        if len(l:results[:10]) <= 1
            break
        else
            let i += len(l:results)
            call extend(l:files, l:results)
        endif
    endwhile

    exe 'argadd '.join(l:files, ' ') 
endf

fu! utils#append_to_dict(word)
    if filereadable(&dictionary) 
        call writefile(split(a:word), expand(&dictionary), 'a') 
        echom 'appended '.string(a:word).' to '.string(&dictionary)
    else 
        echoerr 'dictionary not set'
    endif
endf

fu! utils#async_run(cmd)
    let l:expanded = join(map(split(a:cmd), 'substitute(expand(v:val), "\n", " ", "g")'))
    silent call system("bash -c '".shellescape(l:expanded)."' &")
    echom string(l:expanded).' spawned in the background'
endf

fu! utils#proj_root(anchors)

    let l:this_dir = expand('%:p:h')

    for l:anchor in a:anchors
        let l:candidate = finddir(l:anchor, l:this_dir.';'.$HOME)
        if !empty(l:candidate) && isdirectory(l:candidate) && (l:candidate != $HOME) && (l:candidate =~# $HOME)
            let l:result = fnamemodify(l:candidate, ':h')
            return l:result 
        else
            let l:candidate_file = findfile(l:anchor, l:this_dir.';'.$HOME)
            if !empty(l:candidate_file) && filereadable(l:candidate_file) && (l:candidate_file =~# $HOME)
                let l:result = fnamemodify(l:candidate_file, ':h')
                return l:result
            endif
        endif
    endfor

    return l:this_dir
endf

fu! utils#hl_word()
    let @/ = ''
    if exists('#auto_highlight')
        au! auto_highlight
        aug! auto_highlight
        setl updatetime=4000
        echo 'Highlight current word: OFF'
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
endf

fu! utils#is_stale(file, min_limit)
    return !filereadable(a:file) || ((localtime() - getftime(a:file)) > (60 * a:min_limit))
endf

fu! utils#close_dup_tabs() 
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
    endw
    call reverse(dups)
    for tb in dups
        exe "tabclose ".tb
    endfor
endf

fu! utils#colorize(group, what) 
    setg redrawtime=200
    for i in range(1, 255) 
        echom "Color nr ".i 
        exe 'hi '.a:group.' '.a:what.'='.i 
        redraw 
        sleep 300m
    endfor 
    setg redrawtime=2000
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

fu! utils#reformat_buffer()
    let l:col = virtcol('.')
    let l:ln = line('.')
    if empty(&formatprg) || (&formatprg =~# '^fmt')
        normal gg=G
    elseif &equalprg != ""
        silent exec ("%!".&equalprg)
    else
        silent exec ("%!".&formatprg)
    endif
    exec ':'.l:ln
    exec 'normal '.l:col
endf
