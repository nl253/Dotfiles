fu! opts#letg_default(var_name, val)
    if !exists('g:'.a:var_name)
        exe 'let g:'.a:var_name.' = '.string(a:val)
    endif
endf

fu! opts#omni(omnilist)
    for l:omni in a:omnilist
        if exists('*'.l:omni)
            call opts#safe_setl(['omnifunc='.l:omni])
            break
        endif
    endfor
endf

fu! opts#comma_opt(opt_name, extra_flags)
    let l:sorted_present_flags = sort(split(execute("echo &".a:opt_name), ','))
    for l:flag in a:extra_flags
        if utils#bsearch(l:sorted_present_flags, l:flag) ==# v:false
            call opts#safe_setl([a:opt_name.'+='.l:flag])
        endif
    endfor
endf

fu! opts#letter_opt(opt_name, extra_flags)
    let l:present_flags = utils#str_to_list(eval("&".a:opt_name))
    for l:flag in a:extra_flags
        if index(l:present_flags, l:flag) < 0
            call opts#safe_setl([a:opt_name.'+='.l:flag])
        endif
    endfor
endf

fu! opts#letter_opt_unset(opt_name, extra_flags)
    let l:present_flags = eval("&".a:opt_name)
    for l:flag in l:present_flags
        if index(l:present_flags, l:flag) >= 0
            call opts#safe_setl([a:opt_name.'-='.l:flag])
        endif
    endfor
endf

" @param {[string]} vargs
fu! opts#letg_all(vars)
    for i in filter(a:vars, '!exists("g:".v:val)')
        exec 'let g:'.i.' = 1'
    endfor
endf

" Add to $PATH bin dirs for package managers in case they aren't in $PATH already
fu! opts#append_to_path(paths) abort
    let l:sorted_path = sort(map(filter(split($PATH, ':'), '!empty(v:val)'), 'expand(v:val)'))
    for l:dir in filter(a:paths, 'isdirectory(v:val)')
        if utils#bsearch(l:sorted_path, l:dir) ==# v:false
            let l:sorted_path = call add(l:sorted_path, l:dir)
        endif
    endfor
    let $PATH = join(l:sorted_path, ':') 
endf

fu! opts#safe_setl(opts)
    call opts#safe_set(a:opts, 0)
endf

fu! opts#safe_setg(opts)
    call opts#safe_set(a:opts, 1)
endf

fu! opts#safe_set(opts, is_global)
    let l:cmd = a:is_global ? 'setg' : 'setl'
    for l:option in a:opts
        try
            sil! exe l:cmd.' '.escape(l:option, ' %')
        catch /\vE(518)/
            sil call add(v:errors, 'could not set option '.string(l:option))
        endtry
    endfor
endf

fu! opts#wildignore(pattern_list)
    if has('wildignore')
        let l:sorted_existing_patterns = sort(split(&wildignore, ','))
        for l:pattern in a:pattern_list 
            if utils#bsearch(l:sorted_existing_patterns, l:pattern) ==# v:false
                exe 'setg wildignore+='.l:pattern
            endif
        endfor
    endif
endf

" { string binary : string cmd to run }
fu! opts#formatprg(cfg)
    echo opts#set_if_executable('formatprg', a:cfg, 0) 
endf

" Accepts a dict where
"
"   Key: 
"       type: string 
"       descr: binary              
"
"   Value: 
"       type: string
"       descr: cmd to run if binary is executable
"
fu! opts#makeprg(cfg)
    echo opts#set_if_executable('makeprg', a:cfg, 0) 
endf

" Accepts:
"
"   Option: (opt)
"       type: string  
"       descr: option name
"
"   Configuration: (cfg)
"       type: dict where

"           Key: 
"               type: string 
"               descr: binary              
"
"           Value: 
"               type: string
"               descr: cmd to run if binary is executable
"
"   Global: (global)
"       type: bool
"       descr: setg or setl              
"
fu! opts#set_if_executable(option, cfg, global) 
    for l:bin in keys(a:cfg)
        if executable(l:bin)
            if a:global
                call opts#safe_setg([a:option.'='.a:cfg[l:bin]])
            else
                call opts#safe_setl([a:option.'='.a:cfg[l:bin]])
            endif
            break
        endif
    endfor
endf
