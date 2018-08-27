fu! opts#letg_default(var_name, val)
    if !exists('g:'.a:var_name)
        exe 'let g:'.a:var_name.' = '.string(a:val)
    endif
endf

fu! opts#omni(omnilist)
    for l:omni in a:omnilist
        if exists('*'.l:omni)
            exe 'setl omnifunc='.l:omni
        endif
    endfor
endf

fu! opts#comma_opt(opt_name, extra_flags)
    let l:present_flags = split(execute("echo &".a:opt_name), ',')
    for l:flag in a:extra_flags
        if index(l:present_flags, l:flag) < 0
            exe 'setl '.a:opt_name.'+='.l:flag
        endif
    endfor
endf

fu! opts#letter_opt(opt_name, extra_flags)
    let l:present_flags = utils#str_to_list(eval("&".a:opt_name))
    for l:flag in a:extra_flags
        if index(l:present_flags, l:flag) < 0
            exe 'setl '.a:opt_name.'+='.l:flag
        endif
    endfor
endf

fu! opts#letter_opt_unset(opt_name, extra_flags)
    let l:present_flags = eval("&".a:opt_name)
    for l:flag in l:present_flags
        if index(l:present_flags, l:flag) >= 0
            exe 'setl '.a:opt_name.'-='.l:flag
        endif
    endfor
endf

" @param {[string]} vargs
fu! opts#letg_all(vars)
    for i in filter(a:vars, '!exists("g:".v:val)')
        exec 'let g:'.i.' = 1'
    endfor
endf

fu! opts#grepprg()
	if !executable('rg') 
		let l:git_root = systemlist('git rev-parse --show-toplevel') 
		if v:shell_error || !isdirectory(l:git_root[0])
			exe 'setl grepprg='.escape('grep -n -r $*', ' ') 
        elseif isdirectory(l:git_root[0])
			exe 'setl grepprg='.escape('git grep -n -r $*', ' ') 
		endif
	endif
endf

" Add to $PATH bin dirs for package managers in case they aren't in $PATH already
fu! opts#append_to_path(paths)
    let l:path = map(filter(split($PATH, ':'), 'empty(v:val)'), 'expand(v:val)')
    for l:dir in filter(a:paths, 'isdirectory(v:val)')
        if index(l:path, l:dir) < 0
            let $PATH = l:dir.':'.$PATH
        endif
    endfor
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
            silent! exec l:cmd.' '.escape(l:option, ' ')
        catch /\vE(518)/
            silent call add(v:errors, 'could not set option '.string(l:option))
        endtry
    endfor
endf

fu! opts#wildignore(pattern_list)
    if has('wildignore')
        let l:existing_patterns = split(&wildignore, ',')
        for l:pattern in a:pattern_list 
            if index(l:existing_patterns, l:pattern) < 0
                exe 'setg wildignore+='.l:pattern
            endif
        endfor
    endif
endf

" Intialise global dictionary.
" To be triggered by VimEnter.
fu! opts#dict()
    let l:dict_dir = expand('~/.vim/dicts/')
    let l:dict = l:dict_dir.'frequent.dict'
    if filereadable(l:dict)
        exec 'setg dictionary='.l:dict
        call opts#comma_opt('complete', ['k'])
    endif
endf

" Looks in ~/.vim/dicts for a matching dict. 
" If it finds one thent it sets locally dict to it.
fu! opts#dict_local()
    let l:dict_dir = expand('~/.vim/dicts/')
    let l:candidate = l:dict_dir.&filetype.'.dict'

    if filereadable(l:candidate)
        exec 'setl dictionary='.l:candidate
        call opts#comma_opt('complete', ['k'])
    else
        let l:candidate = l:dict_dir.expand('%:e').'.dict'
        if filereadable(l:candidate)
            exec 'setl dictionary='.l:candidate
            call opts#comma_opt('complete', ['k'])
        endif
    endif
endf

" Set global thesaurus
fu! opts#thesaurus()
    let l:dict_dir = expand('~/.vim/dicts/')
    let l:thesaurus = l:dict_dir.'thesaurus.dict'
    if empty(&thesaurus) && filereadable(l:thesaurus)
        exec 'setg thesaurus='.l:thesaurus
    endif
endf

" Intialise &complete for all filetypes
fu! opts#completion()
    if empty(&omnifunc) && exists('+omnifunc')
        setl omnifunc=syntaxcomplete#Complete
    endif

    if empty(&completefunc) && exists('+completefunc')
        setl completefunc=syntaxcomplete#Complete
    endif

    call opts#comma_opt('complete', ['.', 'k', 't'])

    let l:ext = expand('%:e')
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
                exe 'setg '.a:option.'='.escape(a:cfg[l:bin], ' ')
            else
                exe 'setl '.a:option.'='.escape(a:cfg[l:bin], ' ')
            endif
            break
        endif
    endfor
endf
