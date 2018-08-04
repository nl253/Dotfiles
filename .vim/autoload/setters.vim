"  { string binary : string cmd to run }
fu! setters#formatprg(cfg)
    echo setters#set_if_executable('formatprg', a:cfg, 0) 
endf

"  { string binary : string cmd to run }
fu! setters#makeprg(cfg)
    echo setters#set_if_executable('makeprg', a:cfg, 0) 
endf

"                             string  { string : string } bool
fu! setters#set_if_executable(option, cfg, global) 
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
