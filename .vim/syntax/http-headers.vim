if exists('b:current_syntax') 
    if b:current_syntax == 'http-headers'
        finish
    elseif exists('b:http_headers_syntax_loaded ')
        finish
    else
        let b:http_headers_syntax_loaded = 1
    endif
else
    let b:current_syntax = 'http-headers' 
endif

runtime! syntax/json.vim
runtime! syntax/html.vim

sy match httpHeaderKey '\v^[A-Z][^:]+'
sy match httpHeaderVal '\v:[^:]+$'
sy region httpHeaderComment start='^\s*(#|//)' end="$" oneline 
hi def link httpHeaderKey Statement
hi def link httpHeaderVal Normal
hi def link httpHeaderComment Comment
