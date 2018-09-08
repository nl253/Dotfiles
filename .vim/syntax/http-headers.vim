if exists('b:current_syntax') 
    if b:current_syntax ==# 'http-headers'
        finish
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
