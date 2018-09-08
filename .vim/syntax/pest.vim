if exists('b:current_syntax') 
    if b:current_syntax ==# 'pest'
        finish
    endif
else
    let b:current_syntax = 'pest' 
endif


" rule = ...
sy match pestRule /\v[a-z_]{2,}/

" ANY
" ASCII
" ASCII_ALPHA
" ASCII_ALPHANUMERIC
" ASCII_ALPHA_LOWER
" ASCII_ALPHA_UPPER
" ASCII_BIN_DIGIT
" ASCII_DIGIT
" ASCII_HEX_DIGIT
" ASCII_NONZERO_DIGIT
" ASCII_OCT_DIGIT
" COMMENT
" EOI
" NEWLINE
" PEEK
" PEEK_ALL
" POP
" POP_ALL
" SOI
" WHITESPACE
sy match  pestSpecialRule /\v[A-Z_]{2,}/ contained

" rule = { ... }
sy region pestBrace  start=/{/ end=/}/ contains=pestRule,pestStr,pestOp,pestRange,pestSpecialRule

" "str"
sy region pestStr start=/"/ end=/"/ skip=/\\"/ oneline contained contains=pestEscape

" // comment
sy region pestComment start="//" end="$" oneline

" 'a'..'z'
" 'A'..'Z'
" '0'..'9'
sy match pestRange /\v'[A-Za-z0-9]'\.{2}'\w'/ contained

sy match pestEscape /\\\v./ contained

" a ~ b
sy match pestOp /\~/                     contained

" a*
sy match pestOp /\*/                     contained

" a?
sy match pestOp /?/                      contained

" a | b
sy match pestOp /|/                      contained
" a+

sy match pestOp /+/                      contained

" _{ a ~ b}
sy match pestOp /_/                      

" &a
sy match pestOp /&/                      contained

" @{ a ~ b}
sy match pestOp /@/                      

" !{ a ~ b}
sy match pestOp /!/                      

" a{2,3}
" a{,3}
" a{2,}
" a{2}
sy match pestOp '\v\{\d{,2}(,\d{,2})?\}' contained 

" ^ a 
" a $ 
sy match pestOp "\v\$|\^"                contained

hi def link pestRule        Statement
hi def link pestSpecialRule Special
hi def link pestStr         String
hi def link pestBrace       Delimiter
hi def link pestOp          Operator
hi def link pestRange       Character
hi def link pestComment     Comment
hi def link pestEscape      SpecialChar
