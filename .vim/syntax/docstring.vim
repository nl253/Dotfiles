" Syntax for C-Style docstrings 
" -----------------------------
" NOTE the order here is crucial
sy keyword docstringTODO        TODO FIXME XXX

sy region docstringMetaTag      start='\V* @\v[a-z]{2,}'ms=s+2 end='\v |>|<|$' oneline contained keepend nextgroup=docstringType,docstringTxt

sy match docstringTxt '\v[^\*\n\r]*' contained keepend contains=docstringDelim

sy region docstringType         start=/\v\{+/ end=/\v\}+/  matchgroup=Delimiter contained keepend oneline contains=docstringTypeName,docstringDelim nextgroup=docstringTxt
sy match  docstringDelim        '\v[\=\?!\}\{\|\>\<\.\]\[]' contained keepend
sy match  docstringTypeName     '\v\w+|\*'         contained keepend

sy cluster docstringAll contains=docstringMetaTag,docstringType,docstringTODO,@Spell

hi def link docstringMetaTag  PreProc
hi def link docstringTxt      Identifier
hi def link docstringDelim    Delimiter
hi def link docstringTypeName Type
hi def link docstringTODO     WarningMsg
" hi def link docstringTypeName    Type
