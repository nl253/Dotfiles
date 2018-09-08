" Syntax for C-Style docstrings 
" -----------------------------
sy region docstringMetaTag     start='\V* @\v[a-z]+>'ms=s+2     end='$'  contained keepend oneline 
sy region docstringType        start=/{/                        end=/}/  contained keepend oneline containedin=docstringMetaTag 
sy region docstringParam       start='\v (([a-z][ A-Za-z]*)|$)' end='$'  contained keepend oneline containedin=docstringMetaTag 
sy match  docstringDescr       '\v [A-Z][a-z]+ [[:alpha:],:;]{10,100}\.' contained keepend
sy match  docstringBulletPoint '\V + \v<'                                contained keepend

sy cluster docstringAll contains=docstringType,docstringParam,docstringBulletPoint,docstringMetaTag,docstringDescr

hi def link docstringBulletPoint Delimiter
hi def link docstringDescr       Define
hi def link docstringMetaTag     PreProc
hi def link docstringParam       Identifier
hi def link docstringType        Type
