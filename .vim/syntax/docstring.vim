sy region docStringMetaTag     start='\V* @\v[a-z]+>'ms=s+2     end='$'  contained keepend oneline 
sy region docStringType        start=/{/                        end=/}/  contained keepend oneline containedin=docStringMetaTag 
sy region docStringParam       start='\v (([a-z][ A-Za-z]*)|$)' end='$'  contained keepend oneline containedin=docStringMetaTag 
sy match  docStringDescr       '\v [A-Z][a-z]+ [[:alpha:],:;]{10,100}\.' contained keepend
sy match  docStringBulletPoint '\V + \v<'                                contained keepend

hi link docStringBulletPoint Delimiter
hi link docStringDescr       Define
hi link docStringMetaTag     PreProc
hi link docStringParam       Identifier
hi link docStringType        Type
