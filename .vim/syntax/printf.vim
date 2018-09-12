" Syntax for printf(3) (to be used inside of strings)
" ---------------------------------------------------
sy region printf start="%" end="\v[EFGXacdefgiorsux]" contained nextgroup=printfFlags    keepend oneline contains=printfFlags,printfMinWidth,printfPrec,printfLen 
sy match  printfFlags "\v[-+ 0#]{,5}"                 contained nextgroup=printfMinWidth keepend            
sy match  printfMinWidth "\v(\.\d+)?"                 contained nextgroup=printfPrec     keepend
sy match  printfPrec "\v([1-9]\d*|\*)?"               contained nextgroup=printfLen      keepend
sy match  printfLen "\v([1-9]\d*)?"                   contained                          keepend                             

hi def link printf         SpecialChar
hi def link printfConvType Statement
hi def link printfFlags    SpecialChar
hi def link printfLen      Number
hi def link printfMinWidth Number
hi def link printfPrec     Number
