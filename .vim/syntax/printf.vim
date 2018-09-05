" Syntax for printf(3) (to be used inside of strings)
" ---------------------------------------------------
if exists('b:printf_syntax_loaded ')
    finish
else
    let b:printf_syntax_loaded = 1
endif
sy region printf start="%" end="\v[EFGXacdefgiorsux]" contained nextgroup=printfFlags    contains=printfFlags,printfMinWidth,printfPrec,printfLen  
sy match  printfFlags "\v[-+ 0#]{,5}"                 contained nextgroup=printfMinWidth
sy match  printfMinWidth "\v(\.\d+)?"                 contained nextgroup=printfPrec
sy match  printfPrec "\v([1-9]\d*|\*)?"               contained nextgroup=printfLen
sy match  printfLen "\v([1-9]\d*)?"                   contained 

hi def link printf         SpecialChar
hi def link printfConvType Statement
hi def link printfFlags    SpecialChar
hi def link printfLen      Number
hi def link printfMinWidth Number
hi def link printfPrec     Number
