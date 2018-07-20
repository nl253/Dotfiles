" CORE:
hi link htmlStatement Statement
hi link htmlTagName Statement

" LINKS:
hi clear htmlLink 
hi link htmlLink URI

" HEADINGS:

hi link htmlH1 Statement
hi link htmlH2 Boolean
hi link htmlH3 Conditional
hi htmlH4 guifg=#00af5f ctermfg=35 gui=bold cterm=bold term=bold
hi link htmlH5 Function
hi link htmlH6 Macro

hi link htmlArg Define
hi link htmlEndTag Delimiter
hi link htmlTag Delimiter
