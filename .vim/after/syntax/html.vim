hi clear htmlLink 
hi htmlH4 guifg=#00af5f ctermfg=35 gui=bold cterm=bold term=bold
" sy region htmlStyleTagPair start="[-a-z]{4,}\s*:\s*" end='\v[;"]' contained
" sy region htmlStyleTag start='\v<style\="' end='"' contained con oneline contains=htmlStyleTagPair

hi def link htmlStatement Statement
hi def link htmlTagName   Statement
hi def link htmlLink      URI
hi def link htmlH1        Statement
hi def link htmlH2        Boolean
hi def link htmlH3        Conditional
hi def link htmlH5        Function
hi def link htmlH6        Macro
hi def link htmlArg       Define
hi def link htmlEndTag    Delimiter
hi def link htmlTag       Delimiter
