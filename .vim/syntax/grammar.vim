runtime! syntax/regex.vim

sy match grammarNonTerm "\v<[a-z][a-z_]+>" containedin=grammarOpt
sy match grammarTerm "\v<[A-Z][A-Za-z]+>" 
sy match grammarBNFSymbol " ::= " 
sy match grammarBNFSymbol "\v \|( |$)" 
sy match grammarChar "\v'(.|\\.)'" 
sy region grammarStr start='"' end='"'   oneline keepend
sy region grammarRegex start='/' end='/' oneline contains=regexGroup,regexSet,regexQuant,regexAtom,regexEscape,regexOr keepend
sy region grammarOpt start='\[' end='\]' oneline keepend contains=grammarNonTerm,grammarTerm,grammarStr,grammarRegex,grammarChar,grammarOpt,grammarBNFSymbol,grammarStar,grammarPlus,grammarGroup
sy region grammarGroup start='(' end=')' oneline keepend contains=grammarNonTerm,grammarTerm,grammarStr,grammarRegex,grammarChar,grammarOpt,grammarBNFSymbol,grammarStar,grammarPlus,grammarGroup
sy match grammarStar '*' 
sy match grammarPlus '+' 
sy match grammarOpt  '?' 
sy region grammarComment start="\v(#|//) " end='$' oneline
hi link grammarChar Character
hi link grammarRegex PreProc
hi link grammarStr String
hi link grammarTerm Define
hi link grammarNonTerm Statement
hi link grammarBNFSymbol Delimiter
hi link grammarPlus Operator
hi link grammarOpt Operator
hi link grammarGroup Function
hi link grammarStar Operator
hi link grammarComment Comment
