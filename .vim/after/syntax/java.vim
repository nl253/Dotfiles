sy match  javaClassName "\v<([A-Z][a-z0-9]+)+>"
sy match  javaOperator  "\v (-\>|\|\||\&\&|\?|:|\%\=?|-\=|\+\=|\*\=?|/\=|!\=|\=\=|\<\=?|\>\=?|<|>|instanceof|\=|\+|-|/) "
sy match  javaPunct     "\v;|, |\(|\)|\." 
sy match  javaBrace     "\v\}|\{" 
sy match  javaChevrons  "\v\<|\>"
sy match  javaStickyOp  "\v!|\+\+|--"
sy match  javaLocalVar  "\v<var>"
sy region javaDocTag start="@" end="$" contained containedin=javaComment
sy match  javaScopeResolution "::"

hi javaClassName gui=bold cterm=bold term=bold
hi def link javaParen1          Delimiter
hi def link javaParen2          Quote
hi def link javaParen3          Macro
hi def link javaOperator        Operator
hi def link javaPunct           Comment
hi def link javaDocTag          PreProc
hi def link javaBrace           Comment
hi def link javaScopeResolution Operator
hi def link javaChevrons        Comment
hi def link javaStickyOp        Operator
hi def link javaParen           Delimiter
hi def link javaLocalVar        Type
