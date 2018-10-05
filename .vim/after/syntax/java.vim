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
hi link javaParen1          Delimiter
hi link javaParen2          Quote
hi link javaParen3          Macro
hi link javaOperator        Operator
hi link javaPunct           Operator
hi link javaDocTag          PreProc
hi link javaBrace           Operator
hi link javaScopeResolution Operator
hi link javaChevrons        Operator
hi link javaStickyOp        Operator
hi link javaParen           Delimiter
hi link javaLocalVar        Type
