hi link javaParen1 Delimiter
hi link javaParen2 Quote
hi link javaParen3 Macro
syn match javaClassName "\v<([A-Z][a-z0-9]+)+>"
hi javaClassName gui=bold cterm=bold term=bold
syn match javaOperator "\v (-\>|\|\||\&\&|\?|:|\%\=?|-\=|\+\=|\*\=?|/\=|!\=|\=\=|\<\=?|\>\=?|<|>|instanceof|\=|\+|-|/) "
hi link javaOperator Operator
syn match javaPunct "\v;|, |\(|\)|\." 
hi link javaPunct Comment
hi link javaDocTag PreProc
syn region javaDocTag start="@" end="$" contained containedin=javaComment
hi link  javaBrace Comment
syn match javaBrace "\v\}|\{" 
hi link javaScopeResolution Operator
syn match javaScopeResolution "::"
syn match javaChevrons "\v\<|\>"
hi link javaChevrons Comment
syn match javaStickyOp "\v!|\+\+|--"
hi link javaStickyOp Operator
hi link javaParen Delimiter
sy match javaLocalVar "\v<var>"
hi link javaLocalVar Type
