sy match cOpAround "\v(\=\=|!\=|\<\=|\>\=|\&\&?|\<\<|\>\>|\=|!|:|\<|\>|\+|\*)"
sy match cBrace    "\v\{|\}"
sy match cBracket  "\v\[|\]"
sy match cFunct    "\v(\w+)\(@="

hi link cOpAround  Operator
hi link cFunct     Function
hi link cStructure Statement
hi link cBracket   Normal
hi link cBrace     Normal
