sy match cOpAround "\v(\=\=|!\=|\<\=|\>\=|\&\&?|\<\<|\>\>|\=|!|:|\<|\>|\+|\*)"
sy match cBrace    "\v\{|\}"
sy match cBracket  "\v\[|\]"
sy match cFunct    "\v(\w+)\(@="

hi def link cOpAround  Operator
hi def link cFunct     Function
hi def link cStructure Statement
hi def link cBracket   Delimiter
hi def link cBrace     Delimiter
