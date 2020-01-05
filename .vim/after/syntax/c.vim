sy match cOpAround "\v(\=\=|!\=|\<\=|\>\=|\&\&?|\<\<|\>\>|\=|!|:|\<|\>|\+|\*)"
sy match cBrace   "\v\{|\}"
sy match cBracket "\v\[|\]"
sy match cFunct   "\v([_a-zA-Z]+[_0-9a-zA-Z]*)\(@="

hi link cOpAround  Operator
hi link cFunct     Function
hi link cStructure Statement
hi link cBracket   Normal
hi link cBrace     Normal
