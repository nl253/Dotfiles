sy match cppOpAround "\v(::|\~|\=\=|!\=|\<\=|\>\=|\&\&?|\<\<|\>\>|\=|!|:|\<|\>|\+|\*)"
sy match cppClass    "\v[A-Z][a-z]+"

hi link cppOpAround Operator
hi link cppClass    Identifier
