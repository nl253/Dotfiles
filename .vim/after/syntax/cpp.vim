sy match cppOpAround "\v(::|\~|\=\=|!\=|\<\=|\>\=|\&\&?|\<\<|\>\>|\=|!|:|\<|\>|\+|\*)"
hi link cppOpAround Operator
sy match cppClass "\v[A-Z][a-z]+"
hi link cppClass Identifier
