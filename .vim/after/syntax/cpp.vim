sy match cppOpAround "\v(::|\~|\=\=|!\=|\<\=|\>\=|\&\&?|\<\<|\>\>|\=|!|:|\<|\>|\+|\*)"
sy match cppClass "\v<[A-Z][a-z]+>"
sy match cFunct   "\v([_a-zA-Z]+[_0-9a-zA-Z]*)\(@="

hi link cppOpAround Operator
hi link cppClass Identifier
