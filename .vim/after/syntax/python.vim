" CORE:
hi link pythonRepeat Repeat
hi link pythonBuiltin Builtin
hi link pythonConditional Conditional
hi link pythonFunction Function
hi link pythonInclude Special
hi pythonStatement guifg=gold ctermfg=220
hi link pythonKeyword pythonStatement

" ADDED:
" types (see the typing module)
syn match PythonType "\v[A-Z][a-z][a-zA-Z]+"
syn keyword pythonType deque BZ2File BZ2Compressor BZ2Decompressor ABCMeta ABC UDPServer  TCPServer TCPServer ForkingUDPServer ThreadingTCPServer ThreadingUDPServer HTTPMessage HTTPResponse
hi link pythonType Type
hi link pythonType Type
syn keyword pythonBuiltin abs dict help min setattr all dir hex next slice any divmod id object sorted ascii enumerate input oct staticmethod bin eval int open str bool exec isinstance ord sum bytearray filter issubclass pow super bytes float iter print tuple callable format len property type chr frozenset list range vars classmethod getattr locals repr zip compile globals map reversed complex hasattr max round delattr hash memoryview set
hi link pythonBuiltin PythonFunction
syn keyword pythonBoolean True False None
hi link pythonBoolean Special
" type annotations
hi pythonFunctSignature guifg=DeepSkyBlue2 ctermfg=LightBlue cterm=bold gui=bold
hi link pythonAsync Special
hi pythonExceptionClass guifg=maroon ctermfg=Brown
syn keyword pythonSelf self
hi pythonSelf guifg=LightSeaGreen ctermfg=DarkCyan cterm=bold gui=italic
" punctuation
syn match PythonColon ":" 
syn match PythonComma "," 
hi PythonColon guifg=grey ctermfg=darkgrey
hi link PythonComma PythonColon
syn match PythonDot "\v\."
hi link PythonDot PythonColon
syn match PythonKwArg "\v\w[[:alnum:]_]*\@=\="
hi PythonKwArg guifg=purple
syn keyword pythonKeyword with as pass

" DELIMITERS:
" syn match parenthesis "\v[\(\)]"
" hi parenthesis guifg=#0087d7 gui=bold cterm=bold term=bold ctermfg=32
syn match braces "\v[\{\}]"
syn match brackets "\v[\[\]]"

" OPERATORS:
sy match pythonOperator "\v\*\*"
sy match pythonOperator "\v (-|//?|\+|\V%\v) "
sy match pythonOperator "\v-<"
hi link pythonOperator Operator
" > < <= >= -= == += *= iff surrounded by spaces
syn match pythonComparison "\v (\=?[\*\=<>]|[-\*/\+]\=) "
hi link pythonComparison Operator
hi link pythonMatrixMultiply Operator

" OBJECTS AND CLASSES:
syn match pythonErrorClass "\v ?[A-Za-z]*Error[a-z]* ?"
syn match pythonExceptionClass "\v ?[A-Za-z]*Exception[a-z]* ?"
syn match pythonFunctSignature "->.*:"
hi link pythonErrorClass Exception
hi link pythonException TypeDef

" OTHER:
hi link pythonDoctest Operator
hi link pythonCoding Comment
