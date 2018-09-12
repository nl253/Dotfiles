sy match shOperator "\v( |>|^)(\=\~|\={1,2}|\&{2}|!\=)( |$|<)"
hi def link shOperator    Operator

hi link shConditional Conditional
hi link shDerefSimple PreProc
hi link shDerefVar    SpecialChar
hi link shLoop        Repeat
hi link shQuote       Quote
hi link shRedir       Special
hi link shTestOpr     SpecialChar

hi shCmdSubRegion   ctermfg=DarkCyan guifg=Cyan   
hi shCommandSub     ctermfg=None                 
hi shDerefVar       ctermfg=None                  cterm=None  
" hi shHereDoc        ctermfg=None                  cterm=None  
hi shRange          ctermfg=Blue                              
hi shSetList        ctermfg=Blue                  cterm=bold  
hi shTestOpr        ctermfg=blue                              
hi shVarAssign                                    cterm=bold  
hi shVariable       ctermfg=None     ctermbg=None cterm=bold  
hi zshPluginManager gui=bold                      cterm=bold  

" runtime! syntax/python.vim
" sy region shPython start='\v^\s*python(\d+(\.\d+)?)?\s*\<{2}\s*EOF' end='\v^EOF' contains=pythonAsync,pythonAttribute,pythonBoolean,pythonBraces,pythonBrackets,pythonBuiltin,pythonColon,pythonComma,pythonComment,pythonComparison,pythonConditional,pythonDecorator,pythonDecoratorName,pythonDoctest,pythonDoctestValue,pythonDot,pythonError,pythonEscape,pythonException,pythonExceptions,pythonFunct,pythonFunctSignature,pythonFunction,pythonInclude,pythonKeyword,pythonKwArg,pythonMatMul,pythonMatrixMultiply,pythonNum,pythonNumber,pythonOp,pythonOperator,pythonQuotes,pythonRawString,pythonRepeat,pythonSelf,pythonSpaceError,pythonStatement,pythonStr,pythonString,pythonSync,pythonSyntaxNoise,pythonTodo,pythonTripleQuotes,pythonType
