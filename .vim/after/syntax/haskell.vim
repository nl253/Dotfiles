hi link haskellDeclKeyword TypeDef
hi link haskellDecl TypeDef
hi link haskellLet TypeDef
hi link haskellConditional Statement
hi link haskellWhere Statement
hi link hsStructure Statement
hi link Typedef TypeDef
hi link haskellKeyword Keyword

let s:prelude_functions = [
			\ "abs", 
			\ "acos", 
			\ "acosh", 
			\ "all", 
			\ "and", 
			\ "any", 
			\ "appendFile", 
			\ "asin",
			\ "asinh", 
			\ "asTypeOf", 
			\ "atan", 
			\ "atan2", 
			\ "atanh", 
			\ "break", 
			\ "ceiling", 
			\ "compare", 
			\ "concat", 
			\ "concatMap", 
			\ "const", 
			\ "cos", 
			\ "cosh", 
			\ "curry", 
			\ "cycle", 
			\ "decodeFloat", 
			\ "div", 
			\ "divMod", 
			\ "drop", 
			\ "dropWhile", 
			\ "either", 
			\ "elem", 
			\ "encodeFloat", 
			\ "enumFrom", 
			\ "enumFromThen", 
			\ "enumFromThenTo", 
			\ "enumFromTo", 
			\ "error", 
			\ "errorWithoutStackTrace", 
			\ "even", 
			\ "exp", 
			\ "exponent", 
			\ "fail", 
			\ "filter", 
			\ "flip", 
			\ "floatDigits", 
			\ "floatRadix", 
			\ "floatRange", 
			\ "floor", 
			\ "fmap", 
			\ "foldl", 
			\ "foldl1", 
			\ "foldMap", 
			\ "foldr", 
			\ "foldr1", 
			\ "fromEnum", 
			\ "fromInteger", 
			\ "fromIntegral", 
			\ "fromRational", 
			\ "fst", 
			\ "gcd", 
			\ "getChar", 
			\ "getContents", 
			\ "getLine", 
			\ "head", 
			\ "id", 
			\ "init", 
			\ "interact", 
			\ "ioError", 
			\ "isDenormalized", 
			\ "isIEEE", 
			\ "isInfinite", 
			\ "isNaN", 
			\ "isNegativeZero", 
			\ "iterate", 
			\ "last", 
			\ "lcm", 
			\ "length", 
			\ "lex", 
			\ "lines", 
			\ "log", 
			\ "logBase", 
			\ "lookup", 
			\ "map", 
			\ "mapM", 
			\ "mapM_", 
			\ "mappend", 
			\ "max", 
			\ "maxBound", 
			\ "maximum", 
			\ "maybe", 
			\ "mconcat", 
			\ "mempty", 
			\ "min", 
			\ "minBound", 
			\ "minimum", 
			\ "mod", 
			\ "negate", 
			\ "not", 
			\ "notElem", 
			\ "null", 
			\ "odd", 
			\ "or", 
			\ "otherwise", 
			\ "pi", 
			\ "pred", 
			\ "print", 
			\ "product", 
			\ "properFraction", 
			\ "pure", 
			\ "putChar", 
			\ "putStr", 
			\ "putStrLn", 
			\ "quot", 
			\ "quotRem", 
			\ "read", 
			\ "readFile", 
			\ "readIO", 
			\ "readList", 
			\ "readLn", 
			\ "readParen", 
			\ "reads", 
			\ "readsPrec", 
			\ "realToFrac", 
			\ "recip", 
			\ "rem", 
			\ "repeat", 
			\ "replicate", 
			\ "return", 
			\ "reverse", 
			\ "round", 
			\ "scaleFloat", 
			\ "scanl", 
			\ "scanl1", 
			\ "scanr", 
			\ "scanr1", 
			\ "seq", 
			\ "sequence", 
			\ "sequence_", 
			\ "sequenceA", 
			\ "show", 
			\ "showChar", 
			\ "showList", 
			\ "showParen", 
			\ "shows", 
			\ "showsPrec", 
			\ "showString", 
			\ "significand", 
			\ "signum", 
			\ "sin", 
			\ "sinh", 
			\ "snd", 
			\ "span", 
			\ "splitAt", 
			\ "sqrt", 
			\ "subtract", 
			\ "succ", 
			\ "sum", 
			\ "tail", 
			\ "take", 
			\ "takeWhile", 
			\ "tan", 
			\ "tanh", 
			\ "toEnum", 
			\ "toInteger", 
			\ "toRational", 
			\ "traverse", 
			\ "truncate", 
			\ "uncurry", 
			\ "undefined", 
			\ "unlines", 
			\ "until", 
			\ "unwords", 
			\ "unzip", 
			\ "unzip3", 
			\ "userError", 
			\ "words", 
			\ "writeFile", 
			\ "zip", 
			\ "zip3", 
			\ "zipWith", 
			\ "zipWith3", 
			\ ]

exe 'syn keyword haskellPreludeFunction '.join(s:prelude_functions, ' ')
hi link haskellPreludeFunction Builtin
hi clear ConId
hi link ConId Type

let s:comment_rules = "haddockDocMultiLine,hsLineComment,hsBlockComment"
let s:code_rules = "hsVarSym,hsBoolean,hsDelimiter,ConId,haskellPreludeFunction,hsStructure"

exe 'syn keyword haskellTODO TODO FIXME contained containedin='.s:comment_rules
hi link haskellTODO Exception 

exe 'syn region haddockRepl start="\v \>{3} " end="$" containedin='.s:comment_rules.' contained oneline contains='.s:code_rules
syn match haddockReplMarker ">>>" contained containedin=haddockRepl
hi link haddockReplMarker Delimiter

exe 'syn region haddockCodeLine start="\v^(-- )?\s*\> " end="$" contained oneline contains='.s:code_rules.' oneline containedin='.s:comment_rules
hi link haddockCodeLine Normal

exe 'syn region haddockProp start=" prop>" end="$" contained containedin='s:comment_rules
hi link haddockProp PreCondit

syn match hsDashIgnore "--" contained containedin=haddockDocMultiLine
hi link hsDashIgnore Comment

syn region haddockBulletPoint start="\v^\s*(-- )?\s*(\d\.|\*|\(\d\)) " end="$" containedin=haddockDocMultiLine,haddockDocLine,hsLineComment contained oneline contains=haddockMonospaced,haddockEmpf,haddockBold,haddockRepl,haddockProp,haddockURI,haddockAnchor,haddockChar,hsDashIgnore
hi link haddockBulletPoint Delimiter

hi link haddockURI URI
exe 'syn region haddockURI start="<" end=">" containedin='.s:comment_rules.' oneline concealends contained'
hi link haddockURI Comment
syn match haddockURIInner "\v[^\<\>]*" contained containedin=haddockURI
hi link haddockURIInner URI

for i in range(1, 6)
	exe 'syn region haddockH'.i.' start="\v(\s*-- )\V'.repeat("=", i).' " end="$" containedin=haddockDocMultiLine,haddockDocLine,hsLineComment oneline contained contains=haddockBold,haddockEmpf,haddockMonospaced,hsDashIgnore'
	exe 'hi link haddockH'.i.' HtmlH'.i
endfor

exe 'syn match haddocHR "\v^\s*\~{5,}\s*$" contained containedin='.s:comment_rules
hi link haddocHR Special

exe 'syn region haddocNote start="\v(See )?Note:? ?\[" end="]" contained containedin='.s:comment_rules
hi link haddocNote Function

exe 'syn region haddockBold start="__" end="__" containedin='.s:comment_rules.' contained oneline contains=haddockEmpf,haddockMonospaced'
hi haddockBold gui=bold cterm=bold term=bold

exe 'syn match haddockDollarWord "\v( )@<=(\$[A-Za-z]+)" contained containedin='.s:comment_rules
hi link haddockDollarWord haddockBold

exe 'syn region haddockEmpf start="/\v<" end="\v>/" containedin='.s:comment_rules.' contained oneline contains=haddockMonospaced,haddockBold'
hi haddockEmpf gui=italic cterm=italic term=italic

exe 'syn region haddockMonospaced start="@" end="@" containedin='.s:comment_rules.' contained oneline contains='.s:code_rules
hi link haddockMonospaced Normal

syn match haddockMonospacedMarker "\v\@" contained containedin=haddockMonospaced
hi link haddockMonospacedMarker Comment

exe 'syn match haddockAnchor "\v\w+#\w+|#\w+#" contained contained containedin='.s:comment_rules
hi link haddockAnchor URI

exe 'syn match haddockChar "&#\v\S+" contained containedin='.s:comment_rules
hi link haddockChar Character

syn region haddockDefinitionMarker start="\v\[" end="\]" containedin=haddockDocMultiLine,haddockDocLine,hsLineComment,hsBlockComment contained concealends contains=haddockBold,haddockEmpf,haddockMonospaced
hi link haddockDefinitionMarker Delimiter

exe 'syn region haddockSinceTag start="@since\v ?" end="\v |$" contains=haddockSemVer contained containedin='.s:comment_rules
hi link haddockSinceTag Macro

syn match haddockSemVer "\v\d+(\.\d+)*" contained containedin=haddockSinceTag
hi link haddockSemVer Number

syn region haddocQuoted start="\v'<" end="\v>'" oneline contained containedin=hsComment,hsBlockComment,hsLineComment,haddockDocLine,haddockDocMultiline
hi link haddocQuoted Macro


" vim:foldmethod=indent:
