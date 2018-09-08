if exists('b:current_syntax') 
    if b:current_syntax ==# 'javascript'
        finish
    endif
else
    sy clear
    sy sync ccomment jsComment
    let b:current_syntax = 'javascript' 
endif

sy sync fromstart
sy sync minlines=120

runtime! syntax/regex.vim
runtime! syntax/docstring.vim

" mainly for syntax#complete
sy keyword jsArrayFunct  concat copyWithin every fill filter find findIndex includes indexOf join lastIndexOf map pop push reduce reduceRight reverse shift slice some sort splice unshift
sy keyword jsJSONStatics parse stringify
sy keyword jsMapFunct    clear get set
sy keyword jsMathStatics E LN10 LN2 LOG10E LOG2E PI SQRT1_2 SQRT2 abs acos acosh asin asinh atan atan2 atanh cbrt ceil clz32 cos cosh exp expm1 floor fround hypot imul log log10 log1p log2 max min pow random round sign sin sinh sqrt tan tanh trunc
sy keyword jsObjFunct    hasOwnProperty isPrototypeOf propertyIsEnumerable toLocaleStr toStr valueOf 
sy keyword jsObjStatics  length name assign create defineProperties defineProperty entries freeze getOwnPropertyDescriptor getOwnPropertyDescriptors getOwnPropertyNames getOwnPropertySymbols getPrototypeOf is isExtensible isFrozen isSealed keys preventExtensions seal setPrototypeOf values
sy keyword jsPromise     all race reject resolve 
sy keyword jsSetFunct    add has size 
sy keyword jsStrFunct    charAt charCodeAt codePointAt concat endsWith fixed fontcolor fontsize includes localeCompare match normalize padEnd padStart repeat replace search split startsWith strike sub substr substring toLocaleLowerCase toLocaleUpperCase toLowerCase toUpperCase trim trimEnd trimLeft trimRight trimStart

" #!/usr/bin/env node
sy match jsShebang '\v^#!.+$'

" Comment:
sy region  jsComment      start="/\*"  end="\*/" contains=@Spell,@docstringAll keepend
sy region  jsCommentTodo  start='\v<(FIXME|XXX|TODO)>' end="$" oneline contained
sy match   jsLineComment  "\/\/.*"               contains=@Spell,jsCommentTodo

" Builtin Types:
sy keyword jsBool true false
sy keyword jsCond if else switch
sy keyword jsNull null undefined
sy match   jsNum "-\=\<\d\+L\=\>\|0[xX][0-9a-fA-F]\+\>"

" objects by convention capitalised
sy match jsType   "\v<(([A-Z][a-z]+)+|[A-Z]+[a-z][A-Za-z]+)>"
" mainly for syntax#complete
sy keyword jsType Array Boolean Date Function Number Object String RegExp Infinity Reflect Proxy Math Symbol Error EvalError InternalError RangeError ReferenceError SyntaxError TypeError URIError Generator GeneratorFunction AsyncFunction Promise JSON DataView ArrayBuffer Map Set WeakMap WeakSet Int8Array Uint8Array Uint8ClampedArray Int16Array Uint16Array Int32Array Uint32Array Float32Array Float64Array

" sy region jsObj start='{' end='}' oneline keepend contains=jsObjKey
" sy match jsObjKey "\v<[[:alpha:]]+>:" 
" hi def link jsObjKey String

" Str:
sy match  jsStrEscape "\v\\." contained containedin=jsStrD,jsStrS,jsTemplStr
sy region jsStrD start=+"+  skip=+\\\\\|\\"+  end=+"\|$+ oneline  keepend
sy region jsStrS start=+'+  skip=+\\\\\|\\'+  end=+'\|$+ oneline  keepend
" FIXME Doesn't look nice
" sy match jsStrSQuote "'"  contained containedin=jsStrS
" sy match jsStrDQuote '"' contained containedin=jsStrD
" hi def link jsStrDQuote Delimiter
" hi def link jsStrSQuote Delimiter

" `template string with ${vars}`
sy region jsTemplStr             start='`'   end='`' keepend contains=jsTemplStrSubst,jsTemplStrHtmlTag
sy region jsTemplStrSubst        start='\${' end='}' keepend oneline contained containedin=jsTemplStr

" often we put HTML tags in js templates
sy region jsTemplStrHtmlTag      start="\v\<([a-z]+)( [a-z]+(\=\".{,20}\")?)*\>" end="\v</\1\>" contained contains=jsTemplStrHtmlTagInner,jsTemplStrHtmlTag,jsTemplStrHtmlTagAttr keepend
sy region jsTemplStrHtmlTagInner start="\v\>"ms=e+1 end="\v\<"me=s-1 contained keepend
sy match  jsTemplStrHtmlTagAttr  "\v <[a-z]{3,15}>"ms=s+1 contained keepend
sy region jsTemplStrHtmlTagAttr  start="\v[a-z]{2,15}\=\"" end="\"" oneline contained contains=jsTemplStrHtmlTagAttrName,jsTemplStrHtmlTagAttrValue,jsTemplStrHtmlTagAttrEq keepend
sy match  jsTemplStrHtmlTagAttrName "\v[a-z]{2,15}\="me=e-1 contained keepend
sy match  jsTemplStrHtmlTagAttrEq "\v\=" contained 
sy region jsTemplStrHtmlTagAttrValue start='"' end='"' oneline contained keepend

sy region  jsRegexStr  start=+/[^/*]+me=e-1 skip=+\\\\\|\\/+ end=+/[gim]\{0,2\}\s*$+ end=+/[gim]\{0,2\}\s*[;.,)\]}]+me=e-1 contains=@regexAll oneline keepend

" $vars or $
sy match jsDollar '\v\$([a-z][A-Za-z0-9]*)?'

" Statements:
sy keyword jsKeyword   super this case default arguments
sy keyword jsVarDecl   var let
sy keyword jsConstDecl const
sy keyword jsClassDecl interface class extends enum implements 
sy keyword jsStmt      return with break continue try then catch finally throw next 
sy match   jsStmt      '\v<yield ?\* ?$'
sy keyword jsFunct     async                
sy match   jsFunct     "\v<(async )?function\v ?\*?"
sy keyword jsSpecial   apply call eval bind prototype constructor 
sy match   jsConst    "\v<[A-Z][A-Z_]{2}[A-Z0-9_]+>"

" chained calls
sy match jsSpecial '\v^\s*\.[a-z][a-zA-Z]+'ms=s+1

" import * from "./file.js"
" require("./file.js")
sy keyword jsImport require import export

" Loops:
sy keyword jsRepeat	do for while of in forEach

 " OOP:
sy match jsAccess    "\v<(private|protected|public|static) "
sy match jsSpecifier "\v^\s*(get|set) "
sy match jsSpecifier "\v<(protected|abstract|public|static|final|package|throws|boolean) "

" Deprecated:
" __get_attribute__
sy region jsDepr start='__' end='__' oneline keepend
sy keyword jsDepr caller 

" Macros:
sy keyword jsNodeMacro __dirname __filename 

" Globals:
" Node.js
sy keyword jsNodeGlobal    module exports process 
" Browser
sy keyword jsBrowserGlobal window inspect origin document alert caches screen sessionStorage close confirm prompt stream fetch focus blur getSelection getComputedStyle closed applicationCache open parent screenLeft screenTop scroll scrollBy scrollX scrollY openDatabase print resizeBy resizeTo stop status pageXOffset pageYOffset outerWidth outerHeight opener dispatchEvent removeEventListener
" shared 
sy keyword jsGlobal        setInterval setTimeout console escape unescape eval clearTimeout clearInterval parseInt parseFloat decodeURI decodeURIComponent encodeURIComponent isFinite isNaN crypto
" events
sy match   jsEvent         '\v<on[a-z]{4,}>'

" Operators:
sy keyword jsOp await new typeof delete in

" ! || && : ? 
" and bitwise | &
sy match jsOp '\v (([&]{1,2}|[|]{1,2}|\?)) '
"key: value in objects
sy match jsOp '\v: '
" bitwise continued ... >>> <<< ^ ~
sy match jsOp '\v (\>{2,3}|\<{2,3}|^) '
sy match jsOp '\~'
" logical negation
sy match jsOp '!'
" == === <= >= < >
sy match jsOp "\v (\={2,3}|[><]\=?) "
" !== !=
sy match jsOp "\v (!?\={1,2}) "
" -- ++ - + += -=
sy match jsOp "\v ([-+])\=? "
sy match jsOp " +$"
sy match jsOp "\v([-+]{2})"
" rest / spread ...
sy match jsOp "\v\.{3}"
" * / 
" *= /=
sy match jsOp "\v ([*/])\=? "
" modulo % and %=
sy match jsOp "\v \%\=? "

" x => x.something
sy match jsArrowFunc "\v(\\(\\))? (\=\>)($| )"

" indexing eg xs[0]
sy match jsBracket "\v\]|\["

hi def link jsBracket                  Operator
hi def link jsAccess                   StorageClass
hi def link jsArrowFunc                jsFunct
hi def link jsBool                     Boolean
hi def link jsBrowserGlobal            Builtin
hi def link jsClassDecl                jsDecl
hi def link jsComment                  Comment
hi def link jsCommentTodo              WarningMsg
hi def link jsCond                     Conditional
hi def link jsConst                    Constant
hi def link jsConstDecl                jsDecl
hi def link jsDecl                     jsStmt
hi def link jsDepr                     ErrorMsg
hi def link jsDollar                   jsGlobal
hi def link jsEvent                    Procedure
hi def link jsFunct                    jsStmt
hi def link jsGlobal                   Builtin
hi def link jsImport                   Include
hi def link jsKeyword                  Keyword
hi def link jsLineComment              jsComment
hi def link jsNodeGlobal               Builtin
hi def link jsNodeMacro                Macro
hi def link jsNull                     Symbol
hi def link jsNum                      Number
hi def link jsOp                       Operator
hi def link jsRegexStr                 PreProc
hi def link jsRepeat                   Repeat
hi def link jsShebang                  PreProc
hi def link jsSpecifier                StorageClass
hi def link jsStmt                     Statement
hi def link jsStrD                     String
hi def link jsStrEscape                SpecialChar
hi def link jsStrS                     String
hi def link jsTemplStr                 Macro
hi def link jsTemplStrHtmlTag          Keyword
hi def link jsTemplStrHtmlTagAttrEq    Operator
hi def link jsTemplStrHtmlTagAttr      Constant
hi def link jsTemplStrHtmlTagAttrName  Constant
hi def link jsTemplStrHtmlTagAttrValue String
hi def link jsTemplStrHtmlTagInner     Normal
hi def link jsTemplStrSubst            SpecialChar
hi def link jsType                     Type
hi def link jsVarDecl                  jsDecl
hi def link jsSpecial                  jsSpecifier