" if !exists("main_syntax")
  " " quit when a syntax file was already loaded
  " if exists("b:current_syntax")
    " finish
  " endif
  " let main_syntax = 'javaScript'
" elseif exists("b:current_syntax") && b:current_syntax == "javaScript"
  " finish
" endif

let s:cpo_save = &cpo
set cpo&vim

sy sync fromstart
sy sync maxlines=100

runtime! syntax/regex.vim
runtime! syntax/docstring.vim

sy keyword javaScriptArrayFunct        concat copyWithin every fill filter find findIndex includes indexOf join lastIndexOf map pop push reduce reduceRight reverse shift slice some sort splice unshift
sy keyword javaScriptJSONStatics       parse stringify
sy keyword javaScriptMapFunct          clear get set
sy keyword javaScriptMathStatics       E LN10 LN2 LOG10E LOG2E PI SQRT1_2 SQRT2 abs acos acosh asin asinh atan atan2 atanh cbrt ceil clz32 cos cosh exp expm1 floor fround hypot imul log log10 log1p log2 max min pow random round sign sin sinh sqrt tan tanh trunc
sy keyword javaScriptObjFunct          hasOwnProperty isPrototypeOf propertyIsEnumerable toLocaleStr toStr valueOf 
sy keyword javaScriptObjStatics        length name assign create defineProperties defineProperty entries freeze getOwnPropertyDescriptor getOwnPropertyDescriptors getOwnPropertyNames getOwnPropertySymbols getPrototypeOf is isExtensible isFrozen isSealed keys preventExtensions seal setPrototypeOf values
sy keyword javaScriptPromiseSONStatics all race reject resolve 
sy keyword javaScriptSetFunct          add has size 
sy keyword javaScriptStrFunct          charAt charCodeAt codePointAt concat endsWith fixed fontcolor fontsize includes localeCompare match normalize padEnd padStart repeat replace search split startsWith strike sub substr substring toLocaleLowerCase toLocaleUpperCase toLowerCase toUpperCase trim trimEnd trimLeft trimRight trimStart

" #!/usr/bin/env node
sy match javaScriptShebang '\v^#!.+$'

" Comment:
sy region  javaScriptComment      start="/\*"  end="\*/" contains=@Spell,javaScriptCommentTodo,docStringBulletPoint,docStringMetaTag,docStringDescr keepend
sy match   javaScriptLineComment  "\/\/.*"               contains=@Spell,javaScriptCommentTodo
sy region  javaScriptCommentTodo  start='\v<(FIXME|XXX|TODO)>' end="$" oneline contained

" Builtin Types:
sy keyword javaScriptBool true false
sy keyword javaScriptCond if else switch
sy keyword javaScriptNull null undefined
sy match   javaScriptNum "-\=\<\d\+L\=\>\|0[xX][0-9a-fA-F]\+\>"

" objects by convention capitalised
sy match javaScriptType   "\v<(([A-Z][a-z]+)+|[A-Z]+[a-z][A-Za-z]+)>"
sy keyword javaScriptType Array Boolean Date Function Number Object String RegExp Infinity Reflect Proxy Math Symbol Error EvalError InternalError RangeError ReferenceError SyntaxError TypeError URIError Generator GeneratorFunction AsyncFunction Promise JSON DataView ArrayBuffer Map Set WeakMap WeakSet Int8Array Uint8Array Uint8ClampedArray Int16Array Uint16Array Int32Array Uint32Array Float32Array Float64Array

" sy region javaScriptObj start='{' end='}' oneline keepend contains=javaScriptObjKey
" sy match javaScriptObjKey "\v<[[:alpha:]]+>:" 
" hi link javaScriptObjKey String

" Str:
sy match  javaScriptStrEscape "\v\\." contained containedin=javaScriptStrD,javaScriptStrS,javaScriptTemplStr
sy region javaScriptStrD start=+"+  skip=+\\\\\|\\"+  end=+"\|$+ oneline  keepend
sy region javaScriptStrS start=+'+  skip=+\\\\\|\\'+  end=+'\|$+ oneline  keepend
" FIXME Doesn't look nice
" sy match javaScriptStrSQuote "'"  contained containedin=javaScriptStrS
" sy match javaScriptStrDQuote '"' contained containedin=javaScriptStrD
" hi link javaScriptStrDQuote Delimiter
" hi link javaScriptStrSQuote Delimiter

" `template string with ${vars}`
sy region javaScriptTemplStr             start='`'   end='`' keepend contains=javaScriptTemplStrSubst,javaScriptTemplStrHtmlTag
sy region javaScriptTemplStrSubst        start='\${' end='}' keepend oneline contained containedin=javaScriptTemplStr

" often we put HTML tags in js templates
sy region javaScriptTemplStrHtmlTag      start="\v\<([a-z]+)( [a-z]+(\=\".{,20}\")?)*\>" end="\v</\1\>" contained contains=javaScriptTemplStrHtmlTagInner,javaScriptTemplStrHtmlTag,javaScriptTemplStrHtmlTagAttr keepend
sy region javaScriptTemplStrHtmlTagInner start="\v\>"ms=e+1 end="\v\<"me=s-1 contained keepend
sy match javaScriptTemplStrHtmlTagAttr  "\v <[a-z]{3,15}>"ms=s+1 contained keepend
sy region javaScriptTemplStrHtmlTagAttr  start="\v[a-z]{2,15}\=\"" end="\"" oneline contained contains=javaScriptTemplStrHtmlTagAttrName,javaScriptTemplStrHtmlTagAttrValue,javaScriptTemplStrHtmlTagAttrEq keepend
sy match javaScriptTemplStrHtmlTagAttrName "\v[a-z]{2,15}\="me=e-1 contained keepend
sy match javaScriptTemplStrHtmlTagAttrEq "\v\=" contained 
sy region javaScriptTemplStrHtmlTagAttrValue start='"' end='"' oneline contained keepend

sy region  javaScriptRegexStr  start=+/[^/*]+me=e-1 skip=+\\\\\|\\/+ end=+/[gim]\{0,2\}\s*$+ end=+/[gim]\{0,2\}\s*[;.,)\]}]+me=e-1 contains=regexGroup,regexSet,regexQuant,regexAtom,regexEscape,regexOr oneline keepend

" $vars or $
sy match javaScriptDollar '\v\$([a-z][A-Za-z0-9]*)?'

" Statements:
sy keyword javaScriptKeyword   super this case default arguments
sy keyword javaScriptVarDecl   var let
sy keyword javaScriptConstDecl const
sy keyword javaScriptClassDecl interface class extends enum implements 
sy keyword javaScriptStmt      return with break continue try then catch finally throw next 
sy match   javaScriptStmt      '\v<yield ?\* ?$'
sy keyword javaScriptFunct     async                
sy match   javaScriptFunct     "\v<(async )?function\v ?\*?"
sy keyword javascriptSpecial   apply call eval bind prototype constructor 
sy match javaScriptConst "\v<[A-Z][A-Z_]{2}[A-Z0-9_]+>"

" chained calls
sy match javascriptSpecial '\v^\s*\.[a-z][a-zA-Z]+'ms=s+1

" import * from "./file.js"
" require("./file.js")
sy keyword javaScriptImport require import export

" Loops:
sy keyword javaScriptRepeat	do for while of in forEach

 " OOP:
sy match javaScriptAccess "\v<(private|protected|public|static) "
sy match javaScriptSpecifier "\v^\s*(get|set) "
sy match javaScriptSpecifier "\v<(protected|abstract|public|static|final|package|throws|boolean) "

" Deprecated:
" __get_attribute__
sy region javaScriptDepr start='__' end='__' oneline keepend
sy keyword javaScriptDepr caller 

" Macros:
sy keyword javaScriptNodeMacro __dirname __filename 

" Globals:
" Node.js
sy keyword javaScriptNodeGlobal module exports process 
" Browser
sy keyword javaScriptBrowserGlobal window inspect origin document alert caches screen sessionStorage close confirm prompt stream fetch focus blur getSelection getComputedStyle closed applicationCache open parent screenLeft screenTop scroll scrollBy scrollX scrollY openDatabase print resizeBy resizeTo stop status pageXOffset pageYOffset outerWidth outerHeight opener dispatchEvent removeEventListener
" shared 
sy keyword javaScriptGlobal setInterval setTimeout console escape unescape eval clearTimeout clearInterval parseInt parseFloat decodeURI decodeURIComponent encodeURIComponent isFinite isNaN crypto
" events
sy match javaScriptEvent '\v<on[a-z]{4,}>'

" Operators:
sy keyword javaScriptOp await new typeof delete in

" ! || && : ? 
" and bitwise | &
sy match javaScriptOp '\v (([&]{1,2}|[|]{1,2}|\?)) '
"key: value in objects
sy match javaScriptOp '\v: '
" bitwise continued ... >>> <<< ^ ~
sy match javaScriptOp '\v (\>{2,3}|\<{2,3}|^) '
sy match javaScriptOp '\~'
" logical negation
sy match javaScriptOp '!'
" == === <= >= < >
sy match javaScriptOp "\v (\={2,3}|[><]\=?) "
" !== !=
sy match javaScriptOp "\v (!?\={1,2}) "
" -- ++ - + += -=
sy match javaScriptOp "\v ([-+])\=? "
sy match javaScriptOp " +$"
sy match javaScriptOp "\v([-+]{2})"
" rest / spread ...
sy match javaScriptOp "\v\.{3}"
" * / 
" *= /=
sy match javaScriptOp "\v ([*/])\=? "
" modulo % and %=
sy match javaScriptOp "\v \%\=? "

" x => x.something
sy match javaScriptArrowFunc "\v(\\(\\))? (\=\>)($| )"

" indexing eg xs[0]
sy match javaScriptBracket "\v\]|\["
hi link javaScriptBracket Operator

hi link javaScriptAccess                   StorageClass
hi link javaScriptArrowFunc                javaScriptFunct
hi link javaScriptBool                     Boolean
hi link javaScriptBrowserGlobal            Builtin
hi link javaScriptClassDecl                javaScriptDecl
hi link javaScriptComment                  Comment
hi link javaScriptCommentTodo              WarningMsg
hi link javaScriptCond                     Conditional
hi link javaScriptConst                    Constant
hi link javaScriptConstDecl                javaScriptDecl
hi link javaScriptDecl                     javaScriptStmt
hi link javaScriptDepr                     ErrorMsg
hi link javaScriptDollar                   javaScriptGlobal
hi link javaScriptEvent                    Procedure
hi link javaScriptFunct                    javaScriptStmt
hi link javaScriptGlobal                   Builtin
hi link javaScriptImport                   Include
hi link javaScriptKeyword                  Keyword
hi link javaScriptLineComment              javaScriptComment
hi link javaScriptNodeGlobal               Builtin
hi link javaScriptNodeMacro                Macro
hi link javaScriptNull                     Symbol
hi link javaScriptNum                      Number
hi link javaScriptOp                       Operator
hi link javaScriptRegexStr                 PreProc
hi link javaScriptRepeat                   Repeat
hi link javaScriptShebang                  PreProc
hi link javaScriptSpecifier                StorageClass
hi link javaScriptStmt                     Statement
hi link javaScriptStrD                     String
hi link javaScriptStrEscape                SpecialChar
hi link javaScriptStrS                     String
hi link javaScriptTemplStr                 Macro
hi link javaScriptTemplStrHtmlTag          Keyword
hi link javaScriptTemplStrHtmlTagAttrEq    Operator
hi link javaScriptTemplStrHtmlTagAttr      Constant
hi link javaScriptTemplStrHtmlTagAttrName  Constant
hi link javaScriptTemplStrHtmlTagAttrValue String
hi link javaScriptTemplStrHtmlTagInner     Normal
hi link javaScriptTemplStrSubst            SpecialChar
hi link javaScriptType                     Type
hi link javaScriptVarDecl                  javaScriptDecl
hi link javascriptSpecial                  javaScriptSpecifier

let b:current_syntax = "javascript"
" if main_syntax == 'javascript'
  " unlet main_syntax
" endif
" let &cpo = s:cpo_save
" unlet s:cpo_save

" if main_syntax == "javascript"
  " sy sync ccomment javaScriptComment
" endif
