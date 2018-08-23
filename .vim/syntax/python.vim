" quit when a syntax file was already loaded.
if exists("b:current_syntax") | finish | endif 

" We need nocompatible mode in order to continue lines with backslashes.
" Original setting will be restored.
let s:cpo_save = &cpo
set cpo&vim

sy keyword pythonBuiltin abs all any ascii bin bool breakpoint bytearray bytes callable chr classmethod compile complex delattr dict dir divmod enumerate eval exec filter float format frozenset getattr globals has attr hash help hex id input int isinstance issubclass iter len list locals map max memoryview min next object oct open ord pow print property range repr reversed round set setattr slice so rted staticmethod str sum super tuple type vars zip 

" Python 3.5 introduced the use of the same symbol for matrix multiplication: https://www.python.org/dev/peps/pep-0465/.  
" We now have to exclude the symbol from highlighting when used in that context.
" Single line multiplication.
sy match pythonMatMul "\%(\w\|[])]\)\s*@" contains=ALLBUT,pythonDecoratorName,pythonDecorator,pythonFunct,pythonDoctestValue transparent

" Multiplication continued on the next line after backslash.
sy match pythonMatMul "[^\\]\\\s*\n\%(\s*\.\.\.\s\)\=\s\+@" contains=ALLBUT,pythonDecoratorName,pythonDecorator,pythonFunct,pythonDoctestValue transparent

" Multiplication in a parenthesized expression over multiple lines with @ at
" the start of each continued line; very similar to decorators and complex.
sy match pythonMatMul "^\s*\%(\%(>>>\|\.\.\.\)\s\+\)\=\zs\%(\h\|\%(\h\|[[(]\).\{-}\%(\w\|[])]\)\)\s*\n\%(\s*\.\.\.\s\)\=\s\+@\%(.\{-}\n\%(\s*\.\.\.\s\)\=\s\+@\)*" contains=ALLBUT,pythonDecoratorName,pythonDecorator,pythonFunct,pythonDoctestValue transparent

sy match   pythonFunct	"\h\w*" display contained

sy keyword pythonTodo		FIXME NOTE TODO XXX contained
sy match   pythonComment	"#.*$" contains=pythonTodo,@Spell

" Triple-quoted strings can contain doctests.
sy region pythonStr       matchgroup=pythonQuotes       start=+[uU]\=\z(['"]\)+ end="\z1" skip="\\\\\|\\\z1" contains=pythonEscape,@Spell
sy region pythonStr       matchgroup=pythonTripleQuotes start=+[uU]\=\z('''\|"""\)+ skip=+\\["']+ end="\z1" keepend contains=pythonEscape,pythonSpaceError,pythonDoctest,@Spell
sy region pythonRawString matchgroup=pythonQuotes       start=+[uU]\=[rR]\z(['"]\)+ end="\z1" skip="\\\\\|\\\z1" contains=@Spell
sy region pythonRawString matchgroup=pythonTripleQuotes start=+[uU]\=[rR]\z('''\|"""\)+ end="\z1" keepend contains=pythonSpaceError,pythonDoctest,@Spell

sy match pythonEscape +\\[abfnrtv'"\\]+           contained
sy match pythonEscape "\\\o\{1,3}"                contained
sy match pythonEscape "\\x\x\{2}"                 contained
sy match pythonEscape "\%(\\u\x\{4}\|\\U\x\{8}\)" contained
" Python allows case-insensitive Unicode IDs: http://www.unicode.org/charts/
sy match pythonEscape "\\N{\a\+\%(\s\a\+\)*}"     contained
sy match pythonEscape "\\$"

" It is very important to understand all details before changing the
" regular expressions below or their order.
" The word boundaries are *not* the floating-point number boundaries
" because of a possible leading or trailing decimal point.
" The expressions below ensure that all valid number literals are
" highlighted, and invalid number literals are not.  For example,
"
" - a decimal point in '4.' at the end of a line is highlighted,
" - a second dot in 1.0.0 is not highlighted,
" - 08 is not highlighted,
" - 08e0 or 08j are highlighted,
"
" and so on, as specified in the 'Python Language Reference'.
" https://docs.python.org/2/reference/lexical_analysis.html#numeric-literals
" https://docs.python.org/3/reference/lexical_analysis.html#numeric-literals
" numbers (including longs and complex)
sy match pythonNum "\<0[oO]\=\o\+[Ll]\=\>"
sy match pythonNum "\<0[xX]\x\+[Ll]\=\>"
sy match pythonNum "\<0[bB][01]\+[Ll]\=\>"
sy match pythonNum "\<\%([1-9]\d*\|0\)[Ll]\=\>"
sy match pythonNum "\<\d\+[jJ]\>"
sy match pythonNum "\<\d\+[eE][+-]\=\d\+[jJ]\=\>"
sy match pythonNum "\<\d\+\.\%([eE][+-]\=\d\+\)\=[jJ]\=\%(\W\|$\)\@="
sy match pythonNum "\%(^\|\W\)\zs\d*\.\d\+\%([eE][+-]\=\d\+\)\=[jJ]\=\>"

" avoid highlighting attributes as builtins
sy match pythonAttribute /\.\h\w*/hs=s+1 contains=ALLBUT,pythonBuiltin,pythonFunct,pythonAsync transparent

sy match pythonError "\v<([A-Z][a-z]+)*(Exception|Exit|Error|Warning)([A-Z][a-z]+)*>"

" trailing whitespace
sy match pythonSpaceError display excludenl "\s\+$"
" mixed tabs and spaces
sy match pythonSpaceError display " \+\t"
sy match pythonSpaceError display "\t\+ "

" Do not spell doctests inside strings.
" Notice that the end of a string, either ''', or """, will end the contained
" doctest too.  Thus, we do *not* need to have it as an end pattern.
if !exists("python_no_doctest_code_highlight")
    sy region pythonDoctest start="^\s*>>>\s" end="^\s*$" contained contains=ALLBUT,pythonDoctest,pythonFunct,@Spell
    sy region pythonDoctestValue start=+^\s*\%(>>>\s\|\.\.\.\s\|"""\|'''\)\@!\S\++ end="$" contained
else
    sy region pythonDoctest start="^\s*>>>" end="^\s*$" contained contains=@NoSpell
endif

" Sync at the beginning of class, function, or method definition.
sy sync match pythonSync grouphere NONE "^\%(def\|class\)\s\+\h\w*\s*[(:]"

" Decorators (new in Python 2.4)
" A dot must be allowed because of @MyClass.myfunc decorators.
sy match pythonDecorator	 "@" display contained
sy match pythonDecoratorName "@\s*\h\%(\w\|\.\)*" display contains=pythonDecorator

sy keyword pythonConditional elif else if
sy keyword pythonRepeat	     for while
sy keyword pythonException	 except finally raise try
sy keyword pythonInclude	 from import
sy keyword pythonAsync		 async await
sy keyword pythonStatement	 lambda class def nextgroup=pythonFunct skipwhite
sy keyword pythonKeyword     with as pass nonlocal assert break continue return yield exec global del
sy match   pythonStatement	 '\v<yield\s+from>'
sy keyword pythonBoolean     True False None

sy match   pythonType    "\v<[A-Z][a-z][a-zA-Z]+>"
sy keyword pythonType    deque BZ2File BZ2Compressor BZ2Decompressor ABCMeta ABC UDPServer  TCPServer TCPServer ForkingUDPServer ThreadingTCPServer ThreadingUDPServer HTTPMessage HTTPResponse

" type annotations

" punctuation
sy match pythonColon ":" 
sy match pythonComma "," 
sy match pythonDot "\v\."
sy match pythonKwArg "\v\w[[:alnum:]_]*\@=\="

" DELIMITERS:
" sy match parenthesis "\v[\(\)]"
" hi parenthesis guifg=#0087d7 gui=bold cterm=bold term=bold ctermfg=32
sy match pythonBraces "\v[\{\}]"
sy match pythonBrackets "\v[\[\]]"

" OPERATORS:
" boolean ops
sy keyword pythonOp and in is not or
" + - / * (arithmetic)
" //      (floor division)
" **      (exponentiation)
" %       (modulo)
sy match   pythonOp "\v( |>)(-|//?|\+|\V%\v|\*\*?)( |<|$)"
" set ops: | |= & &=
sy match   pythonOp "\v( |>)(\||\&)\=?( |<|$)"
" negative numbers
sy match   pythonOp "\v-<"
" > < <= >= -= == += *= iff *between* expressions
sy match pythonComparison "\v( |>)(\=?[\*\=<>]|[-\*/\+]\=)( |<|$)"

sy region pythonFunctSignature start="\v\s+-\>\s+" end=":" contains=pythonType,pythonBuiltin,pythonBrackets,pythonError

" CORE:
hi pythonStatement   guifg=gold   ctermfg=220
hi pythonError       guifg=maroon ctermfg=Brown
hi pythonKwArg       guifg=purple
hi pythonSyntaxNoise guifg=grey   ctermfg=darkgrey
hi def link pythonAsync          Statement
hi def link pythonBoolean        Special
hi def link pythonBraces         pythonSyntaxNoise
hi def link pythonBrackets       pythonSyntaxNoise
hi def link pythonBuiltin        Builtin
hi def link pythonColon          pythonSyntaxNoise
hi def link pythonComma          pythonSyntaxNoise
hi def link pythonComment        Comment
hi def link pythonComparison     Operator
hi def link pythonConditional    Conditional
hi def link pythonConditional    Conditional
hi def link pythonDecorator      Define
hi def link pythonDecoratorName  Function
hi def link pythonDoctest        Operator
hi def link pythonDoctest        Special
hi def link pythonDoctestValue   Define
hi def link pythonDot            pythonSyntaxNoise
hi def link pythonEscape         Special
hi def link pythonException      Exception
hi def link pythonFunct          Function
hi def link pythonFunctSignature Operator
hi def link pythonInclude        Include
hi def link pythonInclude        Special
hi def link pythonKeyword        pythonStatement
hi def link pythonMatMul         Operator
hi def link pythonNum            Number
hi def link pythonOp             Operator
hi def link pythonQuotes         String
hi def link pythonRawString      String
hi def link pythonRepeat         Repeat
hi def link pythonRepeat         Repeat
hi def link pythonSelf           Macro
hi def link pythonSpaceError     Visual
hi def link pythonStr            String
hi def link pythonTodo           Todo
hi def link pythonTripleQuotes   pythonQuotes
hi def link pythonType           Type

let b:current_syntax = "python"

let &cpo = s:cpo_save
unlet s:cpo_save
