if exists('b:current_syntax') 
    if (b:current_syntax ==# 'python') || exists('b:python_syntax_loaded')
        finish
    else
        let b:python_syntax_loaded = 1
    endif
else
    sy clear
    let b:current_syntax = 'python' 
endif

runtime! syntax/regex.vim

" mainly for syntax#complete
exe 'sy keyword pythonBuiltin '.join(systemlist("command python3 -c \"import string as s; import builtins as b\n\nfor j in ((i for i in dir(b) if i.lower() == i and i[0] in s.ascii_letters)): print(j)\""), ' ')
sy keyword pythonSelf self

" Python 3.5 introduced the use of the same symbol for matrix multiplication: https://www.python.org/dev/peps/pep-0465/.  
" We now have to exclude the symbol from highlighting when used in that context.
" Single line multiplication.
sy match pythonMatMul "\%(\w\|[])]\)\s*@" contains=ALLBUT,pythonDecoratorName,pythonDecorator,pythonFunct,pythonDoctestValue transparent

" Multiplication continued on the next line after backslash.
sy match pythonMatMul "[^\\]\\\s*\n\%(\s*\.\.\.\s\)\=\s\+@" contains=ALLBUT,pythonDecoratorName,pythonDecorator,pythonFunct,pythonDoctestValue transparent

" Multiplication in a parenthesized expression over multiple lines with @ at
" the start of each continued line; very similar to decorators and complex.
sy match pythonMatMul "^\s*\%(\%(>>>\|\.\.\.\)\s\+\)\=\zs\%(\h\|\%(\h\|[[(]\).\{-}\%(\w\|[])]\)\)\s*\n\%(\s*\.\.\.\s\)\=\s\+@\%(.\{-}\n\%(\s*\.\.\.\s\)\=\s\+@\)*" contains=ALLBUT,pythonDecoratorName,pythonDecorator,pythonFunct,pythonDoctestValue transparent

sy match  pythonTodo    '\v<(FIXME|NOTE|TODO|XXX)>' contained
sy region pythonComment start="#" end="$"           contains=pythonTodo,@Spell oneline 
sy region pythonShebang start='\v%1l^#!' end='$' oneline
hi link pythonShebang PreProc

" Triple-quoted strings can contain doctests.
sy region pythonStr     matchgroup=pythonQuotes       start=+[uU]\?\z(['"]\)+         end="\z1" skip="\\\\\|\\\z1" contains=pythonEscape,@Spell
sy region pythonStr     matchgroup=pythonTripleQuotes start=+[uU]\?\z('''\|"""\)+     end="\z1" skip=+\\["']+      contains=pythonEscape,pythonSpaceError,pythonDoctest,@Spell keepend 

" printf-style str formatting inside *regular* strings (not f-string nor bytes) 
" see <https://docs.python.org/3/library/stdtypes.html#old-string-formatting>
sy match pythonPrintfModifier "%(\v[a-z_]+\V)\v[-+ 0#]?(\d+|\*)?(\.\d+)?\d*[EFGXacdefgiorsux]" contained containedin=pythonStr

" highlight python code inside f-strings 
" see <https://docs.python.org/3.7/library/string.html> 
sy region pythonFmtStr  matchgroup=pythonQuotes       start=+f\z(['"]\)+              end="\z1" skip="\\\\\|\\\z1" contains=pythonEscape,@Spell
sy region pythonFmtStr  matchgroup=pythonTripleQuotes start=+f\z('''\|"""\)+          end="\z1" skip=+\\["']+      contains=pythonEscape,pythonSpaceError,pythonDoctest,@Spell keepend 

" E.g.: f"false negative on {i + 2}"
" NOTE: this CANNOT contain `pythonBraces`
sy region pythonFmtStrInter start='\v\{' end='\}' oneline contained containedin=pythonFmtStr contains=pythonFmtStrInter,pythonBrackets,pythonNum,pythonBuiltin,pythonError,pythonSelf,pythonBoolean,pythonClass,pythonOp,pythonConditional
sy match pythonFmtStrMod '\v(![rsa])?(:(\w[\<\>\=\^]?)?[- \+]?#?0?\d*,?(\.\d+)?[bcdeEfFgGnosxX\%]?)?' contained 

" highlight regular expressions in raw strings e.g.: r"(abc)?"
" see <https://docs.python.org/3/library/re.html>
sy region pythonRawStr  matchgroup=pythonQuotes       start=+[uU]\?[rR]\z(['"]\)+     end="\z1" skip="\\\\\|\\\z1" contains=@Spell,@regexAll
sy region pythonRawStr  matchgroup=pythonTripleQuotes start=+[uU]\?[rR]\z('''\|"""\)+ end="\z1"                    contains=pythonSpaceError,@Spell,@regexAll                  keepend 

sy region pythonByteStr matchgroup=pythonQuotes       start=+b\z(['"]\)+              end="\z1" skip="\\\\\|\\\z1" contains=pythonEscape
sy region pythonByteStr matchgroup=pythonTripleQuotes start=+b\z('''\|"""\)+          end="\z1" skip=+\\["']+      contains=pythonEscape,pythonSpaceError                      keepend 


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

sy match pythonError "\v<([A-Z][a-z]+)*(E(x(ception|it)|rror)|Warning)([A-Z][a-z]+)*>"

" trailing whitespace
sy match pythonSpaceError display "\s\+$" excludenl 
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
sy match   pythonFunct       "\h\w*" contained display 
sy keyword pythonKeyword     with as pass nonlocal assert break continue return yield exec global del
sy match   pythonKeyword	 '\v<yield\s+from>'
sy keyword pythonBoolean     True False None
sy match   pythonClass        "\v<[A-Z][a-z][a-zA-Z]+>"
" a few exceptions that don't match the regexp above
sy keyword pythonClass        deque BZ2File BZ2Compressor BZ2Decompressor ABCMeta ABC UDPServer TCPServer TCPServer ForkingUDPServer ThreadingTCPServer ThreadingUDPServer HTTPMessage HTTPResponse

" punctuation
sy match pythonColon ":" 
sy match pythonComma "," 
sy match pythonDot   "\v\."
sy match pythonKwArg "\v\w[[:alnum:]_]*\@=\="
sy match pythonEq             '='     contained
sy match pythonFunctSignArrow '\v-\>' contained

" DELIMITERS:
" sy match parenthesis "\v[\(\)]"
" hi parenthesis guifg=#0087d7 gui=bold cterm=bold term=bold ctermfg=32
sy match pythonBraces "\v[\{\}]"
sy match pythonBrackets "\v[\[\]]"

" OPERATORS:
"
" binary operators:
"
" pow:           x  ** 2
" floor division x  // 2
" bit shift left x  << 2
" bit shift righ x  >> 2
" eq             3  == 3
" not equal      3  != 3
" not            ! True
" assignment     a  =  20
" less than      3  <  3
" greater than   3  >  3
" modulo         3  %  3
" addition       3  +  3
" subtraction    3  -  3
" multiplication 3  *  3
" division       3  /  3
" union          s  |  s2 
" difference     s  ^  s2 
" intersection   s  &  s2 
" assignment (ie update) version of the above (with '=')
sy match pythonOp "\v( |^|>)(([\*/\>\<]{2}|[-!\|\&\^/\%\*\+\>\<\=])\=?)( |$|<)"

" boolean ops
sy keyword pythonOp and in is not or 

" unary operators:
"
" negative num -10
" bit flip     ~10
sy match   pythonOp "\v[-\~]( |<)"


" CORE:
hi def link pythonAsync          Statement
hi def link pythonBoolean        Symbol
hi def link pythonBraces         pythonSyntaxNoise
hi def link pythonBrackets       pythonSyntaxNoise
hi def link pythonBuiltin        Builtin
hi def link pythonClass          Type
hi def link pythonColon          pythonSyntaxNoise
hi def link pythonComma          pythonSyntaxNoise
hi def link pythonComment        Comment
hi def link pythonConditional    Conditional
hi def link pythonConditional    Conditional
hi def link pythonDecorator      Define
hi def link pythonDecoratorName  Function
hi def link pythonDoctest        Operator
hi def link pythonDoctest        Special
hi def link pythonDoctestValue   Define
hi def link pythonDot            pythonSyntaxNoise
hi def link pythonEq             pythonSyntaxNoise
hi def link pythonEscape         Special
hi def link pythonFmtStrMod      pythonEscape
hi def link pythonPrintfModifier pythonEscape
hi def link pythonException      Exception
hi def link pythonFmtStr         pythonStr 
hi def link pythonFunct          Function
hi def link pythonFunctSignArrow pythonSyntaxNoise
hi def link pythonInclude        Include
hi def link pythonInclude        Special
hi def link pythonKeyword        pythonStatement
hi def link pythonMatMul         Operator
hi def link pythonNum            Number
hi def link pythonOp             Operator
hi def link pythonPolyType       pythonSyntaxNoise
hi def link pythonQuotes         Operator
hi def link pythonRawStr         String
hi def link pythonRepeat         Repeat
hi def link pythonRepeat         Repeat
hi def link pythonSelf           Macro
hi def link pythonSpaceError     Visual
hi def link pythonStr            String
hi def link pythonByteStr        SpecialComment
hi def link pythonTodo           Todo
hi def link pythonTripleQuotes   pythonQuotes
hi def link pythonTypeLabel      Type
hi def      pythonStatement      guifg=gold   ctermfg=220
hi def      pythonError          guifg=maroon ctermfg=Brown
hi def      pythonKwArg          guifg=purple
hi def      pythonSyntaxNoise    guifg=grey   ctermfg=darkgrey

sy cluster pythonTypeInfo contains=pythonTypeLabel,pythonBrackets,pythonComma

" NOTE: this needs to be at the bottom to overwrite the follwing rules:
" - `pythonBuiltin`
" - `pythonClass`
" - `pythonOp`
"
" NOTE: the order of rules below cannot change.
" name of type such as: Text, int, float, Set etc. 
sy match  pythonTypeLabel '\v_?([a-z][_a-z0-9]*|[A-Z]\w*)+' contained nextgroup=pythonPolyType,pythonEq
" ... -> float:
sy region pythonFunctSignature start="\v\s+-\>\s+" end=":" contains=@pythonTypeInfo,pythonFunctSignArrow,pythonColon transparent keepend
" my_var: int = 22 
sy region pythonTypedVar start='\v(^\s*[_a-zA-Z]\w*\s*)@<=:' end='=' oneline contains=@pythonTypeInfo,pythonEq,pythonColon transparent keepend
