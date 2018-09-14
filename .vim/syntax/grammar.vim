" Syntax for PEG and context-free (BNF / EBNF) grammars
" -----------------------------------------------------

if exists('b:current_syntax') 
    if b:current_syntax ==# 'grammar'
        finish
    endif
else
    let b:current_syntax = 'grammar' 
endif

runtime! syntax/regex.vim

sy cluster grammarAll contains=grammarOrSymbol,grammarNonTerm,grammarRange,grammarTerm,grammarStr,grammarRegex,grammarChar,grammarOpt,grammarStar,grammarPlus,grammarGroup 

" terminals have lower snake case: lowercase_terminal 
sy match grammarTerm "\v<[a-z][a-z_]+[0-9]*>"                contains=@Spell

" non-terminals have pascal case: UpperCaseTerm
sy match grammarNonTerm "\v<[A-Z](([A-Za-z]+|[0-9])[0-9]*)>" contains=@Spell

" Rule ::= term1 | term2 # BNF style
" Rule <- term1 | term2  # PEG style 
sy match grammarRuleSymbol "\V::=" 
sy match grammarRuleSymbol "\V<-" 

" some | none
" Rule ::= Some
"        | None
sy match grammarOrSymbol "\v( |^|>)(\||/)( |$|<)" 

" single char must be single quoted: 'a'
sy match grammarChar "\v'\\?.'" 

" string must be double quoted: "something"
sy region grammarStr start='"' end='"'   oneline keepend

" regex between '/' and '/': /\w+/
sy region grammarRegex start='/' end='/' oneline contains=@regexAll keepend

" regex set between '[' and ']': [abc]
sy region grammarSet start='\['ms=s+1 end='\]'me=e-1 oneline keepend contains=grammarSetRange,grammarSetChar

" ( ... )
sy region grammarGroup start='\V(' end='\V)' skip="\v['\"]." oneline keepend contains=@grammarAll

" 'a'..'z' means ( 'a' | 'b' | 'c' ... 'z' )
" essentially the same as [a-z]
sy match grammarRange '\.\.' 

" 'a'{2,3}
" 'a'{,3}
" 'a'{2,}
" 'a'{2}
sy match grammarQuant '\v\{\d{,2}(,\d{,2})?\}'
" 'a'*
sy match grammarStar '\V*' 
" 'a'+
sy match grammarPlus '\V+' 
" 'a'?
sy match grammarOpt  '\V?' 

" // comment
" # comment
" -- comment
sy region grammarComment start="\v(#|//|--) " end='$' oneline contains=@Spell
" /* multi-line comment */
sy region grammarComment start="\V/*" end='\V*/' keepend  contains=@Spell

" ^ Operator 
" Operator $ 
sy match grammarSpecialSym "\v(^| |>)(\$|\^)($| |<)"

" !terminal
sy match grammarNegPred "\V!"
" &'('
sy match grammarPosPred "\V&"

hi def link grammarOpt        Operator
hi def link grammarPlus       Operator
hi def link grammarQuant      Operator
hi def link grammarStar       Operator
hi def link grammarRange      Operator
hi def link grammarPosPred    Operator
hi def link grammarNegPred    Operator

hi def link grammarRuleSymbol Delimiter
hi def link grammarOrSymbol   Delimiter

hi def link grammarChar       Character
hi def link grammarSet        Character

hi def link grammarComment    Comment
hi def link grammarGroup      Function
hi def link grammarNonTerm    Define
hi def link grammarRegex      PreProc
hi def link grammarSpecialSym Special
hi def link grammarStr        String
hi def link grammarTerm       Normal
