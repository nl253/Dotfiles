" Syntax for extended POSIX regular expressions
" ---------------------------------------------
"
" Example usage:
"
"   If you want regex *between* '/' and '/': eg.: (/\w+/)
"
"     sy region mySyntaxRule start='/' end='/' oneline contains=regexGroup,regexSpecial,regexComment,regexSet,regexQuant,regexAtom,regexEscape,regexOr keepend
"
sy match  regexAtom  "\v[[:alnum:]]"                             contained containedin=regexGroup,regexSet
sy match  regexEscape "\v\\."                                    contained containedin=regexGroup
sy match  regexOr    '|'                                         contained containedin=regexGroup
" a+
" a?
" a*
sy match  regexQuant "\v\*|\+|\?"                                contained containedin=regexGroup
" 'a'{2,3}
" 'a'{,3}
" 'a'{2,}
" 'a'{2}
sy match  regexQuant       '\v\{\d{,2}(,\d{,2})?\}'                       contained containedin=regexGroup
sy region regexGroup       start='('  end=')'     skip="\v['\"\\]\V)"     contained                        contains=regexGroup,regexSet,regexQuant,regexOr,regexSpecial oneline keepend
" (?#comment)
sy match regexComment      "\v\(\?#.{-}\)"                                contained
sy region regexSet         start='\[' end='\]'    skip="\v['\"\\]\V]"     contained                        contains=regexAtom                              oneline keepend

" [[:alpha:]]
let s:posix_classes = [
            \ 'al\v(num|pha)\V', 
            \ 'word', 
            \ 'lower', 
            \ 'graph', 
            \ 'cntrl', 
            \ 'blank', 
            \ 'ascii', 
            \ '\vx?\Vdigit', 
            \ 'print'
            \ ]
for s:class in s:posix_classes 
    exe "sy match regiexPosixClass '\\[\:".s:class."\:\\]' contained containedin=regexSet oneline"
endfor

sy match regexSpecial "\v\$|\.|\^" contained containedin=regexGroup 

hi link regexEscape      SpecialChar
hi link regexSpecial     SpecialChar
hi link regexAtom        Normal
hi link regexGroup       Boolean
hi link regexQuant       Operator
hi link regexSet         Type
hi link regiexPosixClass SpecialChar
hi link regexComment     Comment
hi link regexOr          Character
