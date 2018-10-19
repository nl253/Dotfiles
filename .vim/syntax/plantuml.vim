" Syntax for PlantUML
" -------------------
"
" Example usage:
"
"     runtime! syntax/plantuml.vim
"
" This should highlight PlantUML syntax between `@startuml` and `@enduml`.
"
if exists('b:current_syntax') 
    if b:current_syntax ==# 'plantuml' | finish | endif
else
    let b:current_syntax = 'plantuml' 
endif

sy sync minlines=150

" class Content {
"   id         <b>INT PRIMARY KEY AUTOINCREMENT</b>
"   -- FK --
"   creator    <b>REFERENCES User(email)</b>
"   -- other --
"   summary    <b>TEXT</b>
" }
sy match pumlSeparator "\v^\s*--\s*[^-]+\s*--\s*$" contained containedin=pumlInsideClass
sy match pumlSeparator "\v^\s*\.\.\s*[^-]+\s*\.\.\s*$" contained containedin=pumlInsideClass

" @startuml
" @enduml
sy region puml start='\v^\s*\@startuml\s*$' end='\v^\s*\@enduml\s*$' contains=pumlComment,pumlKeyword,pumlPreProcKeyword,pumlArrow,pumlStereotype,pumlInsideClass,pumlStereotypeSpec,pumlStartEnd,pumlNote transparent
sy match pumlStartEnd '\v^\s*\@(start|end)uml\s*$' contained

sy match pumlKeyword "\v<(((abstract\s+)?class)|interface|extends|implements)>" contained
sy region pumlNote start='\v^\s*(note|legend)\s+(left|right|top|bottom)\s+of\s+\w+\s*$' end='\v^\s*end\s+note\s*$' contained contains=@spell

" --->
" -->
" ->
" ---o
" --o
" -o
" ---<>
" --<>
" -<>
" ---|>
" --|>
" -|>
sy match pumlArrow  "\v>\s+[-\<\>\.o\*\|]{2,}\s+<" contained

" Name needs to be *before* Type
sy region pumlAttrs         start=':'ms=s+1    end='\v(,|\)|$)@=' oneline contained keepend matchgroup=Delimiter contains=pumlType,pumlMultiplicity,pumlAssignment,pumlProps 
sy match  pumlType          '\v<(([A-Z][a-z]+\d*)+|[A-F])>'
sy region pumlMultiplicity  start='\v\['ms=s+1 end='\v\]'me=e-1   oneline contained 

sy region pumlAssignment  start='=' end='$' contained keepend oneline contains=pumlEq,pumlNum,pumlStr 
sy match  pumlNum '\v-?\d+(\.\d+)?([Ee]-?\d+)?' contained
sy region pumlStr start='"' end='"' oneline contained contains=@spell
sy region pumlStr start="'" end="'" oneline contained contains=@spell

sy region pumlProps start='{'ms=s+1 end='}'me=e-1 oneline contained contains=pumlComma,pumlProp 
sy match  pumlComma ','        contained
sy match  pumlProp  '\v\w{2,}' contained 
sy match  pumlEq    '='        contained

" e.g.: Builder .> ComplexObject: << create >>
sy region pumlStereotypeSpec       start='\v:\s*\<' end='\v(,|\)|$)@=' oneline keepend contains=pumlStereotype
" method()
sy match pumlOperation  "\v([A-Za-z][a-z0-9]+)+\(@=" contained
" sy match pumlOperation  "\v\)"
" + public
" - private
" # protected
sy match  pumlVisibility "\v^\s*[-\+#]( |<)" contained
sy region pumlInsideClass start='{' end='}' contained contains=pumlVisibility,pumlOperation,pumlAttrs
" ' comment till end of line
sy region pumlComment    start="'"    end="$"    skip="\\'"  oneline contained containedin=pumlInsideClass contains=@Spell
sy region pumlComment    start="/'"   end="'/"                       contained contains=@Spell
sy region pumlStereotype start="\V<<" end="\V>>"             oneline contained

sy match pumlPreProcKeyword "\v<(AttributeIconSize|Font(Name|Color|Size)|skinparam|title|scale|(head|foot)er|(Border|Arrow|Background)Color)>" contained

hi def link pumlArrow          Operator
hi def link pumlAssignment     Normal
hi def link pumlAttrs          Normal
hi def link pumlComma          Operator
hi def link pumlComment        Comment
hi def link pumlEq             Operator
hi def link pumlKeyword        Keyword
hi def link pumlMultiplicity   Number
hi def link pumlNote           SpecialComment
hi def link pumlNum            Number
hi def link pumlPreProcKeyword PreProc
hi def link pumlProps          Special
hi def link pumlSeparator      Special
hi def link pumlStartEnd       PreProc
hi def link pumlStereotype     Statement
hi def link pumlStr            String
hi def link pumlType           Type
hi def link pumlVisibility     Operator
hi def link pumloperation      Function
