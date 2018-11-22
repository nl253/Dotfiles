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

" @startuml
" @enduml
sy region puml start='\v^\s*\@startuml\s*$' end='\v^\s*\@enduml\s*$' contains=pumlComment,pumlKeyword,pumlPreProcKeyword,pumlArrow,pumlStereotype,pumlInsideClass,pumlStereotypeSpec,pumlStartEnd,pumlNote transparent
sy match pumlStartEnd '\v^\s*\@(start|end)uml\s*$' contained

" class Content {
"   id         <b>INT PRIMARY KEY AUTOINCREMENT</b>
"   -- FK --
"   creator    <b>REFERENCES User(email)</b>
"   -- other --
"   summary    <b>TEXT</b>
" }
sy match pumlSeparator "\v^\s*--\s*[^-]+\s*--\s*$" contained containedin=pumlInsideClass
sy match pumlSeparator "\v^\s*\.\.\s*[^-]+\s*\.\.\s*$" contained containedin=pumlInsideClass

" __emphasise__
sy region pumlUnderline start="__" end="__" oneline containedin=puml contained oneline

" **emphasise**
sy region pumlBold start="\v\*\*" end="\v\*\*" oneline containedin=puml contained oneline

" //emphasise//
sy region pumlItalic start="//" end="//" oneline containedin=puml contained oneline

" ""monospaces""
sy region pumlCode start='""' end='""' oneline containedin=puml contained oneline

" ""monospaces""
sy region pumlHTMLTag start='<\z(\v[a-z]+\V\)>' end='</\z1>' oneline matchgroup=Statement containedin=puml,pumlInsideClass contains=ALL contained oneline

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
sy match pumlOperation  "\v([A-Za-z][a-z0-9A-Z]+)+\(@=" contained containedin=pumlInsideClass
" sy match pumlOperation  "\v\)"
" + public
" - private
" # protected
sy match  pumlVisibility "\v^\s*[-\+#]( |<)" contained
sy region pumlInsideClass start='{' end='}' contained contains=pumlVisibility,pumlOperation,pumlAttrs
sy region pumlStereotype start="\V<<" end="\V>>"             oneline contained

sy match pumlPreProcKeyword "\v<(AttributeIconSize|Font(Name|Color|Size)|skinparam|title|scale|(head|foot)er|(Border|Arrow|Background)Color)>" contained

" Commons
" ' comment till end of line
sy region pumlComment start="'"  end="$"  skip="\\'"  oneline contained containedin=pumlInsideClass,pumlActDiagram contains=@Spell
sy region pumlComment start="/'" end="'/"                     contained containedin=pumlInsideClass,pumlActDiagram contains=@Spell
" sy region pumlHeader  start='\v^\s*(head|foot)er\s*$' end='$' contained containedin=puml

" Activity Diagrams
sy region pumlActDiagram  start='\v^\s*start\s*$' end='\v^\s*\@enduml\s*$'me=e-7 keepend contained containedin=puml
sy match  pumlActKeyWord  '\v<((end)(if|fork|while)|repeat|(else)?if|then|partition|detach|while|else|(fork|split(\s+again)?))>' contained containedin=pumlActDiagram
sy match  pumlActSpecial  '\v<(start|stop)>' contained containedin=pumlActDiagram
sy region pumlActSwimLane start='|'ms=s+1 end='|'me=e-1 oneline contained containedin=pumlActDiagram
sy region pumlActBubble   start='\v^\s*:' end=';' contained containedin=pumlActDiagram contains=ALL

" Activity Diagrams
" hi def link pumlActBubble      Statement
hi def link pumlActKeyWord     Keyword
hi def link pumlActSpecial     Special
hi def link pumlActSwimLane    Function

" Commons
hi def link pumlComment        Comment
hi def pumlUnderline           cterm=underline gui=underline term=underline
hi def pumlBold                cterm=bold gui=bold term=bold
hi def pumlItalic              cterm=italic gui=italic term=italic
hi def link pumlCode           Constant
" hi def link pumlHTMLTag        Statement

" Class Diagrams
hi def link pumlArrow          Operator
hi def link pumlAssignment     Normal
hi def link pumlAttrs          Normal
hi def link pumlComma          Operator
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
