if exists('b:current_syntax') 
    if b:current_syntax ==# 'plantuml'
        finish
    endif
else
    let b:current_syntax = 'plantuml' 
endif

sy keyword pumlKeyword        end note left bottom top of right class abstract interface extends implements
sy keyword pumlPreProcKeyword AttributeIconSize FontColor FontName FontSize skinparam ArrowColor BorderColor BackgroundColor
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
sy match pumlArrow      "\v> [-<>.o*|]{2,} <"
sy match pumlName       '\v<([A-Z][a-z]{2,}\d*)+>'
" method()
sy match pumlOperation  "\v([A-Za-z][a-z]+)+\@=\("
sy match pumlOperation  "\v\)"
" @startuml
" @enduml
sy match pumlStartEnd   '@\v(start|end)uml'
" + public
" - private
" # protected
sy match pumlVisibility "\v^[ \t]*(\+|-|#) "
" ' comment till end of line
sy region pumlComment    start="'"    end="$"    skip="\\'"  oneline
sy region pumlComment    start="/'"   end="'/" 
sy region pumlStereotype start="\V<<" end="\V>>"             oneline

hi def link pumlArrow          Operator
hi def link pumlVisibility     Operator

hi def link pumlComment        Comment

hi def link pumlPreProcKeyword PreProc
hi def link pumlStartEnd       PreProc

hi def link pumlKeyword        Keyword
hi def link pumlStereotype     Statement
hi def link pumlOperation      Function

hi def link pumlName           Type
