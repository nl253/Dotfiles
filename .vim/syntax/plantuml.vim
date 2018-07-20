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
sy match   pumlArrow          "\v> [-<>.o*|]{2,} <"
sy match   pumlName           '\v<([A-Z][a-z]{2,}\d*)+>'
" method()
sy match   pumlOperation      "\v([A-Za-z][a-z]+)+\@=\("
sy match   pumlOperation      "\v\)"
" @startuml
" @enduml
sy match   pumlStartEnd       '@\v(start|end)uml'
" + public
" - private
" # protected
sy match   pumlVisibility     "\v^[ \t]*(\+|-|#) "
" ' comment till end of line
sy region  pumlComment        start="'"     end="$"    skip="\\'"  oneline
sy region  pumlComment        start="/'"    end="'/" 
sy region  pumlStereotype     start="\V<<"  end="\V>>"             oneline

hi link pumlArrow          Operator
hi link pumlVisibility     Operator

hi link pumlComment        Comment

hi link pumlPreProcKeyword PreProc
hi link pumlStartEnd       PreProc

hi link pumlKeyword        Keyword
hi link pumlStereotype     Statement
hi link pumlOperation      Function

hi link pumlName           Type
