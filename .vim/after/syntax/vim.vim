
" OVERRIDE:
hi link vimNotation     Macro
hi link vimVar          Variable
hi link vimSetEqual     Operator
hi link vimCommentTitle Identifier
hi link vimCommand      Statement
hi link vimFuncKey      Function
hi link vimIsCommand    Identifier
hi link vimFunctName    Identifier
hi link vimNotFunc      Conditional
hi link vimCmdSep       Operator
hi link vimMapModKey    Macro
" hi link vimBracket Delimiter

" OWN:
syn keyword vimRepeat        for endfor while endwhile try catch endtry endfo wh endwh endwhi endwhil con continue brea break
syn keyword vimTry           try catch endtry endt endtr finally final finall finally throw thr thro
syn keyword vimPluginManager Plug Bundle NeoBundleFetch
syn keyword vimConditional   if else endif elseif elsei el end endi
hi link vimRepeat        Repeat
hi link vimTry           Function
hi link vimConditional   Conditional
hi link vimLet           TypeDef
hi link vimPluginManager CommentTitle
