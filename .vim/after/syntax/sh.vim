"hi link scriptingLanguage Special
"
" runtime! syntax/python.vim
" syn include @shPython syntax/python.vim
" syn region shPython start='\vpython(\d(\.\d)?)?\s*\<{2}\s*EOF' end='\v^EOF' contains=@shPython

hi shCmdSubRegion               ctermfg=DarkCyan guifg=Cyan   
hi shCommandSub                 ctermfg=None                  
hi shDerefVar       cterm=None  ctermfg=None 
hi shHereDoc        cterm=None  ctermfg=None 
hi shRange                      ctermfg=Blue                  
hi shSetList        cterm=bold  ctermfg=Blue     
hi shTestOpr                    ctermfg=blue                  
hi shVarAssign      cterm=bold               
hi shVariable       cterm=bold  ctermfg=None     ctermbg=None
hi zshPluginManager cterm=bold  gui=bold     

sy match shOperator "\v (\=\~|\={1,2}|\&{2}|!\=) "

hi link shOperator    Operator
hi link shConditional Conditional
hi link shDerefSimple PreProc
hi link shDerefVar    SpecialChar
hi link shLoop        Repeat
hi link shQuote       Quote
hi link shRedir       Special
hi link shTestOpr     SpecialChar
