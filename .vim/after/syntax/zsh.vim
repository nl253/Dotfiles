"hi zshHereDoc ctermfg=Cyan
" hi clear zshCommands 
" hi clear zshDelimiter 
" hi clear zshSubst 
hi link zshDelimiter         Repeat
hi link zshScriptingLanguage Special
hi link zshBrackets          Delimiter
hi link zshCommands          Builtin
hi link zshFunction          Function
hi link zshOperator          Operator
hi link zshPluginManager     CommentTitle
hi link zshShortDeref        SpecialChar
hi link zshSubst             Special
hi link zshSubstDelim        Operator
hi link zshEscap             Character

hi unixPath guifg=turquoise2 gui=NONE cterm=NONE
hi zshVariable cterm=bold gui=bold

let s:scriptingLanguage = [
            \ 'python',  
            \ '(ba|z|fi|t?c)?sh',  
            \ 'perl',  
            \ 'lua',  
            \ 'ruby',  
            \ '[mpg]?awk',  
            \ 'php'
            \ ]

exe 'sy match zshScriptingLanguage "\v ('.join(s:scriptingLanguage, '|').')(\d(\.\d)?)? "'
sy match zshEscap "\v\\"
