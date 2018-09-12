if exists('b:current_syntax') 
    if b:current_syntax ==# 'ctags'
        finish
    endif
else
    let b:current_syntax = 'ctags' 
endif

runtime! syntax/regex.vim

sy region ctagsComment start='^#' end='$' oneline contains=@Spell
sy match  ctagsLongOpt '\v^--[a-zA-Z0-9]+(-[a-zA-Z0-9]+)*(\=\S+)?' contains=ctagsRegex
sy match  ctagsShortOpt '\v^-[a-zA-Z0-9](\=\S+)?'
sy region ctagsRegex   start='\v^--regex-[^=]+\=/'ms=s+1 end='$' contains=@regexAll oneline contained

hi def link ctagsLongOpt Normal
hi def link ctagsShortOpt Normal
hi def link ctagsComment Comment

