hi clear VimwikiLink 

sy match VimwikiDefListDelim "\v(^::)|(::$)"
sy match imortant "\v\C\<CNOTE:?"
sy region betweenParenth start="(" end=")"

" hi link VimwikiMarkers Comment
hi VimwikiHeader1 ctermfg=DarkMagenta guifg=magenta     cterm=bold gui=bold
hi VimwikiHeader2 ctermfg=Cyan        guifg=SpringGreen cterm=bold gui=bold 
hi VimwikiHeader3 ctermfg=Green       guifg=SpringGreen cterm=bold gui=bold
hi VimwikiHeader4 ctermfg=DarkMagenta guifg=DarkOrange  cterm=bold gui=bold
hi VimwikiLink    ctermfg=DarkCyan    guifg=DarkOrchid  cterm=bold gui=bold

hi link VimwikiDefListDelim Delimiter
hi link VimwikiListTodo     Repeat
hi link VimwikiMarkers      Comment
hi link betweenParenth      Comment
