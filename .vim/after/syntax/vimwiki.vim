" hi link VimwikiMarkers Comment
hi VimwikiHeader1 ctermfg=DarkMagenta guifg=magenta cterm=bold gui=bold
hi VimwikiHeader2 ctermfg=Cyan guifg=SpringGreen cterm=bold gui=bold ctermfg=Cyan
hi VimwikiHeader3 ctermfg=Green guifg=SpringGreen cterm=bold gui=bold
hi VimwikiHeader4 ctermfg=DarkMagenta guifg=DarkOrange ctermfg=DarkYellow cterm=bold gui=bold
hi VimwikiLink cterm=bold guifg=DarkOrchid ctermfg=DarkCyan gui=bold
hi clear VimwikiLink 
hi link VimwikiDefListDelim  Delimiter
hi link VimwikiListTodo Repeat
hi link VimwikiMarkers Comment
hi link betweenParenth Comment
syn match VimwikiDefListDelim "\v(^::)|(::$)"
syn match imortant "\v\C\<CNOTE:?"
syn region betweenParenth start="(" end=")"
