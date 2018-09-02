syn region helpQuotedText start=+"+ end=+\v"|\n+ oneline
syn match  helpVimKeyB    "\v\<[-a-zA-Z]+\>[a-zA-Z]*"
syn match  helpModeLine   "\vvim(: ?[a-z]+\=\S+)+"
syn match  helpVimVar     "\v[gbls]:[a-zA-Z_#]{3,}"
syn match  helpVimCommand "\v([\t ]+|^):[A-Z][a-zA-Z\[\]]{3,} ?" 
" syn match helpHR "\v^[-=*]{5,}$"

hi helpVim          guifg=orange3      ctermfg=DarkYellow cterm=bold 
hi helpSectionDelim guifg=Magenta4     ctermfg=Magenta    cterm=bold gui=bold
hi helpVimKeyB      guifg=orange3      ctermfg=DarkYellow cterm=bold
hi helpQuotedText   guifg=SpringGreen4 ctermfg=DarkGreen
" hi helpHR guifg=Magenta4 ctermfg=Magenta cterm=bold gui=bold
" hi helpHeading2 guifg=Cyan4 gui=bold cterm=bold ctermfg=Cyan
" hi helpHeading3 guifg=purple ctermfg=DarkMagenta
" hi helpHeadline gui=bold cterm=bold ctermfg=DarkYellow guifg=DarkOrange3
hi link helpHeadline      Title
hi link helpHyperTextJump URI
hi link helpVimCommand    Special
hi link helpVimVar        Boolean
hi link helpModeLine      Comment
" hi def link helpHeading1 Special
" hi def link helpQuotedText SpecialComment
