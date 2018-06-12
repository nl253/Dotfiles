" CORE:
hi helpVim ctermfg=DarkYellow cterm=bold guifg=orange3
" syn match helpHR "\v^[-=*]{5,}$"
" hi helpHR guifg=Magenta4 ctermfg=Magenta cterm=bold gui=bold
hi helpSectionDelim guifg=Magenta4 ctermfg=Magenta cterm=bold gui=bold

" HEADINGS:
" hi link helpHeading1 Special
" hi helpHeading2 guifg=Cyan4 gui=bold cterm=bold ctermfg=Cyan
" hi helpHeading3 guifg=purple ctermfg=DarkMagenta

" hi helpHeadline gui=bold cterm=bold ctermfg=DarkYellow guifg=DarkOrange3
hi link helpHeadline Title
hi link helpHyperTextJump URI

syn match helpVimCommand "\v([\t ]+|^):[A-Z][a-zA-Z\[\]]{3,} ?" 
hi link helpVimCommand Special

syn match helpVimVar "\v[gbls]:[a-zA-Z_#]{3,}"
hi link helpVimVar Boolean

syn match helpModeLine "\vvim(: ?[a-z]+\=\S+)+"
hi link helpModeLine Comment

syn match helpVimKeyB "\v\<[-a-zA-Z]+\>[a-zA-Z]*"
hi helpVimKeyB ctermfg=DarkYellow cterm=bold guifg=orange3

syn region helpQuotedText start=+"+ end=+\v"|\n+ oneline
" hi link helpQuotedText SpecialComment
hi helpQuotedText guifg=SpringGreen4 ctermfg=DarkGreen
