hi def link racketShebang PreProc
sy region racketShebang start='\v%1l^#!' end='$' oneline

finish

" sy region racketParen0           matchgroup=hlLevel0 start="`\=(" end=")" skip="|.\{-}|" contains=@racketListCluster,racketParen1
" sy region racketParen1 contained matchgroup=hlLevel1 start="`\=(" end=")" skip="|.\{-}|" contains=@racketListCluster,racketParen2
" sy region racketParen2 contained matchgroup=hlLevel2 start="`\=(" end=")" skip="|.\{-}|" contains=@racketListCluster,racketParen3
" sy region racketParen3 contained matchgroup=hlLevel3 start="`\=(" end=")" skip="|.\{-}|" contains=@racketListCluster,racketParen4
" sy region racketParen4 contained matchgroup=hlLevel4 start="`\=(" end=")" skip="|.\{-}|" contains=@racketListCluster,racketParen5
" sy region racketParen5 contained matchgroup=hlLevel5 start="`\=(" end=")" skip="|.\{-}|" contains=@racketListCluster,racketParen6
" sy region racketParen6 contained matchgroup=hlLevel6 start="`\=(" end=")" skip="|.\{-}|" contains=@racketListCluster,racketParen7
" sy region racketParen7 contained matchgroup=hlLevel7 start="`\=(" end=")" skip="|.\{-}|" contains=@racketListCluster,racketParen8
" sy region racketParen8 contained matchgroup=hlLevel8 start="`\=(" end=")" skip="|.\{-}|" contains=@racketListCluster,racketParen9
" sy region racketParen9 contained matchgroup=hlLevel9 start="`\=(" end=")" skip="|.\{-}|" contains=@racketListCluster,racketParen0

" sy cluster racketCommentGroup contains=racketTodo,@Spell
" sy match   racketConcat		  "\s\.\s"
" sy match   racketParenError	  ")"
" sy keyword racketTodo		  contained todo todo:

" sy match racketLeadWhite		 contained "^\s\+"
" sy match racketQuotedBarSymbol	  !'|..\{-}|! contains=lispAtomMark
" sy cluster racketListCluster	 contains=@racketBaseListCluster,racketString,racketInString,racketInStringString
" sy cluster racketQuotedCluster	 contains=racketQuotedBarSymbol,racketQuotedList,racketQuotedNmbr0,racketComment,racketSyntax,racketFunc,racketLeadWhite
" sy cluster racketBaseListCluster contains=racketQuoted,racketQuotedBarSymbol,racketQuotedMark,racketBQList,racketBarSymbol,racketComment,racketConcat,racketSyntax,racketFunc,racketList,racketNumber,racketUnquote,racketLeadWhite

" hi def hlLevel0 ctermfg=red         guifg=red1
" hi def hlLevel1 ctermfg=yellow      guifg=orange1
" hi def hlLevel2 ctermfg=green       guifg=yellow1
" hi def hlLevel3 ctermfg=cyan        guifg=greenyellow
" hi def hlLevel4 ctermfg=magenta     guifg=green1
" hi def hlLevel5 ctermfg=red         guifg=springgreen1
" hi def hlLevel6 ctermfg=yellow      guifg=cyan1
" hi def hlLevel7 ctermfg=green       guifg=slateblue1
" hi def hlLevel8 ctermfg=cyan        guifg=magenta1
" hi def hlLevel9 ctermfg=magenta     guifg=purple1

