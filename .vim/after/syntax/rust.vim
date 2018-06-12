syn match rustParen "\v[)(]"
syn match rustBraces "\v[\}\{]"
syn match rustPunct "\v[;,:\.]"
syn keyword rustCollection HashSet HashMap VecDeque LinkedList BinaryHeap BTreeMap BTreeSet
hi link rustCollection Type
" hi link rustParen Comment
" hi link rustBraces Delimiter
hi link rustCommentLineDoc Comment
hi link rustModPathSep Comment
hi link rustModPath Include
hi link rustFuncName Function
hi link rustPunct Comment
hi link rustSelf Macro
hi link rustKeyword Keyword
hi link rustFuncCall Function
