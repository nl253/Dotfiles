runtime! syntax/json.vim
runtime! syntax/html.vim

sy match httpHeaderKey '\v^[A-Z][^:]+'
sy match httpHeaderVal '\v:[^:]+$'
sy region httpHeaderComment start='^\s*(#|//)' end="$" oneline 
hi link httpHeaderKey Statement
hi link httpHeaderVal Normal
hi link httpHeaderComment Comment
