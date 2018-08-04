let b:match_words = '\<if\>:\<elseif\>:\<else\>'

setl tags+=~/.vim/tags complete+=k~/.vimrc tabstop=4 shiftwidth=4 foldmethod=syntax keywordprg=:help

let g:vimsyn_embed = 'P'
let g:vimsyn_folding = 'afP'

call tags#lib(99999, 0, '~/.vim', $VIM.'/runtime')
