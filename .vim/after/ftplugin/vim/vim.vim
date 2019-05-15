let b:match_words = '\<if\>:\<elseif\>:\<else\>'

setl tags+=~/.vim/tags complete+=k~/.vimrc tabstop=4 shiftwidth=4 iskeyword+=: iskeyword+=& iskeyword+=$ path+=~/.vim/plugin keywordprg=:help foldmethod=syntax

let g:vimsyn_embed = 'P'
let g:vimsyn_folding = 'afP'

" exe 'CtagsLib ~/.vim '.$VIM.'/runtime'
" Ctags
