let b:match_words = '\<if\>:\<elseif\>:\<else\>'

setl tags+=~/.vim/tags complete+=k~/.vimrc

let g:vimsyn_embed = 'P'
let g:vimsyn_folding = 'afP'

setl tabstop=4 shiftwidth=4 foldmethod=syntax keywordprg=:help

let s:vim_dir = expand('~/.vim')

if isdirectory(s:vim_dir) && !filereadable(s:vim_dir.'/tags') && executable("ctags") && has('unix')
    silent call system("cd ~/.vim && ctags -R") 
endif

