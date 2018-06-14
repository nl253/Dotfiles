if executable('autopep8') && executable('pycodestyle')
    setl formatprg=autopep8\ -
elseif executable('yapf') 
    setl formatprg=yapf
endif

setl shiftwidth=4 tabstop=4 expandtab foldmethod=expr complete-=k formatoptions=cqjonl1 

let b:match_words = '\<if\>:\<elif\>:\<else\>'
