if executable('shfmt')
    setl formatprg=shfmt\ -ci\ -i\ 2
endif

if executable("bash")
    setl makeprg=bash\ %
endif

setl shiftwidth=2 tabstop=2 foldmethod=marker foldlevel=4 foldmarker={,} omnifunc=BashOmniFunc

" SHELL - built-in
let g:readline_has_bash = 1 
let g:is_bash = 1
" let g:sh_fold_enabled = 4

let b:match_words = '\<if\>:\<elif\>:\<else\>'
