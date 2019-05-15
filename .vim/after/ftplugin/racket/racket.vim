setl sw=2 ts=4 expandtab foldmethod=indent

for s:pair in items({'scmindent': 'scmindent'})
    if executable(s:pair[0])
        exe 'setl formatprg='.s:pair[1]
        break
    endif
endfor
for s:pair in items({'racket': 'racket %'})
    if executable(s:pair[0])
        exe 'setl makeprg='.escape(s:pair[1], ' ')
        break
    endif
endfor

Ctags
" CtagsLib! /usr/share/racket/collects/racket
