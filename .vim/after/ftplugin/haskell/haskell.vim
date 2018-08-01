let s:tw = (&textwidth ==? '' ? 80 : &textwidth)
let s:sw = (&shiftwidth ==? '' ? 2 : &shiftwidth)

compiler ghc

" https://wiki.haskell.org/Vim
setl foldmarker={-,-} tabstop=4 expandtab shiftround expandtab 
setl softtabstop=2 shiftwidth=2 foldmethod=indent indentkeys-=<space>

if exists('*HoogleOmniFunc')
    setl omnifunc=HoogleOmniFunc 
    nnoremap <buffer> K :silent call HoogleLookup()<CR>
else
    " fallback (still very good)
    setl omnifunc=syntaxcomplete#Complete
endif

" + has stack
" + in a stack project
" => run as stack project
if executable('stack') && isdirectory(finddir(".stack-work", "./;".$HOME))
    exe 'setl makeprg='.escape('stack build && stack exec %:p:h:h:t', ' ')

" + not in a stack project but has stack
" + shebang present
" => run as a script
elseif executable('stack') && (getline(1) =~# '\v.*/bin/env.*stack\s*$')
    exe 'setl makeprg='.escape('stack runhaskell %', ' ')

" + no stack but runhaskell is present (bundled with GHC) 
" + shebang present
" => run as a script
elseif executable('runhaskell') && (getline(1) =~# '\v.*/bin/env.*stack\s*$')
    exe 'setl makeprg='.escape('runhaskell %', ' ')

" + has GHC 
" => compile and run with ghc
elseif executable('ghc')
    let &makeprg = 'X=$(mktemp -d) ; stack ghc -- -o $X/%:t:r % && $X/%:t:r'
endif

for s:i in filter(['types', 'debug', 'boolean', 'delimiters', 'more_types'], '!exists("g:hs_highlight_".v:val)')
    exec 'let g:hs_highlight_'.s:i.' = 1'
endfor

for s:pair in [ 
            \ ['hfmt',            'hfmt -'],
            \ ['brittany',        'brittany --indent '.s:sw.' --columns '.s:tw],
            \ ['hindent',         'hindent --sort-imports --line-length '.s:tw],
            \ ['stylish-haskell', 'stylish-haskell'],
            \ ]
    let s:bin = s:pair[0]
    let s:cmd = s:pair[1]
    if executable(s:bin)
        exe 'setl formatprg='.escape(s:cmd, ' ')
        break
    endif
endfor

