let s:tw = (&textwidth ==? '' ? 80 : &textwidth)
let s:sw = (&shiftwidth ==? '' ? 2 : &shiftwidth)


compiler ghc

" https://wiki.haskell.org/Vim
setl foldmarker={-,-} tabstop=4 expandtab shiftround expandtab indentkeys-=<space>
setl softtabstop=2 shiftwidth=2 foldmethod=indent
setl omnifunc=HoogleOmniFunc

" in a stack project
if executable('stack') && isdirectory(finddir(".stack-work", "./;".$HOME))
    setl makeprg=stack\ build

    " not in a stack project but has stack
    " run as a script
elseif executable('stack')
    setl makeprg=stack\ runhaskell\ %

    " run as a script
elseif executable('runhaskell')
    setl makeprg=runhaskell\ %\  

    " if has ghc then compile and run with ghc
elseif executable('ghc')
    let &makeprg = 'X=$(mktemp -d) ; stack ghc -- -o $X/%:t:r % && $X/%:t:r'

endif

for s:i in filter(['types', 'debug', 'boolean', 'delimiters', 'more_types'], '!exists("g:hs_highlight_".v:val)')
    exec 'let g:hs_highlight_'.s:i.' = 1'
endfor

if executable('hfmt')
    setl formatprg=hfmt\ -
elseif executable('brittany')
    exe 'setl formatprg=brittany\\ --indent\\ '.s:sw.'\\ --columns\\ '.s:tw
elseif executable('hindent')
    exe 'setl formatprg=hindent\\ --sort-imports\\ --line-length\\ '.s:tw
elseif executable('stylish-haskell')
    setl formatprg=stylish-haskell
endif

nnoremap <buffer> K :silent call HoogleLookup()<CR>
