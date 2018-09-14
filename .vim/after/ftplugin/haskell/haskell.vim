compiler ghc

" From <https://wiki.haskell.org/Vim>
call opts#safe_setl([
            \ 'foldmarker={-,-}',
            \ 'tabstop=4',
            \ 'expandtab',
            \ 'shiftround',
            \ 'expandtab',
            \ 'softtabstop=2',
            \ 'shiftwidth=2',
            \ 'foldmethod=indent',
            \ 'indentkeys-=<space>',
            \ 'cinwords=where,let,in,do,of',
            \ ])

let s:anchors = ['.stack-work', '.git']
let s:tw = (&textwidth ==? '' ? 80 : &textwidth)
let s:sw = (&shiftwidth ==? '' ?  2 : &shiftwidth)

call opts#omni(['HoogleOmniFunc', 'syntaxcomplete#Complete'])
nn <buffer> K :silent call HoogleLookup()<CR>

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
    exe 'setl makeprg='.escape('__X=$(mktemp -d) && stack ghc -- -o $__X/%:t:r % && $__X/%:t:r', ' ')
endif

call opts#letg_all(map([
            \ 'types',
            \ 'debug',
            \ 'boolean',
            \ 'delimiters',
            \ 'more_types',
            \ ], '"hs_highlight_".v:val'))

call opts#formatprg({
            \ 'hfmt': 'hfmt -',
            \ 'brittany': 'brittany --indent '.s:sw.' --columns '.s:tw,
            \ 'hindent': 'hindent --sort-imports --line-length '.s:tw,
            \ 'stylish-haskell': 'stylish-haskell',
            \ })

" NOTE: you will have to untar them
CtagsLib ~/.stack/indices/Hackage/packages
for s:cmd in ['ProjectFiles', 'Ctags']
    exe s:cmd.' '.join(s:anchors, ' ')
endfor
