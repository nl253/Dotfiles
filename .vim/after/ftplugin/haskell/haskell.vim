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
    " fallback (still pretty good)
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

function! s:gen_hasky_tags()

    " save state
    let l:save_pwd = getcwd()
    let l:shell = &shell
    setg shell=/bin/bash

    let l:stack_work_dir = finddir(".stack-work", '.;')

    if isdirectory(l:stack_work_dir)
        let l:stack_dir = fnamemodify(l:stack_work_dir, ':h')
        exe 'cd '.l:stack_dir
        let l:tags = l:stack_dir.'/tags'
    else
        exe 'cd '.expand('%:p:h') 
        let l:tags = 'tags'
    endif

    " check if stale
    if filereadable(l:tags) && (localtime() - getftime(l:tags)) > (60 * 10)
        " if so, remove
        sil call system("rm ".l:tags)
    endif

    if !filereadable(l:tags)
        if isdirectory(expand('~/.stack/indices/Hackage/packages')) && executable('tar')
            for l:archive in split(expand('~/.stack/indices/Hackage/packages/*/*/*.tar.gz'), '\n')
                " dir.tar.gz => dir
                let l:unpacked = fnamemodify(l:archive, ':r:r')
                if !isdirectory(l:unpacked)
                    " stack stores *.hs files in *.tar.gz archives
                    " this will extract them if needed
                    call system('tar xf '.l:archive.' -C '.fnamemodify(l:unpacked, ':h'))
                endif
            endfor
            " NOTE: it's important that the argv is not too long
            "       there is too many files to pass all of them to bash so `find` is
            "       used instead
            call system('ctags $(find . -name "*.hs") $(find ~/.stack/indices -name "*.hs")')
        else
            call system('ctags $(find . -name "*.hs")')
        endif
    endif

    " if this tag file is not seen by Vim
    if filereadable(l:tags) && !(&tags =~ l:tags)
        " add it 
        exe 'setl tags+='.l:tags 
    endif

    " restore state
    exe 'cd '.l:save_pwd
    exe 'setg shell='.l:shell
endf

com! HaskyTags call s:gen_hasky_tags()
