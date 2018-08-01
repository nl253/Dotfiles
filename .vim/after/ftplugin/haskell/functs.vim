if executable('stack') && isdirectory(finddir(".stack-work", "./;".$HOME))
    let g:hoogle_executable = 'stack hoogle'
elseif executable('hoogle')
    let g:hoogle_executable = 'hoogle'
else
    finish
endif

fu! HoogleLookup()
    lex system(g:hoogle_executable.' '.shellescape(expand('<cword>')))
    lop
endf

let g:hoogled_signatures = {}
let g:hoogle_blacklist = [
            \ 'where',
            \ 'let',
            \ 'class',
            \ 'then',
            \ 'module',
            \ 'qualified',
            \ 'data',
            \ 'type',
            \ 'newtype',
            \ 'case',
            \ 'import',
            \ ]

if !exists('*HoogleSignature')
    fu! HoogleSignature()
        let l:word = expand("<cword>")
        let l:try_hoogle = get(g:hoogled_signatures, l:word, 0)
        if l:try_hoogle != 0
            echom l:try_hoogle
        elseif (len(l:word) > 2) && (get(g:hoogle_blacklist, l:word, 0) == 0)
            let l:lines = filter(readfile(expand('%')), 'v:val =~? "^\s*'.l:word.' :: .*$"')
            let l:match = !empty(l:lines) ?  l:lines[0] : systemlist(g:hoogle_executable." --count=1 ".l:word." ")[0]
            let g:hoogled_signatures[l:word] = l:match
            echom l:match
        en
    endf
endif

let g:hoogled_completions = {}

if !exists('*HoogleOmniFunc')
    fu! HoogleOmniFunc(findstart, base)
        if a:findstart
            " locate the start of the word
            let l:line = getline('.')
            let l:start = col('.') - 1
            while l:start > 0 && l:line[l:start - 1] =~? '\a'
                let l:start -= 1
            endwhile
            return l:start
        elseif len(a:base) <= 2
            return []
        en

        if get(g:hoogle_blacklist, a:base, 0) != 0
            return filter(g:hoogle_blacklist, 'v:val =~? '.a:base)
        endif

        let l:try_cache = get(g:hoogled_completions, a:base, [''])

        if l:try_cache != ['']
            return l:try_cache
        endif

        let l:completions = []

        let l:no_matches = (&pumheight == '') || (&pumheight < 1) ? 12 : min([25, &pumheight])

        for l:i in systemlist(g:hoogle_executable.' --count='.l:no_matches.' '.shellescape(a:base))

            " function
            " eg Data.Sequence.Internal State :: (s -> (s, a)) -> State s a
            if l:i =~? ' :: '
                let l:focus = split(l:i, ' :: ') 
                let l:path_n_name = split(l:focus[0], ' ') 
                let l:path = join(l:path_n_name[0:-2])
                let l:name = l:path_n_name[-1]
                let l:signature = l:focus[-1]
                call add(l:completions, {
                            \ 'word': l:name, 
                            \ 'menu': l:signature,
                            \ 'kind': l:path
                            \ })


            " type alias
            " eg Control.Monad.State.Lazy type State s = StateT s Identity
            elseif l:i =~# ' type '
                let l:focus = split(l:i, ' type ') 
                let l:name = split(l:focus[1], ' ')[0]
                let l:path = l:focus[0]
                let l:info = split(l:focus[1], ' = ')[-1]

                call add(l:completions, {
                            \ 'word': l:name,
                            \ 'menu': l:path,
                            \ 'info': l:info,
                            \ 'kind':  'type'
                            \ })

            " eg Data.Semigroup newtype Sum a
            elseif l:i =~# ' newtype '
                let l:focus = split(l:i, ' newtype ') 
                let l:name = split(l:focus[1], ' ')[0]
                let l:path = l:focus[0]

                call add(l:completions, {
                            \ 'word': l:name,
                            \ 'menu': l:path,
                            \ 'kind': 'newtype'
                            \ })

            " eg Data.Aeson.Types data Parser a
            elseif l:i =~# ' data '
                let l:focus = split(l:i, ' data ') 
                let l:name = split(l:focus[1], ' ')[0]
                let l:path = l:focus[0]

                call add(l:completions, {
                            \ 'word': l:name,
                            \ 'menu': l:path,
                            \ 'kind': 'data'
                            \ })

            " eg Data.Functor.Compat class Functor (f :: * -> *)
            " eg Distribution.Compat.Prelude.Internal class Applicative m => Monad (m :: * -> *)
            elseif l:i =~# ' class '

                let l:focus = split(l:i, ' class ') 
                let l:name = l:focus[1]
                let l:path = l:focus[0]

                call add(l:completions, {
                            \ 'word' : l:name,
                            \ 'menu' : l:path,
                            \ 'kind':  'class'
                            \ })

            elseif l:i =~# '^module '

                let l:path = split(l:i, 'module ')[0]
                let l:name = split(l:path, '\.')[-1]

                call add(l:completions, {
                            \ 'word' : l:name,
                            \ 'menu' : l:path,
                            \ 'kind':  'module'
                            \ })

            elseif l:i =~# '^package '
                continue
            endif
        endfor

        for l:file in split(expand('*.'.expand('%:t:e')), '\n')
            for l:line in filter(readfile(l:file), "v:val =~? '^\s*".a:base.".* :: .*'")
                let l:focus = split(l:line, ' :: ')
                let l:name = l:focus[0]
                let l:signature = l:focus[-1]
                call insert(l:completions, {
                            \ 'word': l:name, 
                            \ 'info': l:file,
                            \ 'menu': l:signature
                            \ }, 0)
            endfor
        endfor

        let g:hoogled_completions[a:base] = l:completions

        call timer_start(20, { -> remove(g:hoogled_completions, a:base)})

        return l:completions
    endf
endif
