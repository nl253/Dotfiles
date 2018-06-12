fu! iabbrs#iab_init(ftype)
    exe 'aug AwesomeAbbreviationsFor'.toupper(a:ftype[0]).a:ftype[1:]
        au!
        exe 'au Filetype '.a:ftype.' call s:_wrapper()'
    aug END
endf

fu! s:_wrapper()
    if exists('g:iab_nouns')
        call s:_iab_nouns(g:iab_nouns)
        let l:capped = map(g:iab_nouns, "s:_capitalise_pair(v:val)")
        call s:_iab_nouns(l:capped)
    endif
    if exists('g:iab_verbs')
        call s:_iab_verbs(g:iab_verbs)
        let l:capped = map(g:iab_verbs, "s:_capitalise_pair(v:val)")
        call s:_iab_verbs(l:capped)
    endif
    if exists('g:iab_adjectives')
        call s:_iab_adjectives(g:iab_adjectives)
        let l:capped = map(g:iab_adjectives, "s:_capitalise_pair(v:val)")
        call s:_iab_adjectives(l:capped)
    endif
    if exists('g:iab_other')
        call s:_iab_other(g:iab_other)
        let l:capped = map(g:iab_other, "s:_capitalise_pair(v:val)")
        call s:_iab_other(l:capped)
    endif
endf

fu! s:_capitalise_pair(pair)
    return [toupper(a:pair[0][0]).a:pair[0][1:],  toupper(a:pair[1][0]).a:pair[1][1:]]
endf

fu! s:_iab_nouns(pairs)
    for l:n in a:pairs
        exe 'iab <buffer> '.l:n[0].' '.l:n[1]
        if l:n[0] =~? '[so]$'
            exe 'iab <buffer> '.l:n[0].'es '.l:n[1].'es'
        elseif l:n[0] =~? 'y$'
            exe 'iab <buffer> '.l:n[0][:-2].'ies '.l:n[1][:-2].'ies'
        else
            exe 'iab <buffer> '.l:n[0].'s '.l:n[1].'s'
        endif
    endfor
endf

fu! s:_iab_other(pairs)
    for l:o in a:pairs
        exe 'iab <buffer> '.l:o[0].' '.l:o[1]
    endfor
endf

fu! s:_iab_verbs(pairs)
    for l:v in a:pairs
        exe 'iab '.l:v[0].' '.l:v[1]
        if l:v[0] =~# 'e$'
            exe 'iab <buffer> '.l:v[0][:-2].'ing '.l:v[1][:-2].'ing'
            exe 'iab <buffer> '.l:v[0].'d '.l:v[1].'d'
            exe 'iab <buffer> '.l:v[0].'s '.l:v[1].'s'
            exe 'iab <buffer> '.l:v[0].'r '.l:v[1].'r'
        else
            exe 'iab <buffer> '.l:v[0].'ing '.l:v[1].'ing'
            exe 'iab <buffer> '.l:v[0].'es '.l:v[1].'es'
            exe 'iab <buffer> '.l:v[0].'er '.l:v[1].'er'
            exe 'iab <buffer> '.l:v[0].'es '.l:v[1].'es'
        endif
    endfor
endf

fu! s:_iab_adjectives(pairs)
    for l:a in a:pairs
        exe 'iab <buffer> '.l:a[0].' '.l:a[1]
        exe substitute('iab '.l:a[0].'lly '.l:a[1].'lly', 'lll', 'll', 'g')
        exe 'iab <buffer> '.l:a[0].'ly '.l:a[1].'ly'
    endfor
endf
