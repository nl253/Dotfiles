"
" Helper function.
"
" Runs binary search on a list of consonants.
"
fu! s:_is_consonant(letter) 
    let l:consonants = ['b', 'c', 'd', 'f', 'g', 'h', 'j', 'k', 'l', 'm', 'n', 'p', 'q', 'r', '\s', 't', 'v', 'x', 'z']
    return s:_bsearch(l:consonants, a:letter)
endf

" Binary search. 
" 
" Time complexity: O(log n). 
"
" NOTE: the list must be sorted.
"
" This is a workaround because VimScript doesn't have sets so there is no
" proper lookup data structure.
"
fu! s:_bsearch(list, val)

    if empty(a:list)
        return 0
    endif

    let l:middle_idx = len(a:list) / 2

    let l:middle_val = a:list[l:middle_idx]

    " found !
    if l:middle_val == a:val 
        return 1

    " NOTE: VimScript slices lists inclusively

    " search left half
    " exclude middle value (checked above)
    elseif a:val < l:middle_val
        if (l:middle_idx - 1) < 0 
            return 0
        else
            return s:_bsearch(a:list[:(l:middle_idx - 1)], a:val)
        endif

    " search right half
    " exclude middle value (checked above)
    else 
        return s:_bsearch(a:list[(l:middle_idx + 1):], a:val)
    endif
endf

" Create an auto command for a file type so that abbreviations for insert mode
" are generated on opening a file. 
"
fu! iabbrs#iab_init(ftype)
    exe 'aug AwesomeAbbreviationsFor'.toupper(a:ftype[0]).a:ftype[1:]
        au!
        exe 'au Filetype '.a:ftype.' call s:_initialise()'
    aug END
endf

" Read word pairs from ~/.vim/iabbrs/{adjectives,nouns,verbs,other}.
"
fu! s:_initialise()

    if filereadable(expand("~/.vim/iabbrs/adjectives"))
        call s:_iab_adjectives(map(map(readfile(expand("~/.vim/iabbrs/adjectives")), 'substitute(v:val, "\\(\\S\\+\\)\\s\\+\\(.*\\)", "[\"\\1\", \"\\2\"]", "")'), 'eval(v:val)'))
        call s:_iab_adjectives(map(map(map(readfile(expand("~/.vim/iabbrs/adjectives")), 'substitute(v:val, "\\(\\S\\+\\)\\s\\+\\(.*\\)", "[\"\\1\", \"\\2\"]", "")'), 'eval(v:val)'), "s:_capitalise_pair(v:val)"))
    endif

    if filereadable(expand("~/.vim/iabbrs/nouns"))
        call s:_iab_nouns(map(map(readfile(expand("~/.vim/iabbrs/nouns")), 'substitute(v:val, "\\(\\S\\+\\)\\s\\+\\(.*\\)", "[\"\\1\", \"\\2\"]", "")'), 'eval(v:val)'))
        call s:_iab_nouns(map(map(map(readfile(expand("~/.vim/iabbrs/nouns")), 'substitute(v:val, "\\(\\S\\+\\)\\s\\+\\(.*\\)", "[\"\\1\", \"\\2\"]", "")'), 'eval(v:val)'), "s:_capitalise_pair(v:val)"))
    endif

    if filereadable(expand("~/.vim/iabbrs/verbs"))
        call s:_iab_verbs(map(map(readfile(expand("~/.vim/iabbrs/verbs")), 'substitute(v:val, "\\(\\S\\+\\)\\s\\+\\(.*\\)", "[\"\\1\", \"\\2\"]", "")'), 'eval(v:val)'))
        call s:_iab_verbs(map(map(map(readfile(expand("~/.vim/iabbrs/verbs")), 'substitute(v:val, "\\(\\S\\+\\)\\s\\+\\(.*\\)", "[\"\\1\", \"\\2\"]", "")'), 'eval(v:val)'), "s:_capitalise_pair(v:val)"))
    endif

    if filereadable(expand("~/.vim/iabbrs/other"))
        call s:_iab_other(map(map(readfile(expand("~/.vim/iabbrs/other")), 'substitute(v:val, "\\(\\S\\+\\)\\s\\+\\(.*\\)", "[\"\\1\", \"\\2\"]", "")'), 'eval(v:val)'))
        call s:_iab_other(map(map(map(readfile(expand("~/.vim/iabbrs/other")), 'substitute(v:val, "\\(\\S\\+\\)\\s\\+\\(.*\\)", "[\"\\1\", \"\\2\"]", "")'), 'eval(v:val)'), "s:_capitalise_pair(v:val)"))
    endif
endf

" capitalise a list of len 2:
"
"   ['fst', 'snd'] => ['Fst', 'Snd']
"
fu! s:_capitalise_pair(pair)
    return [toupper(a:pair[0][0]).a:pair[0][1:],  toupper(a:pair[1][0]).a:pair[1][1:]]
endf

" Generate abbreviations for insert mode.
"
" 'pairs' is a list of pairs
"
"   where 
"
"     fst = badly spelled noun in singular form
"     snd = correct spelling of the noun
"
fu! s:_iab_nouns(pairs)

    for l:pair in a:pairs

        let l:bad  = l:pair[0]
        let l:good = l:pair[1]

        " SINGULAR FORM:

        exe 'iab <buffer> '.l:bad.' '.l:good

        " PLURAL FORM:
        "
        " if the nouns ends with any of:
        "
        "   => 's'  
        "
        "   => 'ss' 
        "
        "   => 'z'
        "
        "   => 'sh' 
        "
        "   => 'x'
        "
        "   => 'ch' 
        "
        " then pluralise by appending 'es'
        "
        "   eg.: a mess -> many messes
        "
        if l:bad =~? '(s[sh]|ch|x|z)?$'
            exe 'iab <buffer> '.l:bad.'es '.l:good.'es'
        "
        " if the nouns ends with a 'y' and the letter BEFORE 'y' is a
        " consonant (see s:_is_consonant() above) then pluralise 
        " by appending 'ies'
        "
        "   e.g.: a fly -> many flies
        "
        elseif (l:bad =~? 'y$') && (s:_is_consonant(l:bad[-2]))
            exe 'iab <buffer> '.l:bad[:-2].'ies '.l:good[:-2].'ies'
        "
        " otherwise append 's'
        "
        "   e.g.: a car -> many cars
        "
        else
            exe 'iab <buffer> '.l:bad.'s '.l:good.'s'
        endif
    endfor
endf

fu! s:_iab_other(pairs)
    for l:o in a:pairs
        exe 'iab <buffer> '.l:o[0].' '.l:o[1]
    endfor
endf

" Generate abbreviations for insert mode.
"
" 'pairs' is a list of pairs
"
"   where 
"
"     fst = badly spelled verb
"     snd = correct spelling of the verb
"
fu! s:_iab_verbs(pairs)

    for l:pair in a:pairs

        let l:bad  = l:pair[0]
        let l:good = l:pair[1]

        " BASIC FORM:

        exe 'iab '.l:bad.' '.l:good

        " OTHER FORMS:

        " handle verbs ending with 'e' differently
        "
        if l:bad =~# 'e$'
            "
            " gerund form ('ing' suffix)
            "
            "   e.g.: snore => snoring
            "
            exe 'iab <buffer> '.l:bad[:-2].'ing '.l:good[:-2].'ing'
            "  
            " past tense:
            "
            "   e.g.: snore => snored
            "
            exe 'iab <buffer> '.l:bad.'d '.l:good.'d'
            " 
            "   e.g.: snore => snores
            "
            exe 'iab <buffer> '.l:bad.'s '.l:good.'s'
            " 
            " verb to noun:
            "
            "   e.g.: snore => snorer (someone who snores ;))
            "
            exe 'iab <buffer> '.l:bad.'r '.l:good.'r'
        else
            "
            " verb to gerund form
            "
            "   e.g.: walk => walking
            "
            exe 'iab <buffer> '.l:bad.'ing '.l:good.'ing'
            "
            "   e.g.: walk => walks
            "
            exe 'iab <buffer> '.l:bad.'s '.l:good.'s'
            "
            " verb to noun
            "
            "   e.g.: walk => walker
            "
            exe 'iab <buffer> '.l:bad.'er '.l:good.'er'
        endif
    endfor
endf

" Generate abbreviations for insert mode.
"
" 'pairs' is a list of pairs
"
"   where 
"
"     fst = badly spelled adjective
"     snd = correct spelling of the adjective
"
fu! s:_iab_adjectives(pairs)

    for l:pair in a:pairs

        let l:bad  = l:pair[0]
        let l:good = l:pair[1]

        " BASIC FORM:
        exe 'iab <buffer> '.l:bad.' '.l:good

        "
        " ADJECTIVE TO ADVERB:
        "
        if l:bad =~# 'y'
            "
            " if the adjective ends in 'y'
            " replace the 'y' with 'i' and append 'ly'
            "
            "  e.g.: easy => easily
            "
            exe 'iab '.l:bad[:-2].'ily '.l:good[:-2].'ily'

        elseif l:bad =~# 'le'
            "
            " if the adjective ends in any of:
            "
            "   => 'able' 
            "   => 'ible'
            "   => 'le'
            "
            " replace the 'e' with 'y'
            "
            "   e.g.: probable => probably
            "
            exe 'iab '.l:bad[:-2].'y '.l:good[:-2].'y'

        elseif l:bad =~# 'ic'
            "
            " if the adjective ends in 'ic', append 'ally'
            "
            " Exception: public => publicly
            "
            "   e.g.: basic => basically
            "
            exe 'iab '.l:bad.'ally '.l:good.'ally'

        else 
            " otherwise simply append 'ly' 
            "
            "   e.g.: fine => finely
            "
            exe 'iab <buffer> '.l:bad.'ly '.l:good.'ly'
        endif
    endfor
endf
