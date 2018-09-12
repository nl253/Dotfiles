fu! emacs#c_cut_till_end()
    if getcmdpos() == 1
        let g:__modifiled_cmdline = ''
    else
        let g:__modifiled_cmdline = getcmdline()[:getcmdpos() - 2]
    endif
endf

" Correct spelling of the current word *without* moving the cursor.
fu! emacs#i_correct_spelling_err()
    let l:col       = virtcol('.') - 1
    let l:bad_word  = expand('<cword>')
    let l:good_word = spellsuggest(l:bad_word)[0]
    exe 'normal ciw'.l:good_word
    normal 0
    exe 'norm '.l:col.'l'
    let l:len_diff = len(l:good_word) - len(l:bad_word)  
    if l:len_diff > 0
        exe 'norm '.l:len_diff.'l'
    elseif l:len_diff < 0
        exe 'norm '.abs(l:len_diff).'h'
    endif
endf

" FIXME emacs#i_transpose_words()
fu! emacs#i_transpose_words()
    norm! viWE"xdi=join(reverse(split(@x, ' ')), ' ')
endf

" Indices are INCLUSIVE
" getcmdpos wrongly gives pos + 1 
fu! emacs#c_navigate_r(pat)
    let l:pos = getcmdpos() - 1
    let l:line = getcmdline()
    call setcmdpos(1 + match(l:line, a:pat, l:pos + 1))
endf

" @param {char[]} chars
fu! emacs#c_navigate_l(chars)
    " getcmdpos over reports by 1 (so - 1)
    " search from the next pos (another - 1)
    " but don't reach negative indices because they wrap around
    " remember that Vim slices inclusively
    let l:end = getcmdpos() - 2
    " slice cmd line from start till your current position
    let l:slice = getcmdline()[ : l:end >= 0 ? l:end : 0]
    let l:best = 0
    " find a match that moves you to the left by the smallest amount
    for l:c in a:chars
        let l:candidate = strridx(l:slice, l:c)
        if l:candidate > l:best
            let l:best = l:candidate
        endif
    endfor
    return setcmdpos(l:best + 1)
endf

fu! emacs#c_transpose()
  let pos = getcmdpos()
  if getcmdtype() =~# '[?/]'
    return "\<C-T>"
  elseif pos > strlen(getcmdline())
    let pre = "\<Left>"
    let pos -= 1
  elseif pos <= 1
    let pre = "\<Right>"
    let pos += 1
  else
    let pre = ""
  endif
  return pre . "\<BS>\<Right>".matchstr(getcmdline()[0 : pos - 2], '.$')
endf

fu! emacs#i_del_space_between_words()
 while getline(".")[getpos(".")[2] - 1:] =~ '\v^\s'
        norm x
    endwhile
    while getline(".")[:getpos(".")[2] - 1] =~ '\v\s$'
        norm i<BS>
    endwhile
endf
