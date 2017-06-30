" <tab> / <s-tab> / <c-v><tab> | super-duper-tab
" ----------------------------------------------------------------------------
function! s:can_complete(func, prefix)
    if empty(a:func)
	return 0
    endif
    let start = call(a:func, [1, ''])
    if start < 0
	return 0
    endif

    let oline  = getline('.')
    let line   = oline[0:start-1] . oline[col('.')-1:]

    let opos   = getpos('.')
    let pos    = copy(opos)
    let pos[2] = start + 1

    call setline('.', line)
    call setpos('.', pos)
    let result = call(a:func, [0, matchstr(a:prefix, '\k\+$')])
    call setline('.', oline)
    call setpos('.', opos)

    if !empty(type(result) == type([]) ? result : result.words)
	call complete(start + 1, result)
	return 1
    endif
    return 0
endfunction

function! s:feedkeys(k)
    call feedkeys(a:k, 'n')
    return ''
endfunction

function! s:super_duper_tab(pumvisible, next)
    let [k, o] = a:next ? ["\<c-n>", "\<tab>"] : ["\<c-p>", "\<s-tab>"]
    if a:pumvisible
	return s:feedkeys(k)
    endif

    let line = getline('.')
    let col = col('.') - 2
    if line[col] !~ '\k\|[/~.]'
	return s:feedkeys(o)
    endif

    let prefix = expand(matchstr(line[0:col], '\S*$'))
    if prefix =~ '^[~/.]'
	return s:feedkeys("\<c-x>\<c-f>")
    endif
    if s:can_complete(&omnifunc, prefix) || s:can_complete(&completefunc, prefix)
	return ''
    endif
    return s:feedkeys(k)
endfunction

if has_key(g:plugs, 'ultisnips')
    " UltiSnips will be loaded only when tab is first pressed in insert mode
    if !exists(':UltiSnipsEdit')
	inoremap <silent> <Plug>(tab) <c-r>=plug#load('ultisnips')?UltiSnips#ExpandSnippet():''<cr>
	imap <tab> <Plug>(tab)
    endif

    let g:SuperTabMappingForward  = "<tab>"
    let g:SuperTabMappingBackward = "<s-tab>"
    function! SuperTab(m)
	return s:super_duper_tab(a:m == 'n' ? "\<c-n>" : "\<c-p>",
		    \ a:m == 'n' ? "\<tab>" : "\<s-tab>")
    endfunction
else
    inoremap <silent> <tab>   <c-r>=<SID>super_duper_tab(pumvisible(), 1)<cr>
    inoremap <silent> <s-tab> <c-r>=<SID>super_duper_tab(pumvisible(), 0)<cr>
endif
