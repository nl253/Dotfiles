fu! s:bind_goto(key, where)
    exe 'nm <buffer> <nowait> '.a:key.' :NERDTreeFind '.a:where.'<CR>cdCD'
endf

" move to the left
nm <buffer> <nowait> h u
" move to the right
nm <buffer> <nowait> l cdCD
" vsplit
nm <buffer> <nowait> v i
" split
nm <buffer> <nowait> v s
" preview
nm <buffer> <nowait> p go
" delete
nm <buffer> <nowait> D md

call s:bind_goto('gh', '~/')
call s:bind_goto('gD', '~/Documents')
call s:bind_goto('gt', '/tmp')
call s:bind_goto('ge', '/etc')
call s:bind_goto('gr', '/')
call s:bind_goto('g/', '/')
call s:bind_goto('gm', '/mnt')
call s:bind_goto('gN', '~/Documents/Notes')
call s:bind_goto('gP', '~/Documents/Programming')
