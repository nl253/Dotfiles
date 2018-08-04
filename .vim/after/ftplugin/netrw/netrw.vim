nnoremap <buffer> <nowait> q :bw!<CR>

fu! s:netrw_navigate(where)
    let l:dims = winrestcmd() 
    bw 
    exe g:netrw_slide_in_width.'Lexplore '.a:where
    exe l:dims
    unlet l:dims
endf

nn <buffer> gD :call s:netrw_navigate('~/Documents')<CR>
nn <buffer> gC :call s:netrw_navigate('~/.config')<CR>
nn <buffer> gL :call s:netrw_navigate('~/.local')<CR>
nn <buffer> gV :call s:netrw_navigate('~/Documents/Vim')<CR>
nn <buffer> gR :call s:netrw_navigate('~/Documents/Notes')<CR>
nn <buffer> gJ :call s:netrw_navigate('~/Documents/Programming/Java')<CR>
nn <buffer> g/ :call s:netrw_navigate('/')<CR>
nn <buffer> gr :call s:netrw_navigate('/')<CR>
nn <buffer> gu :call s:netrw_navigate('/usr')<CR>
nn <buffer> gm :call s:netrw_navigate('/mnt')<CR>
nn <buffer> ge :call s:netrw_navigate('/etc')<CR>
nn <buffer> gh :call s:netrw_navigate('~/')<CR>
nn <buffer> g~ :call s:netrw_navigate('~/')<CR>
nn <buffer> gt :call s:netrw_navigate('/tmp')<CR>
nmap <buffer> h <CR>-
nmap <buffer> <Space> mf
nmap <buffer> l :Ntree<CR>
