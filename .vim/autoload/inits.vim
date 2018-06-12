fu! inits#netrw_navigate(where)
    let l:dims = winrestcmd() 
    bw 
    exe g:netrw_slide_in_width.'Lexplore '.a:where
    exe l:dims
    unlet l:dims
endf

fu! inits#netrw_init()
    nn <buffer> gD :call inits#netrw_navigate('~/Documents')<CR>
    nn <buffer> gC :call inits#netrw_navigate('~/.config')<CR>
    nn <buffer> gL :call inits#netrw_navigate('~/.local')<CR>
    nn <buffer> gV :call inits#netrw_navigate('~/Documents/Vim')<CR>
    nn <buffer> gR :call inits#netrw_navigate('~/Documents/Notes')<CR>
    nn <buffer> gJ :call inits#netrw_navigate('~/Documents/Programming/Java')<CR>
    nn <buffer> g/ :call inits#netrw_navigate('/')<CR>
    nn <buffer> gr :call inits#netrw_navigate('/')<CR>
    nn <buffer> gu :call inits#netrw_navigate('/usr')<CR>
    nn <buffer> gm :call inits#netrw_navigate('/mnt')<CR>
    nn <buffer> ge :call inits#netrw_navigate('/etc')<CR>
    nn <buffer> gh :call inits#netrw_navigate('~/')<CR>
    nn <buffer> g~ :call inits#netrw_navigate('~/')<CR>
    nn <buffer> gt :call inits#netrw_navigate('/tmp')<CR>
    nmap <buffer> h <CR>-
    nmap <buffer> <Space> mf
    nmap <buffer> l :Ntree<CR>
endf

fu! inits#markdown_init() 
    nn <buffer> K     :exe '!wn '.expand('<cword>').' -over \| fold --spaces --width='.(join(systemlist("tput cols"), '') - 5).' \| head -n '.(join(systemlist('tput lines'), '') - 5)<CR>
    nn @b viw<Esc>a**<Esc>hbi**<Esc>ll
    nn @e viw<Esc>a*<Esc>hbi*<Esc>ll
endfunction
