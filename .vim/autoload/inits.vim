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

fu! inits#nerdtree_bind_goto(key, where)
    exe 'nm <buffer> <nowait> '.a:key.' :NERDTreeFind '.a:where.'<CR>cdCD'
endf

fu! inits#nerdtree_init() 
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
    call inits#nerdtree_bind_goto('gh', '~/')
    call inits#nerdtree_bind_goto('gD', '~/Documents')
    call inits#nerdtree_bind_goto('gt', '/tmp')
    call inits#nerdtree_bind_goto('ge', '/etc')
    call inits#nerdtree_bind_goto('gr', '/')
    call inits#nerdtree_bind_goto('g/', '/')
    call inits#nerdtree_bind_goto('gm', '/mnt')
    call inits#nerdtree_bind_goto('gN', '~/Documents/Notes')
    call inits#nerdtree_bind_goto('gP', '~/Documents/Programming')
endfunction
