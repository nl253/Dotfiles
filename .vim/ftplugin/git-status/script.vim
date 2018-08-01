" matchstr(getline('.'), '\v^\w+')
setl sw=2 ts=4 expandtab cursorline

let s:template = expand('<sfile>:p:h').'/template.txt' 

function! s:get_git_cmds()
    let l:line = getline(1)
    if len(l:line) < 3
        let l:line = "git"
    endif
    return l:line
endfunction

function! s:init()
    exe '%d | read '.s:template
    call setline(1, g:GitCmd)
endfunction

nn <buffer> k ?\v^\w+<CR>
nn <buffer> j /\v^\w+<CR>
nn <buffer> <CR> :call GitCmdTransition()<CR>

if line("$") <= 10 
    call s:init()
endif
