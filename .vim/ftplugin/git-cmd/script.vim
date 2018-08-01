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
    call setline(1, s:get_git_cmds())
endfunction

function! GitCmdTransition()
    let l:next_cmd = matchstr(getline('.'), '\v^\w+')
    let g:GitCmd = s:get_git_cmds() . ' ' . l:next_cmd
    enew
    exe 'setf git-'.l:next_cmd
endfunction

" matchstr(getline('.'), '\v^\w+')
setl sw=2 ts=4 expandtab cursorline

nn <buffer> k ?\v^\w+<CR>
nn <buffer> j /\v^\w+<CR>
nn <buffer> <CR> :call GitCmdTransition()<CR>

if line("$") <= 10 
    call s:init()
endif
