if exists('g:loaded_vim_saner_autocommands') || !has('unix') | finish | endif

fu! s:init_completion()
    try
        let l:path = systemlist('git rev-parse --show-toplevel')[0]

        if !v:shell_error && isdirectory(l:path) && l:path != $HOME && l:path =~? $HOME

            let l:ext = expand('%:t:e')

            for l:i in ['.', 'k', 't']
                if !(&complete =~# ','.l:i)
                    exe 'setl complete+='.l:i
                endif
            endfor

            if !(&complete =~# l:path) && l:ext != ''
                exe 'setl complete+=k'.l:path.'/**3/*.'.l:ext
            endif

            if !(&tags =~# l:path)
                exe 'setl tags+='.l:path.'/**3/tags'
            endif

            if !filereadable(l:path.'/tags')
                silent call system("cd ".l:path." && ctags -R")
            endif
        endif
    catch /\vE684/
    endtry

endf

fu! s:init()
    sil call s:init_completion()

    if &omnifunc == '' && exists('+omnifunc')
        setl omnifunc=syntaxcomplete#Complete
    endif

    if &completefunc == '' && exists('+completefunc')
        setl completefunc=syntaxcomplete#Complete
    endif

    if (&makeprg != '') && (&makeprg != 'make')
        nnoremap <buffer> <Space>me :make<CR>
    endif
endf

aug SanerVimEnter
    au!

    " go back to where you left off
    au BufReadPost ??* if line("'\"") > 0 && line("'\"") <= line("$") | exe "normal! g`\"" | endif

    " automatically change dir to the file you are editing
    au BufEnter ??* try | lchdir %:p:h | catch /.*/ | endtry

    " automatically reload external changes NOTE: doesn't always work properly
    au CursorHold,BufEnter * silent! checktime

    " autosave on focus lost
    au BufLeave ??* try | write | catch /.*/ | endtry
    au FocusLost ??* silent!    wall

    au CmdwinEnter * setl updatetime=2000
    au CmdwinLeave * setl updatetime=199

    " init on all filetypes
    au BufReadPost * silent call s:init()

    " save each session before quitting
    au VimLeavePre * SaveSession
aug END

let g:loaded_vim_saner_autocommands = 1
