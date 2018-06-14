
" Title: Vim Plugin
" Maintainer: nl253
" Description: Entry point to other submodules, sets variables.

if exists('g:loaded_vim_programming') | finish | endif

let s:prog_langs = !exists('g:prog_langs') ? ['javascript', 'python', 'sql', 'sh', 'zsh', 'rust', 'haskell'] : g:prog_langs

for s:option in [
            \ 'wildignore+=_*,*.class,*.so,*.o,*.obj,*.hi,.git,.svn,.rope*,*~,*.lock,*.swp,*.iml,*.ctxt',
            \ 'wildignore+=*.db,*.sqlite*,*.beam,*.bk,*.bac,.idea,dist,out,target,build,.egg,.eggs'
            \ ]
    try
        silent! exec 'set '.s:option
    catch /\vE(518)/
        silent call add(v:errors, '[vim-programming] could not set option "'.s:option.'"')
    endtry
endfor

let s:dict_dir = expand('<sfile>:p:h:h').'/dicts/'

fu! s:init()
    for s:option in ['nospell']
        try
            silent! exec 'setl '.s:option
        catch /.*/
            silent call add(v:errors, '[vim-programming] could not set option "'.s:option.'"')
        endtry
    endfor

    for l:i in filter(['.', 'w', 't'], '!(&complete =~# ",".v:val)')
        exe 'setl complete+='.l:i
    endfor

    let l:ext = expand('%:e')

    if l:ext != '' && !(&complete =~? '*.'.l:ext)
        exe 'setl complete+=k*.'.l:ext
    endif

    if (&makeprg != '') && (&makeprg != 'make')
        nnoremap <buffer> <Space>me :make<CR>
    endif

    if &dictionary == ''

        let l:candidate = s:dict_dir.&filetype.'.dict'

        if filereadable(l:candidate)

            exec 'setl dictionary='.l:candidate

            if !(&complete =~# 'k')
                setl complete+=k
            endif
        else
            let l:candidate = s:dict_dir.expand('%:e').'.dict'
            if filereadable(l:candidate)
                exec 'setl dictionary'.l:candidate
                if !(&complete =~# ',k')
                    setl complete+=k
                endif
            endif
        endif
    endif
endfu

aug VimProgramming
    au!
    exe 'au FileType '.join(s:prog_langs, ',').' silent call s:init()'
aug END

let g:loaded_vim_programming = 1
