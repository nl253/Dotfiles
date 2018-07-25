if exists('g:loaded_vim_programming') | finish | endif

let s:prog_langs = !exists('g:prog_langs') ? [
            \ 'javascript', 
            \ 'python', 
            \ 'sql', 
            \ 'sh', 
            \ 'zsh', 
            \ 'rust', 
            \ 'haskell'
            \ ] : g:prog_langs

let s:wild_ignore_patterns = [
            \ '*~',
            \ '*.bac',
            \ '*.beam',
            \ '*.bk',
            \ 'build', 
            \ '*.class',
            \ '*.ctxt', 
            \ '*.db', 
            \ 'dist', 
            \ '.egg', 
            \ '.eggs',
            \ '.git',
            \ '*.hi',
            \ '.idea', 
            \ '*.iml',
            \ '*.lock',
            \ '*.o',
            \ '*.obj',
            \ 'out', 
            \ '.rope*',
            \ '*.so',
            \ '*.sqlite*',
            \ '.svn',
            \ '*.swp',
            \ 'target', 
            \ ]

for s:pattern in s:wild_ignore_patterns 
    " if !(&wildignore =~# escape(s:pattern, '*.[]'))
    if matchstr(&wildignore, ','.escape(s:pattern, '.*~[]').',') == ''
        exe 'setg wildignore+='.s:pattern
    endif
endfor

fu! s:init_completion()
    for l:i in filter(['.', 'w', 't'], '!(&complete =~# ",".v:val)')
        exe 'setl complete+='.l:i
    endfor

    let l:ext = expand('%:e')

    if l:ext != '' && !(&complete =~? '*.'.l:ext)
        exe 'setl complete+=k*.'.l:ext
    endif
endf

fu! s:init_dict()

    if &dictionary == ''

        let l:dict_dir = expand('<sfile>:p:h:h').'/dicts/'

        let l:candidate = l:dict_dir.&filetype.'.dict'

        if filereadable(l:candidate)

            exec 'setl dictionary='.l:candidate

            if !(&complete =~# 'k')
                setl complete+=k
            endif

        else
            let l:candidate = l:dict_dir.expand('%:e').'.dict'
            if filereadable(l:candidate)
                exec 'setl dictionary'.l:candidate
                if !(&complete =~# ',k')
                    setl complete+=k
                endif
            endif
        endif
    endif
endf

fu! s:safe_setl(opts)
    for l:option in a:opts
        try
            silent! exec 'setl '.l:option
        catch /.*/
            silent call add(v:errors, '[vim-programming] could not set option "'.l:option.'"')
        endtry
    endfor
endf

fu! s:init()
    sil call s:safe_setl(['nospell'])
    sil call s:init_completion()
    sil call s:init_dict()
endfu

aug VimProgramming
    au!
    exe 'au FileType '.join(s:prog_langs, ',').' silent call s:init()'
aug END

let g:loaded_vim_programming = 1
