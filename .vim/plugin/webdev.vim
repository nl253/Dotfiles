
" Title: Vim Plugin
" Maintainer: nl253
" Description: Entry point to other submodules, sets variables.

if exists('g:loaded_webdev') | finish | endif

let s:languages = !exists("g:webdev_languages") ? [
            \ 'javascript', 
            \ 'css', 
            \ 'html', 
            \ 'sql', 
            \ 'php'] : g:webdev_languages

let s:wild_ignore_patterns = [
            \ '_*', 
            \ '*~', 
            \ '*_', 
            \ '*.bac', 
            \ '*.bk', 
            \ 'build',
            \ '*.db', 
            \ 'dist', 
            \ '.git', 
            \ '.idea', 
            \ '*.iml', 
            \ '*.lock', 
            \ 'node_modules',
            \ 'out', 
            \ '*.sqlite*', 
            \ '.svn', 
            \ '*.swp', 
            \ 'tags', 
            \ 'target', 
            \ ]

for s:pattern in s:wild_ignore_patterns 
    " if !(&wildignore =~# escape(s:pattern, '*.[]'))
    if matchstr(&wildignore, ','.escape(s:pattern, '.*~[]').',') == ''
        exe 'setg wildignore+='.s:pattern
    endif
endfor

" Return path to dictionary based on filetype.
fu! s:get_dict(ftype)

    let l:dict_dir = expand('<sfile>:p:h:h').'/dicts/'

    let l:ft_based = l:dict_dir.a:ftype.'.dict'

    let l:ext_based = l:dict_dir.expand("%:e").'.dict'

    if filereadable(l:ft_based)
        return l:ft_based

    elseif filereadable(l:ext_based)
        return l:ext_based

    else
        return ""
    endif
endf

fu! s:init_dict()

    if &dictionary == "" 

        let l:try_dict = s:get_dict(&filetype)

        if l:try_dict != ''
            exec "setl dictionary=".l:try_dict
            setl complete+=k
        endif

    endif
endf

fu! s:init()

    setl nospell complete=.,w,t 

    if expand('%:e') != ''
        execute 'setl complete+=k*.'.expand('%:e')
    endif

    sil call s:init_dict()
endfu

aug VimWebDev
    au!
    execute 'au FileType '.join(s:languages, ',').' silent call s:init()' 
aug END 

let g:loaded_webdev = 1
