
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

for option in ['wildignore+=_*,tags,.git,.svn,*~,*.lock,*.swp,*.iml', 
            \  'wildignore+=*.db,*.sqlite*,*.bk,*.bac,.idea,node_modules,dist,out,target,build'] 
    try 
        silent! exec 'set '.option
    catch /.*/
    endtry
endfor

let s:dict_dir = expand('<sfile>:p:h:h').'/dicts/'

fu! s:init()
    setl nospell complete=.,w,t 
    if expand('%:e') != ''
        execute 'setl complete+=k*.'.expand('%:e')
    endif

    if !isdirectory(s:dict_dir)
        echom s:dict_dir." could not be read"
    endif

    if &dictionary == "" 
        if filereadable(s:dict_dir.&filetype.'.dict')
            exec "setl dictionary=".s:dict_dir.&filetype.'.dict'
            setl complete+=k
        elseif filereadable(s:dict_dir.expand("%:e").'.dict')
            exec "setl dictionary=".s:dict_dir.expand("%:e").'.dict'
            setl complete+=k
        endif
    endif
endfu

aug VimWebDev
    au!
    execute 'au FileType '.join(s:languages, ',').' silent call s:init()' 
aug END 

let g:loaded_webdev = 1
