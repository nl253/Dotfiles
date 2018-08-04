" Alter completion to include in user-completion files with
" specified extensions in the current dict
if exists('g:loaded_vim_markup') | finish | endif

let s:markup_languages = !exists('g:markup_languages') ? [
            \ 'markdown', 
            \ 'rst', 
            \ 'vimwiki' , 
            \ 'asciidoc', 
            \ 'org'
            \ ] : g:markup_languages

let s:dict_dir = expand('<sfile>:p:h:h').'/dicts/'

if &thesaurus == ''
    exec 'setg thesaurus='.s:dict_dir.'thesaurus.dict'
endif

if &dictionary == ''
    exec 'setg dictionary='.s:dict_dir.'frequent.dict'
endif

fu! s:init()
    for s:option in [
                \ 'conceallevel=3',
                \ 'spell',
                \ 'tagcase=ignore',
                \ 'expandtab',
                \ 'textwidth=79',
                \ 'linebreak',
                \ 'nowrap',
                \ ]
        try
            silent! exec 'setl '.s:option
        catch /.*/
            silent call add(v:errors, '[vim-markup] could not set option "'.s:option.'"')
        endtry
    endfor

    for l:i in ['t', 'o', 'r', 'c', 'n', 'q', 'j', 'l', '1']
        if !(&fo =~ ','.l:i)
            exe 'setl fo+='.l:i
        endif
    endfor

    for l:i in ['.', 'w', 'k*.'.expand('%:e')]
        if !(&complete =~# ','.l:i)
            exe 'setl complete+='.l:i
        endif
    endfor

    if &dictionary == '' && filereadable(s:dict_dir.'frequent.dict')
        exec 'setg dictionary='.s:dict_dir.'frequent.dict'
        if !(&complete =~# ',k')
            setl complete+=k
        endif
    endif
    call iabbrs#iab_init(&filetype)
endfu

aug MarkupCompletion
    au!
    exe 'au FileType '.join(s:markup_languages, ',').' silent call s:init()'
aug END

let g:loaded_vim_markup = 1
