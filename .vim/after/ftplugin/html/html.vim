let g:html_use_xhtml = 0
let g:html_dynamic_folds = 0
let g:html_no_foldcolumn = 1 
let g:html_use_encoding = 'UTF-8'
let g:html_font = [
            \ 'Sans Serif', 
            \ 'DejaVu Sans Mono', 
            \ 'Consolas', 
            \ 'monospace'
            \ ]
let html_wrong_comments = 1
let g:html_hover_unfold = 1

" might be computationally demanding
" better beauitfy and use indent-based fold
let g:xml_syntax_folding = 0 

" loading markdown loads html as well (prob related to syntax)
" this is a workaround
if &filetype =~# 'html' 
    setl foldmethod=indent shiftwidth=2 tabstop=4 expandtab
    call setters#formatprg({ 
                \ 'js-beautify': 'html-beautify',
                \ 'prettier':    'prettier --stdin --parser markdown',
                \ })
    if exists(':EmmetInstall')
        EmmetInstall
        imap <buffer> <Tab> <plug>(emmet-expand-abbr)
    endif
endif

if len($BROWSER) == 0
    let s:cfg = {}
    for s:browser in [
                \ 'google-chrome-stable', 
                \ 'google-chrome-beta', 
                \ 'google-chrome-unstable', 
                \ 'google-chrome', 
                \ 'chromium', 
                \ 'firefox-developer', 
                \ 'firefox-developer-edition', 
                \ 'firefox']
        let s:cfg[s:browser] = s:browser.' %'
    endfor
    echo setters#makeprg(s:cfg)
endif

