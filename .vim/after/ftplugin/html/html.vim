" loading markdown loads html as well (prob related to syntax)
" this is a workaround
if !(&filetype =~# 'html') | finish | endif

setl foldmethod=indent shiftwidth=2 tabstop=4 expandtab

call opts#formatprg({ 
            \ 'js-beautify': 'html-beautify',
            \ 'prettier':    'prettier --stdin --parser markdown',
            \ })

if exists(':EmmetInstall')
    EmmetInstall
    imap <buffer> <Tab> <plug>(emmet-expand-abbr)
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
    echo opts#makeprg(s:cfg)
endif
