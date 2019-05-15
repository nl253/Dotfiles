" loading markdown loads html as well (prob related to syntax)
" this is a workaround
if !(&filetype =~# 'html') | finish | endif

setl foldmethod=indent shiftwidth=2 tabstop=4 expandtab makeprg=$BROWSER\ %

for s:pair in items({ 
            \ 'js-beautify': 'html-beautify',
            \ 'prettier':    'prettier --stdin --parser markdown',
            \ })
    if executable(s:pair[0])
        exe 'setl formatprg='.s:pair[1]
        break
    endif
endfor

if exists(':EmmetInstall')
    EmmetInstall
    imap <buffer> <Tab> <plug>(emmet-expand-abbr)
endif

com! -buffer ToMarkdown :%!pandoc -f html -t markdown-fenced_divs-inline_code_attributes-fenced_code_attributes-header_attributes-link_attributes-all_symbols_escapable
