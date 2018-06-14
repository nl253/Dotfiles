"Dictionary frequent
fu! s:reference()
    execute 'tabe '.expand('<sfile>:p:h').'/reference.rst'
endfu

com! -buffer RstRefrence silent call s:reference()

if !exists('g:rst_syntax_code_list')
    if !exists('g:markdown_fenced_languages')
        let g:markdown_fenced_languages = ['vim', 'python', 'javascript', 'html', 'haskell', 'sh', 'java']
    endif
    let g:rst_syntax_code_list = g:markdown_fenced_languages
endif
