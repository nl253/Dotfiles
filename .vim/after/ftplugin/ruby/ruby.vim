for i in filter(['ruby_operators', 
            \ 'ruby_space_errors', 
            \ 'ruby_fold', 
            \ 'ruby_spellcheck_strings', 
            \ 'g:rubycomplete_buffer_loading', 
            \ 'g:rubycomplete_classes_in_global', 
            \ 'g:rubycomplete_rails'], '!exists(v:val)')
    exec 'let '.i.' = 1'
endfor
