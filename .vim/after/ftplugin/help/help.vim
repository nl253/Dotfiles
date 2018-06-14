" make it more like less
nnoremap <buffer> <nowait> q :helpclose<CR>
nnoremap <buffer> <nowait> d <C-d>
nnoremap <buffer> <nowait> <Space> <C-d>
nnoremap <buffer> <nowait> u <C-u>
nn <buffer> <Tab> /\v\\|.{1,15}\\|<CR>
nn <buffer> <S-Tab> ?\v\\|.{1,15}\\|<CR>

setl nomodifiable nowrap readonly keywordprg=:help
