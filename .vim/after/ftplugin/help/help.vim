" make it more like less
nn <buffer> <nowait> q :helpclose<CR>
nn <buffer> <nowait> d <C-d>
nn <buffer> <nowait> <Space> <C-d>
nn <buffer> <nowait> u <C-u>
nn <buffer> <Tab> /\v\\|.{1,15}\\|<CR>
nn <buffer> <S-Tab> ?\v\\|.{1,15}\\|<CR>

setl nospell nomodifiable nowrap readonly keywordprg=:help
