setl nowrap readonly nomodifiable keywordprg=:Man

" make it more like less and Emacs help files
nn <buffer> <nowait> q :bw!<CR>
nn <buffer> <nowait> d <C-d>
nn <buffer> <nowait> <Space> <C-d>
nn <buffer> <nowait> u <C-u>
nn <buffer> <nowait> u <C-u>
nn <buffer> [[ ?\v\C^[A-Z]{3,}<CR>
nn <buffer> ]] /\v\C^[A-Z]{3,}<CR>
nn <buffer> <Tab>   /\v\C<[a-z]+[-a-z]+\(\d+\)<CR>
nn <buffer> <S-Tab> ?\v\C<[a-z]+[-a-z]+\(\d+\)<CR>
nm <buffer> <CR>    K
