
" Move by screen lines instead of file lines.
" http://vim.wikia.com/wiki/Moving_by_screen_lines_instead_of_file_lines
noremap k gk
noremap j gj

nn <M-r>   :SubstituteWord<CR>
nn <M-C-l> :ReformatBuffer<CR>
nn <M-t>   :Ctags<CR>
nn <M-w>   :SaveView<CR>
nn <M-S-w> :ReadView<CR>

inoremap <C-space> <C-x><C-o>

if has("nvim")
    tnoremap <Esc> <C-\><C-n>
endif
