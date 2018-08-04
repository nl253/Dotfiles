" New:
vmap <M-r> :s/\v<>c<Left><Left><Left>

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

" find sub-strings by default
" nn * g*
" nn # g#

" sometimes writer is not needed
nn :w<CR> :up<CR>

" Intellij-like behaviour
" if exists(':NERDTree') == 2 
nn <M-1> :NERDTreeToggle<CR>
" else
    " nn <M-1> :silent call splits#toggle_netrw()<CR>
" endif

" quicker window resize
nn <C-w>- <C-w>8-
nn <C-w>+ <C-w>8+
nn <C-w>> <C-w>25>
nn <C-w>< <C-w>25<

" don't make a new file from the output
for s:i in ['grep', 'grepadd', 'make']
    exe 'ca '.s:i.' '.s:i.'!'
    exe 'ca l'.s:i.' l'.s:i.'!'
endfor
for s:i in ['expr', 'getexpr', 'addexpr']
    exe 'ca c'.s:i.' c'.s:i.'!'
    exe 'ca l'.s:i.' l'.s:i.'!'
endfor
for s:i in ['log', 'grep', 'lgrep', 'llog']
    exe 'ca G'.s:i.' G'.s:i.'!'
endfor

" POSIX regex by default
cno %s/ %s/\v
cno g/ g/\v
nn / /\v
nn ? ?\v
vn / /\v
vn ? ?\v

" enhanced X-Mode by default
nn Q gQ

nn <M-f>      :silent call project#project_files_qf()<CR>
" IntelliJ
nn <M-6>      :ToDo<CR>
" IntelliJ
nn <M-9>      :exe expand('%') == 'index' ? 'clo' : 'Gstatus'<CR> 
" Spacemacs
nn <Leader>'  :silent call repl#open_shell()<CR>
" IntelliJ
nn <F12>      :silent call repl#open_shell()<CR>

" Emacs (NOTE: some are commented out becasue tpope rsi.vim plugin handles it)
cno  
cno <M-d> <C-f>de<C-c>
cno <M-*> <C-a>
cno <C-k> <C-r>=emacs#cmd_line_readline_K()<CR><End><C-u><C-r>=g:__modifiled_cmdline<CR>
cno <M-c> <C-f>guegUle<Right><C-C>
cno <C-y> <C-f>p<C-c>
cno <M-l> <C-f>guee<Right><C-C>
cno <M-u> <C-f>gUee<Right><C-C>

" recenter
inoremap <C-l> <C-o>zz<C-o>:redraw<CR>
" inc search
inoremap <C-r> <C-o>?\v
inoremap <C-s> <C-o>/\v
" scroll
inoremap <C-v> <C-o><C-d>
inoremap <M-v> <C-o><C-u>
" switch windows
inoremap <C-x>1 <C-o><C-w>o
inoremap <C-x>2 <C-o><C-w>s
inoremap <C-x>3 <C-o><C-w>v
" list buffers
inoremap <C-x><C-b> <C-o>:ls<CR>
" write i.e. save
inoremap <C-x><C-s> <C-o>:w<CR>
" undo
inoremap <C-x>u <C-o>u
inoremap  <C-o>u
" transpose words
inoremap <M-t> <C-o>:silent call emacs#transpose_words()<CR>
" transpose characters
inoremap <C-t> <C-o>x<C-o>p
" buffer completion
inoremap <M-/> <C-x><C-n>
" switch buffer
inoremap <C-x>b <C-o>:b<Space>
inoremap <C-x>o <C-o><C-w><C-w>
" paste (emacs's yank)
inoremap <C-y> <C-o>p 
" delete mulit-space [WIP]
inoremap <M-Space> <C-o>diw<Space>
" join lines
inoremap <M-^> <C-o>J
" complete from spelling dictionary
inoremap <M-Tab> <C-x><C-k>
" correct spelling
inoremap <M-$> <C-o>z=1<CR>
" go to start of file 
inoremap <M-<> <C-o>gg
" go to end of file 
inoremap <M->> <C-o>G
" sentence motion
inoremap <M-a> <C-o>(
inoremap <M-e> <C-o>)
" captialise word
inoremap <M-c> <Esc><Right>guegUlea
" lowercase word
inoremap <M-l> <Esc><Right>gueea
" uppercase word
inoremap <M-u> <Esc><Right>gUeea
" delete till end of line
inoremap <expr> <C-k> getline(".") == "" ? "\<C-o>dd" : "<C-o>d$"
" next, prev line
inoremap <expr> <C-n> pumvisible() ? "\<C-n>" : "\<Down>"
inoremap <expr> <C-p> pumvisible() ? "\<C-p>" : "\<Up>"
" next character
inoremap <C-f> <Right>

"inoremap <M-b> <C-o>b<Left>
"inoremap <M-f> <C-o>e<Right>
"inoremap <M-d> <C-o>de
"cno <M-f> <S-Right>
"cno <M-BS> <C-W>
"cno <C-a> <Home>
"cno <M-b> <S-Left>
"inoremap <C-a> <Home>
"inoremap <C-b> <Left>
"inoremap <C-e> <End>
"inoremap <C-r> <C-o>?\v
"inoremap <M-BS> <C-w>
"inoremap <C-u> <C-o>d^
