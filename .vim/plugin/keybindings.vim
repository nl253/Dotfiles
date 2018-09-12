nn :e<Space>~ :find<Space>~
nn :e<Space>  :find<Space>./

" send data to :terminal
" nn <Leader>w :call jobsend(g:last_terminal_job_id, expand("<cword>"))<CR>
" nn <Leader>W :call jobsend(g:last_terminal_job_id, expand("<cWORD>"))<CR>
" nn <Space>l  :call jobsend(g:last_terminal_job_id, getline('.'))<CR>
" nn <M-CR>    :call jobsend(g:last_terminal_job_id, "\n")<CR>

" Move by screen lines instead of file lines.
" http://vim.wikia.com/wiki/Moving_by_screen_lines_instead_of_file_lines
no k gk
no j gj

nn <M-r>   :SubstituteWord<CR>
nn <M-C-l> :ReformatBuffer<CR>
nn <M-t>   :Ctags<CR>
nn <M-w>   :SaveView<CR>
nn <M-S-w> :ReadView<CR>

" sometimes write is not needed so just update
nn :w<CR> :up<CR>

if has("nvim") | tno <Esc> <C-\><C-n> | endif

im <C-Space> <C-x><C-o>

" Intellij-like behaviour
if exists(':NERDTree') > 0  
    nn <M-1> :NERDTreeToggle<CR>
else
    nn <M-1> :silent call utils#toggle_netrw()<CR>
endif

" quicker window resize use terminal proportions
nn <C-w>- :silent exe 'silent resize -'.max([3, system("tput lines") / 6])<CR>
nn <C-w>+ :silent exe 'silent resize +'.max([3, system("tput lines") / 6])<CR>
nn <C-w>> :silent exe 'silent vertical resize +'.max([10, system("tput cols") / 7])<CR>
nn <C-w>< :silent exe 'silent vertical resize -'.max([10, system("tput cols") / 7])<CR>

" enhanced X-Mode by default
nn Q gQ

" alway search using very magic (POSIX-ish regex) & case-sensitive mode 
nn  /   /\v\C
nn  ?   ?\v\C
cno %s/ %s/\v\C
cno g/  g/\v\C
vn  /   /\v\C
vn  ?   ?\v\C

" fill qf buffer with files in this project
nn <M-f>      :silent call utils#project_files_qf()<CR>
" IntelliJ  - Show todo 
nn <M-6>      :ToDo<CR>
" IntelliJ  - Display Git Info
nn <M-9>      :exe expand('%') == 'index' ? 'clo' : 'Gstatus'<CR> 
" Spacemacs - Toggle Terminal
nn <Leader>'  :silent call repl#open_shell()<CR>
" IntelliJ  - Toggle Terminal
nn <F12>      :silent call repl#open_shell()<CR>

" automatically append '!' to grep & make 
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

" Emacs:

" => CmdLine:

" => => Movement

" back by a char
cno <C-B> <Left>
" right by a char
cno <expr> <C-F> getcmdpos()>strlen(getcmdline())?&cedit:"\<Lt>Right>"
" start of line
cno <C-A> <Home>

" Don't know what these do:
" cno   <C-X><C-A> <C-A>

" => => Editing

" delete char backword
cno <expr> <C-D> getcmdpos()>strlen(getcmdline())?"\<Lt>C-D>":"\<Lt>Del>"
" cut line till end
cno <C-k> <C-r>=emacs#c_cut_till_end()<CR><End><C-u><C-r>=g:__modifiled_cmdline<CR>
" transpose char
cno <expr> <C-t> emacs#c_transpose()
" captialise word
cno <M-c> <C-f>guegUle<Right><C-C>
" lowercase word
cno <M-l> <C-f>guee<Right><C-C>
" uppercase word
cno <M-u> <C-f>gUee<Right><C-C>
" delete word 
cno <M-BS> <C-W>
" backward delete a word
cno <M-d> <S-Right><C-w>
" cno <M-d> <C-f>de<C-c>

" => => Other

" cancel command
cno  

cno <C-y> <C-f>p<C-c>
cno <M-*> <C-a>
let g:stopchars = ['/', '~', '^', '&', ' ', ':', ';', '!', '?', '$', '.', '(', ')', ']', '[', '{', '}', "'", '"', ',']
cno <M-f> <C-r>=emacs#c_navigate_r('\v'.join(['>', '$'], '\|'))<CR><BS>
cno <M-b> <C-r>=emacs#c_navigate_l(g:stopchars)<CR><BS>

" => Insert

" start of line
ino <C-A> <C-O>^

" => => Editing

" delete word
ino <M-BS> <C-w>
" delete char backword
ino <expr> <C-D> col('.')>strlen(getline('.'))?"\<Lt>C-D>":"\<Lt>Del>"
" delete word backward
ino <M-d> <C-o>de
" undo
ino <C-x>u <C-o>u
ino      <C-o>u
" paste (emacs's yank)
ino <C-y> <C-o>p 
" delete till end of line
ino <expr> <C-k> getline('.') =~# '\v^\s*$' ? "\<C-o>dd" : "<C-o>D"
" transpose words
ino <M-t> <C-o>:silent call emacs#i_transpose_words()<CR>
" transpose chars
ino <C-t> <C-o>x<C-o>p
" captialise word
ino <M-c> <Esc><Right>guegUlea
" lowercase word
ino <M-l> <Esc><Right>gueea
" uppercase word
ino <M-u> <Esc><Right>gUeea
" join lines
ino <M-^> <C-o>J

" => => Movement

" end of line 
ino <expr> <C-E> col('.')>strlen(getline('.'))<bar><bar>pumvisible()?"\<Lt>C-E>":"\<Lt>End>"
" right by a char
ino <expr> <C-F> col('.')>strlen(getline('.'))?"\<Lt>C-F>":"\<Lt>Right>"
" left by a char 
ino <expr> <C-B> getline('.')=~'^\s*$'&&col('.')>strlen(getline('.'))?"0\<Lt>C-D>\<Lt>Esc>kJs":"\<Lt>Left>"
" sentence motion
ino <M-a> <C-o>(
ino <M-e> <C-o>)
" next, prev line
ino <expr> <C-n> pumvisible() ? "\<C-n>" : "\<Down>"
ino <expr> <C-p> pumvisible() ? "\<C-p>" : "\<Up>"
" next character
ino <C-f> <Right>
" scroll
ino <C-v> <C-o><C-d>
ino <M-v> <C-o><C-u>
" switch buffer
ino <C-x>b <C-o>:b<Space>
ino <C-x>o <C-o><C-w><C-w>
" go to start of file 
ino <M-<> <C-o>gg
" go to end of file 
ino <M->> <C-o>G

" => => Other

" recenter & redraw
ino <C-l> <C-o>zz<C-o>:redraw<CR>
" list buffers
ino <C-x><C-b> <C-o>:ls<CR>
" write i.e. save
ino <C-x><C-s> <C-o>:w<CR>
" correct spelling
ino <M-$> <C-o>:call emacs#i_correct_spelling_err()<CR>
" manipulate windows
ino <C-x>1 <C-o><C-w>o
ino <C-x>2 <C-o><C-w>s
ino <C-x>3 <C-o><C-w>v

" => => Search 
"
" incremental search upward 
ino <C-r> <C-o>?\v\C
" incremental search downward 
ino <C-s> <C-o>/\v\C

" => => Completion

" complete from spelling dictionary
ino <M-Tab> <C-x><C-k>
" buffer completion
ino <M-/> <C-x><C-n>

" delete mulit-space
ino <M-Space> <C-o>:call<Space>emacs#i_del_space_between_words()<CR>

" Don't know what these do:
" ino  <C-X><C-A> <C-A>

ino <M-b> <C-o>b<Left>
ino <M-f> <C-o>e<Right>
"cno <M-f> <S-Right>
"cno <C-a> <Home>
"cno <M-b> <S-Left>
"ino <C-a> <Home>
"ino <C-b> <Left>
"ino <C-e> <End>
"ino <C-r> <C-o>?\v
"ino <C-u> <C-o>d^
