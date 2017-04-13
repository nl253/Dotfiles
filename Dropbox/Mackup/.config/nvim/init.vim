
if executable('pandoc')
  command! TOman execute '!pandoc -s -o ' expand('%:p:r') . '.1  -t man ' . expand('%:p') | sleep 250ms | execute 'vs  ' . expand('%:p:r') . '.1'
  command! TOmarkdown execute '!pandoc -s -o ' expand('%:p:r') . '.md  -t markdown_github ' . expand('%:p') | sleep 250ms | execute 'vs  ' . expand('%:p:r') . '.md'
  command! TOrst execute '!pandoc -s -o ' expand('%:p:r') . '.rst  -t rst ' . expand('%:p') | sleep 250ms | execute 'vs  ' . expand('%:p:r') . '.rst'
  command! TOtex execute '!pandoc -s -o ' expand('%:p:r') . '.tex  -t tex ' . expand('%:p') | sleep 250ms | execute 'vs  ' . expand('%:p:r') . '.tex'
  command! TOwordocx execute '!pandoc -s -o ' expand('%:p:r') . '.docx  -t docx ' . expand('%:p') | sleep 250ms | execute 'vs  ' . expand('%:p:r') . '.docx'
  command! TOxhtml execute '!pandoc -s -o ' expand('%:p:r') . '.xhtml  -t xhtml ' . expand('%:p') | sleep 250ms | execute 'vs  ' . expand('%:p:r') . '.xhtml'
endif

if has('nvim') | call plug#begin('~/.local/share/nvim/plugged/') | else | call plug#begin('~/.vim/plugged') | endif

" Place plugins here
Plug 'Haron-Prime/Antares'
Plug 'tpope/vim-dispatch', {'on' : ['Make','Dispatch','Copen','Start','Spawn']}
Plug 'tpope/vim-sleuth' " auto set buffer options
Plug 'xolox/vim-misc'
Plug 'xolox/vim-session' " enhanced session management
let g:session_autosave = 'yes' 
let g:session_autosave_to = 'default'
let g:session_autosave_periodic = 3
let g:session_lock_enabled = 0
let g:session_autoload = 'yes'
let g:session_persist_colors = 1
let g:session_default_to_last = 1
let g:session_persist_globals = [ 
      \'&foldmethod', '&foldcolumn', '&scrolloff', '&spell', '&wrap',
      \'&scrollbind', '&number', '&relativenumber', '&foldmarker', '&background']

if has('nvim') | let g:session_directory = '~/.config/nvim/session'| else | let g:session_directory = '~/.vim/session' | endif

nnoremap <C-x><C-a> :execute 'grep ' . expand('<cword>') . " * "<CR>

Plug 'mattn/webapi-vim'
Plug 'tpope/vim-git'
Plug 'tpope/vim-fugitive'
Plug 'gregsexton/gitv', { 'on': 'Gitv' }
Plug 'airblade/vim-gitgutter', { 'on' : [ 'GitGutterDisable', 'GitGutterToggle', 'GitGutterEnable',
      \'GitGutterLineHighlightToggle', 'GitGutterLineHighlightsToggle']}

let g:gitgutter_map_keys = 0
let g:gitgutter_diff_args = '-w'

if has('nvim')
  Plug 'kassio/neoterm'
  nnoremap <M-CR> :TREPLSendLine<CR>
  vnoremap <M-CR> :TREPLSendSelection<CR>
  let g:neoterm_position = 'vertical'
  let g:neoterm_keep_term_open = 0
  let g:neoterm_size = 50
endif

Plug 'junegunn/vim-easy-align', { 'on' : 'EasyAlign' }
Plug 'Konfekt/FastFold' " more efficient folds
Plug 'scrooloose/nerdcommenter'

Plug 'tpope/vim-eunuch', {'on' : [ 'Move', 'Remove', 'Find', 'Mkdir', 'Wall',
      \'SudoWrite', 'SudoEdit', 'Unlink', 'Chmod', 'Rename', ]}

Plug 'reedes/vim-wordy', { 'on': ['Wordy', 'WordyWordy']}
Plug 'reedes/vim-textobj-sentence'
Plug 'neomake/neomake', {'on' : [ 'Neomake', 'NeomakeProject', 'NeomakeFile' ]}

if filereadable('~/.config/nvim/dicts/frequent.dict') | set dictionary=~/.config/nvim/dicts/frequent.dict | endif
if filereadable('~/.config/nvim/thesaurus.txt') | set thesaurus=~/.config/nvim/thesaurus.txt | endif

if executable('vint') | let g:neomake_vim_enabled_makers = ['vint'] | endif
if executable('tidy') | let g:syntastic_html_checkers = ['tidy'] | endif
if executable('jsonlint') | let g:syntastic_json_checkers = ['jsonlint'] | endif
let g:neomake_markdown_enabled_makers = [ 'mdl', 'textlint', 'writegood', 'proselint' ]
Plug 'dbmrq/vim-ditto', { 'on': [ 'ToggleDitto', 'DittoOff', 'DittoOn', 'DittoSent',
      \'DittoSentOn', 'DittoFile', 'DittoFileOn', 'DittoPar', 'DittoParOn']}

Plug 'Chiel92/vim-autoformat', { 'on': 'Autoformat' }

Plug 'vim-scripts/utl.vim'

Plug 'dhruvasagar/vim-table-mode', { 'on': [ 'TableModeToggle', 'TableModeDisable', 'TableModeEnable', 'Tableize' ]}

Plug 'vim-scripts/utl.vim'
Plug 'jceb/vim-orgmode', {'for' : 'org'}

" Markdown
" ==========
Plug 'tpope/vim-markdown', { 'for': ['markdown', 'ghmarkdown']}
Plug 'jtratner/vim-flavored-markdown', { 'for': ['markdown', 'ghmarkdown']}

Plug 'plasticboy/vim-markdown', { 'for': ['markdown', 'ghmarkdown']}

let g:vim_markdown_no_default_key_mappings = 1
let g:vim_markdown_folding_style_pythonic = 1
let g:vim_markdown_folding_level = 2

Plug 'klen/python-mode', { 'for': 'python' } " Python mode

let g:pymode_breakpoint_bind = '<localleader>b'
let g:pymode_doc = 1
let g:pymode_doc_bind = '<C-q><C-q>'
let g:pymode_indent = 1
let g:pymode_lint = 0 " Neomake is better
let g:pymode_motion = 1
let g:pymode_options = 1
let g:pymode_options_colorcolumn = 1
let g:pymode_paths = ['~/bin/']
let g:pymode_python = 'python3'
let g:pymode_rope = 1
let g:pymode_rope_autoimport = 1
let g:pymode_rope_autoimport_import_after_complete = 1
let g:pymode_rope_change_signature_bind = '<localleader>cs'
let g:pymode_rope_complete_on_dot = 1
let g:pymode_rope_completion = 1
let g:pymode_rope_extract_method_bind = '<localleader>em'
let g:pymode_rope_extract_variable_bind = '<localleader>ev'
let g:pymode_rope_goto_definition_bind = '<C-e><C-e>'
let g:pymode_rope_module_to_package_bind = '<localleade>rmp'
let g:pymode_rope_move_bind = '<localleader>mm'
let g:pymode_rope_organize_imports_bind = '<localleader>i'
let g:pymode_rope_regenerate_on_write = 0
let g:pymode_rope_rename_bind = '<localleader>rr'
let g:pymode_rope_rename_module_bind = '<localleader>rm'
let g:pymode_rope_show_doc_bind = '<localleader>d'
let g:pymode_rope_use_function_bind = '<localleader>uf'
let g:pymode_run_bind = '<leader>me'
let g:pymode_syntax = 1
let g:pymode_syntax_all = 1
let g:pymode_syntax_print_as_function = 1
let g:pymode_trim_whitespaces = 1

Plug 'othree/html5.vim', { 'for': ['html', 'xhtml']}
Plug 'othree/html5-syntax.vim'
Plug 'mattn/emmet-vim', { 'for': [ 'scss', 'xml', 'html', 'xhtml', 'css', 'sass' ]}

Plug 'chrisbra/Colorizer', { 'for': [
      \ 'css', 'html', 'javascript', 'json', 'less', 'markdown',
      \ 'org', 'sass', 'ghmarkdown', 'pandoc', 'scss', 'xhtml', 'yaml']}

let g:emmet_html5 = 1



Plug 'pangloss/vim-javascript', { 'for': 'javascript' }

Plug 'maksimr/vim-jsbeautify', { 'for': [ 'javascript', 'json', 'coffee', 'html',
      \'xhtml', 'xml', 'css', 'sass', 'scss', 'less'] }

call plug#end()
let loaded_matchit = 1
let mapleader = " "
set foldmethod=marker autochdir sessionoptions-=blank completeopt=menuone,longest,preview,noinsert diffopt=filler,vertical,iwhite
set complete=.,w noswapfile mps+=<:> bufhidden=hide wildignorecase shiftwidth=4 autowrite undofile hidden clipboard=unnamed,unnamedplus path+=~/**
set wildignore+=/home/norbert/.bashrc,/home/norbert/.tmuxinator,/home/norbert/.config/nvim/init.vim,*.git,*.class,*.svn
set wildignore+=*cache*,*chrome*,*/.dropbox/*,*intellij*,*fonts*,*libreoffice*,*.png,*.jpg,*.jpeg

aug VIMENTER
  au FileType * if exists("+omnifunc") && &omnifunc == "" | setlocal omnifunc=syntaxcomplete#Complete | endif
  au FileType * if exists("+completefunc") && &completefunc == "" | setlocal completefunc=syntaxcomplete#Complete | endif
  au BufEnter * try | lchdir %:p:h | catch /.*/ | endtry
  au BufReadPost * if line("'\"") > 0 && line("'\"") <= line("$") | exe "normal! g`\"" | endif
  au FileType markdown setl ft=ghmarkdown
  au FileType gitcommit setl spell
  au FileType man setl nowrap
  au CursorHold  * silent!  checktime
  au FocusLost   * silent!  wall
  au CmdwinEnter * setlocal updatetime=2000
  au CmdwinLeave * setlocal updatetime=200
aug END
if executable('pdftotext') 
  au! BufRead *.pdf execute '!pdftotext ' . expand('%:p') . ' ' . expand('%:p:r') .   '.txt' | execute 'enew ' . expand('%:p:r') . '.txt'
endif
try | tnoremap <Esc> <C-\><C-n> | catch /.*/ | endtry

if ! has('nvim')
  syntax enable
  filetype plugin indent on
  set encoding=utf8 syntax=on filetype=on autoindent nocompatible magic incsearch tags ttyfast
  set display=lastline formatoptions=tcqj nrformats=bin,hex complete+=i hlsearch viminfo
endif

colorscheme antares

nnoremap <Leader>gA 	:GHActivity<CR>
nnoremap <Leader>ga 	:GHActivity<Space>
nnoremap <Leader>gb 	:Gblame<CR>
nnoremap <Leader>gB 	:Gbrowse<CR>
nnoremap <Leader>gc 	:Gcommit<CR>
nnoremap <Leader>gd 	:Gdiff<Space>
nnoremap <Leader>gD 	:Gdiff<CR>
nnoremap <Leader>gf 	:Gfetch<Space>
nnoremap <Leader>gm 	:Gmove<Space>
nnoremap <Leader>gh 	:GHDashboard<CR>
nnoremap <Leader>gV 	:Gitv!<CR>
nnoremap <Leader>gv 	:Gitv<CR>
nnoremap <Leader>g? 	:Gitv?<CR>
vnoremap <Leader>gv 	:Gitv<CR>
vnoremap <Leader>g? 	:Gitv?<CR>
nnoremap <Leader>gs 	:Gstatus<CR>

nnoremap <C-s>s         :SaveSession<Space>
nnoremap <C-s><C-s>     :SaveSession<CR>
nnoremap <C-s>o         :OpenSession<Space>
nnoremap <C-s><C-o>     :OpenSession<CR>
nnoremap <C-s>d         :DeleteSession<Space>
nnoremap <C-s><C-d>     :DeleteSession!<CR>
nnoremap <C-s><C-c>     :CloseSession!<CR>
nnoremap <C-s>c         :CloseSession<CR>

