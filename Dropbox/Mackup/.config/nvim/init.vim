
if has('nvim')
    call plug#begin('~/.local/share/nvim/plugged/')
else
    call plug#begin('~/.vim/plugged')
endif

" Place plugins here
Plug 'Haron-Prime/Antares'
Plug 'tpope/vim-dispatch', {'on' : ['Make','Dispatch','Copen','Start','Spawn']}
Plug 'tpope/vim-sleuth' " auto set buffer options
Plug 'xolox/vim-misc'
Plug 'xolox/vim-session' " enhanced session management

let g:session_autosave = 'yes' "
let g:session_default_name = 'default'
let g:session_autosave_to = 'default'
let g:session_autosave_periodic = 3
let g:session_lock_enabled = 0
let g:session_autoload = 'yes'
let g:session_persist_colors = 1
let g:session_default_to_last = 1
let g:session_persist_globals = [
	    \ '&foldmethod', '&foldcolumn',
	    \ '&scrolloff', '&scrollbind', '&wrap',
	    \ '&spell', '&number', '&relativenumber',
	    \ '&foldmarker', '&background']

nnoremap <C-s>s         :SaveSession<Space>
nnoremap <C-s><C-s>     :SaveSession<CR>
nnoremap <C-s>o         :OpenSession<Space>
nnoremap <C-s><C-o>     :OpenSession<CR>
nnoremap <C-s>d         :DeleteSession<Space>
nnoremap <C-s><C-d>     :DeleteSession!<CR>
nnoremap <C-s><C-c>     :CloseSession!<CR>
nnoremap <C-s>c         :CloseSession<CR>


if has('nvim')
    let g:session_directory = '~/.config/nvim/session'
elseif has('unix')
    let g:session_directory = '~/.vim/session'
endif

Plug 'mattn/webapi-vim'
Plug 'tpope/vim-git'
Plug 'tpope/vim-fugitive'
Plug 'gregsexton/gitv', { 'on': 'Gitv' }
Plug 'airblade/vim-gitgutter', { 'on' : [
	    \'GitGutterDisable', 'GitGutterToggle', 'GitGutterEnable',
	    \'GitGutterLineHighlightToggle', 'GitGutterLineHighlightsToggle']}

let g:gitgutter_map_keys = 0
let g:gitgutter_diff_args = '-w'

if has('nvim')
  Plug 'kassio/neoterm'
  let g:neoterm_position = 'vertical'
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
if executable('vint')
    let g:neomake_vim_enabled_makers = ['vint']
endif
if executable('tidy')
    let g:syntastic_html_checkers = ['tidy']
endif
 if executable('jsonlint')
    let g:syntastic_json_checkers = ['jsonlint']
  endif
let g:neomake_markdown_enabled_makers = [ 'mdl', 'textlint', 'writegood', 'proselint' ]
Plug 'dbmrq/vim-ditto', { 'on': [
	    \'ToggleDitto', 'DittoOff',
	    \'DittoOn', 'DittoSent',
	    \'DittoSentOn', 'DittoFile',
	    \'DittoFileOn', 'DittoPar',
	    \'DittoParOn']}

Plug 'Chiel92/vim-autoformat', { 'on': 'Autoformat' }

Plug 'vim-scripts/utl.vim'

Plug 'dhruvasagar/vim-table-mode', {
      \'on': [ 'TableModeToggle', 'TableModeDisable',
      \'TableModeEnable', 'Tableize' ]}

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

let g:pymode_rope = 1
let g:pymode_syntax = 1
let g:pymode_paths = ['~/bin/']
let g:pymode_options_colorcolumn = 1
let g:pymode_rope_goto_definition_bind = '<C-e><C-e>'
let g:pymode_motion = 1
let g:pymode_doc_bind = '<C-q><C-q>'
let g:pymode_rope_show_doc_bind = '<localleader>d'
let g:pymode_run_bind = '<leader>me'
let g:pymode_breakpoint_bind = '<localleader>b'
let g:pymode_rope_completion = 1
let g:pymode_rope_completion = 1
let g:pymode_rope_complete_on_dot = 1
let g:pymode_rope_autoimport = 1
let g:pymode_rope_autoimport_import_after_complete = 1
let g:pymode_rope_rename_bind = '<localleader>rr'
let g:pymode_rope_rename_module_bind = '<localleader>rm'
let g:pymode_rope_organize_imports_bind = '<localleader>i'
let g:pymode_rope_module_to_package_bind = '<localleade>rmp'
let g:pymode_rope_extract_method_bind = '<localleader>em'
let g:pymode_rope_extract_variable_bind = '<localleader>ev'
let g:pymode_rope_use_function_bind = '<localleader>uf'
let g:pymode_rope_move_bind = '<localleader>mm'
let g:pymode_rope_change_signature_bind = '<localleader>cs'
let g:pymode_doc = 1
let g:pymode_syntax_all = 1
let g:pymode_syntax_print_as_function = 1
let g:pymode_indent = 1
let g:pymode_trim_whitespaces = 1
let g:pymode_rope_regenerate_on_write = 0
let g:pymode_python = 'python3'
let g:pymode_options = 1
let g:pymode_lint = 0 " Neomake is better

Plug 'othree/html5.vim', { 'for': ['html', 'xhtml']}
Plug 'othree/html5-syntax.vim'
Plug 'mattn/emmet-vim', { 'for': [ 'scss', 'xml',
	    \ 'html', 'xhtml', 'css', 'sass' ]}

Plug 'chrisbra/Colorizer', { 'for': [
	    \ 'css', 'html', 'javascript',
	    \ 'json', 'less', 'markdown',
	    \ 'org', 'sass', 'ghmarkdown', 'pandoc',
	    \ 'scss', 'xhtml', 'yaml']}

let g:emmet_html5 = 1

Plug 'pangloss/vim-javascript', { 'for': 'javascript' }

Plug 'maksimr/vim-jsbeautify', { 'for': [ 'javascript', 'json', 'coffee', 'html',
	    \'xhtml', 'xml', 'css', 'sass', 'scss', 'less'] }

call plug#end()

set clipboard=unnamed,unnamedplus path+=~/**
let loaded_matchit = 1
let mapleader = " "
set foldmethod=marker completeopt=menuone,longest,preview,noinsert diffopt=filler,vertical,iwhite
set complete=.,w bufhidden=hide shiftwidth=4 autowriteall undofile hidden
set mps+=<:>
au! FileType * if exists("+omnifunc") && &omnifunc == "" | setlocal omnifunc=syntaxcomplete#Complete | endif
au! FileType * if exists("+completefunc") && &completefunc == "" | setlocal completefunc=syntaxcomplete#Complete | endif
au! BufEnter * try | lchdir %:p:h | catch /.*/ | endtry
try | tnoremap <Esc> <C-\><C-n> | catch /.*/ | endtry

au! BufReadPost * if line("'\"") > 0 && line("'\"") <= line("$") | exe "normal! g`\"" | endif
au! FileType markdown setl ft=ghmarkdown

if ! has('nvim')
    syntax enable
    filetype plugin indent on
    set filetype=on syntax=on encoding=utf8
    set autoindent nocompatible magic incsearch tags ttyfast viminfo hlsearch autoread
    set display=lastline set formatoptions=tcqj nrformats=bin,hex set complete+=i
    syntax enable
endif

colorscheme antares

nnoremap <Leader>gA :GHActivity<CR>
nnoremap <Leader>ga :GHActivity<Space>
nnoremap <Leader>gb :Gblame<CR>
nnoremap <Leader>gB :Gbrowse<CR>
nnoremap <Leader>gc :Gcommit<CR>
nnoremap <Leader>gd :Gdiff<Space>
nnoremap <Leader>gf :Gfetch<Space>
nnoremap <Leader>gm :Gmove<Space>
nnoremap <Leader>gh :GHDashboard<CR>
nnoremap <Leader>gV :Gitv!<CR>
nnoremap <Leader>gv :Gitv<CR>
nnoremap <Leader>g? :Gitv?<CR>
vnoremap <Leader>gv :Gitv<CR>
vnoremap <Leader>g? :Gitv?<CR>
nnoremap <Leader>gs :Gstatus<CR>





