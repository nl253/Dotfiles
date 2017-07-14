" PLUG
if has('nvim')
    let g:PLUG_FILE = expand('~/.local/share/nvim/site/autoload/plug.vim')
else " if vim
    let g:PLUG_FILE = expand('~/.vim/autoload/plug.vim')
endif

" Plug - Download if missing:
if ! filereadable(g:PLUG_FILE) && executable('curl')
    echo system('mkdir -p $(dirname '.g:PLUG_FILE.') && curl -flo '.g:PLUG_FILE.' https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim')
    execute 'source '.g:PLUG_FILE
    PlugInstall
elseif ! filereadable(g:PLUG_FILE) 
    finish
endif

if has('nvim')
    call plug#begin('~/.local/share/nvim/plugged/')
else
    call plug#begin('~/.vim/plugged')
endif
" 

" VARIABLES 
if ! exists('g:MARKUP')
    " MARKUP languages you actively use  
    let g:MARKUP = [ 'markdown', 'rst', 'vorg' ]
endif

if ! exists('g:PROGRAMMING')
    " PROGRAMMING LANGUAGES you code in 
    let g:PROGRAMMING =  [ 'xhtml', 'html', 'css', 'javascript', 
                \ 'python', 'php', 'sql', 'sh', 'zsh' ]
endif

" Place Plugins Here:
" ==================

" GENERAL:
Plug 'https://github.com/vim-scripts/SyntaxAttr.vim'
Plug 'tpope/vim-sleuth' 
Plug 'tpope/vim-speeddating' 
Plug 'tpope/vim-repeat'

" A must have if you work with tmux
if executable('tmux') | Plug 'tmux-plugins/vim-tmux-focus-events' | endif

" GIT:
Plug 'tpope/vim-fugitive' 
Plug 'junegunn/gv.vim'

set statusline=%<\ %f\ %r\ %{fugitive#statusline()}%m\ %=%-14.(%{&complete}\ %{&sw}\ %{&ts}%q\ %w\ %y\ %p\ of\ %l%)\ \  

Plug 'konfekt/fastfold' 
Plug 'wellle/targets.vim'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-eunuch', { 'on' : [ 'Move', 'Remove', 'Find', 
            \'Mkdir', 'Wall', 'SudoEdit', 'Chmod',
            \'SudoWrite', 'Unlink', 'Rename' ]}

Plug 'junegunn/fzf.vim'

let g:fzf_layout = { 'up': '~40%' }

let g:fzf_action = {
            \ 'ctrl-t': 'tab split',
            \ 'ctrl-s': 'split',
            \ 'ctrl-v': 'vsplit' }

" PYTHON PLUGINS:
if (has('python') || has('python3')) && ((has('lambda') && has('job') && has('timers')) || has('nvim'))
    Plug 'SirVer/ultisnips'
    let g:UltiSnipsEditSplit = 'vertical'
    "let g:UltiSnipsSnippetDir = g:VIMDIR.'snips'
    let g:UltiSnipsSnippetDirectories = [ g:VIMDIR.'snips' ]
    let g:UltiSnipsEnableSnipMate = 0
    let g:snips_author = "nl253"
    let g:snips_email = "nl253@kent.ac.uk"
    let g:snips_github = "https://github.com/nl253"
    Plug 'maralla/completor.vim'
    let g:completor_whitelist = [ 'python', 'rust' ]
    let g:completor_python_binary = 'python3'
    let g:completor_racer_binary = expand('~/.cargo/bin/racer')
    Plug 'davidhalter/jedi-vim', {'for': 'python'}
    "let g:jedi#completions_enabled = 1
    let g:jedi#force_py_version = 3
    let g:jedi#goto_command = "<C-]>"
    let g:jedi#goto_assignments_command = ",a"
    let g:jedi#goto_definitions_command = ",d"
    let g:jedi#documentation_command = ",D"
    let g:jedi#usages_command = ",u"
    let g:jedi#rename_command = ",r"
    let g:jedi#use_splits_not_buffers = "right"
    let g:jedi#show_call_signatures_delay = 200
    Plug 'tmhedberg/SimpylFold', {'for': 'python'}
    Plug 'editorconfig/editorconfig-vim'
    let g:EditorConfig_exclude_patterns = ['fugitive://.*']
endif

let g:LintHook = '[[ -x $(which pip) ]] && [[ ! -x $(which vint) ]] && pip install --user vim-vint ; [[ -x $(which pip) ]] && [[ ! -x $(which flake8) ]] && pip install --user flake8 ; [[ -x $(which cabal) ]] && [[ ! -x $(which shellcheck) ]] && cabal update && cabal install ShellCheck ; [[ -x $(which gem) ]] && [[ ! -x $(which mdl) ]] && gem install mdl ; [[ -x $(which pip) ]] && [[ ! -x $(which proselint) ]] && pip install --user proselint ; [[ -x $(which pip) ]] && [[ ! -x $(which mypy) ]] && pip install --user mypy ; [[ -x $(which npm) ]] && [[ ! -x $(which standard) ]] && npm install standard --save-dev ; [[ -x $(which npm) ]] && [[ ! -x $(which eslint) ]] && npm install eslint ; [[ -x $(which gem) ]] && [[ ! -x $(which sqlint) ]] && gem install sqlint ; [[ -x $(which pip) ]] && [[ ! -x $(which isort) ]] && pip install --user isort'

if has('patch8') || has('nvim')
    Plug 'w0rp/ale', { 'do' : g:LintHook }
else
    Plug 'vim-syntastic/syntastic', { 'do' : g:LintHook }
endif

unlet g:LintHook

" MARKUP:
Plug 'dkarter/bullets.vim' 
let g:bullets_enabled_file_types = [ 'markdown', 'vorg', 'rst' ]

Plug 'dbmrq/vim-ditto', { 'on': [ 'ToggleDitto', 'DittoOn' ]}

let g:ditto_mode = "paragraph"

Plug 'reedes/vim-wordy', { 'on': [ 'Wordy', 'WordyWordy' ], 'for': 'vimwiki' }

" TABLE MODE:
Plug 'dhruvasagar/vim-table-mode', { 'for': [ 'rst', 'markdown', 'vorg' ] }
let g:table_mode_disable_mappings = 1
let g:table_mode_verbose = 0 
let g:table_mode_syntax = 1 
let g:table_mode_update_time = 800

aug TableModeActivation
    au!
    au BufEnter *.md let g:table_mode_corner = '|'
    au BufEnter *.rst let g:table_mode_corner_corner='+' 
    au BufEnter *.rst let g:table_mode_header_fillchar='='
    au BufEnter *.rst let g:table_mode_header_fillchar='='
    au BufEnter *.vorg let g:table_mode_corner_corner='+' 
    au BufEnter *.rst let g:table_mode_corner_corner='+' 
    au BufEnter *.rst let g:table_mode_corner = '+'
    au BufEnter *.{rst,md,vorg} nnoremap \\ :TableModeRealign<CR>
    au BufEnter *.{rst,md,vorg} nnoremap \, :Tableize<CR>
aug END

" MARKDOWN:
Plug 'mzlogin/vim-markdown-toc', { 'for' : 'markdown' }
Plug 'nelstrom/vim-markdown-folding', { 'for' : 'markdown' }

Plug 'tpope/vim-liquid', { 'for' : 'markdown' }

let g:markdown_fenced_languages =  [ 'vim', 'sh', 'python', 'javascript', 'rust' ]
let g:liquid_highlight_types = g:markdown_fenced_languages
let g:rst_syntax_code_list = g:markdown_fenced_languages

" WEB DEV:
"
" HTML:
Plug 'othree/html5.vim', { 'for': [ 'html', 'xhtml', 'php' ] }
Plug 'othree/html5-syntax.vim', { 'for': [ 'html', 'xhtml', 'php' ] }
Plug 'mattn/emmet-vim', { 'for': [ 'xml', 'html', 'xhtml', 'css', 'php' ] }
"let g:xml_syntax_folding = 1
let g:emmet_html5 = 1 

" PHP:
Plug 'shawncplus/phpcomplete.vim', { 'for': 'php' }

" FOR NVIM:
if has('nvim')
    Plug 'kassio/neoterm', { 'on' : [
                \'TREPLSendSelection', 
                \'TREPLSendLine', 
                \'TREPLSendFile' ]}
    let g:neoterm_position = 'vertical'
    let g:neoterm_keep_term_open = 0
    let g:neoterm_size = 50
endif

" RUST:
let g:rustfmt_autosave = 1
Plug 'cespare/vim-toml', { 'for': 'toml' }

" MY PLUGINS:
" ==========
for plugin in [ 
            \ 'fabulous', 'vim-saner', 'vim-markup',
            \  'vim-programming', 'vim-fzf-extensions', 
            \ 'vim-scratchpads', 'vim-templates', 'fabulous', 
            \ 'vim-utils', 'vim-webdev', 'git-ready', 'vorg-mode' ] 
    if ! isdirectory(expand('~').'/Projects/VimPlugins/'.plugin)
        Plug 'nl253/'.plugin
    else
        Plug '~/Projects/VimPlugins/'.plugin
    endif
endfor

let g:vim_dicts = { 'markdown': [ 'unix-programmers', 'computer-science' ] } 

call plug#end()

colorscheme fabulous

" vim: foldlevel=0 foldmethod=marker nowrap
