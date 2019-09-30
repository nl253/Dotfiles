" vim:ft=vim:

" Built In:
let g:omni_syntax_minimum_length   = 2

" JavaScript:
let g:javascript_plugin_jsdoc      = 1

" Go:
let g:go_fmt_autosave = 0

" NERDTree:
let g:NERDTreeMinimalUI         =  1
let g:NERDTreeChDirMode         =  2
let g:NERDTreeWinSize           = 15
let g:NERDTreeRespectWildIgnore =  0
let g:NERDTreeMouseMode         =  0
let g:NERDTreeAutoDeleteBuffer  =  1

" NERDCommenter:
let g:NERDMenuMode    = 0
let g:NERDSpaceDelims = 1

" UtilSnips:
let g:UltiSnipsEditSplit          = 'vertical'
let g:UltiSnipsSnippetDirectories = [expand('~/.vim/snips')]
let g:UltiSnipsEnableSnipMate     = 0
let g:snips_author                = 'nl253'
let g:snips_email                 = 'norbertlogiewa96@gmail.com'
let g:snips_github                = 'https://github.com/nl253'

" ALE:
let g:ale_enabled                            = 0
let g:ale_linters = {
            \ 'rust': ['rls', 'cargo', 'rustfmt'], 
            \ 'python': [
            \            'flake8', 
            \            'mypy', 
            \            'prospector', 
            \            'pycodestyle', 
            \            'pyflakes', 
            \            'pylint', 
            \            'pyls', 
            \            'pyre', 
            \            'vulture',
            \           ] 
            \ }
let g:ale_sign_error                         = 'E'
let g:ale_sign_warning                       = 'W'

" CSS:
let g:ale_css_stylelint_use_global           = 1

let g:ale_scss_stylelint_use_global          = 1

" JavaScript:
let g:ale_javascript_eslint_use_global       = 1
let g:ale_javascript_flow_use_global         = 0
let g:ale_javascript_flow_use_home_config    = 1 
let g:ale_javascript_flow_use_respect_pragma = 1
let g:ale_javascript_prettier_use_global     = 1

let g:ale_typescript_tslint_use_global       = 1
let g:ale_typescript_tsserver_use_global     = 1
let g:ale_typescript_tslint_config_path      = expand('~/.tslint.json')

" Python:
let g:ale_python_autopep8_use_global         = 1
let g:ale_python_black_use_global            = 1
let g:ale_python_flake8_use_global           = 1
let g:ale_python_isort_use_global            = 1
let g:ale_python_mypy_use_global             = 1
let g:ale_python_pylint_use_global           = 1
let g:ale_python_pyls_use_global             = 1
let g:ale_python_pyre_use_global             = 1
let g:ale_python_vulture_use_global          = 1
let g:ale_python_pycodestyle_use_global      = 1
let g:ale_python_yapf_use_global             = 1 

" Other:

let g:ale_textlint_use_global                = 1
let g:ale_yaml_swaglint_use_global           = 1
let g:ale_gitcommit_gitlint_use_global       = 1
let g:ale_html_htmlhint_use_global           = 1

" Emmet:
let g:user_emmet_install_global = 0
let g:user_emmet_complete_tag   = 1
let g:user_emmet_mode           = 'i'

" Netrw:
let g:netrw_banner         = 0
let g:netrw_browse_split   = 3
let g:netrw_hide           = 1
let g:netrw_list_hide      = '\v^[\._]|\.(beam|hi|pdf|class|lock)$|^tags$'
let g:netrw_liststyle      = 3
let g:netrw_mousemaps      = 0
let g:netrw_preview        = 1 
let g:netrw_scpport        = '-P 22' 
let g:netrw_sizestyle      = 'H'
let g:netrw_special_syntax = 1
let g:netrw_sshport        = '-p 22'
let g:netrw_wiw            = &winminwidth

" XML:
" might be computationally demanding
" better beautify and use indent-based fold
let g:xml_syntax_folding = 0 

" YAML:
let g:yaml_schema  = 'pyyaml'

" HTML:
let g:html_wrong_comments = 1
let g:html_hover_unfold   = 1
let g:html_use_xhtml      = 0
let g:html_dynamic_folds  = 0
let g:html_no_foldcolumn  = 1 
let g:html_use_encoding   = 'UTF-8'
let g:html_font = [
            \ 'Sans Serif', 
            \ 'DejaVu Sans Mono', 
            \ 'Consolas', 
            \ 'monospace'
            \ ]

hi netrwSymLink  term=NONE cterm=NONE gui=NONE ctermfg=Magenta guifg=Magenta
hi netrwCompress term=NONE cterm=NONE gui=NONE ctermfg=Yellow guifg=Yellow 
 "hi netrwData	  term=NONE cterm=NONE gui=NONE ctermfg=9 guifg=blue ctermbg=0 guibg=black
 "hi netrwHdr	  term=NONE cterm=NONE,italic gui=NONE guifg=SeaGreen1
 "hi netrwLex	  term=NONE cterm=NONE,italic gui=NONE guifg=SeaGreen1
 "hi netrwYacc	  term=NONE cterm=NONE,italic gui=NONE guifg=SeaGreen1
 "hi netrwLib	  term=NONE cterm=NONE gui=NONE ctermfg=14 guifg=yellow
 "hi netrwObj	  term=NONE cterm=NONE gui=NONE ctermfg=12 guifg=red
 "hi netrwTilde	  term=NONE cterm=NONE gui=NONE ctermfg=12 guifg=red
 "hi netrwTmp	  term=NONE cterm=NONE gui=NONE ctermfg=12 guifg=red
 "hi netrwTags	  term=NONE cterm=NONE gui=NONE ctermfg=12 guifg=red
 "hi netrwDoc	  term=NONE cterm=NONE gui=NONE ctermfg=220 ctermbg=27 guifg=yellow2 guibg=Blue3

" GitGutter:
let g:gitgutter_enabled = 0

" Bullets:
let g:bullets_enabled_file_types      = g:markup_langs + g:config_ftypes + ['gitcommit']
let g:bullets_enable_in_empty_buffers = 1

" HTML Omni:
let g:html5_event_handler_attributes_complete = 0
let g:html5_rdfa_attributes_complete          = 0
let g:html5_microdata_attributes_complete     = 0
let g:html5_aria_attributes_complete          = 0

" Lang Server:
" Use an absolute configuration path if you want system-wide settings
let g:LanguageClient_autoStart      = 0
let g:LanguageClient_settingsPath   = expand('~/.config/nvim/settings.json')
let g:LanguageClient_serverCommands = {
            \ 'c':              ['/usr/bin/clangd'],
            \ 'cpp':            ['/usr/bin/clangd'],
            \ 'css':            ['~/.config/yarn/global/node_modules/.bin/css-languageserver', '--stdio'],
            \ 'html':           ['~/.config/yarn/global/node_modules/.bin/html-languageserver', '--stdio'],
            \ 'javascript':     ['~/.config/yarn/global/node_modules/.bin/javascript-typescript-stdio'],
            \ 'json':           ['~/.config/yarn/global/node_modules/.bin/vscode-json-languageserver', '--stdio'],
            \ 'python':         ['~/.local/bin/pyls'],
            \ 'rust':           ['~/.cargo/bin/rustup', 'run', 'nightly', 'rls'],
            \ 'sh':             ['~/.config/yarn/global/node_modules/.bin/bash-language-server', 'start'],
            \ 'go':             ['~/go/bin/go-langserver'],
            \ 'sql':            ['~/.config/yarn/global/node_modules/.bin/sql-language-server', 'up', '--method', 'stdio'],
            \ 'typescript':     ['~/.config/yarn/global/node_modules/.bin/javascript-typescript-stdio'],
            \ 'yaml':           ['/usr/bin/node', '~/.config/yarn/global/node_modules/.bin/yaml-language-server', '--stdio'],
            \ }

let g:LanguageClient_rootMarkers = {
            \ 'javascript': ['package.json', '.git'],
            \ 'typescript': ['package.json', '.git'],
            \ 'json':       ['package.json', '.git'],
            \ 'python':     ['requirements.txt', 'setup.py', 'Pipfile', 'setup.cfg', 'MANIFEST.in', 'tox.ini', '.git', 'README.md', 'LICENSE', 'Makefile'],
            \ 'rust':       ['Cargo.toml', '.git'],
            \ }
