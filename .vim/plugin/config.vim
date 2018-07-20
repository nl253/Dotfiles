" vim:ft=vim:
let g:rust_fold = 2
let g:rust_bang_comment_leader = 1

" VimRacer:
let g:racer_experimental_completer = 1

" VimProgramming:
let g:haskell_indent_if    = 2
let g:haskell_indent_where = 2
let g:haskell_indent_do    = 2

" VimJavaScript:
let g:javascript_plugin_jsdoc = 1

" VimGo:
let g:go_fmt_autosave = 0

" NERDTree:
let g:NERDTreeMinimalUI         = 1
let g:NERDTreeChDirMode         = 2
let g:NERDTreeWinSize           = 15
let g:NERDTreeRespectWildIgnore = 0
let g:NERDTreeMouseMode         = 0
let g:NERDTreeAutoDeleteBuffer  = 1

" NERDCommenter:
let g:NERDMenuMode = 0
let g:NERDSpaceDelims = 1

" UltiSnips:
let g:UltiSnipsEditSplit          = 'vertical'
let g:UltiSnipsSnippetDirectories = [expand('~/.vim/snips')]
let g:UltiSnipsEnableSnipMate     = 0
let g:snips_author                = 'nl253'
let g:snips_email                 = 'norbertlogiewa96@gmail.com'
let g:snips_github                = 'https://github.com/nl253'

" ALE:
let g:ale_css_stylelint_use_global           = 1
let g:ale_typescript_tslint_use_global       = 1
let g:ale_enabled                            = 0
let g:ale_typescript_tsserver_use_global     = 1
let g:ale_typescript_tslint_config_path      = expand('~/.tslint.json')
let g:ale_gitcommit_gitlint_use_global       = 1
let g:ale_html_htmlhint_use_global           = 1
let g:ale_javascript_eslint_use_global       = 1
let g:ale_javascript_flow_use_global         = 0
let g:ale_javascript_flow_use_home_config    = 1 
let g:ale_javascript_flow_use_respect_pragma = 1
let g:ale_javascript_prettier_use_global     = 1
let g:ale_scss_stylelint_use_global          = 1
let g:ale_sign_error                         = 'E'
let g:ale_sign_warning                       = 'W'
let g:ale_textlint_use_global                = 1
let g:ale_yaml_swaglint_use_global           = 1
let g:ale_linters = {'rust': ['rls', 'cargo', 'rustfmt']}

" Emmet:
let g:user_emmet_install_global = 0
let g:user_emmet_complete_tag   = 1
let g:user_emmet_mode           = 'i'

" VimTex:
let g:vimtex_syntax_minted = map(g:prog_langs + ['xml'], '{"lang": v:val}')
let g:vimtex_compiler_latexmk = {
            \ 'backend' : has('nvim') ? 'nvim' : 'jobs',
            \ 'background' : 1,
            \ 'build_dir' : '../build',
            \ 'callback' : has('clientserver') || has('nvim') ? 1 : 0,
            \ 'continuous' : 1,
            \ 'executable' : 'latexmk',
            \ 'options' : [
            \   '-pdf',
            \   '-view=pdf',
            \   '-verbose',
            \   '-output-directory=../build',
            \   '-file-line-error',
            \   '-synctex=1',
            \   '-print=pdf',
            \   '-aux-directory=/tmp/latexmk/aux',
            \   '-interaction=nonstopmode',
            \ ],
            \}

" Netrw:
let g:netrw_banner         = 0
let g:netrw_browse_split   = 3
let g:netrw_hide           = 1
let g:netrw_list_hide      = '\v^[\._]|\.(beam|hi|pdf|class|lock)$|^tags$'
let g:netrw_liststyle      = 3
let g:netrw_sizestyle      = 'H'
let g:netrw_special_syntax = 1
let g:netrw_wiw            = &winminwidth

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
let g:bullets_enabled_file_types      = g:markup_langs + g:config_ftypes
let g:bullets_enable_in_empty_buffers = 1

" HTML Omni:
let g:html5_event_handler_attributes_complete = 0
let g:html5_rdfa_attributes_complete          = 0
let g:html5_microdata_attributes_complete     = 0
let g:html5_aria_attributes_complete          = 0
