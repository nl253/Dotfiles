'vim-syntastic/syntastic'
endif

" MARKUP:
Plug 'dkarter/bullets.vim' 
let g:bullets_enabled_file_types = g:MARKUP

Plug 'dbmrq/vim-ditto', { 'on': ['ToggleDitto', 'DittoOn'], 'for': g:MARKUP }

let g:ditto_mode = "paragraph"

Plug 'reedes/vim-wordy', { 'on': ['Wordy', 'WordyWordy'], 'for': g:MARKUP }

" TABLE MODE:
Plug 'dhruvasagar/vim-table-mode', { 'for': g:MARKUP }
let g:table_mode_disable_mappings = 1
let g:table_mode_verbose = 0 
let g:table_mode_syntax = 1 
let g:table_mode_update_time = 800

aug TableModeActivation
    au!
    au BufEnter *.md let g:table_mode_corner = '|'
    au BufEnter *.rst let g:table_mode_corner_corner = '+' 
    au BufEnter *.rst let g:table_mode_header_fillchar = '='
    au BufEnter *.rst let g:table_mode_header_fillchar =' ='
    au BufEnter *.vorg let g:table_mode_corner_corner = '+' 
    au BufEnter *.rst let g:table_mode_corner_corner = '+' 
    au BufEnter *.rst let g:table_mode_corner = '+'
    au BufEnter *.{rst,md,vorg} nnoremap <buffer> \\ :TableModeRealign<CR>
    au BufEnter *.{rst,md,vorg} nnoremap <buffer> \, :Tableize<CR>
aug END

" MARKDOWN:
Plug 'mzlogin/vim-markdown-toc', { 'for' : 'markdown' }

let g:markdown_fenced_languages = ['vim', 'sh', 'python', 'javascript', 'rust']
let g:rst_syntax_code_list = g:markdown_fenced_languages

" WEB DEV:
"
" HTML:

for i in ['othree/html5.vim', 'othree/html5-syntax.vim', 'mattn/emmet-vim']
	Plug i, { 'for': ['html', 'xhtml', 'php', 'htmldjango', 'jinja'] } 
endfor

Plug 'cakebaker/scss-syntax.vim'
Plug 'othree/csscomplete.vim'
"let g:xml_syntax_folding = 1 " might be computationally demanding
let g:user_emmet_complete_tag = 1
let g:emmet_html5 = 1

for i in ['pangloss/vim-javascript', 'othree/javascript-libraries-syntax.vim', 'isRuslan/vim-es6']
	Plug i, { 'for' : 'javascript' } 
endfor

" Libraries:
" ---------
" - jQuery
" - underscore.js
" - lo-dash
" - Backbone.js
" - prelude.ls
" - AngularJS
" - AngularUI
" - AngularUI Router
" - React
" - Flux
" - RequireJS
" - Sugar.js
" - Jasmine
" - Chai
" - Handlebars 
" - Ramda
" - Vue
" - d3

let g:used_javascript_libs = 'jquery,'

Plug 'Glench/Vim-Jinja2-Syntax'

aug TemplateFiletypes
	au!
	au BufNew,BufNewFile *.twig,*.nunj setl ft=jinja 
aug END

" PHP:
Plug 'shawncplus/phpcomplete.vim', { 'for': 'php' }

" MY PLUGINS:
" ==========
for plugin in ['fabulous', 'vim-saner', 'vim-markup',
			\ 'vim-programming', 'vim-fzf-extensions', 
			\ 'vim-scratchpads', 'vim-templates', 'fabulous', 
			\ 'vim-webdev', 'git-ready', 'vorg-mode'] 

	if !isdirectory(expand('~').'/Projects/Vim/'.plugin)
		Plug 'nl253/'.plugin
	else
		Plug '~/Projects/Vim/'.plugin
	endif
endfor

let g:vim_dicts = { 'markdown': ['unix-programmers', 'computer-science'] } 

call plug#end()

colorscheme fabulous
