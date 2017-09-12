" vim: foldlevel=0 foldmethod=marker nowrap

" PLUG:
if has('nvim')
    let g:PLUG_FILE = expand('~/.local/share/nvim/site/autoload/plug.vim')
else " if vim
    let g:PLUG_FILE = expand('~/.vim/autoload/plug.vim')
endif

" Plug - Download if missing:
if !filereadable(g:PLUG_FILE) && executable('curl')
    echo system('mkdir -p $(dirname '.g:PLUG_FILE.') && curl -flo '.g:PLUG_FILE.' https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim')
    execute 'source '.g:PLUG_FILE
    PlugInstall
elseif !filereadable(g:PLUG_FILE) 
    finish
endif

if has('nvim')
    call plug#begin('~/.local/share/nvim/plugged/')
else
    call plug#begin('~/.vim/plugged')
endif

" Add to $PATH bin dirs for package managers in case they aren't in $PATH already
for i in ['~/node_modules/.bin', '~/.gem/ruby/*/bin'] + split(expand('~/{.fzf,.cargo,.local}/bin'))
	let bin_dir = expand(i)
	if !empty(bin_dir) && !($PATH =~ bin_dir)
		let $PATH = expand(i).':'.$PATH
	endif
endfor


" VARIABLES:

" MARKUP languages you actively use
if !exists('g:MARKUP') | let g:MARKUP = ['markdown', 'rst', 'vorg'] | endif

" PROGRAMMING LANGUAGES you code in 
if !exists('g:PROGRAMMING')
    let g:PROGRAMMING = ['xhtml', 
					   \ 'html', 
					   \ 'css', 
					   \ 'javascript', 
					   \ 'rust', 
					   \ 'python', 
					   \ 'php', 
					   \ 'sql', 
					   \ 'sh', 
					   \ 'zsh']
endif

" Place Plugins Here:
" ===================  

" GENERAL:
"
for i in ['https://github.com/vim-scripts/SyntaxAttr.vim', 
	    \ 'scrooloose/nerdcommenter', 
	  	\ 'editorconfig/editorconfig-vim', 
	   	\ 'konfekt/fastfold', 
	   	\ 'wellle/targets.vim'] 
	Plug i
endfor

for i in split(expand('tpope/vim-{speeddating,repeat,fugitive}')) + split(expand('junegunn/{fzf,fzf.vim}'))
	Plug i
endfor

" A must have if you work with tmux
if executable('tmux') | Plug 'tmux-plugins/vim-tmux-focus-events' | endif

" GIT:
if has('perl') | Plug 'vim-scripts/dbext.vim' | endif
Plug 'junegunn/gv.vim', {'on': ['GV']}

set statusline=%<\ %f\ %r\ %{fugitive#statusline()}%m\ %=%-14.(\ %{&sw}\ %{&ts}%q\ %w\ %y\ %p\ of\ %l%)\ \  

Plug 'tpope/vim-eunuch', {'on' : ['Move', 
								\ 'Remove', 
								\ 'Find', 
								\ 'Mkdir', 
								\ 'Wall', 
								\ 'SudoEdit', 
								\ 'Chmod',
								\ 'SudoWrite', 
								\ 'Unlink', 
								\ 'Rename']}

let g:fzf_layout = {'up': '~40%'}

let g:fzf_action = {'ctrl-t': 'tab split', 'ctrl-s': 'split', 'ctrl-v': 'vsplit'}

" RUST:
let g:rustfmt_autosave = 1
Plug 'rust-lang/rust.vim', {'for': 'rust'}

" PYTHON PLUGINS:
if (has('python') || has('python3')) && ((has('lambda') && has('job') && has('timers')) || has('nvim'))
	Plug 'SirVer/ultisnips'
	let g:UltiSnipsEditSplit = 'vertical'
	let g:UltiSnipsSnippetDirectories = [g:VIMDIR.'snips']
	let g:UltiSnipsEnableSnipMate = 0
	let g:snips_author = "nl253"
	let g:snips_email = "nl253@kent.ac.uk"
	let g:snips_github = "https://github.com/nl253"

	function! SyncPackages()

		if executable("yarn") 

			let yarn_packages = system('yarn global list 2>/dev/null')

			for i in ['tern', 
						\ 'scss-lint', 
						\ 'js-beautify', 
						\ 'eslint', 
						\ 'textlint', 
						\ 'write-good']
				try
					if !(yarn_packages =~ $i)
						echo system('cd && yarn global add '.i) 
					endif
				catch /.*/
					break
				endtry
			endfor
		endif

		if executable("cargo") && !executable("racer")
			echo system('cd && cargo install racer')
		endif

		if executable('pip')
			let pip_packages = systemlist("pip list --format legacy \| grep -Eo '^\\w+'")
			for i in ['jedi', 'mypy', 'pyflakes', 'vulture', 'isort', 'pylint']
				try
					if index(pip_packages, i) < 0
						echo system('cd && pip install --user --pre '.i)
					endif
				catch /.*/
					break
				endtry
			endfor
		endif
	endfunction

	command! SyncPackages call SyncPackages()

	Plug 'maralla/completor.vim'
	let g:completor_min_chars = 1
	let g:completor_whitelist = ['python', 
							   \ 'rust', 
							   \ 'yaml', 
							   \ 'javascript',
							   \ 'css', 
							   \ 'rst', 
							   \ 'html', 
							   \ 'jinja', 
							   \ 'scss.css',
							   \ 'gitcommit', 
							   \ 'markdown',  
							   \ 'scss', 
							   \ 'php']
	let g:completor_python_binary = '/usr/bin/env python3'
	let g:completor_node_binary = '/usr/bin/env node --harmony --expose-http2 --harmony_typeof'
	let g:completor_racer_binary = expand('~/.cargo/bin/racer')
	let g:completor_css_omni_trigger = '([\w-]+|@[\w-]*|[\w-]+:\s*[\w-]*)$'
	let g:completor_scss_omni_trigger = g:completor_css_omni_trigger
	let g:completor_rust_omni_trigger = 
				\ '(\w{3,}|\.\w*|::\{?|(use|unsafe|type|struct|fn|\w>|pub|impl|extern create|\w:) | (->|=>|=) )'
	let g:completor_php_omni_trigger = '\$?[a-zA-Z_]{2,}|<[a-z]{,6}|\S+ [-a-z]{2,}|-> ?'
	let g:completor_xhtml_omni_trigger = '<\[A-Z]{,6}|\S+ [-a-z]{2,}'
	let g:completor_html_omni_trigger = '<[a-z]{,6}|\S+ [-a-z]{2,}'
	let g:completor_jade_omni_trigger = '\w{2,}'
	let g:completor_javascript_omni_trigger = '\.\w*| (=>|>|<|=) '
	let g:completor_pug_omni_trigger = g:completor_jade_omni_trigger
	let g:completor_jinja_omni_trigger = g:completor_html_omni_trigger 
	let g:completor_htmldjango_omni_trigger = g:completor_html_omni_trigger 
	let g:completor_gitcommit_omni_trigger = '\w{2,}'
	let g:completor_yaml_omni_trigger = '\w{5,}'
	Plug 'davidhalter/jedi-vim', {'for': 'python'}
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
	Plug 'racer-rust/vim-racer', {'for': 'rust'}
	let g:racer_cmd = expand("~/.cargo/bin/racer")
	let g:racer_experimental_completer = 1
endif

if has('patch8') || has('nvim')
	Plug 'neomake/neomake'
	let g:neomake_yaml_enabled_makers = ['yamllint']
	let g:neomake_vim_enabled_makers = ['vint']
	let g:neomake_sql_enabled_makers = ['sqlint']
	let g:neomake_bash_enabled_makers = ['sh', 'shellcheck']
	let g:neomake_scss_enabled_makers = ['scss-lint']
	let g:neomake_css_enabled_makers = ['stylelint']
	let g:neomake_javascript_enabled_makers = ['eslint']
	let g:neomake_python_enabled_makers = ['mypy', 
										 \ 'flake8', 
 										 \ 'vulture',  
										 \ 'pylint',
										 \ 'pyflakes', 
										 \ 'pylama']
else
	Plug 'vim-syntastic/syntastic'
endif

" MARKUP:
Plug 'dkarter/bullets.vim' 
let g:bullets_enabled_file_types = g:MARKUP

Plug 'dbmrq/vim-ditto', {'on': ['ToggleDitto', 'DittoOn'], 'for': g:MARKUP}

let g:ditto_mode = "paragraph"

Plug 'reedes/vim-wordy', {'on': ['Wordy', 'WordyWordy'], 'for': g:MARKUP}

" TABLE MODE:
Plug 'dhruvasagar/vim-table-mode', {'for': g:MARKUP}
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
    au BufEnter *.{rst,md,vorg} nnoremap <buffer> \\ :TableModeRealign<CR>
    au BufEnter *.{rst,md,vorg} nnoremap <buffer> \, :Tableize<CR>
aug END

" MARKDOWN:
Plug 'mzlogin/vim-markdown-toc', {'for' : 'markdown'}

let g:markdown_fenced_languages = ['sh', 'python', 'javascript', 'css', 'scss']
let g:rst_syntax_code_list = g:markdown_fenced_languages

" WEB DEV:

for i in ['othree/html5.vim', 'othree/html5-syntax.vim', 'mattn/emmet-vim']

	Plug i, {'for': ['xml', 
				   \ 'html', 
				   \ 'xhtml', 
				   \ 'css', 
				   \ 'php', 
				   \ 'htmldjango', 
				   \ 'jinja']}
endfor

Plug 'cakebaker/scss-syntax.vim' | Plug 'othree/csscomplete.vim'
"let g:xml_syntax_folding = 1 " might be computationally demanding
let g:user_emmet_complete_tag = 1
let g:emmet_html5 = 1

for i in ['pangloss/vim-javascript', 'isRuslan/vim-es6', 'othree/javascript-libraries-syntax.vim']
	Plug i, {'for': ['javascript']}
endfor

Plug 'elzr/vim-json', {'for': 'json'}

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

let g:used_javascript_libs = 'jquery,react,'
au! BufRead,BufNewFile *.ts,*.tsx setl ft=javascript

" Plug 'dNitro/vim-pug-complete', {'for': ['jade', 'pug']}
" Plug 'digitaltoad/vim-pug', {'for': ['jade', 'pug']}

Plug 'Glench/Vim-Jinja2-Syntax'


" PHP:
Plug 'shawncplus/phpcomplete.vim', {'for': 'php'}

" MY PLUGINS:
" ==========
for plugin in ['fabulous', 'fabulous', 'git-ready', 'vorg-mode'] + split(expand('vim-{saner,markup,programming,scratchpads,fzf-extensions,webdev,templates}'))
	if !isdirectory(expand('~/Projects/Vim/'.plugin))
		Plug 'nl253/'.plugin
	else
		Plug '~/Projects/Vim/'.plugin
	endif
endfor

let g:vim_dicts = {'markdown': ['unix-programmers', 'computer-science']} 

call plug#end()

colorscheme fabulous
