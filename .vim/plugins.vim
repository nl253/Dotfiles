" vim: foldlevel=0 foldmethod=marker nowrap

" VARIABLES:

" MARKUP languages you actively use
if !exists('g:MARKUP') | let g:MARKUP = ['markdown', 'rst', 'vorg'] | endif

" PROGRAMMING LANGUAGES you code in 
if !exists('g:PROGRAMMING')
	let g:PROGRAMMING = ['xhtml',
					   \ 'html',
					   \ 'css', 
					   \ 'javascript',
					   \ 'python',  
					   \ 'java',  
					   \ 'php', 
					   \ 'typescript',  
					   \ 'sql', 
					   \ 'sh', 
					   \ 'zsh']
endif

" Place Plugins Here:
" ===================  

" GENERAL:

for i in [
	    \ 'scrooloose/nerdcommenter', 
	   	\ 'konfekt/fastfold', 
	   	\ 'wellle/targets.vim'] 
	Plug i
endfor

" 'https://github.com/vim-scripts/SyntaxAttr.vim', 
" \ 'editorconfig/editorconfig-vim', 

" bin == just the binary, all == bin + shell keybindings
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --bin' }
Plug 'junegunn/fzf.vim'

for i in split(expand('tpope/vim-{speeddating,repeat,fugitive}'))
	Plug i
endfor

" A must have if you work with tmux
if executable('tmux') | Plug 'tmux-plugins/vim-tmux-focus-events' | endif

" GIT:
"if has('perl') | Plug 'vim-scripts/dbext.vim', {'for': ['sql', 'plsql', 'mysql']} | endif
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

if index(g:PROGRAMMING, 'rust') >= 0
	let g:rustfmt_autosave = 1
	Plug 'rust-lang/rust.vim', {'for': 'rust'}
endif

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
					\ 'typescript',
					\ 'tslint',
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

	"Plug 'tpope/vim-fireplace', {'for': 'clojure'}
	"Plug 'eagletmt/neco-ghc', {'for': 'haskell'}
	
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
							   \ 'gitcommit', 
							   \ 'markdown',  
							   \ 'scss', 
							   \ 'php']
	let g:completor_python_binary = '/usr/bin/env python3'
	let g:completor_racer_binary = expand('~/.cargo/bin/racer')
	let g:completor_rust_omni_trigger = 
				\ '(\w{3,}|\.\w*|::\{?|(use|unsafe|type|struct|fn|\w>|pub|impl|extern create|\w:) | (->|=>|=|&&|\|{2}) )'
	let g:completor_php_omni_trigger = '\$?[a-zA-Z_]{2,}|<[a-z]{,6}|\S+ [-a-z]{2,}|-> ?'
    "let g:completor_haskell_omni_trigger = 'import |\w{2,}|\.|( (->|=>|=|::|\w{2,}|\|) .+)$' 
    "let g:completor_clojure_omni_trigger = '\w{2,}' 
    let g:completor_xhtml_omni_trigger = '<\[A-Z]{,6}|\S+ [-a-z]{2,}'
	"let g:haskellmode_completion_ghc = 0
	for i in ['javascript', 'typescript', 'coffee']
		exec 'let g:completor_'.i.'_omni_trigger = "\.|\w{4,}| (=>|>|<|=) |(import|as|export|default|new|await|async|public|static|get|protected|private|instanceof|throw|yield|in|extends) "'
	endfor
	for i in ['jinja', 'jinja2', 'twig', 'nunjucks', 'html', 'htmldjango']
		exec 'let g:completor_'.i.'_omni_trigger = "<[a-z]{,6}|\S+ [-a-z]{2,}"'
	endfor
	for i in ['markdown', 'rst', 'vorg', 'gitcommit', 'yaml']
		exec 'let g:completor_'.i.'_omni_trigger = "\w{4,}" '
	endfor
	for i in ['less', 'css', 'scss', 'sass']
		exec 'let g:completor_'.i.'_omni_trigger = "(  |\t)+[-a-z]+|@([\w-]+)?|(  |\t)+-?\w+: [-\w]*"'
	endfor
	let g:completor_disable_buffer = 
				\ ['less', 'css', 'scss', 'sass', 'jinja', 'jinja2', 'twig', 'nunjucks', 
				\  'html', 'htmldjango', 'javascript', 'typescript', 'coffee', 'rust', 'python', 'php'] 
	Plug 'davidhalter/jedi-vim', {'for': 'python'} 
	let g:jedi#force_py_version = 3
	let g:jedi#goto_command = "<C-]>"
	let g:jedi#goto_assignments_command = ",a"
	let g:jedi#goto_definitions_command = ",d"
	let g:jedi#documentation_command = ",d"
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
	let g:neomake_javascript_enabled_makers = ['standard']
	let g:neomake_json_enabled_makers = ['jsonlint']
	let g:neomake_python_enabled_makers = ['mypy', 
										 \ 'flake8', 
 										 \ 'vulture',  
										 \ 'pylint',
										 \ 'pyflakes', 
										 \ 'pylama']
	if executable('yarn')
		for i in ['eslint', 'standard']
			for j in ['javascript', 'json']
				execute 'let g:neomake_'.j.'_'.i.'_exe = "'.expand('~/.yarn/bin/'.i).'"'
			endfor
		endfor
		if executable('proselint')
			for i in ['rst', 'markdown']
				execute 'let g:neomake_'.i.'_enabled_makers = '.string(['proselint', 'writegood'])
				execute 'let g:neomake_'.i.'_writegood_exe = "'.expand('~/.yarn/bin/write-good').'"'
			endfor
		endif
	endif

	if executable('mdl')
		let g:neomake_markdown_enabled_makers += ['mdl']
	endif
else
	Plug 'vim-syntastic/syntastic'
endif

" MARKUP:
Plug 'dkarter/bullets.vim' 
let g:bullets_enabled_file_types = g:MARKUP

Plug 'dbmrq/vim-ditto', {'on': ['ToggleDitto', 'DittoOn'], 'for': g:MARKUP}
let g:ditto_mode = "paragraph"
exec "au! FileType ".join(g:MARKUP, ',')." DittoOn"

Plug 'reedes/vim-wordy', {'on': ['Wordy', 'WordyWordy'], 'for': g:MARKUP}

" TABLE MODE:
"Plug 'dhruvasagar/vim-table-mode', {'for': g:MARKUP}
"let g:table_mode_disable_mappings = 1
"let g:table_mode_verbose = 0 
"let g:table_mode_syntax = 1 
"let g:table_mode_update_time = 800

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
let g:markdown_fenced_languages = g:PROGRAMMING
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

"Plug 'cakebaker/scss-syntax.vim' 
Plug 'othree/csscomplete.vim', {'for': ['less', 'css', 'scss', 'sass']}
let g:user_emmet_complete_tag = 1
let g:emmet_html5 = 1


if index(g:PROGRAMMING, 'javascript') >= 0
	for i in ['othree/javascript-libraries-syntax.vim', 'moll/vim-node', 'Quramy/vim-js-pretty-template', 'Quramy/tsuquyomi']
		Plug i, {'for': ['javascript', 'typescript']}
	endfor

	for i in ['pangloss/vim-javascript', 'isRuslan/vim-es6']
		Plug i, {'for': ['javascript']}
	endfor

	if index(g:PROGRAMMING, 'rust') >= 0
		Plug 'leafgarland/typescript-vim', {'for': ['typescript']}
	endif

	let g:tsuquyomi_completion_detail = 1
	let g:tsuquyomi_javascript_support = 1
	let g:tsuquyomi_completion_preview = 1
	let g:javascript_plugin_jsdoc = 1 
endif

Plug 'elzr/vim-json', {'for': 'json'}

" Libraries:
" ---------
" - AngularJS
" - AngularUI
" - AngularUI Router
" - Backbone.js
" - Chai
" - Flux
" - Handlebars
" - Jasmine
" - Ramda
" - React
" - RequireJS
" - Sugar.js
" - Vue
" - d3
" - jQuery
" - lo-dash
" - prelude.ls
" - underscore.js

let g:used_javascript_libs = 'jquery,react,'

" Templating Engines:
" -------------------
" Pug:

if index(g:PROGRAMMING, 'php') >= 0 ||  index(g:PROGRAMMING, 'jade') >= 0 
	Plug 'dNitro/vim-pug-complete', {'for': ['jade', 'pug']}
	Plug 'digitaltoad/vim-pug', {'for': ['jade', 'pug']}
	au! FileType jade setl ft=pug 
endif

" Jinja Twig Nunjucks:
for i in ["twig", "jinja", "jinja2", "njk", "nunjucks"]
	if index(g:PROGRAMMING, i) >= 0
		Plug 'Glench/Vim-Jinja2-Syntax'
		au! BufNewFile,BufRead *.{twig,njk} setl fs=jinja
		break
	endif
endfor

" PHP:
"
if index(g:PROGRAMMING, 'php') >= 0
	Plug 'shawncplus/phpcomplete.vim', {'for': 'php'}
endif

" MY PLUGINS:
" ==========
for plugin in ['fabulous', 'git-ready', 'vorg-mode'] + split(expand('vim-{saner,markup,programming,scratchpads,fzf-extensions,webdev,templates}'))
	Plug 'nl253/'.plugin
endfor

let g:vim_dicts = {'markdown': ['unix-programmers', 'computer-science']} 

call plug#end()
call neomake#configure#automake('rw', 1000) 
colorscheme fabulous
" vim: foldmethod=indent
