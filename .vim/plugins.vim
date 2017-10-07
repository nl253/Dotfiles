" vim: foldlevel=0 foldmethod=marker nowrap

" VARIABLES:

" MARKUP languages you actively use (markdown, rst asciidoc etc.)
if !exists('g:MARKUP_LANGUAGES') 
	let g:MARKUP_LANGUAGES = ['markdown', 'rst', 'vorg'] 
endif

" TEMPLATE languages you actively use (jinja, pug etc.)
if !exists('g:TEMPLATE_LANGUAGES') 
	let g:TEMPLATE_LANGUAGES = ['jinja'] 
endif

" STYLESHEET lanugages (css, pcss, postcss, stylus, sass etc.)
if !exists('g:STYLESHEET_LANGUAGES') 
	let g:STYLESHEET_LANGUAGES = ['css'] 
endif

" PROGRAMMING LANGUAGES you code in  (python, c, cpp etc.)
if !exists('g:PROGRAMMING_LANGUAGES')
	let g:PROGRAMMING_LANGUAGES = ['html', 'sql', 'cpp', 'python', 'sh']
endif

" Place Plugins Here:
" ===================  

" GENERAL:

for i in ['scrooloose/nerdcommenter', 
	   	\ 'konfekt/fastfold', 
	   	\ 'wellle/targets.vim'] 
	Plug i
endfor

let g:NERDSpaceDelims = 1

if index(g:PROGRAMMING_LANGUAGES, 'vim') >= 0
	Plug 'vim-scripts/SyntaxAttr.vim'
endif

if !executable('fzf')
	" bin == just the binary, all == bin + shell keybindings
	Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --bin' }
endif

Plug 'junegunn/fzf.vim'

let g:fzf_layout = {'up': '~40%'}
let g:fzf_action = {'ctrl-t': 'tab split', 'ctrl-s': 'split', 'ctrl-v': 'vsplit'}

for i in split(expand('tpope/vim-{speeddating,repeat,fugitive}'))
	Plug i
endfor

" A must have if you work with tmux
if executable('tmux') && exists('$TMUX') | Plug 'tmux-plugins/vim-tmux-focus-events' | endif

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

" CPP:

if index(g:PROGRAMMING_LANGUAGES, 'cpp') >= 0 && (executable("g++") || executable("clang"))
	for i in ['octol/vim-cpp-enhanced-highlight', ]
		Plug i, {'for': ['c', 'cpp']}
	endfor

	if has('python') || has('python3')
		Plug 'Rip-Rip/clang_complete', {'for': ['c', 'cpp'], 'do': 'make install'}
		" path to directory where library can be found
		"
		let g:clang_library_path='/usr/lib/libclang.so.5.0'
		let g:clang_complete_optional_args_in_snippets = 1
		let g:clang_complete_auto = 1
		let g:clang_trailing_placeholder = 1
		let g:clang_snippets = 1
		let g:clang_snippets_engine = 'ultisnips'
		let g:clang_close_preview = 1
		let g:clang_complete_macros = 1
		let g:clang_complete_patterns = 1
		let g:clang_user_options = '-std=c++17 -faligned-allocation -Wdeprecated -frelaxed-template-template-args -fsized-deallocation -fno-dollars-in-identifiers -fmodules -fcxx-exceptions -fcoroutines-ts -fblocks -fexceptions -I/usr/include/c++/7.2.0/ -I/usr/include/'
	else
		Plug 'vim-scripts/OmniCppComplete', {'for': ['c', 'cpp']}
		" OmniCppComplete
		let OmniCpp_NamespaceSearch = 1
		let OmniCpp_GlobalScopeSearch = 1
		let OmniCpp_ShowAccess = 1
		let OmniCpp_ShowPrototypeInAbbr = 1 " show function parameters
		let OmniCpp_MayCompleteDot = 1 " autocomplete after .
		let OmniCpp_MayCompleteArrow = 1 " autocomplete after ->
		let OmniCpp_MayCompleteScope = 1 " autocomplete after ::
		let OmniCpp_DefaultNamespaces = ["std", "_GLIBCXX_STD"]
	endif
endif

" RUST:

if index(g:PROGRAMMING_LANGUAGES, 'rust') >= 0 && executable("rustc")
	for i in ["rust-lang/rust.vim"]
		Plug i, {'for': 'rust'}
	endfor
	if executable(expand('~/.cargo/bin/rustfmt'))
		let g:rustfmt_autosave = 1
	endif
	if executable('~/.cargo/bin/racer')
		Plug 'racer-rust/vim-racer', {'for': 'rust'}
		let g:racer_cmd = expand("~/.cargo/bin/racer")
		let g:racer_experimental_completer = 1
	endif
endif

" PYTHON PLUGINS:

if executable("pacman") && system("hostname") =~ "Chummy-Laptop"
	if has('python') || has('python3')
		if (has('lambda') && has('job') && has('timers') && has("patch-7.4.1578")) || has('nvim')
			Plug 'SirVer/ultisnips'
			let g:UltiSnipsEditSplit = 'vertical'
			let g:UltiSnipsSnippetDirectories = [g:VIMDIR.'snips']
			let g:UltiSnipsEnableSnipMate = 0
			let g:snips_author = "nl253"
			let g:snips_email = "norbertlogiewa96@gmail.com"
			let g:snips_github = "https://github.com/nl253"
			
			" let g:COMPLETOR_FILETYPES = g:PROGRAMMING_LANGUAGES + 
						" \ g:TEMPLATE_LANGUAGES + g:STYLESHEET_LANGUAGES + ['html', 'yaml', 'gitcommit', 'xhtml'] + g:MARKUP_LANGUAGES

			" for i in ['c', 'cpp'] " managed by clang complete
				" call remove(g:COMPLETOR_FILETYPES, i)
			" endfor

			" let g:completor_whitelist = g:COMPLETOR_FILETYPES

			" Plug 'maralla/completor.vim', {'for': g:COMPLETOR_FILETYPES}

			" au! Filetype * if !exists("g:completor_".&ft."_omni_trigger") | exec "let g:completor_".&ft."_omni_trigger = '\w{2,}'" | endif

			" let g:completor_min_chars = 1
			" let g:completor_python_binary = substitute(system("/usr/bin/env python3"), " ", "", "g")
			" let g:completor_xhtml_omni_trigger = '<\[A-Z]{,6}|\S+ [-a-z]{2,}'

			" for i in ['c', 'cpp']
				" if index(g:PROGRAMMING_LANGUAGES, i) >= 0
					" exec 'let g:completor_'.i.'_omni_trigger = "\w{2,}|\.|->|::"'
				" endif
			" endfor

			" if index(g:PROGRAMMING_LANGUAGES, 'rust') >= 0
				" if executable('~/.cargo/bin/racer')
					" let g:completor_racer_binary = expand('~/.cargo/bin/racer')
				" endif
				" let g:completor_rust_omni_trigger = 
							" \ '(\w{3,}|\.\w*|::\{?|(use|unsafe|type|struct|fn|\w>|pub|impl|extern create|\w:) | (->|=>|=|&&|\|{2}) )'
			" endif

			" if index(g:PROGRAMMING_LANGUAGES, 'php') >= 0
				" let g:completor_php_omni_trigger = '\$?[a-zA-Z_]{2,}|<[a-z]{,6}|\S+ [-a-z]{2,}|-> ?'
			" endif

			" if index(g:PROGRAMMING_LANGUAGES, 'haskell') >= 0
				" let g:haskellmode_completion_ghc = 0
				" let g:completor_haskell_omni_trigger = 'import |\w{2,}|\.|( (->|=>|=|::|\w{2,}|\|) .+)$' 
			" endif

			" if index(g:PROGRAMMING_LANGUAGES, 'javascript') >= 0
				" for i in ['javascript', 'typescript']
					" exec 'let g:completor_'.i.'_omni_trigger = "\.|\w{4,}| (=>|>|<|=) |(import|as|export|default|new|await|async|public|static|get|protected|private|instanceof|throw|yield|in|extends) "'
				" endfor
			" endif

			" for i in g:TEMPLATE_LANGUAGES
				" exec 'let g:completor_'.i.'_omni_trigger = "<[a-z]{,6}|\S+ [-a-z]{2,}"'
			" endfor

			" for i in g:STYLESHEET_LANGUAGES
				" exec 'let g:completor_'.i.'_omni_trigger = "(  |\t)+[-a-z]+|@([\w-]+)?|(  |\t)+-?\w+: [-\w]*"'
			" endfor

			" let g:completor_disable_buffer = g:PROGRAMMING_LANGUAGES + g:STYLESHEET_LANGUAGES
			" call add(g:completor_disable_buffer, 'html')

			if index(g:PROGRAMMING_LANGUAGES, 'python') >= 0 && executable("python")
				Plug 'davidhalter/jedi-vim', {'for': 'python'} 
				let g:jedi#force_py_version = 3
				let g:jedi#goto_command = "<C-]>"
				let g:jedi#goto_assignments_command = "<localleader>a"
				let g:jedi#goto_definitions_command = "<localleader>d"
				let g:jedi#documentation_command = "<localleader>d"
				let g:jedi#usages_command = "<localleader>u"
				let g:jedi#rename_command = "<localleader>r"
				let g:jedi#use_splits_not_buffers = "right"
				let g:jedi#show_call_signatures_delay = 200
				Plug 'tmhedberg/SimpylFold', {'for': 'python'}
			endif
		endif
	endif
endif

" Haskell:

if index(g:PROGRAMMING_LANGUAGES, 'haskell') >= 0
	Plug 'eagletmt/neco-ghc', {'for': 'haskell'}
endif

" Clojure:

if index(g:PROGRAMMING_LANGUAGES, 'clojure') >= 0
	Plug 'tpope/vim-fireplace', {'for': 'clojure'}
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
	let g:neomake_python_enabled_makers = []

	if index(g:PROGRAMMING_LANGUAGES, 'python') >= 0 
		for i in ['mypy', 'flake8', 'vulture',  'pylint', 'pyflakes', 'pylama']
			if executable(i)
				call add(g:neomake_python_enabled_makers, i)
			endif
		endfor
	endif

	if executable('yarn')
		if index(g:PROGRAMMING_LANGUAGES, 'javascript') >= 0 
			for i in ['eslint', 'standard']
				for j in ['javascript', 'json']
					execute 'let g:neomake_'.j.'_'.i.'_exe = "'.expand('~/.yarn/bin/'.i).'"'
				endfor
			endfor
		endif
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

let g:bullets_enabled_file_types = g:MARKUP_LANGUAGES

Plug 'dbmrq/vim-ditto', {'on': ['ToggleDitto', 'DittoOn'], 'for': g:MARKUP_LANGUAGES}
let g:ditto_mode = "paragraph"

exec "au! FileType ".join(g:MARKUP_LANGUAGES, ',')." DittoOn"

Plug 'reedes/vim-wordy', {'on': ['Wordy', 'WordyWordy'], 'for': g:MARKUP_LANGUAGES}

" TABLE MODE:
"Plug 'dhruvasagar/vim-table-mode', {'for': g:MARKUP_LANGUAGES}
"let g:table_mode_disable_mappings = 1
"let g:table_mode_verbose = 0 
"let g:table_mode_syntax = 1 
"let g:table_mode_update_time = 800

"aug TableModeActivation
    "au!
    "au BufEnter *.md let g:table_mode_corner = '|'
    "au BufEnter *.rst let g:table_mode_corner_corner='+' 
    "au BufEnter *.rst let g:table_mode_header_fillchar='='
    "au BufEnter *.rst let g:table_mode_header_fillchar='='
    "au BufEnter *.vorg let g:table_mode_corner_corner='+' 
    "au BufEnter *.rst let g:table_mode_corner_corner='+' 
    "au BufEnter *.rst let g:table_mode_corner = '+'
    "au BufEnter *.{rst,md,vorg} nnoremap <buffer> \\ :TableModeRealign<CR>
    "au BufEnter *.{rst,md,vorg} nnoremap <buffer> \, :Tableize<CR>
"aug END

" MARKDOWN:
let g:markdown_fenced_languages = g:PROGRAMMING_LANGUAGES
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

Plug 'othree/csscomplete.vim', {'for': g:STYLESHEET_LANGUAGES}
let g:user_emmet_complete_tag = 1
let g:emmet_html5 = 1

if index(g:PROGRAMMING_LANGUAGES, 'javascript') >= 0 
	for i in ['othree/javascript-libraries-syntax.vim', 'moll/vim-node', 'Quramy/vim-js-pretty-template', 'Quramy/tsuquyomi']
		Plug i, {'for': ['javascript', 'typescript']}
	endfor

	for i in ['pangloss/vim-javascript', 'isRuslan/vim-es6']
		Plug i, {'for': ['javascript']}
	endfor

	if index(g:PROGRAMMING_LANGUAGES, 'typescript') >= 0 && executable("tsc")
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
if index(g:TEMPLATE_LANGUAGES, 'pug') >= 0 ||  index(g:TEMPLATE_LANGUAGES, 'jade') >= 0 
	Plug 'dNitro/vim-pug-complete', {'for': ['jade', 'pug']}
	Plug 'digitaltoad/vim-pug', {'for': ['jade', 'pug']}
	au! FileType jade setl ft=pug 
endif

" Jinja Twig Nunjucks:
for i in ["twig", "jinja", "jinja2", "njk", "nunjucks"]
	if index(g:TEMPLATE_LANGUAGES, i) >= 0
		Plug 'Glench/Vim-Jinja2-Syntax'
		au! BufNewFile,BufRead *.{twig,njk} setl fs=jinja
		break
	endif
endfor

" PHP:

if index(g:PROGRAMMING_LANGUAGES, 'php') >= 0 && executable("php")
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
