" vim: foldlevel=0 foldmethod=marker nowrap

" VARIABLES: (unfortunates these need to be global)

let g:markup_languages = ['markdown', 'rst', 'vorg'] 
let g:template_languages = ['jinja'] 
let g:stylesheet_languages = ['css'] 
let g:programming_languages = ['html', 'sql', 'python', 'sh', 'vim', 'java']

" Place Plugins Here:
" ===================  

" GENERAL:

for i in ['scrooloose/nerdcommenter', 'konfekt/fastfold', 'wellle/targets.vim'] 
	Plug i
endfor

let g:NERDSpaceDelims = 1

Plug 'majutsushi/tagbar', {'on': ['Tagbar', 'TagbarOpen']}

aug TagBarAutoOpen
	au!
	au FileType java,javascript,python,rust,sh,cpp,php,zsh if !&diff && (winnr("$") == 1) | TagbarOpen | endif
aug END

if index(g:programming_languages, 'vim') >= 0
	Plug 'vim-scripts/SyntaxAttr.vim'
endif

" if has('unix') && empty(expand('~/AppData'))
	Plug 'junegunn/fzf', {'dir': expand('~/.local/share/fzf'), 'do': './install --bin'}
" endif

if executable('fzf') | Plug 'junegunn/fzf.vim' | endif

let g:fzf_layout = {'up': '~40%'}
let g:fzf_action = {'ctrl-t': 'tab split', 'ctrl-s': 'split', 'ctrl-v': 'vsplit'}

for i in ['tpope/vim-speeddating' , 'tpope/vim-repeat', 'tpope/vim-fugitive']
	Plug i
endfor

if executable('cargo') && index(g:markup_languages, 'markdown') >= 0
	function! BuildComposer(info)
		if a:info.status != 'unchanged' || a:info.force
			silent call system(has('nvim') ? "cargo build --release" : "cargo build --release --no-default-features --features json-rpc")
		endif
	endfunction
	Plug 'euclio/vim-markdown-composer', {'do': function('BuildComposer')}
endif

" A must have if you work with tmux
if executable('tmux') && exists('$TMUX') | Plug 'tmux-plugins/vim-tmux-focus-events' | endif

" GIT:
Plug 'junegunn/gv.vim', {'on': 'GV'}

set statusline=%<\ %f\ %r\ %{fugitive#statusline()}%m\ %=%-14.(\ %{&sw}\ %{&ts}%q\ %w\ %y\ %p\ of\ %l%)\ \  

if has('unix')
	Plug 'tpope/vim-eunuch', {'on' : ['Move', 'Remove', 'Find', 'Mkdir', 'Wall', 'SudoEdit', 
									\ 'Chmod', 'SudoWrite', 'Unlink', 'Rename']} 
endif

" Rust: 

if index(g:programming_languages, 'rust') >= 0 && executable('rustc')
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


if has('python') || has('python3')
	Plug 'SirVer/ultisnips'
	let g:UltiSnipsEditSplit = 'vertical'
	let g:UltiSnipsSnippetDirectories = [expand('~/.vim/snips')]
	let g:UltiSnipsEnableSnipMate = 0
	let g:snips_author = "nl253"
	let g:snips_email = "norbertlogiewa96@gmail.com"
	let g:snips_github = "https://github.com/nl253"

	" Python:
	if index(g:programming_languages, 'python') >= 0 && executable('python')
		Plug 'davidhalter/jedi-vim', {'for': 'python'} 
		let g:jedi#force_py_version = 3
		let g:jedi#goto_command = "<C-]>"
		let g:jedi#goto_assignments_command = "<localleader>a"
		let g:jedi#goto_definitions_command = "<localleader>d"
		let g:jedi#documentation_command = "<localleader>d"
		let g:jedi#usages_command = "<localleader>u"
		let g:jedi#rename_command = "<localleader>r"
		let g:jedi#use_splits_not_buffers = 'right'
		let g:jedi#show_call_signatures_delay = 200
		Plug 'tmhedberg/SimpylFold', {'for': 'python'}
	endif

	" CPP C:
	if index(g:programming_languages, 'cpp') >= 0 || index(g:programming_languages, 'c')  >= 0
		Plug 'octol/vim-cpp-enhanced-highlight', {'for': ['c', 'cpp']}

		if executable('clang')
			Plug 'Rip-Rip/clang_complete', {'for': ['c', 'cpp'], 'do': 'make install'}
			" path to directory where library can be found
			let g:clang_library_path='/usr/lib/libclang.so.5.0'
			let g:clang_complete_optional_args_in_snippets = 1
			let g:clang_complete_auto = 1
			let g:clang_trailing_placeholder = 1
			let g:clang_snippets = 1
			let g:clang_snippets_engine = 'ultisnips'
			let g:clang_close_preview = 1
			let g:clang_complete_macros = 1
			let g:clang_complete_patterns = 1
			let g:clang_user_options = '-std=c++17 -faligned-allocation -Wdeprecated '.
						\ '-frelaxed-template-template-args -fsized-deallocation '.
						\ '-fno-dollars-in-identifiers -fmodules -fcxx-exceptions '.
						\ '-fcoroutines-ts -fblocks -fexceptions -I/usr/include/c++/7.2.0/ -I/usr/include/'
		endif 
	endif 
endif

" Haskell:

if index(g:programming_languages, 'haskell') >= 0
	Plug 'eagletmt/neco-ghc', {'for': 'haskell'}
endif

" Clojure:

if index(g:programming_languages, 'clojure') >= 0
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

	if index(g:programming_languages, 'python') >= 0 
		for i in ['mypy', 'flake8', 'vulture',  'pylint', 'pyflakes', 'pylama']
			if executable(i)
				call add(g:neomake_python_enabled_makers, i)
			endif
		endfor
	endif

	if executable('yarn')
		if index(g:programming_languages, 'javascript') >= 0 
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

	if executable('mdl') && exists('g:neomake_markdown_enabled_makers')
		let g:neomake_markdown_enabled_makers += ['mdl']
	endif
else
	Plug 'vim-syntastic/syntastic'
endif

" MARKUP:

Plug 'dkarter/bullets.vim', {'for': g:markup_languages + ['gitcommit', 'gitconfig', 'yaml', 'cfg', 'conf', 'config']}

let g:bullets_enabled_file_types = g:markup_languages

Plug 'dbmrq/vim-ditto', {'on': ['ToggleDitto', 'DittoOn'], 'for': g:markup_languages}
let g:ditto_mode = 'paragraph'

exec "au! FileType ".join(g:markup_languages, ',')." DittoOn"

Plug 'reedes/vim-wordy', {'on': ['Wordy', 'WordyWordy'], 'for': g:markup_languages}

" MARKDOWN:
let g:markdown_fenced_languages = g:programming_languages
let g:rst_syntax_code_list = g:markdown_fenced_languages

" WEB DEV:

for i in ['othree/html5.vim', 'othree/html5-syntax.vim']
	Plug i, {'for': ['xml', 'html', 'xhtml', 'php', 'markdown'] + g:stylesheet_languages + g:template_languages}
endfor

" if has('nvim') || (has('python3') || has('python'))
	" Plug 'mattn/emmet-vim', {'for': ['xml', 'html', 'xhtml', 'php', 'markdown'] + g:stylesheet_languages + g:template_languages}
	" let g:user_emmet_expandabbr_key = '<Tab>'
	" " let g:user_emmet_complete_tag = 1
	" " let g:user_emmet_mode = 'i'
	" let g:emmet_html5 = 1
	" " let g:user_emmet_install_global = 0 
" endif

Plug 'othree/csscomplete.vim', {'for': g:stylesheet_languages + ['xml', 'html', 'xhtml', 'php']}

if index(g:programming_languages, 'javascript') >= 0 
	for i in ['othree/javascript-libraries-syntax.vim', 'moll/vim-node', 'Quramy/vim-js-pretty-template', 'Quramy/tsuquyomi']
		Plug i, {'for': ['javascript', 'typescript', 'xml' 'html', 'xhtml', 'php']}
	endfor

	for i in ['pangloss/vim-javascript', 'isRuslan/vim-es6']
		Plug i, {'for': ['javascript','xml' 'html', 'xhtml', 'php']}
	endfor

	if index(g:programming_languages, 'typescript') >= 0 && executable('tsc')
		Plug 'leafgarland/typescript-vim', {'for': ['typescript']}
	endif

	let g:tsuquyomi_completion_detail = 1
	let g:tsuquyomi_javascript_support = 1
	let g:tsuquyomi_completion_preview = 1
	let g:javascript_plugin_jsdoc = 1 
endif

Plug 'elzr/vim-json', {'for': 'json'}

let g:used_javascript_libs = 'jquery,'

" Templating Engines:
" -------------------

" Pug:
if index(g:template_languages, 'pug') >= 0 ||  index(g:template_languages, 'jade') >= 0 
	Plug 'dNitro/vim-pug-complete', {'for': ['jade', 'pug']}
	Plug 'digitaltoad/vim-pug', {'for': ['jade', 'pug']}
	au! FileType jade setl ft=pug 
endif

" Jinja Twig Nunjucks:
for i in ['twig', 'jinja']
	if index(g:template_languages, i) >= 0
		Plug 'Glench/Vim-Jinja2-Syntax'
		au! BufNewFile,BufRead *.{twig,njk} setl fs=jinja
		break
	endif
endfor

" PHP:

if index(g:programming_languages, 'php') >= 0 && executable('php')
	Plug 'shawncplus/phpcomplete.vim', {'for': 'php'}
endif

" MY PLUGINS:
" ==========

try
	if empty(expand('~/Projects/VimScript')) 
		silent call mkdir('~/Projects/VimScript', 'p')
	endif

	for plugin in ['fabulous', 'vorg-mode', 'vim-saner', 'vim-fzf-extensions', 'vim-templates']
		Plug 'nl253/'.plugin, {'frozen': 1, 'dir': expand('~/Projects/VimScript/').plugin}
	endfor

	Plug 'nl253/vim-markup', {'frozen': 1, 'dir': expand('~/Projects/VimScript/vim-markup'), 'for': g:markup_languages}
	Plug 'nl253/vim-programming', {'frozen': 1, 'dir': expand('~/Projects/VimScript/vim-programming'), 'for': g:programming_languages}
	Plug 'nl253/vim-webdev', {'frozen': 1, 'dir': expand('~/Projects/VimScript/vim-webdev'), 'for': g:stylesheet_languages + g:template_languages + ['xml', 'javascript', 'typescript', 'markdown']}

	let g:vim_dicts = {'markdown': ['unix-programmers', 'computer-science']} 

catch /.*/
endtry

call plug#end()

" these need to be called after plug#end()
if has('patch8') || has('nvim')  
	call neomake#configure#automake('rw', 1000) 
endif

if $TERM =~ '256' || has('gui') || (has('nvim') && $TERM == '')
	colorscheme fabulous
else
	colorscheme darkblue
endif

" vim: foldmethod=indent
