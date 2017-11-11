" vim: foldlevel=0 foldmethod=marker nowrap

for i in ['bash', 'yarn', 'cargo']
	exec 'let g:has_'.i.' ='.executable(i)
endfor

" Variables: (unfortunates these need to be global)
let g:markup_languages = ['markdown', 'rst', 'vorg', 'tex'] 

let g:config_ftypes = [
			\ 'yaml', 
			\ 'gitconfig', 
			\ 'cfg', 
			\ 'dosini', 
			\ 'conf', 
			\ 'json',
			\ 'config']

let g:programming_languages = [
			\ 'sql', 
			\ 'python', 
			\ 'haskell', 
			\ 'sh', 
			\ 'vim', 
			\ 'java']

" Packages:

" echo g:InstallPackages(
" \ 'yarn', 
" \ 'global add', 
" \ 'yarn global list', [
" \ 'stylelint', 
" \ 'js-beautify', 
" \ 'typescript', 
" \ 'yo', 
" \ 'jsonlint', 
" \ 'eslint', 
" \ 'tslint', 
" \ 'htmlhint', 
" \ 'tern', 
" \ 'uglify-es', 
" \ 'write-good'])

" echo g:InstallPackages(
" \ 'cargo', 
" \ 'install', 
" \ 'ls ~/.cargo/registry/src/github.com-*', 
" \ ['rustfmt', 'racer', 'tokei'])

" echo g:InstallPackages(
" \ 'pip', 
" \ 'install --user --pre', 
" \ 'pip list', [
" \ 'flake8',
" \ 'Cython',
" \ 'docopt',
" \ 'lxml',
" \ 'jedi',
" \ 'Jinja2',
" \ 'matplotlib',
" \ 'pandas',
" \ 'mypy',
" \ 'numpy',
" \ 'proselint',
" \ 'pyflakes',
" \ 'pylama',
" \ 'Sphinx',
" \ 'pylint',
" \ 'nltk',
" \ 'scipy',
" \ 'pygments',
" \ 'seaborn',
" \ 'sqlalchemy',
" \ 'bokeh',
" \ 'vulture',
" \ 'yamllint',
" \ 'ranger-fm',
" \ 'networkx',
" \ 'prompt-toolkit',
" \ 'pydocstyle',
" \ 'PyYAML',
" \ 'sympy'])

" echo g:InstallPackages(
" \ 'stack', 
" \ 'install', 
" \ 'ls ~/.stack/programs/x86_64-*/ghc*/bin ~/.local/bin', [
" \ 'ShellCheck', 
" \ 'pandoc', 
" \ 'happy', 
" \ 'hlint'])

" TODO echo g:InstallPackages('gem', 'install', '~/.gem/bin', ['mdl', 'sqlint'])

" ----------------------------------------------

" Place Plugins Here:
" ===================  

" GENERAL:

for i in ['scrooloose/nerdcommenter', 'konfekt/fastfold', 'wellle/targets.vim'] 
	Plug i
endfor

let g:NERDSpaceDelims = 1

if executable('ctags')
	Plug 'majutsushi/tagbar', {'on': ['Tagbar', 'TagbarOpen']}
endif

if index(g:programming_languages, 'vim') >= 0
	Plug 'vim-scripts/SyntaxAttr.vim'
endif

Plug 'junegunn/fzf', {'dir': expand('~/.local/share/fzf'), 'do': './install --bin'}
if executable('fzf') | Plug 'junegunn/fzf.vim' | endif
let g:fzf_layout = {'up': '~40%'}
let g:fzf_action = {'ctrl-t': 'tab split', 'ctrl-s': 'split', 'ctrl-v': 'vsplit'}

for i in map(['speeddating', 'repeat', 'fugitive'], '"tpope/vim-".v:val')
	Plug i
endfor

if g:has_cargo
	" function! BuildMarkdownComposer(info)
	" if a:info.status != 'unchanged' || a:info.force
	" silent call system(
	" \ "cargo build --release".
	" \ has('nvim') 
	" \ ? "" 
	" \ : "--no-default-features --features json-rpc")
	" endif
	" endfunction
	Plug 'euclio/vim-markdown-composer', {'do': 'cargo build --release'}
endif

" A must have if you work with tmux
if executable('tmux') && exists('$TMUX') | Plug 'tmux-plugins/vim-tmux-focus-events' | endif

" GIT:
Plug 'junegunn/gv.vim', {'on': 'GV'}

set statusline=\ %f\ %r\ %m%=%-14.(%{&sw}\ %{&ts}%q\ %w\ %y\ %p\ of\ %l%)\ \  

if has('unix')
	Plug 'tpope/vim-eunuch', {'on' : [
				\ 'Move', 'Remove', 'Find', 'Mkdir',
				\ 'Wall', 'SudoEdit',  'Unlink',
				\ 'Chmod', 'SudoWrite', 'Rename']} 
endif

" Java:
if index(g:programming_languages, 'java') >= 0 && executable('mvn')
	Plug 'mikelue/vim-maven-plugin', {'for': 'java'}
endif

" Rust: 
if g:has_cargo
	if index(g:programming_languages, 'rust') >= 0 
		Plug 'rust-lang/rust.vim', {'for': 'rust'}
		if executable('racer')
			Plug 'racer-rust/vim-racer', {'for': 'rust'}
			let g:racer_experimental_completer = 1
		endif
		if executable('rustfmt')
			let g:rustfmt_autosave = 1
		endif
	endif

	if index(g:markup_languages, 'markdown') >= 0
		function! BuildMarkdownComposer(info)
			if a:info.status != 'unchanged' || a:info.force
				silent call system("cargo build --release".
							\ has('nvim') ? "" 
							\ : "--no-default-features --features json-rpc")
			endif
		endfunction
		Plug 'euclio/vim-markdown-composer', {'do': function('BuildMarkdownComposer')}
	endif
endif

if has('nvim') || (has('python3') && has('patch8'))
	if has('nvim')
		" Plug 'Shougo/deoplete.nvim', {'do': ':UpdateRemotePlugins'}
	elseif has('python3') && has('patch8')
		" Plug 'Shougo/deoplete.nvim'
		" Plug 'roxma/nvim-yarp'
		" Plug 'roxma/vim-hug-neovim-rpc'
	endif
	Plug 'Shougo/neco-vim', {'for': 'vim'}
	if index(g:programming_languages, 'rust') >= 0 
		Plug 'sebastianmarkow/deoplete-rust'
	endif
	if index(g:programming_languages, 'python') >= 0 
		Plug 'zchee/deoplete-jedi', {'for': 'python'}
	endif
	Plug 'Shougo/neco-syntax'
	if executable('tmux') && exists('$TMUX') | Plug 'wellle/tmux-complete.vim' | endif
	let g:deoplete#enable_at_startup = 1
	let g:deoplete#max_list = 15
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

	if index(g:programming_languages, 'python') >= 0
		" Plug 'davidhalter/jedi-vim', {'for': 'python'} 
		let g:jedi#force_py_version = 3
		let g:jedi#goto_command = "<C-]>"
		let g:jedi#goto_assignments_command = "<localleader>a"
		let g:jedi#goto_definitions_command = "<localleader>d"
		let g:jedi#completions_enabled = 0
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

" LaTeX:

if index(g:markup_languages, 'tex') >= 0
	Plug 'lervag/vimtex', {'for': 'tex'}
endif

" Haskell:
if index(g:programming_languages, 'haskell') >= 0
	Plug 'neovimhaskell/haskell-vim', {'for': 'haskell'}
	let g:haskell_enable_quantification = 1   " to enable highlighting of `forall`
	let g:haskell_enable_recursivedo = 1      " to enable highlighting of `mdo` and `rec`
	let g:haskell_enable_arrowsyntax = 1      " to enable highlighting of `proc`
	let g:haskell_enable_pattern_synonyms = 1 " to enable highlighting of `pattern`
	let g:haskell_enable_typeroles = 1        " to enable highlighting of type roles
	let g:haskell_enable_static_pointers = 1  " to enable highlighting of `static`
	let g:haskell_backpack = 1                " to enable highlighting of backpack keywords
endif

if has('patch8') || has('nvim')
	Plug 'neomake/neomake'
	function! g:MakersForFType(ftype, makers)
		execute 'let g:neomake_'.a:ftype.'_enabled_makers = '.
					\ string(filter(a:makers, 'executable(v:val)'))

	endfunction
	call g:MakersForFType('yaml', ['yamllint'])
	call g:MakersForFType('vim', ['vint'])
	call g:MakersForFType('sql', ['sqlint'])
	call g:MakersForFType('sh', ['shcheck', 'sh'])
	call g:MakersForFType('css', ['stylelint'])
	call g:MakersForFType('javascript', ['eslint'])
	call g:MakersForFType('json', ['jsonlint'])
	call g:MakersForFType('tex', ['proselint', 'rubber', 'lacheck', 'chktex'])
	call g:MakersForFType('markdown', ['mdl', 'proselint', 'writegood'])
	call g:MakersForFType('python', ['mypy', 'vulture', 'pylint', 'pylama'])

	if g:has_yarn && index(g:programming_languages, 'javascript') >= 0 
		for package in filter(map(['eslint'], 'expand("~/.yarn/bin/".v:val)'), 'executable(v:val)')
			for ftype in ['javascript', 'json']
				execute 'let g:neomake_'.ftype.'_'.fnamemodify(package, ':t').'_exe = '.package
			endfor
		endfor
	endif
else
	Plug 'vim-syntastic/syntastic'
endif

" MARKUP:

Plug 'dkarter/bullets.vim', {'for': g:markup_languages + g:config_ftypes}
let g:bullets_enabled_file_types = g:markup_languages + g:config_ftypes

Plug 'dbmrq/vim-ditto', {'on': ['ToggleDitto', 'DittoOn'], 'for': g:markup_languages}
let g:ditto_mode = 'paragraph'

Plug 'reedes/vim-wordy', {'on': ['Wordy', 'WordyWordy'], 'for': g:markup_languages + ['gitcommit']}

aug MarkupAutoTools
	au!
	exec "au FileType ".string(g:markup_languages)." DittoOn"
	exec "au FileType ".string(g:markup_languages)." WordyWordy"
    au BufReadPre *.tex let b:vimtex_main = 'main.tex'
aug END

" MARKDOWN:
let g:markdown_fenced_languages = g:programming_languages
let g:rst_syntax_code_list = g:markdown_fenced_languages

" WEB DEV:

for i in ['othree/html5.vim', 'othree/html5-syntax.vim']
	Plug i, {'for': ['markdown', 'html']}
endfor

Plug 'alvan/vim-closetag', {'for': ['html', 'xml', 'markdown']}
let g:closetag_filenames = '*.html,*.md,*.xml'

" if has('nvim') || (has('python3') || has('python'))
" Plug 'mattn/emmet-vim', {'for': ['xml', 'html', 'xhtml', 'php', 'markdown'] + ['css']
" let g:user_emmet_expandabbr_key = '<Tab>'
" " let g:user_emmet_complete_tag = 1
" " let g:user_emmet_mode = 'i'
" let g:emmet_html5 = 1
" " let g:user_emmet_install_global = 0 
" endif

Plug 'othree/csscomplete.vim', {'for': ['css', 'html']}

if index(g:programming_languages, 'javascript') >= 0 || index(g:programming_languages, 'typescript') >= 0 
	for i in ['othree/javascript-libraries-syntax.vim', 'moll/vim-node', 'pangloss/vim-javascript', 'isRuslan/vim-es6', 'Quramy/vim-js-pretty-template']
		Plug i, {'for': ['javascript', 'typescript']}
	endfor
	let g:used_javascript_libs = 'jquery,'
endif

if index(g:programming_languages, 'typescript') >= 0 
	for i in ['Quramy/tsuquyomi', 'leafgarland/typescript-vim']
		Plug i, {'for': 'typescript'}
	endfor
	for i in ['tsuquyomi_completion_detail', 'tsuquyomi_javascript_support', 'tsuquyomi_completion_preview', 'javascript_plugin_jsdoc']
		execute 'let g:'.i.' = 1'
	endfor
endif

Plug 'elzr/vim-json', {'for': 'json'}

" Templating Engines:
" -------------------
" PHP:

if index(g:programming_languages, 'php') >= 0 && executable('php')
	Plug 'shawncplus/phpcomplete.vim', {'for': 'php'}
endif

" MY PLUGINS:
" ==========

if !isdirectory(expand('~/Documents/Vim')) 
	call mkdir(expand('~/Documents/Vim'), 'p')
endif

if isdirectory(expand('~/Documents/Vim')) 

	for plugin in ['fabulous', 'vorg-mode', 'vim-saner', 'vim-fzf-extensions', 'vim-templates']

		Plug 'nl253/'.plugin, {
					\ 'frozen': 1, 
					\ 'dir': expand('~/Documents/Vim/').plugin
					\ }
	endfor

	Plug 'nl253/vim-markup', {
				\ 'frozen': 1, 
				\ 'dir': expand('~/Documents/Vim/vim-markup'), 
				\ 'for': g:markup_languages
				\ }

	Plug 'nl253/vim-programming'.i, {
				\ 'frozen': 1, 
				\ 'dir': expand('~/Documents/Vim/vim-programming'), 
				\ 'for': g:programming_languages
				\ }

	Plug 'nl253/vim-webdev', {
				\ 'frozen': 1, 
				\ 'dir': expand('~/Documents/Vim/vim-webdev'), 
				\ 'for': ['css', 'html', 'javascript', 'typescript']
				\ }
endif

let g:vim_dicts = {'markdown': ['unix-programmers', 'computer-science']} 

call plug#end()

" these need to be called after plug#end()
if (has('patch8') || has('nvim')) && (has('python3') || has('python') || has('python-dyn'))
	call neomake#configure#automake('rw', 1000) 
endif

if $TERM =~ '256' || has('gui') || (has('nvim') && $TERM == '')
	colorscheme fabulous
else
	colorscheme darkblue
endif

exec 'set suffixesadd='.join(g:programming_languages + g:markup_languages + g:config_ftypes, ',')

" vim: foldmethod=indent
