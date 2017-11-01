" vim: foldlevel=0 foldmethod=marker nowrap

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
	    \ 'sh', 
	    \ 'vim', 
	    \ 'java']

" Packages: (where key is executable name and value is package name)
let g:cargo_crates = ['rustfmt', 'racer']

let g:ruby_gems = ['mdl', 'sqlint']

let g:pip_packages = [
	    \ 'jedi',
	    \ 'pylint',
	    \ 'yamllint',
	    \ 'mypy',
	    \ 'pylama',
	    \ 'flake8',
	    \ 'proselint',
	    \ 'pyflakes',
	    \ 'vulture']

let g:yarn_packages = [
	    \ 'stylelint', 
	    \ 'js-beautify', 
	    \ 'tsc', 
	    \ 'yo', 
	    \ 'jsonlint', 
	    \ 'gitbook', 
	    \ 'eslint', 
	    \ 'tslint', 
	    \ 'htmlhint', 
	    \ 'tern', 
	    \ 'uglifyjs', 
	    \ 'write-good']

let g:stack_packages = ['shellcheck']

" ----------------------------------------------
" Sanity Tests:
" =============
" TODO tests and assertions

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

if exists('g:yarn_packages') && type(g:yarn_packages) == 3 && executable('bash') && executable('yarn')
    let j = join(map(split(expand("~/.yarn/bin/*")), 'fnamemodify(v:val, ":t")'))
    let i = filter(g:yarn_packages, '!(j =~ v:val)')
    if len(i) > 0
	silent call system("bash -c 'cd && yarn global add ".join(i)." &'")
    endif
endif

Plug 'junegunn/fzf', {'dir': expand('~/.local/share/fzf'), 'do': './install --bin'}
if executable('fzf') | Plug 'junegunn/fzf.vim' | endif
let g:fzf_layout = {'up': '~40%'}
let g:fzf_action = {'ctrl-t': 'tab split', 'ctrl-s': 'split', 'ctrl-v': 'vsplit'}

for i in map(['speeddating', 'repeat', 'fugitive'], '"tpope/vim-".v:val')
    Plug i
endfor

if executable('cargo')
    function! BuildMarkdownComposer(info)
	if a:info.status != 'unchanged' || a:info.force
	    silent call system(
			\ "cargo build --release".
			\ has('nvim') 
			\ ? "" 
			\ : "--no-default-features --features json-rpc")
	endif
    endfunction
    Plug 'euclio/vim-markdown-composer', {'do': function('BuildMarkdownComposer')}
endif

" A must have if you work with tmux
if executable('tmux') && exists('$TMUX') | Plug 'tmux-plugins/vim-tmux-focus-events' | endif

" GIT:
Plug 'junegunn/gv.vim', {'on': 'GV'}

set statusline=\ %f\ %r\ %m%=%-14.(%{&sw}\ %{&ts}%q\ %w\ %y\ %p\ of\ %l%)\ \  

if has('unix')
    Plug 'tpope/vim-eunuch', {'on' : ['Move', 'Remove', 'Find', 'Mkdir', 'Wall', 'SudoEdit', 
		\ 'Chmod', 'SudoWrite', 'Unlink', 'Rename']} 
endif

" Java:
if index(g:programming_languages, 'java') >= 0 && executable('mvn')
    Plug 'mikelue/vim-maven-plugin', {'for': 'java'}
endif

" Rust: 
if executable('rustc') && executable('cargo')
    if exists('g:cargo_crates') && type(g:cargo_crates) == 4 && executable('bash')
	for i in filter(keys(g:cargo_crates), '!executable(v:val)')
	    silent call system("bash -c 'cd && cargo install --release ".g:cargo_crates[i]." &'")
	endfor
    endif
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
endif

if index(g:markup_languages, 'markdown') >= 0 && executable('cargo')
    function! BuildMarkdownComposer(info)
	if a:info.status != 'unchanged' || a:info.force
	    silent call system("cargo build --release".
			\ has('nvim') ? "" 
			\ : "--no-default-features --features json-rpc")
	endif
    endfunction
    Plug 'euclio/vim-markdown-composer', {'do': function('BuildMarkdownComposer')}
endif

if executable('pip') && exists('g:pip_packages') && type(g:pip_packages) == 3 && executable('bash')
    let j = join(map(split(expand("~/.local/lib/python*/site-packages/*")), 'fnamemodify(v:val, ":t")'))
    silent call system("bash -c 'cd && pip install --user --pre ".join(filter(g:pip_packages, '!(v:val =~? j)'))." &'")
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

" LateX
Plug 'lervag/vimtex', {'for': 'tex'}

" Haskell:

if executable('stack') && exists('g:stack_packages') && type(g:stack_packages) == 3 && len(g:stack_packages) > 0  && executable('bash')
    let j = join(map(split(expand("~/.local/bin/*")), 'fnamemodify(v:val, ":t")'))
    for i in filter(g:stack_packages, '!(j =~ v:val)')
	silent call system('bash -c "stack install '.i.' &"')
    endfor
endif

if index(g:programming_languages, 'haskell') >= 0
    Plug 'eagletmt/neco-ghc', {'for': 'haskell'}
endif

if has('patch8') || has('nvim')
	Plug 'neomake/neomake'
	if executable('yamllint')
		let g:neomake_yaml_enabled_makers = ['yamllint']
	endif
	if executable('vint')
		let g:neomake_vim_enabled_makers = ['vint']
	endif
	if executable('sqlint')
		let g:neomake_sql_enabled_makers = ['sqlint']
	endif
	if executable('shellcheck')
		let g:neomake_bash_enabled_makers = ['sh', 'shellcheck']
	endif
	if executable('stylelint')
		let g:neomake_css_enabled_makers = ['stylelint']
	endif
	if executable('standard')
		let g:neomake_javascript_enabled_makers = ['standard']
	endif
	if executable('eslint')
		call add(g:neomake_javascript_enabled_makers, 'eslint')
	endif
	if executable('jsonlint')
		let g:neomake_json_enabled_makers = ['jsonlint']
	endif
	let g:neomake_python_enabled_makers = [] 
	if index(g:programming_languages, 'python') >= 0 
		for i in filter(['mypy', 'vulture',  
					\ 'pylint', 'pylama'], 'executable(v:val)')
			call add(g:neomake_python_enabled_makers, i)
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
    endif

    if executable('mdl') && exists('g:neomake_markdown_enabled_makers')
	let g:neomake_markdown_enabled_makers += ['mdl']
    endif
else
    Plug 'vim-syntastic/syntastic'
endif

" MARKUP:

Plug 'dkarter/bullets.vim', {'for': g:markup_languages + ['gitcommit', 'gitconfig', 'yaml']}
let g:bullets_enabled_file_types = g:markup_languages + ['gitcommit', 'gitconfig', 'yaml']

Plug 'dbmrq/vim-ditto', {'on': ['ToggleDitto', 'DittoOn'], 'for': g:markup_languages}
let g:ditto_mode = 'paragraph'

Plug 'reedes/vim-wordy', {'on': ['Wordy', 'WordyWordy'], 'for': g:markup_languages + ['gitcommit']}

aug MarkupAutoTools
    au!
    exec "au FileType ".string(g:markup_languages)." DittoOn"
    exec "au FileType ".string(g:markup_languages)." WordyWordy"
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

if index(g:programming_languages, 'javascript') >= 0 
    for i in ['othree/javascript-libraries-syntax.vim', 'moll/vim-node']
	Plug i, {'for': ['javascript'] + (index(g:programming_languages, 'typescript') >= 0)  ? ['typescript'] : []}
    endfor
    for i in ['pangloss/vim-javascript', 'isRuslan/vim-es6', 'Quramy/vim-js-pretty-template']
	Plug i, {'for': 'javascript'}
    endfor
endif

if index(g:programming_languages, 'typescript') >= 0 
    for i in ['Quramy/tsuquyomi', 'leafgarland/typescript-vim']
	Plug i, {'for': 'typescript'}
    endfor
    for i in ['tsuquyomi_completion_detail', 'tsuquyomi_javascript_support', 'tsuquyomi_completion_preview', 'javascript_plugin_jsdoc']
	execute 'let g:'.i.' = 1'
    endfor
endif

if index(g:config_ftypes, 'json') >= 0
    Plug 'elzr/vim-json', {'for': 'json'}
endif

let g:used_javascript_libs = 'jquery,'

" Templating Engines:
" -------------------
" PHP:

if index(g:programming_languages, 'php') >= 0 && executable('php')
    Plug 'shawncplus/phpcomplete.vim', {'for': 'php'}
endif

" MY PLUGINS:
" ==========

if !empty(expand('~/Documents/Vim')) 
    for plugin in ['fabulous', 'vorg-mode', 'vim-saner', 'vim-fzf-extensions', 'vim-templates']
	Plug 'nl253/'.plugin, {
		    \ 'frozen': 1, 
		    \ 'dir': expand('~/Documents/Vim/').plugin
		    \ }
    endfor

    Plug 'nl253/vim-programming', {
		\ 'frozen': 1, 
		\ 'dir': expand('~/Documents/Vim/vim-programming'), 
		\ 'for': g:.programming_languages
		\ }

    Plug 'nl253/vim-markup', {
		\ 'frozen': 1, 
		\ 'dir': expand('~/Documents/Vim/vim-markup'), 
		\ 'for': g:markup_languages
		\ }

    Plug 'nl253/vim-webdev', {
		\ 'frozen': 1, 
		\ 'dir': expand('~/Documents/Vim/vim-webdev'), 
		\ 'for': ['xml', 'css', 'javascript', 'typescript', 'markdown']
		\ }
endif
let g:vim_dicts = {'markdown': ['unix-programmers', 'computer-science']} 

" if has('nvim')
	" Plug 'kassio/neoterm', {'for': ['sh', 'python', 'javascript', 'haskell']}
	" nnoremap <M-CR> :TREPLSendLine<CR>
	" vnoremap <M-CR> :TREPLSendSelection<CR>
" endif

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

exec 'set suffixesadd='.join(g:programming_languages + g:markup_languages, ',')

" vim: foldmethod=indent
