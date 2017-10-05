" vim: foldlevel=0 foldmethod=marker nowrap

" VARIABLES:

" MARKUP languages you actively use
if !exists('g:MARKUP') | let g:MARKUP = ['markdown', 'rst', 'vorg'] | endif

" PROGRAMMING LANGUAGES you code in 
if !exists('g:PROGRAMMING')
" -------------------
" Pug:

if index(g:PROGRAMMING, 'pug') >= 0 ||  index(g:PROGRAMMING, 'jade') >= 0 
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
