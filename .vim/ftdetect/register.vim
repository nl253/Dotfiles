fu! s:register(patterns, filetype)
    exe 'au BufNewFile,BufRead '.join(a:patterns, ',').' if &filetype == "" | setl ft='.a:filetype.' | endif' 
endf

fu! s:force(patterns, filetype)
    exe 'au BufNewFile,BufRead '.join(a:patterns, ',').' setl ft='.a:filetype
endf

sil cal s:register(['requrements.txt', '.{myclirc,mypyrc,gitstats,flake8}'], 'cfg')
sil cal s:register(['.ideavimrc'], 'vim')
sil cal s:register(['*.pest'], 'pest')
sil cal s:register(['grammar', '*.{grammar,peg,bnf,ebnf}'], 'grammar')
sil cal s:register(['.{space,e}macs'], 'lisp')
sil cal s:register(['*.gv'], 'dot')
sil cal s:register(['*.toml'], 'cfg')
sil cal s:register(['*{ignore,conf}*'], 'config')
sil cal s:register(['yarn.lock'], 'yaml')
sil cal s:register(['*.puml'], 'plantuml')
sil cal s:register(['.tern-{config,project}', '.{markdown,html,es,style}lintrc', '.{babel,jsbeautify}rc' ,'*.lock', '.tsconfig'], 'json')
sil cal s:register(['*.{twig,nunj,njk}'], 'jinja')
sil cal s:register(['*.*css'], 'css')

sil cal s:force(['*.{hbs,njk,jinja}'], 'htmldjango')
sil cal s:force(['*.{v,coq}'], 'coq')
sil cal s:force(['*.ctags'], 'ctags')
sil cal s:force(['*.ts'], 'typescript')
