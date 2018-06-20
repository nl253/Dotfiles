
fu! s:register(patterns, filetype)
    exec 'au BufNewFile,BufRead '.join(a:patterns, ',').' if &filetype == "" | setl ft='.a:filetype.' | endif' 
endf

sil call s:register(['requrements.txt', '.flake8', '.gitstats', '.mypyrc', '.myclirc'], 'cfg')
sil call s:register(['.ideavimrc'], 'vim')
sil call s:register(['.emacs', '.spacemacs'], 'lisp')
sil call s:register(['*.gv'], 'dot')
sil call s:register(['*.toml'], 'cfg')
sil call s:register(['*{ignore,conf}*'], 'config')
sil call s:register(['yarn.lock'], 'yaml')
sil call s:register(['*.puml'], 'plantuml')
sil call s:register(['*.ts'], 'typescript.javascript')
sil call s:register(['.tern-{config,project}', '.{markdown,html,es,style}lintrc', '.{babel,jsbeautify}rc' ,'*.lock', '.tsconfig'], 'json')
sil call s:register(['*.{twig,nunj,njk}'], 'jinja')
sil call s:register(['*.*css'], 'css')
