au BufNewFile,BufRead *{ignore,conf}* if &filetype == "" | setl ft=config | endif
au BufNewFile,BufRead yarn.lock if &filetype == "" | setl ft=yaml | endif
au BufNewFile,BufRead *.ts setl ft=typescript.javascript
au BufNewFile,BufRead .tern-{config,project},.{markdown,html,es,style}lintrc,.{babel,jsbeautify}rc,*.lock,.tsconfig if &filetype == "" | setl ft=json | endif
au BufNewFile,BufRead *.{twig,nunj,njk} if &filetype == "" | setl ft=jinja | endif
au BufNewFile,BufRead *.*css if &filetype == "" | setl ft=css | endif
