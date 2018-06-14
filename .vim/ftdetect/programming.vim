
au BufNewFile,BufRead requrements.txt,.flake8,.gitstats,.mypyrc,.myclirc if &filetype == "" | setl ft=cfg | endif
au BufNewFile,BufRead .ideavimrc if &filetype == "" | setl ft=vim | endif
au BufNewFile,BufRead .emacs,.spacemacs if &filetype == "" | setl ft=clojure | endif
au BufNewFile,BufRead *.gv if &filetype == "" | setl ft=dot | endif
if !(&runtimepath =~? 'toml') 
    au BufNewFile,BufRead *.toml if &filetype == "" | setl filetype=cfg | endif
endif
