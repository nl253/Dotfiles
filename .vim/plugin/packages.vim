" don't source
finish

let s:stack_packages = [
            \ 'brittany', 
            \ 'hindent', 
            \ 'hfmt', 
            \ 'ghc-mod'
            \ ]

let s:pip_packages = [
            \ 'gitlint', 
            \ 'proselint', 
            \ 'yamllint',  
            \ 'rstcheck'
            \ ]

let s:yarn_packages = [
            \ 'write-good', 
            \ 'alex', 
            \ 'js-beautify', 
            \ 'stylelint', 
            \ 'htmlhint'
            \ ]

call packages#install_packages('pip'  , 'install --user --local --pre', s:pip_packages)
call packages#install_packages('yarn' , 'global add'                         , s:yarn_packages)
call packages#install_packages('stack', 'install'                     , s:stack_packages)
call packages#install_package('pip install --user --pre --local', 'vim-vint', 'vint')
