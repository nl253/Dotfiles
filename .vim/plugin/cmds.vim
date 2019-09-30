exe 'let $CMDS = '.string(expand('<sfile>'))
com! TemplateSubstitute call templates#template_subst()

com! -nargs=? -complete=file_in_path VS exe 'vs '.<f-args>
com! -nargs=1 -complete=customlist,complete#buffers_words Define :!command wn <args> -over | command pandoc -f markdown -t markdown

com! -range=% SubstituteWord call utils#safe_subst("".<line1>, "".<line2>)

com! HL echo exists('*utils#_syntax_attr') ? utils#syntax_attr() : join(map(synstack(line('.'), col('.')), 'synIDattr(v:val, "name")'), '/')

com!       -complete=file  -nargs=* Root   exe "let g:_x = utils#proj_root(".join(map(split(<q-args>, " "), 'string(v:val)'), ', ').") | exe 'lchdir '.g:_x | pwd | unlet g:_x"
com! -bang                          GFiles call utils#git_files_qf()
com! -bang                          PFiles call utils#project_files_qf()

com! CtagsDelete for i in tagfiles() | echo system("rm ".shellescape(i)." && echo removed ".shellescape(i)) | endfor
