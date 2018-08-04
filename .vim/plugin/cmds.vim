com! TemplateSubstitute call templates#template_subst()

com! Scripts       e ~/.vim/plugin/autocmds.vim

com! WhiteSpace    %s/\v^\s+$| +$//
com! ToDo          call utils#toggle_todo()
com! BufferWipeout call utils#buffer_wipeout()

com! Define        !(wn <cword> -over) | fmt
com! Synonym       !(wn <cword> -synsn || wn <cword> -synsv || wn <cword> -synsa || wn <cword> -synsr) | fmt 
com! Hypernym      !(wn <cword> -hypen || wn <cword> -hypev || wn <cword> -hypea || wn <cword> -hyper) | fmt
com! Hyponym       !(wn <cword> -hypon || wn <cword> -hypov || wn <cword> -hypoa || wn <cword> -hypor) | fmt
com! Antonym       !(wn <cword> -antsn || wn <cword> -antsv || wn <cword> -antsa || wn <cword> -antsr) | fmt

com! -nargs=1 -complete=tag      AppendToDict   call utils#append_to_dict(<q-args>)
com! -range=%                    SubstituteWord call utils#safe_subst("".<line1>, "".<line2>)
com! -nargs=+ -complete=shellcmd AsyncRun       call utils#async_run(<q-args>)
com!                             CloseDupTabs   call utils#close_dup_tabs()
com!                             HLCurrentWord  call utils#hl_word()
com!                             ReformatBuffer call utils#reformat_buffer()

com! HL    ec exists('*SyntaxAttr') ?  SyntaxAttr() : join(map(synstack(line('.'), col('.')), 'synIDattr(v:val, "name")'), '/')
com! -complete=dir -nargs=* Root exec 'silent lcd '.utils#proj_root(split(<q-args>, ' ') + ['.git']) | echom '$PWD = '.string($PWD)
com! -bang -complete=file -nargs=* Ctags call tags#project(split(<q-args>, ' ') + ['.git'], <bang>0)

com! -range=% SnakeToCamel  <line1>,<line2>s#_\(\l\)#\u\1#g
com! -range=% SnakeToPascal <line1>,<line2>s#\(\%(\<\l\+\)\%(_\)\@=\)\|_\(\l\)#\u\1\2#g
com! -range=% SNAKEToSnake  <line1>,<line2>SNAKEToPascal | <line1>,<line2>PascalToSnake
com! -range=% SNAKEToPascal <line1>,<line2>s#_*\(\u\)\(\u*\)#\1\L\2#g
com! -range=% SNAKEToCamel  <line1>,<line2>SNAKEToPascal | <line1>,<line2>PascalToCamel
com! -range=% CamelToSnake  <line1>,<line2>s/\v([a-z]+)([A-Z])([a-z]+)/\1_\L\2\3/g
com! -range=% CamelToPascal <line1>,<line2>CamelToSnake | <line1>,<line2>SnakeToPascal
com! -range=% PascalToSnake <line1>,<line2>s#\(\<\u\l\+\|\l\+\)\(\u\)#\l\1_\l\2#g
com! -range=% PascalToCamel <line1>,<line2>PascalToSnake | <line1>,<line2>SnakeToCamel

com! -nargs=? -bang -complete=customlist,views#list DeleteView call views#delete(<q-args>, <bang>0)
com! -nargs=? -bang -complete=customlist,views#list RemoveView call views#delete(<q-args>, <bang>0)
com! -nargs=?       -complete=customlist,views#list SaveView   call views#save(<q-args>)
com! -nargs=?       -complete=customlist,views#list ReadView   call views#read(<q-args>)

com! -nargs=? -bang -complete=customlist,sessions#list DeleteSession call sessions#delete(<q-args>, <bang>0)
com! -nargs=? -bang -complete=customlist,sessions#list RemoveSession call sessions#delete(<q-args>, <bang>0)
com! -nargs=?       -complete=customlist,sessions#list SaveSession   call sessions#save(<q-args>)
com! -nargs=?       -complete=customlist,sessions#list ReadSession   call sessions#read(<q-args>)
