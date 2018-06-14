if exists("b:loaded_vim_webdev_sql")
    finish 
endif

" SQL - built-in
let g:sql_type_default = 'plsql' 
"let msql_sql_query = 1
let g:ftPlugin_sql_omni_key = ',' " shadows localleader

if executable('sqlformat')
    setl formatprg=sqlformat\ --keywords\ upper\ --identifiers\ upper\ --reindent\ --use_space_around_operators\ -
endif

let b:loaded_vim_webdev_sql = 1
