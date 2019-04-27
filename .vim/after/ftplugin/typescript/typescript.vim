" source javascript runtime files 
exec 'so '.expand('<sfile>:p:h:h').'/javascript/javascript.vim'

if executable("tsc")
    let s:out_dir  = '%:p:h'
    let s:out_file = s:out_dir.'/%:t:r.js'
    let s:libs = ['es5', 
                \ 'es2015', 
                \ 'es2016', 
                \ 'es2017', 
                \ 'es2018', 
                \ 'esnext', 
                \ 'dom', 
                \ 'scripthost',
                \ 'webworker',  
                \ ]
    let s:flags = join([
                \ '--lib '.join(s:libs, ','),
                \ '--listEmittedFiles',
                \ '--sourceMap',
                \ '--removeComments',
                \ '--noUnusedLocals',
                \ '--noUnusedParameters',
                \ '--noFallthroughCasesInSwitch',
                \ '--allowJs',
                \ '--strictNullChecks',
                \ '--strictFunctionTypes',
                \ '--outFile '.s:out_file,
                \ ], ' ')
    let s:tsc_cmd  = 'tsc '.s:flags.' %'
    exe 'setl makeprg='.escape(s:tsc_cmd, ' ') 
endif

