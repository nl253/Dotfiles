" source javascript runtime files 
exec 'so '.expand('<sfile>:p:h:h').'/javascript/javascript.vim'

                                                      

if executable("tsc")
    let s:out_dir  = '/tmp/vim/typescript'
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
    let s:tsc_cmd  = 'tsc --listEmittedFiles --forceConsistentCasingInFileNames --sourceMap --outFile '.s:out_file.' --lib '.join(s:libs, ',').' --removeComments --esModuleInterop --allowJs %'
    exe 'setl makeprg='.escape(s:tsc_cmd, ' ') 
endif

