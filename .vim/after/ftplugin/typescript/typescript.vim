" source javascript runtime files 
exec 'so '.expand('<sfile>:p:h:h').'/javascript/javascript.vim'

let s:out_dir  = '/tmp/vim/typescript'
let s:out_file = s:out_dir.'/%:t:r.js'
let s:tsc_cmd  = 'tsc --sourceMap --outFile '.s:out_file.' --lib es5,es6,es7,esnext,dom --removeComments --esModuleInterop --allowJs %'
let s:node_cmd = 'node '.s:out_file

if executable("tsc")
    exe 'setl makeprg='.escape(s:tsc_cmd.' ; '.s:node_cmd, ' ')
endif

com! -buffer Compile exe '!'.s:tsc_cmd
