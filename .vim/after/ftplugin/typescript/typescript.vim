" source javascript runtime files 
exec 'so '.expand('<sfile>:p:h:h').'/javascript/javascript.vim'

if executable("tsc")
    exe 'setl makeprg='.escape('tsc --outFile ./%:r.js --lib es5,es6,es7,esnext,dom --removeComments --esModuleInterop --allowJs %', ' ') 
endif
