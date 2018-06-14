if executable("cargo") 
    compiler cargo 
    exe 'setl makeprg='.escape(join(['cargo', 'run', '--jobs', '4', '--quiet']), ' ')
    exe 'setl formatprg=rustfmt'
endif
