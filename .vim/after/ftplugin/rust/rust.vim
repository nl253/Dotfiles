setl foldmethod=manual foldmarker={,}

let b:rust_project_root = fnamemodify(findfile("Cargo.toml", '.;'), ':p:h')
let b:project_name      = fnamemodify(b:rust_project_root, ':t')
let b:logging_level     = 'debug'

if executable("cargo") 
    compiler cargo 
    exe 'setl makeprg='.escape(join(['RUST_LOG\='.b:project_name.'\='.b:logging_level, 'cargo', 'run', '--jobs', '4', '--quiet', '--color', 'never']), ' ')
    exe 'setl formatprg='.escape(join(['rustfmt', '--color', 'never']), ' ')
endif

nmap <LocalLeader>D <Plug>(rust-def)
nmap <LocalLeader>v <Plug>(rust-def-vertical)
nmap <LocalLeader>d <Plug>(rust-doc)

let s:tags = fnamemodify(findfile("Cargo.toml"), ":p:h").'/tags'

if filereadable(s:tags) && (localtime() - getftime(s:tags)) > (60 * 10)
    sil call system("rm ".s:tags)
endif

if !filereadable(s:tags)
    let s:save_pwd = getcwd()
    exe 'cd '.fnamemodify(findfile("Cargo.toml"), ":p:h")
    if executable('rusty-tags')
        sil call system('rusty-tags vi')
    elseif executable('ctags')
        !ctags -R
    endif
    exe 'cd '.s:save_pwd
endif
