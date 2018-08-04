setl foldmethod=manual foldmarker={,}

let b:rust_project_root = fnamemodify(findfile("Cargo.toml", '.;'), ':p:h')
let b:project_name      = fnamemodify(b:rust_project_root, ':t')
let b:logging_level     = 'debug'

if executable("cargo") 
    compiler cargo 
    exe 'setl makeprg='.escape(join(['RUST_LOG\='.b:project_name.'\='.b:logging_level, 'cargo', 'run', '--jobs', '4', '--quiet', '--color', 'never']), ' ')
    exe 'setl formatprg='.escape(join(['rustfmt', '--color', 'never']), ' ')
endif

let s:cargo_toml = fnamemodify(findfile("Cargo.toml", '.;'), ':p:h')

let s:tags = fnamemodify(findfile("Cargo.toml", '.;'), ":p:h").'/tags'

" not in a cargo project - exit
if empty(s:cargo_toml) 
    finish
endif

com! RustyTags call system('ctags '.substitute(expand(s:cargo_toml.'/**/*.rs'), '\n', ' ', 'g').' '.substitute(expand('~/.rustup/toolchains/'.$DEFAULT_TOOLCHAIN.'-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/src/**/*.rs'), '\n', ' ', 'g'))

" check if stale
if filereadable(s:tags) && (localtime() - getftime(s:tags)) > (60 * 10)
    " if so, remove
    sil call system("rm ".s:tags)
endif

let s:tags = fnamemodify(findfile("Cargo.toml", '.;'), ":p:h").'/tags'

if empty(s:tags) && executable('ctags')
    " make them 
    " also make tags for stdlib
    RustyTags
    echom 'created fresh tags'
endif
