setl foldmethod=manual foldmarker={,}

let b:rust_project_root = fnamemodify(findfile("Cargo.toml", '.;'), ':p:h')
let b:project_name      = fnamemodify(b:rust_project_root, ':t')
let b:logging_level     = 'debug'

if executable("cargo") 
    compiler cargo 
    exe 'setl makeprg='.escape(join(['RUST_LOG\='.b:project_name.'\='.b:logging_level, 'cargo', 'run', '--jobs', '4', '--quiet', '--color', 'never']), ' ')
    exe 'setl formatprg='.escape(join(['rustfmt', '--color', 'never']), ' ')
endif

com! -bang RustyTags call s:generate_tags_from_sources('~/.rustup/toolchains/'.$DEFAULT_TOOLCHAIN.'-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/src', 'Cargo.toml', 0, <bang>0)

RustyTags
