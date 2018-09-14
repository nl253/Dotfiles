setl nowrap foldmethod=manual foldmarker={,}

let g:rust_logging_lvl  = 'debug'
let s:anchors           = ['Cargo.toml', '.git']

let s:root = execute('echo utils#proj_root('.join(map(s:anchors, 'string(v:val)'), ',').')')

if isdirectory(s:root) && executable("cargo")
    let s:project_name = fnamemodify(s:root, ':t')
    compiler cargo
    exe 'setl makeprg='.escape(join(['RUST_LOG\='.s:project_name.'\='.g:rust_logging_lvl, 'cargo', 'run', '--jobs', utils#cpu_cores(), '--quiet', '--color', 'never']), ' ')
endif

call opts#formatprg({'rustfmt': 'rustfmt --color never'})

exe 'Ctags '.join(s:anchors, ' ') 

if !empty($DEFAULT_TOOLCHAIN) 
    exe 'CtagsLib ~/.rustup/toolchains/'.$DEFAULT_TOOLCHAIN.'-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/src'
endif

exe 'ProjectFiles '.join(s:anchors, ' ') 
