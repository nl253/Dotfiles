setl nowrap foldmethod=manual foldmarker={,}

let g:rust_logging_lvl  = 'debug'
let s:anchors           = ['Cargo.toml', '.git']

let s:root = execute('echo utils#proj_root('.join(map(s:anchors, 'string(v:val)'), ',').')')

if isdirectory(s:root) && executable("cargo")
    let s:project_name = fnamemodify(s:root, ':t')
    compiler cargo
    exe 'setl makeprg='.escape(join(['RUST_LOG\='.s:project_name.'\='.g:rust_logging_lvl, 'cargo', 'run', '--jobs', utils#cpu_cores(), '--quiet', '--color', 'never']), ' ')
endif
