exe 'setl makeprg='.escape(join(['javac', '%:p:h/*.java', '&&', 'java', '%:t:r']), ' ')

setl expandtab tabstop=8 shiftwidth=4 foldmethod=marker foldmarker={,} foldlevel=1

if executable('astyle')
    let s:opts = [
                \ 'astyle', 
                \ '--indent='.(&expandtab ? 'spaces' : 'tab').'='.max([&shiftwidth, 2]), 
                \ '--mode=java',
                \ '--style=java',
                \ '--max-code-length='.max([&textwidth, 79]),
                \ '--pad-oper',
                \ '--pad-comma',
                \ '--delete-empty-lines',
                \ '--align-pointer=name',
                \ '--align-reference=name',
                \ '--unpad-paren',
                \ '--indent-classes',
                \ '--remove-braces',
                \ '--indent-switches',
                \ '--break-blocks',
                \ ] 
    if &expandtab
        call add(s:opts, '--convert-tabs')
    endif

    let s:try_find_cfg = findfile(".astylerc", $HOME."/;")

    if s:try_find_cfg != '' 
        call add(s:opts, "--options=".s:try_find_cfg)
    endif

    exe 'setl formatprg='.escape(join(s:opts), ' ')

elseif executable('clang')
    exec 'setl formatprg='.escape(join(['clang-format', '-style=file', '-fallback-style=llvm']), ' ')
endif

let g:java_highlight_java_lang_ids = 1
let g:java_highlight_functions = 'style'
let g:java_highlight_debug = 1
let g:java_javascript = 0 
let g:java_css = 0
let g:java_vb = 0 
