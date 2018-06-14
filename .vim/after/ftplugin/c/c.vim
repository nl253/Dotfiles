compiler gcc

setl cindent cinoptions=h1,l1,g1,t0,i4,+4,(0,w1,W4 expandtab foldmarker={,} foldmethod=marker keywordprg=:Man shiftwidth=2 softtabstop=2 tabstop=2 textwidth=79

for i in filter(['c_gnu', 'c_comment_strings', 'c_space_errors'], '!exists("g:".v:val)')
    exec 'let g:'.i.' = 1'
endfor

if executable("gcc") && (&makeprg == "make" || &makeprg == "")
    let &makeprg = "_VIM_C_COMPILE_TMP_DIR=$(mktemp -d) && gcc -Wall -o ${_VIM_C_COMPILE_TMP_DIR}/%:r % && ${_VIM_C_COMPILE_TMP_DIR}/%:r"
endif 

if executable('doxygen') && !exists('g:load_doxygen_syntax')
    let g:load_doxygen_syntax = 1
endif

if executable('astyle')
    let s:opts = [
                \ 'astyle', 
                \ '--mode=c',
                \ '--style=google',
                \ '--indent='.(&expandtab ? 'spaces' : 'tab').'='.max([&shiftwidth, 2]), 
                \ '--max-code-length='.max([&textwidth, 79]),
                \ '--align-pointer=name',
                \ '--align-reference=name',
                \ '--break-blocks',
                \ '--delete-empty-lines',
                \ '--indent-classes',
                \ '--indent-switches',
                \ '--pad-comma',
                \ '--pad-oper',
                \ '--remove-braces',
                \ '--unpad-paren',
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
    exec 'setl formatprg='.escape(join(['clang-format', '-style=file', '-fallback-style=google']), ' ')
endif
