setl sw=2 tw=4 foldmethod=indent noautoindent fo=
let s:output_dir = '/tmp/'.(has('nvim') ? 'neovim' : 'vim').'/%:p:h:t:r'
let s:output_file = s:output_dir.'/%:t:r.png'
let s:skins = join(map([
            \ 'ClassBackgroundColor=White',
            \ 'ClassArrowColor=Black',
            \ 'ClassBorderColor=Black',
            \ 'ClassAttributeIconSize=8',
            \ 'ClassFontColor=Black',
            \ 'ClassFontSize=14',
            \ 'titleFontSize=18',
            \ ], '"-S".v:val'), ' ')
exe 'setl makeprg='.escape("cat %:p \\| sed 's/@startuml/&\\nhide empty fields\\nhide empty methods/' \\| plantuml -pipe ".s:skins.' > '.s:output_file, ' \\|\\|\\\\|')
exe 'nn <buffer> <M-p> :!google-chrome-beta '.s:output_file.'<CR>'
