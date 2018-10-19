setl sw=2 ts=4 foldmethod=indent noautoindent fo=

let s:output_dir = '/tmp/'.(has('nvim') ? 'neovim' : 'vim').'/%:p:h:t:r'
let s:output_file = s:output_dir.'/%:t:r.png'
let s:skins = ""
let s:skins_cfg = {
            \ 'ClassArrowColor':        'Black',
            \ 'ClassAttributeIconSize': '8',
            \ 'ClassBackgroundColor':   'White',
            \ 'ClassBorderColor':       'Black',
            \ 'ClassFontColor':         'Black',
            \ 'ClassFontSize':          '14',
            \ 'titleFontSize':          '18',
            \}

for s:key in keys(s:skins_cfg)
    let s:skins += '-S'.s:key.'='.s:skins_cfg[s:key].' '
endfor

exe 'setl makeprg='.escape(join([
            \ "plantuml", "%:p", s:skins, "-o", s:output_dir, "&&", "echo", s:output_file], ' '), ' ')
exe 'nn <buffer> <M-p> :!feh '.s:output_file.' \|\| $BROWSER '.s:output_file.'<CR>'
