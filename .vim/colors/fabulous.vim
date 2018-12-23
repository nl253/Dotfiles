setg background=light

hi clear

if v:version > 580
    " no guarantees for version 5.8 and below, but this makes it stop complaining
    hi clear
    if exists('g:syntax_on') | syntax reset | endif
endif

" not in Vim8
fu! s:trim(s)
    while a:s[0] == ' '
        let a:s = a:s[1:]
    endwhile
    let l:i = len(a:s) - 1
    while (i >= 0) && a:s[l:i] == ' '
        let a:s = a:s[0:((l:i) - 1)]
        let l:i = l:i - 1
    endwhile
    return a:s
endf

let g:colors_name = 'fabulous'

" refer to http://www.calmar.ws/vim/256-xterm-24bit-rgb-color-chart.html
let s:color256 = [
            \ '#000000', '#800000', '#008000', '#808000', '#000080', '#800080', '#008080' , '#c0c0c0',
            \ '#808080', '#ff0000', '#00ff00', '#ffff00', '#0000ff', '#ff00ff', '#00ffff', '#ffffff',
            \ '#000000', '#00005f', '#000087', '#0000af', '#0000d7', '#0000ff', '#005f00', '#005f5f', '#005f87', '#005faf', '#005fd7', '#005fff',
            \ '#008700', '#00875f', '#008787', '#0087af', '#0087d7', '#0087ff', '#00af00', '#00af5f', '#00af87', '#00afaf', '#00afd7', '#00afff',
            \ '#00d700', '#00d75f', '#00d787', '#00d7af', '#00d7d7', '#00d7ff', '#00ff00', '#00ff5f', '#00ff87', '#00ffaf', '#00ffd7', '#00ffff',
            \ '#5f0000', '#5f005f', '#5f0087', '#5f00af', '#5f00d7', '#5f00ff', '#5f5f00', '#5f5f5f', '#5f5f87', '#5f5faf', '#5f5fd7', '#5f5fff',
            \ '#5f8700', '#5f875f', '#5f8787', '#5f87af', '#5f87d7', '#5f87ff', '#5faf00', '#5faf5f', '#5faf87', '#5fafaf', '#5fafd7', '#5fafff',
            \ '#5fd700', '#5fd75f', '#5fd787', '#5fd7af', '#5fd7d7', '#5fd7ff', '#5fff00', '#5fff5f', '#5fff87', '#5fffaf', '#5fffd7', '#5fffff',
            \ '#870000', '#87005f', '#870087', '#8700af', '#8700d7', '#8700ff', '#875f00', '#875f5f', '#875f87', '#875faf', '#875fd7', '#875fff',
            \ '#878700', '#87875f', '#878787', '#8787af', '#8787d7', '#8787ff', '#87af00', '#87af5f', '#87af87', '#87afaf', '#87afd7', '#87afff',
            \ '#87d700', '#87d75f', '#87d787', '#87d7af', '#87d7d7', '#87d7ff', '#87ff00', '#87ff5f', '#87ff87', '#87ffaf', '#87ffd7', '#87ffff',
            \ '#af0000', '#af005f', '#af0087', '#af00af', '#af00d7', '#af00ff', '#af5f00', '#af5f5f', '#af5f87', '#af5faf', '#af5fd7', '#af5fff',
            \ '#af8700', '#af875f', '#af8787', '#af87af', '#af87d7', '#af87ff', '#afaf00', '#afaf5f', '#afaf87', '#afafaf', '#afafd7', '#afafff',
            \ '#afd700', '#afd75f', '#afd787', '#afd7af', '#afd7d7', '#afd7ff', '#afff00', '#afff5f', '#afff87', '#afffaf', '#afffd7', '#afffff',
            \ '#d70000', '#d7005f', '#d70087', '#d700af', '#d700d7', '#d700ff', '#d75f00', '#d75f5f', '#d75f87', '#d75faf', '#d75fd7', '#d75fff',
            \ '#d78700', '#d7875f', '#d78787', '#d787af', '#d787d7', '#d787ff', '#d7af00', '#d7af5f', '#d7af87', '#d7afaf', '#d7afd7', '#d7afff',
            \ '#d7d700', '#d7d75f', '#d7d787', '#d7d7af', '#d7d7d7', '#d7d7ff', '#d7ff00', '#d7ff5f', '#d7ff87', '#d7ffaf', '#d7ffd7', '#d7ffff',
            \ '#ff0000', '#ff005f', '#ff0087', '#ff00af', '#ff00d7', '#ff00ff', '#ff5f00', '#ff5f5f', '#ff5f87', '#ff5faf', '#ff5fd7', '#ff5fff',
            \ '#ff8700', '#ff875f', '#ff8787', '#ff87af', '#ff87d7', '#ff87ff', '#ffaf00', '#ffaf5f', '#ffaf87', '#ffafaf', '#ffafd7', '#ffafff',
            \ '#ffd700', '#ffd75f', '#ffd787', '#ffd7af', '#ffd7d7', '#ffd7ff', '#ffff00', '#ffff5f', '#ffff87', '#ffffaf', '#ffffd7', '#ffffff',
            \ '#080808', '#121212', '#1c1c1c', '#262626', '#303030', '#3a3a3a', '#444444', '#4e4e4e', '#585858', '#606060', '#666666', '#767676',
            \ '#808080', '#8a8a8a', '#949494', '#9e9e9e', '#a8a8a8', '#b2b2b2', '#bcbcbc', '#c6c6c6', '#d0d0d0', '#dadada', '#e4e4e4', '#eeeeee',
            \ ] 

let s:colors = {
            \  16: '#292b2e',  24: '#3C8380',  28: '#c269fe',  30: '#2aa1ae',  36: '#20af81',  40: '#00ff00',
            \  59: '#FF73B9',  68: '#4f97d7',  75: '#FF62B0',  76: '#86dc2f',  81: '#f9bb00',  88: '#330033',
            \ 104: '#df90ff', 114: '#67b11d', 128: '#e76a49', 135: '#B7B7FF', 136: '#dc752f', 139: '#d698fe',
            \ 140: '#b888e2', 141: '#9a9aba', 151: '#74BAAC', 160: '#e0211d', 161: '#E469FE', 167: '#ce537a',
            \ 168: '#ce537a', 169: '#bc6ec5', 171: '#6094DB', 173: '#e18254', 176: '#E697E6', 177: '#D881ED',
            \ 178: '#d1951d', 179: '#d4b261', 196: '#e0211d', 204: '#ce537a', 207: '#FF68DD', 214: '#FF4848',
            \ 218: '#d19a66', 225: '#FFC8C8', 229: '#fff06a', 233: '#303030', 234: '#212026', 235: '#292b2e',
            \ 236: '#34323e', 238: '#544a65', 241: '#534b5d', 244: '#b4d1b6',
            \ }

fu! s:hi(item, fg, bg, ...)
    let l:fg = empty(a:fg) ? '' : printf('ctermfg=%d guifg=%s', a:fg, get(s:colors, a:fg, s:color256[a:fg]))
    let l:bg = empty(a:bg) ? '' : printf('ctermbg=%d guibg=%s', a:bg, get(s:colors, a:bg, s:color256[a:bg]))

    if a:0 ==# 0
        let l:effects = "cterm=none gui=none"
    else
        let l:s = join(map(deepcopy(a:000), 's:trim(v:val)'), ',')
        let l:effects = "cterm=".l:s." gui=".l:s
    endif

    exe join(['hi', a:item, l:fg, l:bg, l:effects], ' ')
endfu


let s:fg = 0
let s:bg = 15

hi clear Conceal
hi clear Cursor
hi clear Error
hi clear FoldColumn 
hi clear Function
hi clear EndOfBuffer
hi clear Identifier
hi clear LineNr Comment
hi clear NonText
hi clear SpecialComment 
hi clear TermCursor
hi clear Todo
hi clear TypeDef
hi clear VertSplit 

call s:hi('Boolean'     ,  130,   '', 'bold')
call s:hi('Character'   ,  190,   '')
call s:hi('ColorColumn' ,   '',   '')
call s:hi('Comment'     ,   32,  '')
" call s:hi('Conceal'     ,   '',   s:bg)
" hi link Conceal Normal
call s:hi('Conditional' ,  130,   '', 'bold')
call s:hi('Constant'    ,  239,   '', 'italic')
call s:hi('Cursor'      ,   '',  130)
call s:hi('CursorLine'  ,   '',  255)
call s:hi('CursorLineNr',  204,   '')
call s:hi('DiffAdd'     ,   '',  120)
call s:hi('DiffChange'  ,   '',  159)
call s:hi('DiffDelete'  ,  125,  125)
call s:hi('DiffText'    ,   '',  255)
call s:hi('Directory'   ,   25,   '')
call s:hi('FoldColumn'  ,   67,   '')
call s:hi('Function'    ,  130,   '', 'bold')
call s:hi('Include'     ,  160,   '', 'bold')
call s:hi('Keyword'     ,   99,   '', 'bold')
call s:hi('Macro'       ,    0,   '', 'italic')
call s:hi('MatchParen'  ,   '',   '', 'inverse')
call s:hi('ModeMsg'     ,   89,   '')
call s:hi('Number'      ,    3,   '')
call s:hi('Operator'    ,  240,   '', 'bold')
call s:hi('Pmenu'       ,   '',    7)
call s:hi('PmenuSbar'   ,   '',  244)
call s:hi('PmenuSel'    ,   '',  230)
call s:hi('PreProc'     ,  197,   '', 'italic')
call s:hi('Repeat'      ,   31,   '', 'bold')
call s:hi('SignColumn'  ,   '',   '')
call s:hi('Special'     ,   56,   '')
call s:hi('SpecialChar' ,   89,   '', 'bold')
call s:hi('SpecialKey'  ,  174,   '', 'bold')
call s:hi('SpellBad'    ,  168,   '', 'underline')
call s:hi('SpellCap'    ,  105,   '', 'underline')
call s:hi('SpellLocal'  ,  253,   '', 'underline')
call s:hi('SpellRare'   ,  218,   '', 'underline')
call s:hi('Statement'   ,  126,   '', 'bold')
call s:hi('StatusLine'  ,   '',  210)
call s:hi('StatusLineNC',   '',    7, 'bold')
call s:hi('StorageClass',  138,   '', 'bold')
call s:hi('String'      ,    2,   '')
call s:hi('TabLine'     ,   '',   '')
call s:hi('TabLineFill' ,   '',  255)
call s:hi('TabLineSel'  ,   '',  214, 'bold')
call s:hi('Title'       ,  136,   '', 'bold')
call s:hi('Type'        ,   56,   '', 'bold')
call s:hi('TypeDef'     ,   90,   '')
call s:hi('VertSplit'   ,  234,   '')
call s:hi('WarningMsg'  ,  124,   '', 'bold')
call s:hi('WildMenu'    ,   '',    7, 'bold')

" OWN
call s:hi('Builtin'     ,  93,   '')
call s:hi('URI'         ,  21, '', 'underline')
call s:hi('Quote'       ,  51, '')

hi link Error          Exception
hi link NonText        Comment
hi link Identifier     Normal
hi link Ignore         Comment
hi link Label          Identifier
hi link Symbol         SpecialKey
hi link Tag            Builtin
hi link TermCursor     Cursor
hi link Todo           WarningMsg
hi link PreCondit      Builtin
hi link PreConditBold  Builtin
hi link Procedure      Repeat
hi link Define         TypeDef
hi link Value          Normal
hi link Variable       Normal
hi link SpecialComment Comment

hi link HtmlH1 Title
hi link HtmlH2 Special
hi link HtmlH3 Type

"call s:hi('Delimiter'   , 151 , '' , 'None' , 'None')
"hi CommentTitle term=bold cterm=BOLD gui=BOLD ctermfg=NONE guifg=ivory2
"hi IncSearch guifg=gold guibg=black ctermbg=black ctermfg=yellow cterm=BOLD,italic gui=BOLD,italic
"hi Search ctermbg=cyan ctermfg=Black guibg=cyan guifg=black
"hi Visual term=INVERSE cterm=INVERSE gui=INVERSE

" OWN
" hi ALEWarning cterm=NONE
