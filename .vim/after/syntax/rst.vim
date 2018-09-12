" Much Taken From: Rykka G.F
" Date: 2017-07-06

hi clear rstExDirective 

let s:concealends = has('conceal') ? ' concealends' : ''

" Link 
fun! s:inline_char(name, start, end, char_left, char_right) 
    exe 'sy match rst'.a:name
      \ '+'.a:char_left.'\zs'.a:start.'\ze[^[:space:]'
      \.a:char_right.a:start[strlen(a:start)-1].'][^'
      \.a:start[strlen(a:start)-1]
      \.'\\]*'.a:end.'\ze\%($\|\s\|[''")\]}>/:.,;!?\\-]\)+'
endfun 

sy sync match rstHighlight groupthere NONE #^\_s\@!#

for pair in ['""', "''", '()', '{}', '<>']
    call s:inline_char('PhaseHyperLinkReference', '`', '`__\=', pair[0] ,pair[1],)
endfor

call s:inline_char('PhaseHyperLinkReference', '`', '`__\=', '\[','\]')
call s:inline_char('PhaseHyperLinkReference', '`', '`__\=', '\%(^\|\s\|[/:]\)','')

" List:
sy match rstDefinitionList `\v^(\s*)\h[^:]*\ze%(\s:\s.*)*\n\1\s+\S`
sy match rstBulletList     `\v^\s*[-*+]\ze\s+`
sy match rstEnumeratedList `\v\c^\s*%(\d+|[#a-z]|[imlcxvd]+)[.)]\ze\s+`
sy match rstEnumeratedList `\v\c^\s*\(%(\d+|[#a-z]|[imlcxvd]+)\)\ze\s+`
sy match rstOptionList     `\v^\s*%(-\w%( \w+)=|--[[:alnum:]_-]+%(\=\w+)=|/\u)%(, %(-\w%( \w+)=|--[[:alnum:]_.-]+%(\=\w+)=|/\u))*%(  |\t)\ze\s*\S`
sy match rstFieldList      `\v^\s*:[^:[:space:]][^:]+:\_s`
sy match rstRoles          `\v\s:\zs\w+\ze:\``
sy match rstBibliographicField `\v^\s*:(Author|Authors|Organization|Contact|Address|Version|Status|Date|Copyright|Dedication|Abstract):\_s`

sy match rstBlockQuoteAttr `\v%(\_^\s*\n)@<=\s+---=\s.*`

sy match rstCommentTitle   '\v(^\s+|(^\.\.\s+)@<=):=\u\w*(\s+\u\w*)*:' contained 

sy cluster rstCruft        add=rstStandaloneHyperlink
sy cluster rstCommentGroup contains=rstCommentTitle,rstTodo
sy cluster rstCommentGroup add=@rstLinkGroup

" Code:

" Add block indicator for code directive
sy match rstCodeBlockIndicator `^\_.` contained

" Todo: 
sy cluster rstTodoGroup contains=rstTodoPrior,rstTodoTmBgn,rstTodoTmsEnd

" Highlights: "
if &background == 'light'
    hi def rstFileLink guifg=#437727 gui=underline ctermfg=28 cterm=underline
else
    hi def rstFileLink guifg=#58A261 gui=underline ctermfg=77 cterm=underline
endif

hi def link rstBibliographicField      Constant
hi def link rstBlockQuoteAttr          Exception
hi def link rstBulletList              Function
hi def link rstCitationReference       Underlined
hi def link rstCommentTitle            SpecialComment
hi def link rstDefinitionList          Statement
hi def link rstEnumeratedList          Function
hi def link rstFieldList               rstComment
hi def link rstFileExtLink             rstFileLink
hi def link rstFileExtLinkConceal      rstFileLink
hi def link rstFootnoteReference       Underlined
hi def link rstHyperLinkReference      Underlined
hi def link rstInlineInternalTargets   Keyword
hi def link rstOptionList              Statement
hi def link rstPhaseHyperLinkReference Underlined
hi def link rstRoles                   Operator
hi def link rstSections                Special
hi def link rstStandaloneHyperlink     Underlined
hi def link rstTodoPrior               Include
hi def link rstTodoTmBgn               Number
hi def link rstTodoTmEnd               Number

" --------------------------------------------------------------------
" MY
" --------------------------------------------------------------------
sy match  rstMetaTagValue "\v(^:[a-z]+: )@<=([-:a-zA-Z0-9_]+,? ?)+" 
sy match  rstSha "\v\C\w@<![a-z0-9]{40}\w@!"
sy region rstBetweenParenthesis start="\v\(" end="\v\)" oneline 

hi link rstSha                PreProc
hi link rstExDirective        SpecialComment
hi link rstBetweenParenthesis rstComment
hi link rstFileExtLink        rstHyperlinkReference
hi link rstURIChevrons        rstHyperlinkReference

"hi HR cterm=bold guifg=Grey1 gui=bold ctermfg=DarkGrey
hi rstComment            guifg=grey29     ctermfg=DarkGrey 
hi rstDefinitionList     guifg=goldenrod1 ctermfg=DarkYellow  gui=bold      cterm=bold
hi rstDirective          guifg=goldenrod1 ctermfg=DarkYellow  gui=bold      cterm=bold
hi rstEmphasis           guifg=cyan3      ctermfg=Yellow      gui=italic    cterm=underline 
hi rstHyperlinkReference guifg=SkyBlue2   ctermfg=LightBlue   gui=underline cterm=underline 
hi rstInlineLiteral      guifg=purple     ctermfg=LightBlue   gui=NONE      cterm=NONE 
hi rstMetaTagValue       guifg=#7c317f    ctermfg=DarkGrey  
hi rstSpecial            guifg=DeepPink3  ctermfg=DarkMagenta gui=bold      cterm=bold

hi rstStrongEmphasis gui=bold cterm=bold 
hi rstCodeBlockIndicator guibg=grey25 ctermfg=NONE
hi rstPhaseHyperLinkReference ctermfg=LightMagenta guifg=#ff80ff  
sy region rstURIChevrons matchgroup=rstComment start="\v\C\<(\w{2,15}(:/{,2}))?" end="\v\>" oneline concealends

" INLINE AND LINE EDITING:
for i in [[ 'Emphasis', '\*' ], [ 'StrongEmphasis', '\*\*' ], ['InlineLiteral', '``'] ]
  exe 'sy region rst'.i[0].' matchgroup=rstSpecial start="\v\\@<!'.i[1].'" end="\v\\@<!'.i[1].'" oneline '.s:concealends
endfor
