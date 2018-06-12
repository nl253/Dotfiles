" Much Taken From: Rykka G.F
" Date: 2017-07-06

syn sync match rstHighlight groupthere NONE #^\_s\@!#

let s:concealends = has('conceal') ? ' concealends' : ''

" Link 
fun! s:def_inline_char(name, start, end, char_left, char_right) 
    exe 'syn match rst'.a:name
      \ '+'.a:char_left.'\zs'.a:start.'\ze[^[:space:]'
      \.a:char_right.a:start[strlen(a:start)-1].'][^'
      \.a:start[strlen(a:start)-1]
      \.'\\]*'.a:end.'\ze\%($\|\s\|[''")\]}>/:.,;!?\\-]\)+'
endfun 

for pair in ['""', "''", '()', '{}', '<>']
    call s:def_inline_char('PhaseHyperLinkReference', '`', '`__\=', pair[0] ,pair[1],)
endfor
call s:def_inline_char('PhaseHyperLinkReference', '`', '`__\=', '\[','\]')
call s:def_inline_char('PhaseHyperLinkReference', '`', '`__\=', '\%(^\|\s\|[/:]\)','')

" List:
syn match rstDefinitionList `\v^(\s*)\h[^:]*\ze%(\s:\s.*)*\n\1\s+\S`
syn match rstBulletList `\v^\s*[-*+]\ze\s+`
syn match rstEnumeratedList `\v\c^\s*%(\d+|[#a-z]|[imlcxvd]+)[.)]\ze\s+`
syn match rstEnumeratedList `\v\c^\s*\(%(\d+|[#a-z]|[imlcxvd]+)\)\ze\s+`
syn match rstOptionList `\v^\s*%(-\w%( \w+)=|--[[:alnum:]_-]+%(\=\w+)=|/\u)%(, %(-\w%( \w+)=|--[[:alnum:]_.-]+%(\=\w+)=|/\u))*%(  |\t)\ze\s*\S`
syn match rstFieldList `\v^\s*:[^:[:space:]][^:]+:\_s`
syn match rstRoles `\v\s:\zs\w+\ze:\``
syn match rstBibliographicField `\v^\s*:(Author|Authors|Organization|Contact|Address|Version|Status|Date|Copyright|Dedication|Abstract):\_s`

syn match rstBlockQuoteAttr  `\v%(\_^\s*\n)@<=\s+---=\s.*`

syn match   rstCommentTitle '\v(^\s+|(^\.\.\s+)@<=):=\u\w*(\s+\u\w*)*:' contained 
syn cluster rstCommentGroup contains=rstCommentTitle,rstTodo

" File:
syn cluster rstCruft add=rstStandaloneHyperlink
syn cluster rstCommentGroup add=@rstLinkGroup

" Code:

" Add block indicator for code directive
syn match rstCodeBlockIndicator `^\_.` contained

" Todo: 
syn cluster rstTodoGroup contains=rstTodoPrior,rstTodoTmBgn,rstTodoTmsEnd

" Highlights: "
if &background == 'light'
    hi def rstFileLink    guifg=#437727  gui=underline ctermfg=28 cterm=underline
else
    hi def rstFileLink    guifg=#58A261  gui=underline ctermfg=77 cterm=underline
endif

hi link rstFileExtLink rstFileLink
hi link rstFileExtLinkConceal rstFileLink

hi def link rstTodoPrior    Include
hi def link rstTodoTmBgn    Number
hi def link rstTodoTmEnd    Number

hi link rstStandaloneHyperlink          Underlined
hi link rstSections Special
hi link rstFootnoteReference            Underlined
hi link rstCitationReference            Underlined
hi link rstHyperLinkReference           Underlined
hi link rstInlineInternalTargets        Keyword
hi link rstPhaseHyperLinkReference      Underlined

hi def link rstBulletList                   Function
hi def link rstEnumeratedList               Function
hi def link rstDefinitionList               Statement
hi link     rstFieldList                    rstComment
hi def link rstBibliographicField           Constant
hi def link rstOptionList                   Statement
hi def link rstRoles                        Operator

hi def link rstBlockQuoteAttr               Exception
hi def link rstCommentTitle                 SpecialComment

" --------------------------------------------------------------------
" MY
" --------------------------------------------------------------------
" ==> NEW

hi rstMetaTagValue guifg=#7c317f ctermfg=DarkGrey  
syn match rstMetaTagValue "\v(^:[a-z]+: )@<=([-:a-zA-Z0-9_]+,? ?)+" 
syn match rstSha "\v\C\w@<![a-z0-9]{40}\w@!"
hi link rstSha PreProc
hi rstComment ctermfg=DarkGrey guifg=grey29
hi rstSpecial guifg=DeepPink3 ctermfg=DarkMagenta gui=bold cterm=bold

" ==> OVERRIDE

hi clear rstExDirective 
hi link rstExDirective SpecialComment
hi link rstBetweenParenthesis rstComment
syn region rstBetweenParenthesis start="\v\(" end="\v\)" oneline 
"hi HR cterm=bold guifg=Grey1 gui=bold ctermfg=DarkGrey
hi link rstFileExtLink rstHyperlinkReference
hi link rstURIChevrons rstHyperlinkReference
hi rstCodeBlockIndicator guibg=grey25 ctermfg=NONE
hi rstDefinitionList guifg=goldenrod1 ctermfg=DarkYellow gui=bold cterm=bold
hi rstDirective guifg=goldenrod1 ctermfg=DarkYellow gui=bold cterm=bold
hi rstEmphasis gui=italic cterm=underline ctermfg=Yellow guifg=cyan3
hi rstHyperlinkReference guifg=SkyBlue2 gui=underline cterm=underline ctermfg=LightBlue
hi rstInlineLiteral guifg=purple ctermfg=LightBlue cterm=NONE gui=NONE
hi rstPhaseHyperLinkReference ctermfg=LightMagenta guifg=#ff80ff  
hi rstStrongEmphasis cterm=bold gui=bold 
syn region rstURIChevrons matchgroup=rstComment start="\v\C\<(\w{2,15}(:/{,2}))?" end="\v\>" oneline concealends

" INLINE AND LINE EDITING:
for i in [[ 'Emphasis', '\*' ], [ 'StrongEmphasis', '\*\*' ], ['InlineLiteral', '``'] ]
  exec 'syn region rst'.i[0].' matchgroup=rstSpecial start="\v\\@<!'.i[1].'" end="\v\\@<!'.i[1].'" oneline '.s:concealends
endfor
