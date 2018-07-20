if exists("b:current_syntax") | finish | endif

if !exists('main_syntax')
    let main_syntax = 'markdown'
endif

runtime! syntax/html.vim
unlet! b:current_syntax

syn sync minlines=10
syn case ignore

sy match mdToDo "\v<TODO>" contained containedin=htmlComment
hi link mdToDo Boolean

syn region markdownCode matchgroup=markdownCodeDelimiter start="^```.*$" end="^```" keepend
sy region markdownCode start="\v`" end="\v`" skip="\v\\`" oneline keepend

syn match markdownFootnote "\[^[^\]]\+\]"
syn match markdownFootnoteDefinition "^\[^[^\]]\+\]:"

syn region markdownLinkText matchgroup=markdownLinkTextDelimiter start="!\=\[\%(\_[^]]*]\%( \=[[(]\)\)\@=" end="\]\%( \=[[(]\)\@=" nextgroup=markdownLink,markdownId skipwhite contains=@markdownInline,markdownLineStart
syn region markdownLink matchgroup=markdownLinkDelimiter start="(" end=")" contains=markdownUrl keepend contained
syn region markdownId matchgroup=markdownIdDelimiter start="\[" end="\]" keepend contained
syn region markdownAutomaticLink matchgroup=markdownUrlDelimiter start="<\%(\w\+:\|[[:alnum:]_+-]\+@\)\@=" end=">" keepend oneline

syn region markdownBlockquote start="^ {,3}\> ?" end="$" contained oneline contains=@Spell

" FIXME
" syn match markdownSetextRule1 "\v^ {,3}=[= ]*$"
" syn match markdownSetextRule2 "\v^ {,3}-[- ]*$"

" syn match markdownH1 "^.\+\n=\+$" contained contains=@markdownInline,markdownHeadingRule,markdownAutomaticLink
" syn match markdownH2 "^.\+\n-\+$" contained contains=@markdownInline,markdownHeadingRule,markdownAutomaticLink

syn region markdownBlock start="    \|\t" end="$" contained oneline
syn cluster markdownBlock contains=markdownH1,markdownH2,markdownH3,markdownH4,markdownH5,markdownH6,markdownBlockquote,markdownListMarker,markdownOrderedListMarker,markdownCodeBlock,markdownRule
syn cluster markdownInline contains=markdownLineBreak,markdownLinkText,markdownItalic,markdownBold,markdownCode,markdownEscape,@htmlTop,markdownError

syn match markdownValid '[<>]\c[a-z/$!]\@!'
syn match markdownValid '&\%(#\=\w*;\)\@!'

syn match markdownLineBreak " \{2,\}$"

syn match markdownRule "\* *\* *\*[ *]*$" contained
syn match markdownRule "- *- *-[ -]*$" contained

syn region markdownIdDeclaration matchgroup=markdownLinkDelimiter start="^ \{0,3\}!\=\[" end="\]:" oneline keepend nextgroup=markdownUrl skipwhite
syn match markdownUrl "\S\+" nextgroup=markdownUrlTitle skipwhite contained
syn region markdownUrl matchgroup=markdownUrlDelimiter start="<" end=">" oneline keepend nextgroup=markdownUrlTitle skipwhite contained
syn region markdownUrlTitle matchgroup=markdownUrlTitleDelimiter start=+"+ end=+"+ keepend contained
syn region markdownUrlTitle matchgroup=markdownUrlTitleDelimiter start=+'+ end=+'+ keepend contained
syn region markdownUrlTitle matchgroup=markdownUrlTitleDelimiter start=+(+ end=+)+ keepend contained

sy region markdownBold start="\v\*\*" end="\v\*\*" skip="\v\\\*" oneline keepend
sy region markdownItalic start="\v\*" end="\v\*" skip="\v\\\*" oneline keepend

hi link markdownAsterisk Delimiter
sy match markdownAsterisk "\*" containedin=markdownBold,markdownItalic,markdownItalicBold,markdownBoldItalic

"sy region markdownFootnoteInTxt start="\^\[" end="]" concealends
"hi link markdownFootnoteInTxt PreProc
" Blocks:
sy region pandocFencedDiv start="\v:::+" end="\v:::+" contains=pandocAttr,pandocExampleListRef,pandocSuperscript,pandocSubscript,markdownBold,markdownItalic,markdownItalicDelimiter,markdownBoldItalic,markdownBoldDelimiter,markdownAutomaticLink,markdownEscape,markdownCode,pandocStrikethrough,markdownLinkText,markdownLinkTextDelimiter,markdownLink,markdownFootnoteInTxt keepend
sy region pandocAttr start="\v\{" end="\v\}" skip="\v\\\}" contained oneline
"
" Meta:
sy region pandocTitleBlock start="^%" end="$" oneline
sy region pandocYamlMetaBlock start="\v%^\n*---" end="\v^(\.{3}|-{3})"


" Line Blocks:
sy region pandocLineBlockPipe start="\v^\|[ \t]*" end="$" oneline contains=pandocExampleListRef,pandocSuperscript,pandocSubscript,markdownBold,markdownItalic,markdownItalicDelimiter,markdownBoldItalic,markdownBoldDelimiter,markdownAutomaticLink,markdownEscape,markdownCode,pandocStrikethrough,markdownLinkText,markdownLinkTextDelimiter,markdownLink,markdownFootnoteInTxt

" Subscript Superscript: 
sy region pandocSubscript start="\v\~" end="\v\~" skip="\\\~" oneline 
sy region pandocSuperscript start="\v\^" end="\v\^" oneline concealends

" Strikethrough:
sy region pandocStrikethrough start="\~\~" end="\~\~" skip="\\\~" oneline  contains=markdownBold,markdownItalic,markdownItalicDelimiter,markdownBoldItalic,markdownBoldDelimiter,markdownAutomaticLink,markdownEscape

" Lists: 
sy match pandocBulletListMarker "\v^[ \t]*(\(?[a-z0-9]\)|[1-9#iIVv]\.|\+|-|\*) " 
sy region pandocExampleListRef start="\v\(\@" end="\v\)" skip="\v\\\)" oneline
sy match pandocDefMarker "\v^ {,2}[\~:] " 

" Links:
sy region markdownLinkChev start="\v\<([a-z]+:|\.{,2}\/)" end=">" skip="\\>" oneline 
" sy region markdownInLineReference start="\[" end="\]" skip="\v\\\]" oneline keepend contains=markdownInLineReferenceLabel,markdownInLineReferenceURI 
" sy region markdownInLineReferenceLabel start="\[" end="\]" skip="\\\]" keepend contained matchgroup=Delimiter
" sy region markdownInLineReferenceURI start="(" end=")" skip="\\)" contained keepend matchgroup=Delimiter

" Math:
sy region pandocInlineMath start="\\\\(" end="\\\\)" oneline contained 
sy region pandocMultiLineMath start="\$\$" end="\$\$" skip="\\\$" 

syn region markdownH1 matchgroup=markdownHeadingDelimiter start="^##\@!"      end="$" keepend oneline contains=@markdownInline,markdownAutomaticLink contains=@Spell
syn region markdownH2 matchgroup=markdownHeadingDelimiter start="^###\@!"     end="$" keepend oneline contains=@markdownInline,markdownAutomaticLink contains=@Spell
syn region markdownH3 matchgroup=markdownHeadingDelimiter start="^####\@!"    end="$" keepend oneline contains=@markdownInline,markdownAutomaticLink contains=@Spell
syn region markdownH4 matchgroup=markdownHeadingDelimiter start="^#####\@!"   end="$" keepend oneline contains=@markdownInline,markdownAutomaticLink contains=@Spell
syn region markdownH5 matchgroup=markdownHeadingDelimiter start="^######\@!"  end="$" keepend oneline contains=@markdownInline,markdownAutomaticLink contains=@Spell
syn region markdownH6 matchgroup=markdownHeadingDelimiter start="^#######\@!" end="$" keepend oneline contains=@markdownInline,markdownAutomaticLink contains=@Spell

hi link markdownSetextRule1 markdownH1
hi link markdownSetextRule2 markdownH2

hi link markdownBlock SpecialComment
hi link markdownBlockquote SpecialComment
hi link markdownBold htmlBold
hi link markdownBoldDelimiter Delimiter
hi link markdownCode PreProc
hi link markdownCodeDelimiter Comment
hi link markdownHeadingDelimiter Comment
" hi link markdownInLineReferenceLabel URI
" hi link markdownInLineReferenceURI Comment
hi link markdownItalic htmlItalic
hi link markdownItalicDelimiter Delimiter
hi link markdownLinkChev URI 
hi link markdownRule Special
hi link markdownUrl Comment
hi link pandocAttr PreProc 
hi link pandocBulletListMarker Delimiter
hi link pandocDefMarker Special
hi link pandocExampleListRef Define
hi link pandocFencedDiv SpecialComment
hi link pandocInlineMath Special
hi link pandocLineBlockPipe SpecialComment
hi link pandocMultiLineMath Special
hi link pandocStrikethrough Comment
hi link pandocSubscript Character
hi link pandocSuperscript Character
hi link pandocTitleBlock PreProc
hi link pandocYamlMetaBlock PreProc

let b:current_syntax = "markdown"
if main_syntax ==# 'markdown'
    unlet main_syntax
endif
