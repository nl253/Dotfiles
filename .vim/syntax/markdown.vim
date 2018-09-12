if exists('b:current_syntax') 
    if b:current_syntax ==# 'markdown'
        finish
    endif
else
    sy clear
    let b:current_syntax = 'markdown' 
endif

sy sync minlines=200

runtime! syntax/html.vim

" sy region markdownParagraph start='\v^( {,3}|\t)' end='\n\n' contains=@Spell

" `int i = 0`
sy region markdownCode start="\v`" end="\v`" skip="\v\\`" oneline keepend
" ``int i = 0``
sy region markdownCode start="``" end="``" skip="\v\\`"   oneline keepend
" ```
"
" code ...
"
" ```
sy region markdownFenced start="\v {,3}`{3,}" end="\v {,3}`{3,}" keepend contains=pandocAttr
" ~~~
"
" code ...
"
" ~~~
sy region pandocFenced start="\v {,3}\~{3,}" end="\v {,3}\~{3,}" keepend contains=pandocAttr
" Indented block: (WIP)
"
"   34ddb532d73c71e147782d5b3...  data-structures.md
"   da439b3a0b2178463fda1d45b...  floyd-warshall.md
"   e0af9ab8b0ba319a1e0a2376d...  runtime-complexity-estimation.md
"
sy region markdownLiteralBlock start='\v    +[^ ]{20,}'ms=s+4   end='$'                         oneline
sy match  markdownFootnote           "\[^[^\]]\+\]"
sy match  markdownFootnoteDefinition "^\[^[^\]]\+\]:"

sy region markdownLinkText matchgroup=markdownLinkTextDelimiter start="!\=\[\%(\_[^]]*]\%( \=[[(]\)\)\@=" end="\]\%( \=[[(]\)\@=" nextgroup=markdownLink,markdownId skipwhite contains=@markdownInline,markdownLineStart,@Spell
sy region markdownLink     matchgroup=markdownLinkDelimiter start="(" end=")" contains=markdownUrl keepend contained
sy region markdownId       matchgroup=markdownIdDelimiter start="\[" end="\]" keepend contained contains=@Spell

" FIXME setext headings
" sy match markdownH1 "\v^ {,3}[^#+*-]+\n {,3}\=+$" contained contains=@Spell,markdownInline,markdownHeadingRule

sy match markdownHeadingRule "\v^ {,3}[=-]+$" contained
" <./relative/link_1>
" </root/is/absolute>
" <~/relative/to/$HOME>
" <marko99@gmail.com>
" <ssh://me:pass>
" <https://www.google.com>
" and all other protocols such as pop3, sftp, ftp ... 
sy region pandocURI start="\v\<((<[a-z234]{3,7}(://|\@))|(\~|\.?/))" end=">" oneline
" this is the same but doesn't require to be enclosed in '<' and '>'
" it must consist of non-ws chars
" FIXME too slow
" sy match pandocURI "\v((<[a-z234]{3,7}(://|\@))|\~|\.?/)\S{5,140}"

sy region markdownIdDeclaration matchgroup=markdownLinkDelimiter     start="^ \{0,3\}!\=\[" end="\]:" oneline keepend nextgroup=markdownUrl skipwhite contains=@Spell
sy match  markdownUrl "\S\+"    nextgroup=markdownUrlTitle skipwhite contained
sy region markdownUrl           matchgroup=markdownUrlDelimiter      start="<" end=">" oneline keepend nextgroup=markdownUrlTitle skipwhite contained
sy region markdownUrlTitle      matchgroup=markdownUrlTitleDelimiter start=+"+ end=+"+ keepend contained contains=@Spell
sy region markdownUrlTitle      matchgroup=markdownUrlTitleDelimiter start=+'+ end=+'+ keepend contained contains=@Spell
sy region markdownUrlTitle      matchgroup=markdownUrlTitleDelimiter start=+(+ end=+)+ keepend contained contains=@Spell

" Blocks:
" ::: My fenced div :::
sy region pandocFencedDiv start="\v:::+" end="\v:::+" contains=pandocAttr,pandocExampleListRef,pandocSuperscript,pandocSubscript,markdownBold,markdownItalic,markdownBoldItalic,markdownURI,markdownEscape,markdownCode,pandocStrikethrough,markdownLinkText,markdownLinkTextDelimiter,markdownLink keepend
" {.pretty #fancy}
sy region pandocAttr start="{" end="}" skip="\v\\\}" contained oneline containedin=markdownCode
"
" Meta:
sy region pandocTitleBlock start="^%" end="$" oneline
sy region pandocYamlMetaBlock start="\v%^\n*---" end="\v^(\.{3}|-{3})"

" Line Blocks:
" |  Left aligned text
" |      with some indentation.
sy region pandocLineBlockPipe start="\v^\|\s*" end="$" oneline contains=pandocExampleListRef,pandocSuperscript,pandocSubscript,markdownBold,markdownItalic,markdownItalicDelimiter,markdownBoldItalic,markdownURI,markdownEscape,markdownCode,pandocStrikethrough,markdownLinkText,markdownLinkTextDelimiter,markdownLink,@Spell

" Subscript Superscript: 
" x~i~
sy region pandocSubscript   start="\v\~" end="\v\~" skip="\\\~" oneline
" 2^n^
sy region pandocSuperscript start="\v\^" end="\v\^"             oneline concealends

" Strikethrough:
" ~~woooo~~
sy region pandocStrikethrough start="\~\~" end="\~\~" skip="\\\~" oneline contains=markdownBold,markdownItalic,markdownBoldItalic,markdownURI,markdownEscape,@Spell

" Lists: 
sy match  pandocBulletListMarker "\v^\s*(\(?[a-z0-9]\)|[1-9#iIVv]\.|\+|-|\*) "
" same but not stuck to start of line (note lack of '^')
sy match  pandocBulletListMarkerBare "\v\s+(\(?[a-z0-9]\)|[1-9#iIVv]\.|\+|-|\*) " contained
sy region pandocExampleListRef start="\v\(\@" end="\v\)" skip="\v\\\)" oneline
" Term
" ~ def1
" ~ def2
sy match pandocDefMarker "\v^ {,2}[\~:] " 
sy match pandocAmbiguous "\v> +- +<|\s+$"
" something -- obviously
sy match pandocDash "\v\s--[ \t\n]"

" HR: (must be loaded after lists)
sy match markdownHR "\v^ {,3}((\* {,3})|(- {,3})|(_ {,3})){3,}$"

" Links:
" sy region markdownInLineReference      start="\[" end=")"  skip="\v\\\]|\\\)" keepend   contains=markdownInLineReferenceLabel,markdownInLineReferenceURI           
" sy region markdownInLineReferenceLabel start="\[" end="\]" skip="\\\]"        contained matchgroup=Delimiter                                                       
" sy region markdownInLineReferenceURI   start="("  end=")"  skip="\\)"         contained matchgroup=Delimiter

" Math:
" \dostuff
sy match  pandocTexCmd   "\v\\[a-z]+" contained containedin=pandocInlineMath,pandocMultiLineMath
" \dostuff{withthis}
sy match  pandocTexBrace "\v\{|\}"    contained containedin=pandocMultiLineMath,pandocInlineMath
" \( math \)
sy region pandocInlineMath    start="\\("    end="\\)"                  oneline       
sy region pandocInlineMath    start="\$"     end="\$"     skip="\v\\\$" oneline
" $$ 
"    math ...
" $$
sy region pandocMultiLineMath start="\$\$"   end="\$\$"   skip="\v\\\$"                                     
sy region pandocMultiLineMath start="\v\\\[" end="\v\\\]"                                                   


for s:i in range(1, 6)
    exe 'sy region markdownH'.s:i.' matchgroup=markdownHeadingDelimiter start="^'.repeat('#', s:i).' " end="\v('.repeat('#', s:i).')?$" keepend oneline contains=@Spell'
    exe 'hi def link markdownH'.s:i.' htmlH'.s:i
endfor

" Inlines:
" *italic*
" _italic_
sy region markdownItalic start="\v\*" end="\v\*" skip="\v\\\*" oneline keepend matchgroup=Delimiter concealends contains=markdownAsterisk,@Spell
sy region markdownItalic start="_" end="_" skip="\v\\_"        oneline keepend matchgroup=Delimiter concealends contains=@Spell
" **bold**
" __bold__
sy region markdownBold    start="\v\*\*" end="\v\*\*" skip="\v\\\*" oneline keepend matchgroup=Delimiter concealends contains=markdownAsterisk,@Spell
sy region markdownBold    start="__"     end="__"     skip="\v\\_"  oneline keepend matchgroup=Delimiter concealends contains=@Spell
sy match markdownAsterisk '\v\*+' contained

" Simple Table:
"
"           Right       Left     Center     Default
"           -------     ------ ----------   -------
"               12      12        12            12
"               123     123       123          123
"               1       1          1             1
"
" NonHeader Version:
"
"           -------     ------ ----------   -------
"               12      12        12            12
"               123     123       123          123
"               1       1          1             1
"           -------     ------ ----------   -------
"
sy region pandocSimpleTable start="\v(^  +[^\n]*)?$\n^  +----+( +----+)*" end="\n\n" contains=@Spell
" FIXME extended pandoc table
" sy region pandocSimpleTable start="\v^  +-{20,}$"                         end="\v^  +-{20,}$\n" contains=@Spell
sy match  pandocSimpleTableError "\v------+ {2,}-----+" contained containedin=pandocSimpleTable 
sy match  pandocSimpleTableError "\v^\s?\S+"         contained containedin=pandocSimpleTable 
sy match  pandocSimpleTableLine "\v----+"                         containedin=pandocSimpleTable

" BlockQuote:
" > quoted text
sy region markdownBlockQuote            start="\v^ {,3}\> ?"         end="$" oneline contains=pandocBulletListMarkerBare,markdownBold,markdownItalic,pandocStrikethrough,markdownBoldItalic,markdownCode,markdownURI,pandocInlineMath,pandocDash,@Spell
" > > double quoted text
sy region markdownBlockQuoteQuote       start="\v^ {,3}\> ?\> ?"     end="$" oneline contains=pandocBulletListMarkerBare,markdownBold,markdownItalic,pandocStrikethrough,markdownBoldItalic,markdownCode,markdownURI,pandocInlineMath,pandocDash,@Spell
" > > > triple quoted text
sy region markdownBlockQuoteQuotedQuote start="\v^ {,3}\> ?\> ?\> ?" end="$" oneline contains=pandocBulletListMarkerBare,markdownBold,markdownItalic,pandocStrikethrough,markdownBoldItalic,markdownCode,markdownURI,pandocInlineMath,pandocDash,@Spell

hi def link markdownAsterisk              Delimiter
hi def link markdownBlock                 SpecialComment
hi def link markdownBlockQuote            String
hi def link markdownBlockQuoteQuote       Macro
hi def link markdownBlockQuoteQuotedQuote Operator
hi def link markdownBlockquote            SpecialComment
hi def link markdownBoldDelimiter         Delimiter
hi def link markdownCode                  PreProc
hi def link markdownFenced                markdownCode                 
hi def link markdownHR                    Special
hi def link markdownHeadingDelimiter      Comment
hi def link markdownInLineReferenceLabel  URI
hi def link markdownInLineReferenceURI    Comment
hi def link markdownItalicDelimiter       Delimiter
hi def link markdownLiteralBlock          PreProc
hi def link markdownUrl                   Comment
hi def link markdownLinkText              URI
hi def link pandocAttr                    SpecialChar
hi def link pandocBulletListMarker        Delimiter
hi def link pandocBulletListMarkerBare    Delimiter
hi def link pandocDash                    Character
hi def link pandocDefMarker               Special
hi def link pandocExampleListRef          Define
hi def link pandocFenced                  markdownFenced                 
hi def link pandocFencedDiv               SpecialComment
hi def link pandocInlineMath              Special
hi def link pandocLineBlockPipe           SpecialComment
hi def link pandocMultiLineMath           Special
hi def link pandocSimpleTable             SpecialComment
hi def link pandocSimpleTableError        pandocAmbiguous
hi def link pandocSimpleTableLine         Define
hi def link pandocStrikethrough           Comment
hi def link pandocSubscript               Character
hi def link pandocSuperscript             Character
hi def link pandocTexBrace                Character
hi def link pandocTexCmd                  Constant
hi def link pandocTitleBlock              PreProc
hi def link pandocURI                     URI
hi def link pandocYamlMetaBlock           PreProc
" hi def link markdownParagraph             Normal

hi def markdownBold             gui=bold     cterm=bold
hi def markdownItalic           gui=italic   cterm=italic
hi def markdownHeadingDelimiter guifg=Grey30 ctermfg=239
hi def pandocAmbiguous                       ctermfg=Red guibg=Red 
