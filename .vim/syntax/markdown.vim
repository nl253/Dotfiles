"if exists("b:current_syntax") | finish | endif

if !exists('main_syntax')
    let main_syntax = 'markdown'
endif

sy clear

runtime! syntax/html.vim

unlet! b:current_syntax

sy sync minlines=10
sy case ignore

" `int i = 0`
sy region markdownCode start="\v`" end="\v`" skip="\v\\`" oneline keepend
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
" Indented block:
"
"   34ddb532d73c71e147782d5b3...  data-structures.md
"   da439b3a0b2178463fda1d45b...  floyd-warshall.md
"   e0af9ab8b0ba319a1e0a2376d...  runtime-complexity-estimation.md
"
sy region markdownLiteralBlock start='\v    +[^ ]{20,}'ms=s+4   end='$'                         oneline
sy match  markdownFootnote           "\[^[^\]]\+\]"
sy match  markdownFootnoteDefinition "^\[^[^\]]\+\]:"

sy region markdownLinkText matchgroup=markdownLinkTextDelimiter start="!\=\[\%(\_[^]]*]\%( \=[[(]\)\)\@=" end="\]\%( \=[[(]\)\@=" nextgroup=markdownLink,markdownId skipwhite contains=@markdownInline,markdownLineStart
sy region markdownLink     matchgroup=markdownLinkDelimiter start="(" end=")" contains=markdownUrl keepend contained
sy region markdownId       matchgroup=markdownIdDelimiter start="\[" end="\]" keepend contained

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

" HR:
sy match markdownHR "\* *\* *\*[ *]*$" contained
sy match markdownHR "- *- *-[ -]*$"    contained
sy match markdownHR "\v^((-{3,})|(_{3,})|(\*{3,}))"
sy match markdownHR "\v^(- ){2,}-[ -]*"
sy match markdownHR "\v^(\* ){2,}-[ \*]*"
sy match markdownHR "\v^(_ ){2,}-[ _]*"

sy region markdownIdDeclaration matchgroup=markdownLinkDelimiter start="^ \{0,3\}!\=\[" end="\]:" oneline keepend nextgroup=markdownUrl skipwhite
sy match markdownUrl "\S\+" nextgroup=markdownUrlTitle skipwhite contained
sy region markdownUrl      matchgroup=markdownUrlDelimiter start="<" end=">" oneline keepend nextgroup=markdownUrlTitle skipwhite contained
sy region markdownUrlTitle matchgroup=markdownUrlTitleDelimiter start=+"+ end=+"+ keepend contained
sy region markdownUrlTitle matchgroup=markdownUrlTitleDelimiter start=+'+ end=+'+ keepend contained
sy region markdownUrlTitle matchgroup=markdownUrlTitleDelimiter start=+(+ end=+)+ keepend contained

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
sy region pandocLineBlockPipe start="\v^\|[ \t]*" end="$" oneline contains=pandocExampleListRef,pandocSuperscript,pandocSubscript,markdownBold,markdownItalic,markdownItalicDelimiter,markdownBoldItalic,markdownURI,markdownEscape,markdownCode,pandocStrikethrough,markdownLinkText,markdownLinkTextDelimiter,markdownLink

" Subscript Superscript: 
" x~i~
sy region pandocSubscript   start="\v\~" end="\v\~" skip="\\\~" oneline
" 2^n^
sy region pandocSuperscript start="\v\^" end="\v\^"             oneline concealends

" Strikethrough:
" ~~woooo~~
sy region pandocStrikethrough start="\~\~" end="\~\~" skip="\\\~" oneline contains=markdownBold,markdownItalic,markdownBoldItalic,markdownURI,markdownEscape

" Lists: 
sy match pandocBulletListMarker "\v^[ \t]*(\(?[a-z0-9]\)|[1-9#iIVv]\.|\+|-|\*) "
" same but not stuck to start of line (note lack of '^')
sy match pandocBulletListMarkerBare "\v[ \t]+(\(?[a-z0-9]\)|[1-9#iIVv]\.|\+|-|\*) " contained
sy region pandocExampleListRef start="\v\(\@" end="\v\)" skip="\v\\\)" oneline
" Term
" ~ def1
" ~ def2
sy match pandocDefMarker "\v^ {,2}[\~:] " 
sy match pandocAmbiguous "\v> +- +<|[ \t]+$"
" something -- obviously
sy match pandocDash "\v[ \t]--[ \t\n]"

" Links:
sy region markdownInLineReference      start="\[" end=")"  skip="\v\\\]|\\\)" keepend   contains=markdownInLineReferenceLabel,markdownInLineReferenceURI           
sy region markdownInLineReferenceLabel start="\[" end="\]" skip="\\\]"        contained matchgroup=Delimiter                                                       
sy region markdownInLineReferenceURI   start="("  end=")"  skip="\\)"         contained matchgroup=Delimiter

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

hi link pandocTexBrace Character
hi link pandocTexCmd Constant

for s:i in range(1, 6)
    exe 'sy region markdownH'.s:i.' matchgroup=markdownHeadingDelimiter start="^'.repeat('#', s:i).' " end="\v('.repeat('#', s:i).')?$" keepend oneline contains=@Spell'
    exe 'hi link markdownH'.s:i.' htmlH'.s:i
endfor

" Inlines:
" *italic*
sy region markdownItalic start="\v\*" end="\v\*" skip="\v\\\*"   oneline keepend  matchgroup=Delimiter concealends contains=markdownAsterisk
" **bold**
sy region markdownBold   start="\v\*\*" end="\v\*\*" skip="\v\\\*" oneline keepend  matchgroup=Delimiter concealends contains=markdownAsterisk
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
sy match  pandocSimpleTableError "\v^[ \t]?\S+"         contained containedin=pandocSimpleTable 
sy match  pandocSimpleTableLine "\v----+"                         containedin=pandocSimpleTable

" BlockQuote:
" > quoted text
sy region markdownBlockQuote            start="\v^ {,3}\> ?"         end="$" oneline contains=pandocBulletListMarkerBare,markdownBold,markdownItalic,pandocStrikethrough,markdownBoldItalic,markdownCode,markdownURI,pandocInlineMath,pandocDash
" > > double quoted text
sy region markdownBlockQuoteQuote       start="\v^ {,3}\> ?\> ?"     end="$" oneline contains=pandocBulletListMarkerBare,markdownBold,markdownItalic,pandocStrikethrough,markdownBoldItalic,markdownCode,markdownURI,pandocInlineMath,pandocDash
" > > > triple quoted text
sy region markdownBlockQuoteQuotedQuote start="\v^ {,3}\> ?\> ?\> ?" end="$" oneline contains=pandocBulletListMarkerBare,markdownBold,markdownItalic,pandocStrikethrough,markdownBoldItalic,markdownCode,markdownURI,pandocInlineMath,pandocDash

hi link markdownAsterisk              Delimiter
hi link markdownBlock                 SpecialComment
hi link markdownBlockQuote            String
hi link markdownBlockQuoteQuote       Macro
hi link markdownBlockQuoteQuotedQuote Operator
hi link markdownBlockquote            SpecialComment
hi link markdownBold                  htmlBold
hi link markdownBoldDelimiter         Delimiter
hi link markdownCode                  PreProc
hi link markdownFenced                markdownCode                 
hi link markdownHR                    Special
hi link markdownHeadingDelimiter      Comment
hi link markdownInLineReferenceLabel  URI
hi link markdownInLineReferenceURI    Comment
hi link markdownItalic                htmlItalic
hi link markdownItalicDelimiter       Delimiter
hi link markdownLiteralBlock          PreProc
hi link markdownUrl                   Comment
hi link pandocAttr                    SpecialChar
hi link pandocBulletListMarker        Delimiter
hi link pandocBulletListMarkerBare    Delimiter
hi link pandocDash                    Character
hi link pandocDefMarker               Special
hi link pandocExampleListRef          Define
hi link pandocFenced                  markdownFenced                 
hi link pandocFencedDiv               SpecialComment
hi link pandocInlineMath              Special
hi link pandocLineBlockPipe           SpecialComment
hi link pandocMultiLineMath           Special
hi link pandocSimpleTable             SpecialComment
hi link pandocSimpleTableError        pandocAmbiguous
hi link pandocSimpleTableLine         Define
hi link pandocStrikethrough           Comment
hi link pandocSubscript               Character
hi link pandocSuperscript             Character
hi link pandocTitleBlock              PreProc
hi link pandocURI                     URI
hi link pandocYamlMetaBlock           PreProc

hi markdownHeadingDelimiter guifg=Grey30 ctermfg=239
hi pandocAmbiguous guibg=Red ctermfg=Red

let b:current_syntax = "markdown"

if main_syntax ==# 'markdown'
    unlet main_syntax
endif
