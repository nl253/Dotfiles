sy sync minlines=350

runtime! syntax/html.vim

if exists('b:current_syntax') 
    if b:current_syntax ==# 'markdown'
        finish
    endif
else
    sy clear
    let b:current_syntax = 'markdown' 
endif

" necessary else the line below won't source
unlet b:current_syntax 
exe 'sy include @pandocTex '.$VIMRUNTIME.'/syntax/tex.vim'

sy match markdownEscape '\v\\.'

" <./relative/link_1>
" <~/relative/to/$HOME>
" <marko99@gmail.com>
" <ssh://me:pass>
" <https://www.google.com>
" and all other protocols such as pop3, sftp, ftp ... 
" NOTE: </root/is/absolute> is not allowed as it clashes with html syntax
sy region markdownURI start="\v\<((<[a-z234]{3,7}(://|\@))|(\~|\./))" end=">" oneline
sy match  markdownUrl "\v\S+" nextgroup=markdownUrlTitle skipwhite                        contained
sy region markdownUrlTitle matchgroup=Delimiter start=+\v\z(["'])+ end="\v\z1" keepend contained contains=@Spell
sy region markdownUrlTitle matchgroup=Delimiter start=+(+          end=+)+     keepend contained contains=@Spell

" Indented block: (WIP)
"
"   34ddb532d73c71e147782d5b3...  data-structures.md
"   da439b3a0b2178463fda1d45b...  floyd-warshall.md
"   e0af9ab8b0ba319a1e0a2376d...  runtime-complexity-estimation.md
"
" sy region markdownLiteralBlock start='\v    +[^ ]{20,}'ms=s+4   end='$'                         oneline

" I get 10 times more traffic from [Google] [1] than from [Yahoo] [2] or [MSN] [3].
sy match markdownFootnoteRef        "\v\[(\d+|[^\]]{3,})\]((( {1,3}|\t)?)@=(\[[^\]]{,150}\]))?"
" [1]: http://google.com/        "Google"
" [2]: http://search.yahoo.com/  "Yahoo Search"
" [3]: http://search.msn.com/    "MSN Search"
sy region markdownFootnoteDefinition start="\v^\s*\[[^\]]+\]:" end="$" oneline contains=markdownAutoURI

sy region markdownLinkText matchgroup=markdownLinkTextDelimiter start="!\=\[\%(\_[^]]*]\%( \=[[(]\)\)\@=" end="\]\%( \=[[(]\)\@=" nextgroup=markdownLink,markdownId skipwhite contains=@Spell
sy region markdownLink     matchgroup=markdownLinkDelimiter     start="("  end=")"  keepend contained contains=markdownUrl 
sy region markdownId       matchgroup=markdownIdDelimiter       start="\[" end="\]" keepend contained contains=@Spell

" FIXME setext headings
" sy match markdownH1 "\v^ {,3}[^#+*-]+\n {,3}\=+$" contained contains=@Spell,markdownInline,markdownHeadingRule
" sy match markdownHeadingRule "\v^ {,3}[=-]+$" contained

" this is the same but doesn't require to be enclosed in '<' and '>'
" it must consist of non-ws chars
" FIXME too slow to be global
sy match markdownAutoURI "\v((<[a-z234]{3,7}(://|\@))|\~|\./)[-a-zA-Z0-9_%\.\~/]{5,140}" contained

" Blocks:
" ::: My fenced div :::
sy region pandocFencedDiv start="\v:::+" end="\v:::+" contains=pandocAttr,pandocExampleListRef,pandocSuperscript,pandocSubscript,markdownBold,markdownItalic,markdownURI,markdownEscape,markdownCode,pandocStrikethrough,markdownLinkText,markdownLinkTextDelimiter,markdownLink keepend
" {.pretty #fancy}
sy region pandocAttr start="{" end="}" skip="\v\\\}" contained oneline keepend
sy match  pandocAttrId    "\v#\w+>"   contained containedin=pandocAttr
sy match  pandocAttrClass "\v\.\w+>"  contained containedin=pandocAttr
sy match  pandocAttrKV "\v<\w+\=\w+>" contained containedin=pandocAttr
"
" Meta:
sy region pandocTitleBlock start="^%" end="$" oneline
sy region pandocYamlMetaBlock start="\v%^\n*---" end="\v^(\.{3}|-{3})"

" Line Blocks:
" |  Left aligned text
" |      with some indentation.
sy region pandocLineBlockPipe start="\v^\|(\s*)@=" end="$" oneline contains=pandocExampleListRef,pandocSuperscript,pandocSubscript,markdownBold,markdownItalic,markdownItalicDelimiter,markdownURI,markdownEscape,markdownCode,pandocStrikethrough,markdownLinkText,markdownLinkTextDelimiter,markdownLink,@Spell

" Subscript Superscript: 
" x~i~
sy region pandocSubscript   start="\v\~\~@!" end="\v\~" skip="\\\~" oneline
" 2^n^
sy region pandocSuperscript start="\v\^\^@!" end="\v\^"             oneline concealends

" Strikethrough:
" ~~woooo~~
sy region pandocStrikethrough start="\v\~{2}\~@!" end="\v\~{2}" skip="\\\~" oneline contains=markdownEscape,@Spell

" Lists: 
sy match  pandocBulletListMarker "\v^\s*(\(?([a-zA-Z]|[1-9][0-9]*)[).]|#\.|[-+*])( {1,3}|\t)"
sy region pandocExampleListRef start="\v\(\@" end="\v\)" skip="\v\\\)" oneline contains=@Spell
" Term
" ~ def1
" ~ def2
sy match pandocDefMarker "\v^( {1,3}|\t)?[\~:](\s+)@=" 
" - instead of -- 
" trailing whiespace
sy match pandocAmbiguous "\v>\s+-\s+<|\s+$"
" something -- obviously
sy match pandocDash "\v(\s+|>|^)@=--(\s+|<|$)@="

" HR: (must be loaded after lists)
sy match markdownHR "\v^( {1,3}|\t)?((\*( {1,3}|\t)?)|(-( {1,3}|\t)?)|(_( {1,3}|\t)?)){3,}\s*$"

" Links:
" sy region markdownInLineReference      start="\[" end=")"  skip="\v\\\]|\\\)" keepend   contains=markdownInLineReferenceLabel,markdownInLineReferenceURI           
" sy region markdownInLineReferenceLabel start="\[" end="\]" skip="\\\]"        contained matchgroup=Delimiter                                                       
" sy region markdownInLineReferenceURI   start="("  end=")"  skip="\\)"         contained matchgroup=Delimiter

" Math:
" \( math \)
" $math$
sy region pandocInlineMath start="\\(" end="\\)" skip='\v\\\)' oneline matchgroup=Delimiter contains=@pandocTex transparent
sy region pandocInlineMath start="\$"  end="\$"  skip="\v\\\$" oneline matchgroup=Delimiter contains=@pandocTex transparent
" $$ 
"    math ...
" $$
sy region pandocMultiLineMath start="\$\$"   end="\$\$"   skip="\v\\\$" matchgroup=Delimiter transparent
sy region pandocMultiLineMath start="\v\\\[" end="\v\\\]" skip="\v\\\]" matchgroup=Delimiter transparent

for s:i in range(1, 6)
    exe 'sy region markdownH'.s:i.' matchgroup=Operator start="^'.repeat('#', s:i).'\v @=" end="\v('.repeat('#', s:i).')?$" keepend oneline contains=markdownBold,markdownURI,markdownItalic,markdownEscape,markdownCode,@Spell'
    exe 'hi def link markdownH'.s:i.' htmlH'.s:i
endfor

" Inlines:
" *italic*
" _italic_
sy region markdownItalic start="\v\*\*@!"ms=s+1 end="\v\*"me=e-1 skip="\v\\\*" oneline keepend matchgroup=Delimiter concealends contains=markdownCode,markdownBold,pandocStrikethrough,pandocSubscript,pandocSuperscript,markdownURI,pandocInlineMath,pandocDash,@Spell
sy region markdownItalic start="\v__@!"ms=s+1 end="_"me=e-1 skip="\v\\_"       oneline keepend matchgroup=Delimiter concealends contains=markdownCode,markdownBold,pandocStrikethrough,pandocSubscript,pandocSuperscript,markdownURI,pandocInlineMath,pandocDash,@Spell
" **bold**
" __bold__
sy region markdownBold start="\v\*{2}\*@!"ms=s+2 end="\v\*\*"ms=e-2 skip="\v\\\*"   oneline keepend matchgroup=Delimiter concealends contains=markdownCode,markdownItalic,pandocStrikethrough,pandocSubscript,pandocSuperscript,markdownURI,pandocInlineMath,pandocDash,@Spell
sy region markdownBold start="\v_{2}_@!"ms=s+2     end="__"ms=e-2     skip="\v\\_"  oneline keepend matchgroup=Delimiter concealends contains=markdownCode,markdownItalic,pandocStrikethrough,pandocSubscript,pandocSuperscript,markdownURI,pandocInlineMath,pandocDash,@Spell

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
sy region pandocSimpleTable start="\v(^  +[^\n]*)?\n  +----+( +----+)*" end="\n\n" contains=markdownBold,markdownItalic,markdownURI,pandocStrikethrough,pandocSubscript,pandocSuperscript,markdownCode,pandocInlineMath,pandocDash,@Spell
 
" FIXME extended pandoc table
" sy region pandocSimpleTable start="\v^  +-{20,}$"                         end="\v^  +-{20,}$\n" contains=@Spell
sy match pandocSimpleTableError "\v------+ {2,}-----+" contained containedin=pandocSimpleTable 
sy match pandocSimpleTableError "\v^\s?\S+"            contained containedin=pandocSimpleTable 
sy match pandocSimpleTableLine  "\v----+"              contained containedin=pandocSimpleTable

" BlockQuote:
"
" NOTE: the order here is crucial.
" > quoted text
sy region markdownBlockQuote            start="\v^( {1,3}|\t)?\>( {1,3}|\t)?" end="$" oneline contains=pandocBulletListMarker,markdownBold,markdownItalic,pandocStrikethrough,markdownCode,markdownURI,pandocInlineMath,pandocDash,pandocSubscript,pandocSuperscript,markdownFootnoteRef,@Spell
" > > double quoted text
sy region markdownBlockQuoteQuote       start="\v^(( {1,3}|\t)?\>){2}"        end="$" oneline contains=pandocBulletListMarker,markdownBold,markdownItalic,pandocStrikethrough,markdownCode,markdownURI,pandocInlineMath,pandocDash,pandocSubscript,pandocSuperscript,markdownFootnoteRef,@Spell
" > > > triple quoted text
sy region markdownBlockQuoteQuotedQuote start="\v^(( {1,3}|\t)?\>){3}"        end="$" oneline contains=pandocBulletListMarker,markdownBold,markdownItalic,pandocStrikethrough,markdownCode,markdownURI,pandocInlineMath,pandocDash,pandocSubscript,pandocSuperscript,markdownFootnoteRef,@Spell

" NOTE: Single-backtick `markdownCode` *MUST* come *BEFORE* double backtick `markdownCode`.
" `int i = 0`
sy region markdownCode start="\v``@!"    end="\v`" skip="\v\\`" oneline keepend matchgroup=Comment contains=@NoSpell
" ``int i = 0``
sy region markdownCode start="\v`{2}`@!" end="``"  skip="\v\\`" oneline keepend matchgroup=Comment contains=@NoSpell

" ```
" code ...
" ```
" NOTE: the order here is crucial.
"
" 1. `markdownHighlight` below *MUST* come *AFTER* double backtick (``) and single backtic (`) inlines (see `markdownCode` above).
" 2. `markdownHighlight` below *MUST* come *AFTER* `pandocStrikethrough`.
" 3. The regular `markdownFenced` rule *MUST* come *BEFORE* `markdownHighlight`.
sy region markdownFenced start="\v^(\s*)@=\z(`{,3}|\~{3,}){3,}(\s*)@=" end="\v\z1" keepend contains=pandocAttr

" get all fenced languages in this file (faster than it looks)
let s:languages = systemlist("command grep -Eo '^\\s*(`{3,}|~{3,})\\s*(\\{\\s*\\.)?\\w+' < ".fnameescape(expand('%:p'))." | command sort | command uniq | command sed -E 's/\\s*(`{3,}|~{3,})\\s*(\\{\\s*\\.)?(\\w+).*/\\3/'")
" TODO load from all types of attr not only fenced code but also inline code

if v:shell_error ==# v:false
    for s:type in s:languages
        " necessary else the logic below won't source any syntax files
        unlet! b:current_syntax
        if filereadable($HOME.'/syntax/'.s:type.'.vim')
            exe 'sy include @markdownHighlight'.s:type.'2 '.$HOME.'/syntax/'.s:type.'.vim' 
        elseif filereadable($VIMRUNTIME.'/syntax/'.s:type.'.vim')
            exe 'sy include @markdownHighlight'.s:type.'1 '.$VIMRUNTIME.'/syntax/'.s:type.'.vim' 
        else
            continue
        endif
        " NOTE: don't change the order!
        " `ls -a`{.sh}
        exe 'sy region markdownHighlight'.s:type.' start="\v``@!"                                                                             end="\v`\s*\{\s*([^\}]+\s+)*\.'.s:type.'(\s+[^\}]+)*\s*\}" matchgroup=markdownCode contains=@markdownHighlight'.s:type.'1,@markdownHighlight'.s:type.'2,@NoSpell keepend oneline'
        " ```{.sh}
        " ls -a
        " ```
        " ~~~ {.sh}
        " ls -a
        " ~~~
        exe 'sy region markdownHighlight'.s:type.' start="\v^( {1,3}|\t)?\z(\~{3,}|`{3,})\s*\{\s*([^\}]+\s+)*\.'.s:type.'>[^\}\n\r]{-}\}\s*$" end="\v^( {1,3}|\t)?\z1\s*$"                               matchgroup=markdownCode contains=@markdownHighlight'.s:type.'1,@markdownHighlight'.s:type.'2,@NoSpell keepend'
        " ```sh
        " ls -a
        " ```
        exe 'sy region markdownHighlight'.s:type.' start="\v^( {1,3}|\t)?`{3,}\s*'.s:type.'\s*$"                                              end="\v^( {1,3}|\t)?`{3,}\s*$"                             matchgroup=markdownCode contains=@markdownHighlight'.s:type.'1,@markdownHighlight'.s:type.'2,@NoSpell keepend'
    endfor
endif

hi def link markdownAutoURI               String
hi def link markdownBlock                 SpecialComment
hi def link markdownBlockQuote            String
hi def link markdownBlockQuoteQuote       Macro
hi def link markdownBlockQuoteQuotedQuote Operator
hi def link markdownBlockquote            SpecialComment
hi def link markdownBoldDelimiter         Delimiter
hi def link markdownCode                  PreProc
hi def link markdownEscape                Error
hi def link markdownFenced                markdownCode                 
hi def link markdownFootnoteDefinition    URI                 
hi def link markdownFootnoteRef           URI
hi def link markdownHR                    Special
hi def link markdownInLineReferenceLabel  URI
hi def link markdownInLineReferenceURI    Comment
hi def link markdownItalicDelimiter       Delimiter
hi def link markdownLinkText              URI
hi def link markdownLiteralBlock          PreProc
hi def link markdownURI                   URI
hi def link markdownUrl                   String
hi def link markdownUrlTitle              String
hi def link pandocAttr                    SpecialChar
hi def link pandocAttrId                  Constant
hi def link pandocAttrClass               Constant
hi def link pandocAttrKV                  Constant
hi def link pandocBulletListMarker        Delimiter
hi def link pandocDash                    Character
hi def link pandocDefMarker               Special
hi def link pandocExampleListRef          Define
hi def link pandocFenced                  markdownFenced                 
hi def link pandocFencedDiv               SpecialComment
hi def link pandocLineBlockPipe           SpecialComment
" hi def link pandocInlineMath              Special
" hi def link pandocMultiLineMath           Special
hi def link pandocSimpleTable             SpecialComment
hi def link pandocSimpleTableError        pandocAmbiguous
hi def link pandocSimpleTableLine         Define
hi def link pandocStrikethrough           Comment
hi def link pandocSubscript               Character
hi def link pandocSuperscript             Character
hi def link pandocTitleBlock              PreProc
hi def link pandocYamlMetaBlock           PreProc

hi def markdownBold    gui=bold   cterm=bold   term=bold
hi def markdownItalic  gui=italic cterm=italic term=italic
hi def pandocAmbiguous ctermfg=Red  guibg=Red 

setl conceallevel=0
