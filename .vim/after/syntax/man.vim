hi clear manSectionHeading 

sy match manEscapedChar "\v(<|\s+)\\\S(>|\s+)"
sy match manBulletPoint "\v^[ \t]*\(?([1-9a-z]\)|·|o|\*|[1-9a-z]\.) "
sy match manLongOption  "\v--[a-z]+(-[a-z]+)*(\=[a-z]+(-[a-z]+)*)?"
sy match manPreProcInst "\v#(pragma|include|define)"
sy match manOptionalOpt "\v\[.{-1,40}\]"
sy match manRequiredOpt "\v\<[^\<]{2,30}\>"
sy match manShellVar    "\C\v\$(\{\S{3,30}\}|[A-Z][_A-Z]+|[a-z][_a-z]+)"
sy match manShortOption "\v(<|\s+)(-|−)[a-zA-Z](>|\s+)"
sy match manURL         "\v<(https?|s?ftp|ssh|rsync)://\S+>"
sy match manUnixPath    /\k\@<![\/~]\S\+\(\/\|[^ [:punct:]]\)/

sy region manQuoted start="'\v\w{3,}" end="'"   oneline 
sy region manQuoted start="``"        end="''"  oneline 
sy region manQuoted start="“"         end="”"   oneline 
sy region manQuoted start='"'         end='"'   oneline 
sy region manQuoted start="`"         end="`"   oneline 

hi link manShellCodeLn    PreProc
hi link manPreProcInst    PreProc
hi link manEscapedChar    Statement
hi link manLongOption     SpecialKey
hi link manOptionalOpt    Builtin
hi link manRequiredOpt    Conditional
hi link manQuoted         Macro
hi link manSectionHeading Special
hi link manShellVar       PreProc
hi link manShortOption    Conditional
hi link manURL            URI
hi link manUnixPath       Directory
hi link manBulletPoint    Delimiter
