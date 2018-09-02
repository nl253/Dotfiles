hi clear manSectionHeading 

hi def link manLongOption     SpecialKey
hi def link manOptionalOpt    Builtin
hi def link manRequiredOpt    Conditional
hi def link manQuoted         Macro
hi def link manSectionHeading Special
hi def link manShellVar       PreProc
hi def link manShortOption    Conditional
hi def link manURL            URI
hi def link manUnixPath       Directory
hi def link manBulletPoint    Delimiter

sy match manEscapedChar "\v(<|\s+)\\\S(>|\s+)"
sy match manBulletPoint "\v^[ \t]*\(?([1-9a-z]\)|·|o|\*|[1-9a-z]\.) "
sy match manLongOption  "\v--[a-z]+(-[a-z]+)*(\=[a-z]+(-[a-z]+)*)?"
sy match manPreProcInst "\v#(pragma|include|define)"
sy match manOptionalOpt "\v\[.{-1,40}\]"
sy match manQuoted      "\v`.{-,80}'"
sy match manRequiredOpt "\v\<[^\<]{2,30}\>"
sy match manShellVar    "\C\v\$(\{\S{3,30}\}|[A-Z][_A-Z]+|[a-z][_a-z]+)"
sy match manShortOption "\v(<|\s+)(-|−)[a-zA-Z](>|\s+)"
sy match manURL         "\v<(https?|s?ftp|ssh|rsync)://\S+>"
sy match manUnixPath    /\k\@<![\/~]\S\+\(\/\|[^ [:punct:]]\)/

sy region manQuoted start="'\v\w{3,}" end="'"   oneline 
sy region manQuoted start="``"        end="''"  oneline 
sy region manQuoted start="“"         end="”"   oneline 
sy region manQuoted start='"'         end='"'   oneline 

hi def link manShellCodeLn PreProc
hi def link manPreProcInst PreProc
hi def link manEscapedChar Statement
