hi clear ConId
sy keyword haskellPreludeFunction abs acos acosh all and any appendFile asin asinh asTypeOf atan atan2 atanh break ceiling compare concat concatMap const cos cosh curry cycle decodeFloat div divMod drop dropWhile either elem encodeFloat enumFrom enumFromThen enumFromThenTo enumFromTo error errorWithoutStackTrace even exp exponent fail filter flip floatDigits floatRadix floatRange floor fmap foldl foldl1 foldMap foldr foldr1 fromEnum fromInteger fromIntegral fromRational fst gcd getChar getContents getLine head id init interact ioError isDenormalized isIEEE isInfinite isNaN isNegativeZero iterate last lcm length lex lines log logBase lookup map mapM mapM_ mappend max maxBound maximum maybe mconcat mempty min minBound minimum mod negate not notElem null odd or otherwise pi pred print product properFraction pure putChar putStr putStrLn quot quotRem read readFile readIO readList readLn readParen reads readsPrec realToFrac recip rem repeat replicate return reverse round scaleFloat scanl scanl1 scanr scanr1 seq sequence sequence_ sequenceA show showChar showList showParen shows showsPrec showString significand signum sin sinh snd span splitAt sqrt subtract succ sum tail take takeWhile tan tanh toEnum toInteger toRational traverse truncate uncurry undefined unlines until unwords unzip unzip3 userError words writeFile zip zip3 zipWith zipWith3 

sy keyword haskellTODO     TODO FIXME   contained containedin=@haddockCommentAll
sy match haddockReplMarker ">>>"        contained containedin=haddockRepl
sy match hsDashIgnore      "--"         contained containedin=haddockDocMultiLine
sy match haddockURIInner   "\v[^\<\>]*" contained containedin=haddockURI

sy region haddockURI  start="<"         end=">" contained containedin=@haddockCommentAll oneline concealends 
sy region haddockRepl start="\v \>{3} " end="$" contained containedin=@haddockCommentAll contains=@haddockCodeAll oneline 
sy region haddockProp start=" prop>"    end="$" contained containedin=@haddockCommentAll

sy region haddockCodeLine start="\v^(-- )?\s*\> " end="$" contained containedin=@haddockCommentAll contains=@haddockCodeAll oneline 

sy region haddockBulletPoint start="\v^\s*(-- )?\s*(\d\.|\*|\(\d\)) " end="$" contained containedin=haddockDocMultiLine,haddockDocLine,hsLineComment contains=@haddockInlineAll oneline 

for i in range(1, 6)
	exe 'sy region haddockH'.i.' start="\v(\s*-- )\V'.repeat("=", i).' " end="$" containedin=haddockDocMultiLine,haddockDocLine,hsLineComment oneline contained contains=@haddocInlineMinimal'
	exe 'hi def link haddockH'.i.' HtmlH'.i
endfor

sy match  haddocHR                "\v^\s*\~{5,}\s*$"      contained containedin=@haddockCommentAll
sy match  haddockDollarWord       "\v( )@<=(\$[A-Za-z]+)" contained containedin=@haddockCommentAll
sy match  haddockMonospacedMarker "\v\@"                  contained containedin=haddockMonospaced
sy match  haddockAnchor           "\v\w+#\w+|#\w+#"       contained containedin=@haddockCommentAll contained 
sy match  haddockChar             "&#\v\S+"               contained containedin=@haddockCommentAll
sy match  haddockSemVer           "\v\d+(\.\d+)*"         contained containedin=haddockSinceTag

sy region haddocQuoted            start="\v'<"                end="\v>'"  contained containedin=hsComment,hsBlockComment,hsLineComment,haddockDocLine,haddockDocMultiline oneline 
sy region haddockDefinitionMarker start="\v\["                end="\]"    contained containedin=haddockDocMultiLine,haddockDocLine,hsLineComment,hsBlockComment contains=@haddocInlineMinimal concealends 
sy region haddockEmpf             start="/\v<"                end="\v>/"  contained containedin=@haddockCommentAll contains=@haddocInlineMinimal oneline 
sy region haddockMonospaced       start="@"                   end="@"     contained containedin=@haddockCommentAll contains=@haddockCodeAll      oneline 
sy region haddocNote              start="\v(See )?Note:? ?\[" end="]"     contained containedin=@haddockCommentAll
sy region haddockBold             start="__"                  end="__"    contained containedin=@haddockCommentAll contains=haddockEmpf,haddockMonospaced oneline 
sy region haddockSinceTag         start="@since\v ?"          end="\v |$" contained containedin=@haddockCommentAll contains=haddockSemVer  

sy cluster haddockCodeAll       contains=hsVarSym,hsBoolean,hsDelimiter,ConId,haskellPreludeFunction,hsStructure
sy cluster haddockCommentAll    contains=haddockDocMultiLine,hsLineComment,hsBlockComment
sy cluster haddockInlineAll     contains=haddockMonospaced,haddockEmpf,haddockBold,haddockRepl,haddockProp,haddockURI,haddockAnchor,haddockChar,hsDashIgnore
sy cluster haddockInlineMinimal contains=haddockMonospaced,haddockEmpf,haddockBold,haddockURI,haddockAnchor

hi haddockEmpf gui=italic cterm=italic term=italic
hi haddockBold gui=bold   cterm=bold   term=bold
hi def link ConId                   Type
hi def link Typedef                 TypeDef
hi def link haddocHR                Special
hi def link haddocNote              Function
hi def link haddocQuoted            Macro
hi def link haddockAnchor           URI
hi def link haddockBulletPoint      Delimiter
hi def link haddockChar             Character
hi def link haddockCodeLine         Normal
hi def link haddockDefinitionMarker Delimiter
hi def link haddockDollarWord       haddockBold
hi def link haddockMonospaced       Normal
hi def link haddockMonospacedMarker Comment
hi def link haddockProp             PreCondit
hi def link haddockReplMarker       Delimiter
hi def link haddockSemVer           Number
hi def link haddockSinceTag         Macro
hi def link haddockURI              Comment
hi def link haddockURI              URI
hi def link haddockURIInner         URI
hi def link haskellConditional      Statement
hi def link haskellDecl             TypeDef
hi def link haskellDeclKeyword      TypeDef
hi def link haskellKeyword          Keyword
hi def link haskellLet              TypeDef
hi def link haskellPreludeFunction  Builtin
hi def link haskellTODO             Exception
hi def link haskellWhere            Statement
hi def link hsDashIgnore            Comment
hi def link hsStructure             Statement
" vim:foldmethod=indent:
