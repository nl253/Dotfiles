if exists('b:current_syntax') 
    if b:current_syntax ==# 'rust'
        finish
    endif
else
    let b:current_syntax = 'rust' 
endif

sy sync minlines=200
sy sync maxlines=500

" Syntax definitions {{{1
" Basic keywords {{{2
sy keyword rustConditional match if else
sy keyword rustRepeat      for loop while
sy keyword rustTypedef     type nextgroup=rustIdentifier skipwhite skipempty
sy keyword rustStructure   struct enum nextgroup=rustIdentifier skipwhite skipempty
sy keyword rustUnion       union nextgroup=rustIdentifier skipwhite skipempty contained
sy match   rustUnionContextual /\<union\_s\+\%([^[:cntrl:][:space:][:punct:][:digit:]]\|_\)\%([^[:cntrl:][:punct:][:space:]]\|_\)*/ transparent contains=rustUnion
sy keyword rustOperator    as

sy match   rustAssert  "\<assert\(\w\)*!" contained
sy match   rustPanic   "\<panic\(\w\)*!" contained
sy keyword rustKeyword break
sy keyword rustKeyword box nextgroup=rustBoxPlacement skipwhite skipempty
sy keyword rustKeyword continue
sy keyword rustKeyword extern nextgroup=rustExternCrate,rustObsoleteExternMod skipwhite skipempty
sy keyword rustKeyword fn nextgroup=rustFuncName skipwhite skipempty
sy keyword rustKeyword in impl let
sy keyword rustKeyword pub nextgroup=rustPubScope skipwhite skipempty
sy keyword rustKeyword return
sy keyword rustSuper   super
sy keyword rustKeyword unsafe where
sy keyword rustKeyword use nextgroup=rustModPath skipwhite skipempty
" FIXME: Scoped impl's name is also fallen in this category
sy keyword rustKeyword mod trait nextgroup=rustIdentifier skipwhite skipempty
sy keyword rustStorage move mut ref static const
sy match   rustDefault /\<default\ze\_s\+\(impl\|fn\|type\|const\)\>/

sy keyword   rustInvalidBareKeyword crate

sy keyword rustPubScopeCrate crate contained
sy match rustPubScopeDelim /[()]/ contained
sy match rustPubScope /([^()]*)/ contained contains=rustPubScopeDelim,rustPubScopeCrate,rustSuper,rustModPath,rustModPathSep,rustSelf transparent

sy keyword rustExternCrate crate contained nextgroup=rustIdentifier,rustExternCrateString skipwhite skipempty
" This is to get the `bar` part of `extern crate "foo" as bar;` highlighting.
sy match   rustExternCrateString /".*"\_s*as/ contained nextgroup=rustIdentifier skipwhite transparent skipempty contains=rustString,rustOperator
sy keyword rustObsoleteExternMod mod contained nextgroup=rustIdentifier skipwhite skipempty

sy match  rustIdentifier  contains=rustIdentifierPrime "\%([^[:cntrl:][:space:][:punct:][:digit:]]\|_\)\%([^[:cntrl:][:punct:][:space:]]\|_\)*" display contained
sy match  rustFuncName    "\%([^[:cntrl:][:space:][:punct:][:digit:]]\|_\)\%([^[:cntrl:][:punct:][:space:]]\|_\)*" display contained

sy region rustBoxPlacement matchgroup=rustBoxPlacementParens start="(" end=")" contains=TOP contained
" Ideally we'd have syntax rules set up to match arbitrary expressions. Since
" we don't, we'll just define temporary contained rules to handle balancing
" delimiters.
sy region rustBoxPlacementBalance start="(" end=")" containedin=rustBoxPlacement transparent
sy region rustBoxPlacementBalance start="\[" end="\]" containedin=rustBoxPlacement transparent
" {} are handled by rustFoldBraces

sy region rustMacroRepeat matchgroup=rustMacroRepeatDelimiters start="$(" end=")" contains=TOP nextgroup=rustMacroRepeatCount
sy match rustMacroRepeatCount ".\?[*+]" contained
sy match rustMacroVariable "$\w\+"

" Reserved (but not yet used) keywords {{{2
sy keyword rustReservedKeyword alignof become do offsetof priv pure sizeof typeof unsized yield abstract virtual final override macro

" Built-in types {{{2
sy keyword rustType isize usize char bool u8 u16 u32 u64 u128 f32
sy keyword rustType f64 i8 i16 i32 i64 i128 str Self

" Things from the libstd v1 prelude (src/libstd/prelude/v1.rs) {{{2
" This section is just straight transformation of the contents of the prelude,
" to make it easy to update.

" Reexported core operators {{{3
sy keyword rustTrait       Copy Send Sized Sync
sy keyword rustTrait       Drop Fn FnMut FnOnce

" Reexported functions {{{3
" There’s no point in highlighting these; when one writes drop( or drop::< it
" gets the same highlighting anyway, and if someone writes `let drop = …;` we
" don’t really want *that* drop to be highlighted.
"sy keyword rustFunction drop

" Reexported types and traits {{{3
sy keyword rustTrait       Box
sy keyword rustTrait       ToOwned
sy keyword rustTrait       Clone
sy keyword rustTrait       PartialEq PartialOrd Eq Ord
sy keyword rustTrait       AsRef AsMut Into From
sy keyword rustTrait       Default
sy keyword rustTrait       Iterator Extend IntoIterator
sy keyword rustTrait       DoubleEndedIterator ExactSizeIterator
sy keyword rustEnum        Option
sy keyword rustEnumVariant Some None
sy keyword rustEnum        Result
sy keyword rustEnumVariant Ok Err
sy keyword rustTrait       SliceConcatExt
sy keyword rustTrait       String ToString
sy keyword rustTrait       Vec

" Other syntax {{{2
sy keyword   rustSelf        self
sy keyword   rustBoolean     true false

" If foo::bar changes to foo.bar, change this ("::" to "\.").
" If foo::bar changes to Foo::bar, change this (first "\w" to "\u").
sy match     rustModPath     "\w\(\w\)*::[^<]"he=e-3,me=e-3
sy match     rustModPathSep  "::"

sy match     rustFuncCall    "\w\(\w\)*("he=e-1,me=e-1
sy match     rustFuncCall    "\w\(\w\)*::<"he=e-3,me=e-3 " foo::<T>();

" This is merely a convention; note also the use of [A-Z], restricting it to
" latin identifiers rather than the full Unicode uppercase. I have not used
" [:upper:] as it depends upon 'noignorecase'
"sy match     rustCapsIdent    display "[A-Z]\w\(\w\)*"

sy match rustOperator     display "\%(+\|-\|/\|*\|=\|\^\|&\||\|!\|>\|<\|%\)=\?"
" This one isn't *quite* right, as we could have binary-& with a reference
sy match rustSigil        display /&\s\+[&~@*][^)= \t\r\n]/he=e-1,me=e-1
sy match rustSigil        display /[&~@*][^)= \t\r\n]/he=e-1,me=e-1
" This isn't actually correct; a closure with no arguments can be `|| { }`.
" Last, because the & in && isn't a sigil
sy match rustOperator     display "&&\|||"
" This is rustArrowCharacter rather than rustArrow for the sake of matchparen,
" so it skips the ->; see http://stackoverflow.com/a/30309949 for details.
sy match rustArrowCharacter display "->"
sy match rustQuestionMark display "?\([a-zA-Z]\+\)\@!"

sy match rustMacro       '\w\(\w\)*!' contains=rustAssert,rustPanic
sy match rustMacro       '#\w\(\w\)*' contains=rustAssert,rustPanic

sy match  rustEscapeError   display contained /\\./
sy match  rustEscape        display contained /\\\([nrt0\\'"]\|x\x\{2}\)/
sy match  rustEscapeUnicode display contained /\\u{\x\{1,6}}/
sy match  rustStringContinuation display contained /\\\n\s*/
sy region rustString      start=+b"+ skip=+\\\\\|\\"+ end=+"+ contains=rustEscape,rustEscapeError,rustStringContinuation
sy region rustString      start=+"+ skip=+\\\\\|\\"+ end=+"+ contains=rustEscape,rustEscapeUnicode,rustEscapeError,rustStringContinuation,@Spell
sy region rustString      start='b\?r\z(#*\)"' end='"\z1' contains=@Spell

sy region rustAttribute   start="#!\?\[" end="\]" contains=rustString,rustDerive,rustCommentLine,rustCommentBlock,rustCommentLineDocError,rustCommentBlockDocError
sy region rustDerive      start="derive(" end=")" contained contains=rustDeriveTrait
" This list comes from src/libsyntax/ext/deriving/mod.rs
" Some are deprecated (Encodable, Decodable) or to be removed after a new snapshot (Show).
sy keyword rustDeriveTrait contained Clone Hash RustcEncodable RustcDecodable Encodable Decodable PartialEq Eq PartialOrd Ord Rand Show Debug Default FromPrimitive Send Sync Copy

" Number literals
sy match rustDecNumber   display "\<[0-9][0-9_]*\%([iu]\%(size\|8\|16\|32\|64\|128\)\)\="
sy match rustHexNumber   display "\<0x[a-fA-F0-9_]\+\%([iu]\%(size\|8\|16\|32\|64\|128\)\)\="
sy match rustOctNumber   display "\<0o[0-7_]\+\%([iu]\%(size\|8\|16\|32\|64\|128\)\)\="
sy match rustBinNumber   display "\<0b[01_]\+\%([iu]\%(size\|8\|16\|32\|64\|128\)\)\="

" Special case for numbers of the form "1." which are float literals, unless followed by
" an identifier, which makes them integer literals with a method call or field access,
" or by another ".", which makes them integer literals followed by the ".." token.
" (This must go first so the others take precedence.)
sy match rustFloat       display "\<[0-9][0-9_]*\.\%([^[:cntrl:][:space:][:punct:][:digit:]]\|_\|\.\)\@!"
" To mark a number as a normal float, it must have at least one of the three things integral values don't have:
" a decimal point and more numbers; an exponent; and a type suffix.
sy match rustFloat       display "\<[0-9][0-9_]*\%(\.[0-9][0-9_]*\)\%([eE][+-]\=[0-9_]\+\)\=\(f32\|f64\)\="
sy match rustFloat       display "\<[0-9][0-9_]*\%(\.[0-9][0-9_]*\)\=\%([eE][+-]\=[0-9_]\+\)\(f32\|f64\)\="
sy match rustFloat       display "\<[0-9][0-9_]*\%(\.[0-9][0-9_]*\)\=\%([eE][+-]\=[0-9_]\+\)\=\(f32\|f64\)"

" For the benefit of delimitMate
sy region rustLifetimeCandidate display start=/&'\%(\([^'\\]\|\\\(['nrt0\\\"]\|x\x\{2}\|u{\x\{1,6}}\)\)'\)\@!/ end=/[[:cntrl:][:space:][:punct:]]\@=\|$/ contains=rustSigil,rustLifetime
sy region rustGenericRegion display start=/<\%('\|[^[cntrl:][:space:][:punct:]]\)\@=')\S\@=/ end=/>/ contains=rustGenericLifetimeCandidate
sy region rustGenericLifetimeCandidate display start=/\%(<\|,\s*\)\@<='/ end=/[[:cntrl:][:space:][:punct:]]\@=\|$/ contains=rustSigil,rustLifetime

"rustLifetime must appear before rustCharacter, or chars will get the lifetime highlighting
sy match rustLifetime    display "\'\%([^[:cntrl:][:space:][:punct:][:digit:]]\|_\)\%([^[:cntrl:][:punct:][:space:]]\|_\)*"
sy match rustLabel       display "\'\%([^[:cntrl:][:space:][:punct:][:digit:]]\|_\)\%([^[:cntrl:][:punct:][:space:]]\|_\)*:"
sy match rustCharacterInvalid   display contained /b\?'\zs[\n\r\t']\ze'/
" The groups negated here add up to 0-255 but nothing else (they do not seem to go beyond ASCII).
sy match rustCharacterInvalidUnicode   display contained /b'\zs[^[:cntrl:][:graph:][:alnum:][:space:]]\ze'/
sy match rustCharacter   /b'\([^\\]\|\\\(.\|x\x\{2}\)\)'/ contains=rustEscape,rustEscapeError,rustCharacterInvalid,rustCharacterInvalidUnicode
sy match rustCharacter   /'\([^\\]\|\\\(.\|x\x\{2}\|u{\x\{1,6}}\)\)'/ contains=rustEscape,rustEscapeUnicode,rustEscapeError,rustCharacterInvalid

sy match rustShebang /\%^#![^[].*/
sy region rustCommentLine                                                  start="//"                      end="$"   contains=rustTodo,@Spell
sy region rustCommentLineDoc                                               start="//\%(//\@!\|!\)"         end="$"   contains=rustTodo,@Spell
sy region rustCommentLineDocError                                          start="//\%(//\@!\|!\)"         end="$"   contains=rustTodo,@Spell contained
sy region rustCommentBlock             matchgroup=rustCommentBlock         start="/\*\%(!\|\*[*/]\@!\)\@!" end="\*/" contains=rustTodo,rustCommentBlockNest,@Spell
sy region rustCommentBlockDoc          matchgroup=rustCommentBlockDoc      start="/\*\%(!\|\*[*/]\@!\)"    end="\*/" contains=rustTodo,rustCommentBlockDocNest,@Spell
sy region rustCommentBlockDocError     matchgroup=rustCommentBlockDocError start="/\*\%(!\|\*[*/]\@!\)"    end="\*/" contains=rustTodo,rustCommentBlockDocNestError,@Spell contained
sy region rustCommentBlockNest         matchgroup=rustCommentBlock         start="/\*"                     end="\*/" contains=rustTodo,rustCommentBlockNest,@Spell contained transparent
sy region rustCommentBlockDocNest      matchgroup=rustCommentBlockDoc      start="/\*"                     end="\*/" contains=rustTodo,rustCommentBlockDocNest,@Spell contained transparent
sy region rustCommentBlockDocNestError matchgroup=rustCommentBlockDocError start="/\*"                     end="\*/" contains=rustTodo,rustCommentBlockDocNestError,@Spell contained transparent

" FIXME: this is a really ugly and not fully correct implementation. Most
" importantly, a case like ``/* */*`` should have the final ``*`` not being in
" a comment, but in practice at present it leaves comments open two levels
" deep. But as long as you stay away from that particular case, I *believe*
" the highlighting is correct. Due to the way Vim's syntax engine works
" (greedy for start matches, unlike Rust's tokeniser which is searching for
" the earliest-starting match, start or end), I believe this cannot be solved.
" Oh you who would fix it, don't bother with things like duplicating the Block
" rules and putting ``\*\@<!`` at the start of them; it makes it worse, as
" then you must deal with cases like ``/*/**/*/``. And don't try making it
" worse with ``\%(/\@<!\*\)\@<!``, either...

sy keyword rustTodo contained TODO FIXME XXX NB NOTE

" Folding rules {{{2
" Trivial folding rules to begin with.
" FIXME: use the AST to make really good folding
sy region rustFoldBraces start="{" end="}" transparent fold

" Default highlighting {{{1
hi def link rustArrowCharacter          rustOperator                                                                        
hi def link rustAssert                  PreCondit                                                                           
hi def link rustAttribute               PreProc                                                                             
hi def link rustBinNumber               rustNumber                                                                          
hi def link rustBoolean                 Boolean                                                                             
hi def link rustBoxPlacementParens      Delimiter                                                                           
hi def link rustCapsIdent               rustIdentifier                                                                      
hi def link rustCharacter               Character                                                                           
hi def link rustCharacterInvalid        Error                                                                               
hi def link rustCharacterInvalidUnicode rustCharacterInvalid                                                                
hi def link rustCollection              rustType                                                                            
hi def link rustCommentBlock            rustCommentLine                                                                     
hi def link rustCommentBlockDoc         rustCommentLineDoc                                                                  
hi def link rustCommentBlockDocError    Error                                                                               
hi def link rustCommentLine             Comment                                                                             
hi def link rustCommentLineDoc          Comment                                                                             
hi def link rustCommentLineDoc          SpecialComment                                                                      
hi def link rustCommentLineDocError     Error                                                                               
hi def link rustConditional             Conditional                                                                         
hi def link rustConstant                Constant                                                                            
hi def link rustDecNumber               rustNumber                                                                          
hi def link rustDefault                 StorageClass                                                                        
hi def link rustDerive                  PreProc                                                                             
hi def link rustDeriveTrait             rustTrait                                                                           
hi def link rustEnum                    rustType                                                                            
hi def link rustEnumVariant             rustConstant                                                                        
hi def link rustEscape                  Special                                                                             
hi def link rustEscapeError             Error                                                                               
hi def link rustEscapeUnicode           rustEscape                                                                          
hi def link rustExternCrate             rustKeyword                                                                         
hi def link rustFloat                   Float                                                                               
hi def link rustFuncCall                Function                                                                            
hi def link rustFuncName                Function                                                                            
hi def link rustFunction                Function                                                                            
hi def link rustHexNumber               rustNumber                                                                          
hi def link rustIdentifier              Identifier                                                                          
hi def link rustIdentifierPrime         rustIdentifier                                                                      
hi def link rustInvalidBareKeyword      Error                                                                               
hi def link rustKeyword                 Keyword                                                                             
hi def link rustLabel                   Label                                                                               
hi def link rustLifetime                Special                                                                             
hi def link rustMacro                   Macro                                                                               
hi def link rustMacroRepeatCount        rustMacroRepeatDelimiters                                                           
hi def link rustMacroRepeatDelimiters   Macro                                                                               
hi def link rustMacroVariable           Define                                                                              
hi def link rustModPath                 Include                                                                             
hi def link rustModPathSep              Comment                                                                             
hi def link rustModPathSep              Delimiter                                                                           
hi def link rustNumber                  Number                                                                              
hi def link rustObsoleteExternMod       Error                                                                               
hi def link rustObsoleteStorage         Error                                                                               
hi def link rustOctNumber               rustNumber                                                                          
hi def link rustOperator                Operator                                                                            
hi def link rustPanic                   PreCondit                                                                           
hi def link rustPubScopeCrate           rustKeyword                                                                         
hi def link rustPubScopeDelim           Delimiter                                                                           
hi def link rustPunct                   Operator                                                                             
hi def link rustQuestionMark            Special                                                                             
hi def link rustRepeat                  Conditional                                                                         
hi def link rustReservedKeyword         Error                                                                               
hi def link rustSelf                    Constant                                                                            
hi def link rustShebang                 Comment                                                                             
hi def link rustSigil                   StorageClass                                                                        
hi def link rustStorage                 StorageClass                                                                        
hi def link rustString                  String                                                                              
hi def link rustStringContinuation      Special                                                                             
" More precise is Structure                               
hi def link rustStructure               Keyword                   
hi def link rustSuper                   rustKeyword                                                                         
hi def link rustTodo                    Todo                                                                                
hi def link rustTrait                   rustType                                                                            
hi def link rustType                    Type                                                                                
" More precise is Typedef,  but it doesn't feel right for Rust
hi def link rustTypedef                 Keyword                   
hi def link rustUnion                   rustStructure                                                                       
" Other Suggestions:
" hi rustAttribute ctermfg=cyan
" hi rustDerive ctermfg=cyan
" hi rustAssert ctermfg=yellow
" hi rustPanic ctermfg=red
" hi rustMacro ctermfg=magenta

sy keyword rustCollection HashSet HashMap VecDeque LinkedList BinaryHeap BTreeMap BTreeSet
sy match rustPunct "\v[;,:\.]"
" sy match rustType "\v<([A-Z][a-z]{2,})+>" 
