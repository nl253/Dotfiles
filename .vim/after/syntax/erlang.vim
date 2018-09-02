" EDoc
sy region erlangEDocMetaTag  start="@\v[a-z]{2,}" end="$" contained containedin=erlangComment keepend
sy region erlangEDocParam    start="\v\@param "   end="$" contained containedin=erlangComment
sy region erlangEDocDescr    start="\v\@doc "     end="$" contained containedin=erlangComment keepend
sy match erlangEDocParamVal      "\v<([A-Z][a-zA-Z]*)>" contained containedin=erlangEDocParam keepend
sy match erlangEDocMetaTagMarker "@\v[a-z]{2,}"         contained containedin=erlangEDocMetaTag,erlangEDocDescr

hi link erlangVariable          Constant
hi link erlangStringModifier    SpecialChar
hi link erlangAtom              Symbol
hi link erlangBracket           NONE

hi def link erlangEDocDescr         Type
hi def link erlangEDocParamVal      Type
hi def link erlangEDocMetaTagMarker Special
hi def link erlangEDocParam         erlangEDocMetaTagMarker
hi def link erlangEDocMetaTag       erlangComment
