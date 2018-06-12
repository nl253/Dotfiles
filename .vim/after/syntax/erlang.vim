hi link erlangVariable Constant
hi link erlangStringModifier SpecialChar

" EDoc
sy region erlangEDocMetaTag  start="@\v[a-z]{2,}" end="$" contained containedin=erlangComment keepend
hi link erlangEDocMetaTag erlangComment

sy match erlangEDocMetaTagMarker "@\v[a-z]{2,}" contained containedin=erlangEDocMetaTag,erlangEDocDescr
hi link erlangEDocMetaTagMarker Special

sy region erlangEDocParam  start="\v\@param " end="$" contained containedin=erlangComment
sy match erlangEDocParamVal  "\v<([A-Z][a-zA-Z]*)>" contained containedin=erlangEDocParam keepend
hi link erlangEDocParamVal Type
hi link erlangEDocParam erlangEDocMetaTagMarker

sy region erlangEDocDescr  start="\v\@doc " end="$" contained containedin=erlangComment keepend
hi link erlangEDocDescr Type

