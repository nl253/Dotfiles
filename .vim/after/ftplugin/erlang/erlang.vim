let g:erlang_highlight_special_atoms = 1

setl shiftwidth=4 tabstop=8 foldmethod=indent

if executable("erl")
    compiler erlang
endif

if executable("escript")
    setl makeprg=escript\ %
endif
