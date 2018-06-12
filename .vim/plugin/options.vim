let g:markdown_fenced_languages = g:prog_langs + ['java']
let g:rst_syntax_code_list = g:markdown_fenced_languages

au! BufWritePost,VimEnter,BufRead ~/**/* let b:git_status_summary = opts#git_status_summary()

exe 'setg statusline='.escape(' %-35.(%f #%n %q%r %w%m%) %=%-14.120(%(%<%{exists("b:git_status_summary") ? b:git_status_summary : ""} %{&tw} %{&wrap ? "wrap " : ""}%{&sw} %{&ts} %{&expandtab ? "expandtab " :""}%{&foldmethod == "marker" ? &foldmarker : &foldmethod}%) %(%y %p%% of %L%)%)     ', ' :",|')

setg tabline=%!opts#my_tabline()

"let s:base_tags = expand('<sfile>:p:h:h').'/tags/haskell'

aug MoreOptions
    au! 
    au Filetype haskell setl cinwords=where,let,in,do,of
    au Filetype * exe 'setl suffixesadd='.join(['erl', 'hs', 'py', 'md', 'ini', expand('%:e'), 'java', 'vim', 'rst', 'yaml', 'yml'], ',')
    "au Filetype haskell if !(&tags =~? s:base_tags) | exec 'setl tags+='.s:base_tags | endif
    "exe 'au BufRead '.fnamemodify(s:base_tags, ':h').'/base/**/*.hs setl readonly nomodifiable'
    "exe 'au BufRead '.fnamemodify(s:base_tags, ':h').'/base/**/*.hs ALEDisableBuffer'
aug END
