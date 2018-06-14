setl sw=4 ts=8 expandtab makeprg=dot\ -Tpng\ %:p\ -o%:p:r.png
nn <buffer> <M-p> :!feh %:p:r.png<CR>

