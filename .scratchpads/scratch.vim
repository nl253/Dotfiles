

let prompt = " >> "
let break = " "

inoremap <buffer> <expr> <CR> len(getline(".")) > 1 ? "\<C-R>=break\<CR>\<C-R>=system(substitute(getline('.'), prompt, '', ''))\<CR>\<CR>\<BS>\<C-R>=prompt\<CR>" : "\<CR>"


inoremap <buffer> <C-l> <C-o>:%d<CR>
