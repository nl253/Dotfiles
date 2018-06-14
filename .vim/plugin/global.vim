com!          Abbrs         silent 20vnew ~/.vim/plugin/lang.vim
com!          Scripts       silent e ~/.vim/plugin/global.vim
com!          WhiteSpace    %s/\v^\s+$| +$//
com!          Synonym       !wn <cword> -synsn
com! -nargs=0 ToDo          silent call splits#toggle_todo()
com!          BufferWipeout silent call bufs#buffer_wipeout()

setg winwidth=20 winminwidth=20 errorformat+=%f path+=~/.vim/plugin wildignore+=*.ipynb

let g:template_vars = {
            \ 'author': systemlist("git config user.name")[0],
            \ 'year': strftime("%Y"),
            \ 'description': '',
            \ 'keywords': '',
            \ 'now': strftime("%c"),
            \ }

com! TemplateSubstitute exe "python3 import vim, pystache; vim.current.buffer[:] = list(map(lambda x: pystache.render(x, ".string(g:template_vars)."), vim.current.buffer[:]))"

aug VariousAutoCmds
    au!
    au BufRead,BufNew,BufNewFile *.puml setf plantuml
    au VimEnter        bash-fc.*     setf sh
    au BufRead         /{etc,usr}/** setl nomodifiable readonly
    au FileType        nerdtree      nmap <buffer> h u
    au FileType        nerdtree      nmap <buffer> l C
    au WinEnter        *             setl winwidth=20
    au Filetype        ejs           setl ft=ejs.javascript
    au BufDelete       ~/**/*        sil call system("rm -f ./NetrwTreeListin*")
    au FileType        *             sil call templates#read_template()
    au Filetype        netrw         sil call inits#netrw_init()
    au Filetype        markdown      sil call inits#markdown_init()
    au BufReadPre      *.tex         let b:vimtex_main = 'main.tex'
    au FileType        xml,html      imap <buffer> <Tab> <plug>(emmet-expand-abbr)
    au BufEnter        term://*      setl modifiable nospell noreadonly | nn <buffer> <Leader>' :close<CR>
    au QuickFixCmdPost lexpr,lgrep,lgrepadd,lgetexpr,laddexpr exe 'botright lwindow '.((len(getloclist(win_getid())[:10]) < 8) ? len(getloclist(win_getid())) + 1 : "")
    au QuickFixCmdPost caddexpr,cgetexpr,cexpr,grep,grepadd   exe 'botright cwindow '.((len(getqflist()[:10]) < 8) ? len(getqflist()) + 1 : "")
    exe "au FileType ".join(g:markup_langs + ['gitcommit'], ',')." silent WordyWordy"
aug END
