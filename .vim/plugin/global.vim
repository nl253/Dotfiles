com!          Abbrs         sil 20vnew ~/.vim/plugin/lang.vim
com!          Scripts       sil e ~/.vim/plugin/global.vim
com!          WhiteSpace    %s/\v^\s+$| +$//
com!          Synonym       !wn <cword> -synsn
com! -nargs=0 ToDo          sil call splits#toggle_todo()
com!          BufferWipeout sil call bufs#buffer_wipeout()

setg rtp^=~/.vim/after winwidth=20 winminwidth=20 errorformat+=%f path+=~/.vim/plugin wildignore+=*.ipynb
setg errorfile=.errors.log makeef=.make-output.log

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
    " au BufDelete       ~/**/*        sil call system("rm -f ./NetrwTreeListin*")
    au BufEnter        term://*      setl modifiable nospell noreadonly | nn <buffer> <Leader>' :close<CR>
    au BufRead         /{etc,usr}/** setl nomodifiable readonly
    au BufReadPre      *.tex         let b:vimtex_main = 'main.tex'
    au FileType        *             sil call templates#read_template()
    au FileType        nerdtree      nm <buffer> h u
    au FileType        nerdtree      nm <buffer> l C
    au FileType        xml,html      im <buffer> <Tab> <plug>(emmet-expand-abbr)
    au Filetype        markdown      sil call inits#markdown_init()
    au Filetype        netrw         sil call inits#netrw_init()
    au QuickFixCmdPost caddexpr,cgetexpr,cexpr,grep,grepadd   exe 'botright cwindow '.((len(getqflist()[:10]) < 8) ? len(getqflist()) + 1 : "")
    au QuickFixCmdPost lexpr,lgrep,lgrepadd,lgetexpr,laddexpr exe 'botright lwindow '.((len(getloclist(win_getid())[:10]) < 8) ? len(getloclist(win_getid())) + 1 : "")
    au VimEnter        bash-fc.*     setf sh
    au WinEnter        *             setl winwidth=20
    exe "au FileType ".join(g:markup_langs + ['gitcommit'], ',')." sil WordyWordy"
aug END
