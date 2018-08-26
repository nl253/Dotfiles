exe 'setg statusline='.escape(' %-35.(%f #%n %q%r %w%m%) %=%-14.120(%(%<%{exists("b:git_status_summary") ? b:git_status_summary : ""} %{&tw} %{&wrap ? "wrap " : ""}%{&sw} %{&ts} %{&expandtab ? "expandtab " :""}%{&foldmethod == "marker" ? &foldmarker : &foldmethod}%) %(%y %p%% of %L%)%)     ', ' :",|')

aug VariousAutoCmds
    au!


    au BufWritePost,VimEnter,BufRead ~/**/*                     let b:git_status_summary = opts#git_status_summary()
    au BufReadPre                    *.tex                      let b:vimtex_main = 'main.tex'
    au VimEnter                      bash-fc.*                  setf sh
    au WinEnter                      ??*                        setl winwidth=20

    au FileType                      *                          call templates#read_template()
    au BufReadPost                   *                          call inits#all()
    exe 'au FileType             '.join(g:markup_langs, ',').'  call inits#markup()'
    exe 'au FileType             '.join(g:prog_langs,   ',').'  call inits#programming()'
    au BufRead                       /{etc,usr,opt}/**          call inits#non_home()
    au BufEnter                      term://*                   call inits#term()
    au FileType                      xml,html                   call inits#emmet()
    au FileType                      c,cpp,python,typescript,javascript,sh,rust 
                                                              \ call inits#lang_server()

    " automatically change dir to the file you are editing
    au BufEnter                      ??*                        try | lchdir %:p:h | catch /\vE(472|344|13)/ | endtry

    " automatically reload external changes NOTE: doesn't always work properly
    au CursorHold,BufEnter           *                          silent! checktime

    " autosave on focus lost
    au BufLeave,FocusLost            ??*                        try | up | catch /\vE(472|344|13)/ | endtry

    au CmdwinEnter                   *                          setl updatetime=2000
    au CmdwinLeave                   *                          setl updatetime=199

    au QuickFixCmdPost               cadde,cex,grep,grepa     exe 'botright cwindow '.((len(getqflist()[:10]) < 8) ? len(getqflist()) + 1 : "")
    au QuickFixCmdPost               lex,lgr,lgrepa,lgete,lad exe 'botright lwindow '.((len(getloclist(win_getid())[:10]) < 8) ? len(getloclist(win_getid())) + 1 : "")

    exe "au FileType ".join(g:markup_langs + ['gitcommit'], ',')." WordyWordy"

    " save each session before quitting
    " au VimLeavePre * SaveSession
aug END
