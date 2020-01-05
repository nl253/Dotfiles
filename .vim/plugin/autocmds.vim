exe 'let $AUTOCMDS = '.string(expand('<sfile>'))

exe 'setg statusline='.escape(' %-36.(%f #%n %q%r %w%m%) %=%-14.120(%(%<%{exists("b:git_status_summary") ? b:git_status_summary : ""} %{&tw} %{&wrap ? "wrap " : ""}%{&sw} %{&ts} %{&expandtab ? "expandtab " :""}%{&foldmethod == "marker" ? &foldmarker : &foldmethod}%) %(%y %3p%% of %L%)%)     ', ' :",|')

let g:SECOND       = 1
let g:MINUTE       = 60 * g:SECOND
let g:MAX_AGE_TAGS = 10 * g:MINUTE

aug VariousAutoCmds
    au!
    au VimEnter bash-fc.* setf sh

    au BufWritePost,VimEnter,BufRead ~/* let b:git_status_summary = utils#git_status_summary()

    au FileType * call inits#all() | call templates#read_template()
    exe 'au FileType '.join(g:markup_langs, ',').' call inits#markup()'
    exe 'au FileType '.join(g:prog_langs,   ',').' call inits#programming()'
    au BufRead /{etc,usr,opt,sys,var,srv,run,proc}/* call inits#non_home()

    if has('nvim')
        au TermOpen * call inits#term() 
    endif

    " auto-generate tags if stale or not exist
    au DirChanged * if (fnamemodify(".", ":p") =~? $HOME) && (!filereadable(fnamemodify('tags', ':p')) || ((localtime() - getftime(fnamemodify("tags", ":p"))) >= g:MAX_AGE_TAGS)) | silent! call system('bash -c "ctags -R . &"') | endif

    au FileType xml,html,htmldjango call inits#emmet() | if line("$") <= 1000 | syntax sync fromstart | endif
    " au FileType c{pp,},python,sh,rust,{java,type}script,json,go call inits#lang_server()

    au Syntax              * if &completefunc == '' | setl completefunc=syntaxcomplete#Complete | endif

    " automatically change dir to the file you are editing
    au BufEnter ??* try | sil! lch %:p:h | cat /\vE(472|344|13)/ | endtry

    " automatically reload external changes NOTE: doesn't always work properly
    au CursorHold,BufEnter ~/* sil! checkt

    " autosave on focus lost
    au BufLeave,FocusLost ??* try | up | cat /\vE(472|344|13)/ | echo 'saved '.expand('%:p:h:t') | endt

    au WinEnter    ??* setl winwidth=20
    au CmdwinEnter *   setl updatetime=2000
    au CmdwinLeave *   setl updatetime=199

    " while grepping start from the project root
    au QuickFixCmdPre vim,lv,gr,lgr Root .git
    au QuickFixCmdPost vim,lv,gr,lgr try | sil! lch %:p:h | catch /\vE(472|344|13)/ | endtry

    " auto-display quickfix / location list window
    au QuickFixCmdPost c{ex,adde},grep{,a}* exe 'bo cw '.((len(getqflist()[:10]) < 8) ? len(getqflist()) + 1 : "")
    au QuickFixCmdPost l{ex,gr,grepa,gete,ad}* exe 'bo lw '.((len(getloclist(win_getid())[:10]) < 8) ? len(getloclist(win_getid())) + 1 : "")

    " au BufReadPost                   markdown                   if max(map(readfile(expand("%")), 'len(v:val)')) >= &textwidth | setl wrap | endif
    exe "au FileType ".join(g:markup_langs + ['gitcommit'], ',')." WordyWordy"
aug END
