if exists('b:current_syntax') 
    if b:current_syntax ==# 'coq'
        finish
    endif
else
    sy clear
    let b:current_syntax = 'coq' 
endif

sy keyword coqStmt    Definition Eval Print Fixpoint Theorem Proof Set Unset Strict Implicit Require Import Arguments
sy keyword coqType    nat Prop Type
sy keyword coqBuiltin intros induction simpl reflexivity rewrite exact
sy keyword coqKeyword match in with end forall Qed
sy match   coqNum   "\v[0-9]+(\.[0-9]+)?"
sy match   coqDelim '\v\.($| )'
sy match   coqDelim '\v,($| )'
sy match   coqDelim ')'
sy match   coqDelim '('
sy match   coqOp '\v(<| |^)-\>(>| |$)'
sy match   coqOp '\v( |<|>|^)\=\>( |>|<)'
sy match   coqOp '\v^\s*\|( |<)'
sy match   coqOp '\v(<|>| ):\=?( |$|<|>)'
sy region  coqComment start='(\*' end='\*)'

hi def link coqStmt    Statement
hi def link coqKeyword Keyword
hi def link coqBuiltin Builtin
hi def link coqType    Type
hi def link coqOp      Operator
hi def link coqDelim   Delimiter
hi def link coqComment Comment
hi def link coqNum     Number
