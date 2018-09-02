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

hi link coqStmt    Statement
hi link coqKeyword Keyword
hi link coqBuiltin Builtin
hi link coqType    Type
hi link coqOp      Operator
hi link coqDelim   Delimiter
hi link coqComment Comment
hi link coqNum     Number
