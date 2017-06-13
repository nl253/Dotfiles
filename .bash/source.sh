
# SOURCE

safe-source(){ # {{{
  for i in $@; do
    [[ -f $i ]] && source $i
  done
} # }}}

# Setup fzf {{{
# ---------
# - Auto-completion
# - Key bindings
[[ $- == *i* ]] && safe-source ~/.applications/fzf/shell/{completion,key-bindings}.bash 2> /dev/null
# }}}

