
# SOURCE 

safe-source(){ # {{{
  for i in $@; do
    [[ -f $i ]] && source $i
  done
} # }}}

# Setup fzf
# ---------
# - Completion
# - Key Bindings
# ---------------
if [[ $- == *i* ]]; then 
  safe-source ~/.applications/fzf/shell/{completion,key-bindings}.zsh 2> /dev/null
fi

unset -f safe-source

# vim: foldmethod=marker
