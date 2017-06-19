
# SOURCE 

safe-source(){ # {{{
  for i in $@; do
    [[ -f $i ]] && source $i
  done
} # }}}

# FZF Completion and Key Bindings
safe-source ~/.applications/fzf/shell/{completion,key-bindings}.zsh 2> /dev/null

# added by travis gem
safe-source /home/norbert/.travis/travis.sh

# CONFIG TO BE RUN ONLY FOR MY PC 
[[ -f ~/.pc ]] && safe-source ~/.pc.zsh

unset -f safe-source

# vim: foldmethod=marker
