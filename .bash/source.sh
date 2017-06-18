
# SOURCE

safe-source(){ # {{{
  for i in $@; do
    [[ -f $i ]] && source $i
  done
} # }}}

# - fzf Auto-completion
# - fzf Key bindings
safe-source ~/.applications/fzf/shell/{completion,key-bindings}.bash 2> /dev/null

# execute only by my PC at home 
[[ -f ~/.pc ]] && safe-source ~/.pc.sh

# added by travis gem
safe-source /home/norbert/.travis/travis.sh
