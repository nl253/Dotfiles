# Setup fzf
# ---------
if [[ ! "$PATH" == *~/.applications/fzf/bin* ]]; then
  export PATH=$PATH:~/.applications/fzf/bin
fi

# Auto-completion
# ---------------
[[ $- == *i* ]] && source ~/.applications/fzf/shell/completion.bash 2> /dev/null

# Key bindings
# ------------
source ~/.applications/fzf/shell/key-bindings.bash

