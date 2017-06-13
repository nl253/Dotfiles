# Setup fzf
# ---------
if [[ ! "$PATH" == */home/norbert/.fzf/bin* ]]; then
  export PATH=$PATH:~/.applications/fzf/bin
fi

# Auto-completion
# ---------------
[[ $- == *i* ]] && source /.applications/fzf/shell/completion.zsh 2> /dev/null

# Key bindings
# ------------
source ~/.applications/fzf/shell/key-bindings.zsh

