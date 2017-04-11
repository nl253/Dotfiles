# Setup fzf
# ---------
if [[ ! "$PATH" == */home/norbert/.fzf/bin* ]]; then
  export PATH="$PATH:/home/norbert/.fzf/bin"
fi

# Auto-completion
# ---------------
[[ $- == *i* ]] && source "/home/norbert/.fzf/shell/completion.bash" 2> /dev/null

# Key bindings
# ------------
source "/home/norbert/.fzf/shell/key-bindings.bash"

