# Setup fzf
# ---------
if [[ ! "$PATH" == */home/norbert/.fzf/bin* ]]; then
  [[ -d ~/.fzf/bin ]] && export PATH=$PATH:~/.fzf/bin
fi

# Auto-completion
# ---------------
[[ $- == *i* ]] && [[ -f ~/.fzf/shell/completion.bash ]] && source ~/.fzf/shell/completion.bash 2> /dev/null

# Key bindings
# ------------
[[ -f ~/.fzf/shell/key-bindings.bash ]] && source ~/.fzf/shell/key-bindings.bash

