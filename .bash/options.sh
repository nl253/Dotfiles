
#  SHELL OPTIONS 

stty -ixon    # enable inc search <C-s> which is often disabled by terminal emulators
stty -ctlecho # turn off control character echoing

shopt -s autocd
shopt -s cdspell      # correct minor spelling errors
shopt -s checkwinsize # update the value of LINES and COLUMNS after each command if altered
shopt -s direxpand    # replaces directory names with expansion when <tab>
shopt -s dirspell     # correct minor spelling errors
shopt -s dotglob      # Include dotfiles in pathname expansion
shopt -s checkjobs    # Bash lists the status of any stopped and running jobs before exiting an interactive shell. If any jobs are running, this causes the exit to be deferred until a second exit is attempted
shopt -s extglob      # Enable extended pattern-matching features
shopt -s nullglob
shopt -s nocaseglob # matches filenames in a case-insensitive fashion when performing pathname expansion.
shopt -s globstar   # ** becomes a recursive wildstar
shopt -s histappend # Append each session's history to $HISTFILE
shopt -s histverify # History expansions will be verified before execution.
shopt -s histreedit # Allow use to re-edit a faild history substitution.
