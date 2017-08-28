
#  BASH SHOPTIONS

# enable inc search <C-s> which is often disabled by terminal emulators
stty -ixon

# turn off control character echoing
stty -ctlecho

# update the value of LINES and COLUMNS after each command if altered
shopt -s checkwinsize

# replaces directory names with expansion when <tab>
shopt -s direxpand

# correct minor spelling errors
shopt -s dirspell

# Include dotfiles in pathname expansion
shopt -s dotglob

# Bash lists the status of any stopped and running jobs before exiting an interactive shell.
# If any jobs are running, this causes the exit to be deferred until a second exit is attempted
shopt -s checkjobs

# Enable extended pattern-matching features
shopt -s extglob

shopt -s nullglob

# matches filenames in a case-insensitive fashion when performing pathname expansion.
shopt -s nocaseglob

# ** becomes a recursive wildstar
shopt -s globstar

# Append each session's history to $HISTFILE
shopt -s histappend

# History expansions will be verified before execution.
shopt -s histverify

# Allow use to re-edit a faild history substitution.
shopt -s histreedit
