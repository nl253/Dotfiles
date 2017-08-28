
#  BASH SHOPTIONS

# don't put duplicate lines or lines starting with space in the history.
# See bash(1) for more options
HISTCONTROL=ignoreboth

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=1000
HISTFILESIZE=2000

# append to the history file, don't overwrite it
shopt -s histappend

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# Include dotfiles in pathname expansion
shopt -s dotglob

# matches filenames in a case-insensitive fashion when performing pathname expansion.
shopt -s nocaseglob

# Bash lists the status of any stopped and running jobs before exiting an interactive shell.
# If any jobs are running, this causes the exit to be deferred until a second exit is attempted
shopt -s checkjobs

# If set, the pattern "**" used in a pathname expansion context will
# match all files and zero or more directories and subdirectories.
shopt -s globstar

## enable inc search <C-s> which is often disabled by terminal emulators
#stty -ixon

## turn off control character echoing
#stty -ctlecho
