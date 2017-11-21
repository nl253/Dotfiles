# ~/.bashrc  executed by bash(1)
# NOTE here it is moved to ~/.config/bash/bashrc.sh
# setting $BASH_ENV to this file makes it act as ~/.bashrc normally would

[[ -d ~/.config/sh ]] && export SHDOTDIR=$HOME/.config/sh
[[ -d ~/.config/bash ]] && export BASHDOTDIR=$HOME/.config/bash
[[ -f $SHDOTDIR/init.sh ]] && source $SHDOTDIR/init.sh

# Check if bash version is at least 4 to run some of my scripts.
if ((BASH_VERSINFO < 4)); then
  echo "Your bash is outdated. Install bash >= 4."
  return 0
fi

EXECIGNORE="/{usr/,}bin/{grub,pamac,x86,xdg,xfs,yuv,tiff,sndfile,quad,ntfs,nl-,mtp-,mkfs,jfs_,jack_,ip6table,iptable,idevice,gvfs,fsck,encryptfs,dvi,djv}*:"
PS1="\n\[$(tput bold)\]\[$(tput sgr0)\]\[\033[38;5;3m\]\u\[$(tput sgr0)\]\[$(tput sgr0)\]\[\033[38;5;40m\]@\[$(tput bold)\]\[$(tput sgr0)\]\[\033[38;5;31m\]\h\[$(tput sgr0)\]\[$(tput sgr0)\]\[\033[38;5;15m\] \n\[$(tput sgr0)\]\[\033[38;5;241m\]\w\[$(tput sgr0)\]\[\033[38;5;15m\] \[$(tput bold)\]\[$(tput sgr0)\]\[\033[38;5;88m\]>\[$(tput sgr0)\]\[\033[38;5;89m\]>\[$(tput sgr0)\]\[$(tput sgr0)\]\[\033[38;5;15m\] \[$(tput sgr0)\]"

[[ -f $BASHDOTDIR/completions.sh ]] && source $BASHDOTDIR/completions.sh

# append to the history file, don't overwrite it
shopt -s histappend

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# If set, the pattern "**" used in a pathname expansion context will
# match all files and zero or more directories and subdirectories.
shopt -s globstar

# # Include dotfiles in pathname expansion
shopt -s dotglob

# # matches filenames in a case-insensitive fashion when performing pathname expansion.
shopt -s nocaseglob

# If set, minor errors in the spelling of a directory component in a cd command will be corrected.
# The errors checked for are transposed characters, a missing character, and one character too many.
# If a correction is found, the corrected filename is printed,  and  the  command  proceeds.
# This option is only used by interactive shells.
shopt -s cdspell

# If set, bash attempts spelling correction on directory names during word completion if the directory name initially supplied does not exist.
# dotglob If set, bash includes filenames beginning with a `.' in the results of pathname expansion.
shopt -s dirspell

# If set, bash checks that a command found in the hash table exists before trying
# to execute it. If a hashed command no longer exists, a normal path search is performed.
shopt -s checkhash

# # If  set,  and  readline  is  being used, bash will not attempt to
# # search the PATH for possible completions when completion is attempted on an empty
# shopt -s no_empty_cmd_completion

# # If  set,  bash  replaces  directory names with the results of word expansion when performing filename completion.
# # This changes the contents of the readline editing buffer.
# # If not set, bash attempts to preserve what the user typed.
# shopt -s direxpand

# # If set, range expressions used in pattern matching bracket expressions (see Pattern Matching above)
# # behave as if in the traditional C  locale  when performing  comparisons.
# # That  is,  the current locale's collating sequence is not taken into account, so b will not collate
# # between A and B, and upper-case and lower-case ASCII characters will collate together
# shopt -s globasciiranges

# # If set, bash allows patterns which match no files (see Pathname Expansion above) to expand to a null string, rather than themselves.
# shopt -s nullglob
[[ $(hostname) =~ raptor ]] && source /etc/bash_completion
