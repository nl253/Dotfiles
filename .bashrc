
# ~/.bashrc: executed by bash(1)

[[ -f ~/.shinit ]] && source ~/.shinit

# Check if bash version is at least 4 to run some of my scripts.
if ((BASH_VERSINFO < 4)); then
  echo "Your bash is outdated. Install bash >= 4."
  return 0
fi

export PS1="\n\[$(tput bold)\]\[$(tput sgr0)\]\[\033[38;5;3m\]\u\[$(tput sgr0)\]\[$(tput sgr0)\]\[\033[38;5;40m\]@\[$(tput bold)\]\[$(tput sgr0)\]\[\033[38;5;31m\]\h\[$(tput sgr0)\]\[$(tput sgr0)\]\[\033[38;5;15m\] \n\[$(tput sgr0)\]\[\033[38;5;241m\]\w\[$(tput sgr0)\]\[\033[38;5;15m\] \[$(tput bold)\]\[$(tput sgr0)\]\[\033[38;5;88m\]>\[$(tput sgr0)\]\[\033[38;5;89m\]>\[$(tput sgr0)\]\[$(tput sgr0)\]\[\033[38;5;15m\] \[$(tput sgr0)\]" 

[[ -x $(command which pandoc 2>/dev/null) ]] && eval "$(pandoc --bash-completion)"
[[ -x $(command which stack 2>/dev/null) ]] && eval "$(stack --bash-completion-script stack)"

# if ! shopt -oq posix; then
  # if [[ -f /usr/share/bash-completion/bash_completion ]]; then
    # . /usr/share/bash-completion/bash_completion
  # elif [[ -f /etc/bash_completion ]]; then
    # . /etc/bash_completion
  # fi
# fi 

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

# # If  set,  bash  replaces  directory names with the results of word expansion when performing filename completion.  
# # This changes the contents of the readline editing buffer.  
# # If not set, bash attempts to preserve what the user typed.
# shopt -s direxpand
                      
# # Bash lists the status of any stopped and running jobs before exiting an interactive shell.
# # If any jobs are running, this causes the exit to be deferred until a second exit is attempted
# shopt -s checkjobs

# # If set, range expressions used in pattern matching bracket expressions (see Pattern Matching above) 
# # behave as if in the traditional C  locale  when performing  comparisons.
# # That  is,  the current locale's collating sequence is not taken into account, so b will not collate 
# # between A and B, and upper-case and lower-case ASCII characters will collate together 
# shopt -s globasciiranges

# # If set, bash allows patterns which match no files (see Pathname Expansion above) to expand to a null string, rather than themselves.
# shopt -s nullglob

# # If set, minor errors in the spelling of a directory component in a cd command will be corrected.  
# # The errors checked for are transposed characters, a missing character, and one character too many.  
# # If a correction is found, the corrected filename is printed,  and  the  command  proceeds.   
# # This option is only used by interactive shells.  
# shopt -s cdspell 

# # If set, bash attempts spelling correction on directory names during word completion if the directory name initially supplied does not exist.
# # dotglob If set, bash includes filenames beginning with a `.' in the results of pathname expansion.
# shopt -s dirspell

# # If  set,  and  readline  is  being used, bash will not attempt to 
# # search the PATH for possible completions when completion is attempted on an empty
# shopt -s no_empty_cmd_completion

# # If set, bash checks that a command found in the hash table exists before trying 
# # to execute it. If a hashed command no longer exists, a normal path search is performed.
# shopt -s  checkhash 
