
#  bash(1) options

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

# # If set, shell error messages are written in the standard GNU error message format.
# shopt -s gnu_errfmt

# # If  set,  and  readline  is  being used, bash will not attempt to 
# # search the PATH for possible completions when completion is attempted on an empty
# shopt -s no_empty_cmd_completion

# # If set, bash checks that a command found in the hash table exists before trying 
# # to execute it. If a hashed command no longer exists, a normal path search is performed.
# shopt -s  checkhash

# complete -c sudo
# complete -c while
# complete -v $
# complete -d cd
# complete -d find
