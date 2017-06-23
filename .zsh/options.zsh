             
export HISTFILE=~/.zsh_history
export SAVEHIST=10000

# don't nice background tasks
setopt NO_BG_NICE                       
setopt NO_HUP
setopt NO_LIST_BEEP
# allow functions to have local options
setopt LOCAL_OPTIONS                    
 # allow functions to have local traps
setopt LOCAL_TRAPS                      
# don't record dupes in history
setopt HIST_IGNORE_ALL_DUPS             
setopt HIST_REDUCE_BLANKS
setopt COMPLETE_IN_WORD
setopt NO_BEEP
# {a-c} -> a b c
setopt BRACE_CCL                        
setopt list_types
# Compact completion
setopt auto_list
setopt auto_param_slash
setopt auto_param_keys
setopt list_packed
setopt auto_pushd
setopt pushd_minus
setopt pushd_ignore_dups
# Check original command in alias completion
setopt complete_aliases                 
# Ignore add history if space
setopt hist_ignore_space                
# Expand globs when completion
setopt glob_complete                    
# Add "/" if completes directory
setopt mark_dirs                        
unsetopt correct_all
