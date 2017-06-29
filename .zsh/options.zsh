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
setopt LIST_TYPES
# Compact completion
setopt AUTO_LIST
setopt AUTO_MENU
setopt AUTO_PARAM_SLASH
setopt AUTO_PARAM_KEYS
setopt LIST_PACKED
setopt LIST_ROWS_FIRST
setopt AUTO_PUSHD
setopt PUSHD_MINUS
setopt PUSHD_IGNORE_DUPS
setopt GLOB_DOTS
# Check original command in alias completion
setopt COMPLETE_ALIASES                 
# Ignore add history if space
setopt HIST_IGNORE_SPACE                
# Expand globs when completion
# setopt GLOB_COMPLETE                    
# Add "/" if completes directory
setopt MARK_DIRS                        
unsetopt CORRECT_ALL

zstyle ':completion:*' menu select
bindkey '^[[Z' reverse-menu-complete

zstyle ':completion:*' matcher-list '' 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=*' 'l:|=* r:|=*'
# set -o monitor

#bindkey -v
#typeset -g -A key
#bindkey '^?' backward-delete-char
#bindkey '^[[5~' up-line-or-history
#bindkey '^[[3~' delete-char
#bindkey '^[[6~' down-line-or-history
#bindkey '^[[A' up-line-or-search
#bindkey '^[[D' backward-char
#bindkey '^[[B' down-line-or-search
#bindkey '^[[C' forward-char 
#bindkey "^[[H" beginning-of-line
#bindkey "^[[F" end-of-line
