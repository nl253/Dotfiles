
# ~/.zshrc

[[ ! -e ~/.antigen ]] && git clone https://github.com/zsh-users/antigen.git ~/.antigen

source ~/.antigen/antigen.zsh

for i in jreese/zsh-titles zsh-users/zsh-{autosuggestions,completions} srijanshetty/zsh-pandoc-completion RobSis/zsh-completion-generator voronkovich/gitignore.plugin.zsh zdharma/fast-syntax-highlighting; do
    antigen bundle $i
done
    
antigen apply

# OPTIONS {{{
             
export HISTFILE=~/.zsh_history
export SAVEHIST=10000

setopt NO_BG_NICE                       # don't nice background tasks
setopt NO_HUP
setopt NO_LIST_BEEP
setopt LOCAL_OPTIONS                    # allow functions to have local options
setopt LOCAL_TRAPS                      # allow functions to have local traps
setopt HIST_IGNORE_ALL_DUPS             # don't record dupes in history
setopt HIST_REDUCE_BLANKS
setopt COMPLETE_IN_WORD
setopt NO_BEEP
setopt BRACE_CCL                        # {a-c} -> a b c

setopt list_types

# Compact completion
setopt auto_list
setopt auto_param_slash
setopt auto_param_keys
setopt list_packed
setopt auto_pushd
setopt pushd_minus
setopt pushd_ignore_dups
setopt complete_aliases                 # Check original command in alias completion
setopt hist_ignore_space                # Ignore add history if space
setopt glob_complete                    # Expand globs when completion
setopt mark_dirs                        # Add "/" if completes directory
unsetopt correct_all
# }}}

# Enable Ctrl-x-e to edit command line
autoload -U edit-command-line

# Emacs style
zle -N edit-command-line
bindkey '^xe' edit-command-line
bindkey '^x^e' edit-command-line

for i in ~/.shells/{variables,source,setup,fzf,aliases}.sh ~/.zsh/{source,functions,aliases}.zsh ; do
    source $i
done

# vim: foldmethod=marker
