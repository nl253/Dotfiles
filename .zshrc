
# ~/.zshrc

[[ ! -e ~/.antigen ]] && git clone https://github.com/zsh-users/antigen.git ~/.antigen

source ~/.antigen/antigen.zsh

antigen bundle jreese/zsh-titles
antigen bundle zsh-users/zsh-completions
antigen bundle srijanshetty/zsh-pandoc-completion
antigen bundle zsh-users/zsh-autosuggestions
antigen bundle joepvd/zsh-hints
antigen bundle RobSis/zsh-completion-generator
antigen bundle voronkovich/gitignore.plugin.zsh
antigen bundle zdharma/fast-syntax-highlighting

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

for i in ~/.shells/{variables,source,setup,fzf,aliases}.sh ~/.zsh/{source,functions,aliases}.zsh ; do
    source $i
done

# vim: foldmethod=marker
