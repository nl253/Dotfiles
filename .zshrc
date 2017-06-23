
# ~/.zshrc

[[ ! -e ~/.zplug ]] && curl -sL --proto-redir -all,https https://raw.githubusercontent.com/zplug/installer/master/installer.zsh | zsh

source ~/.zplug/init.zsh

zplug 'zplug/zplug', hook-build:'zplug --self-manage'
#zplug "nl253/Dotfiles", use:.zshrc
zplug "RobSis/zsh-completion-generator"
zplug "joepvd/zsh-hints"
zplug "jreese/zsh-titles"
zplug "srijanshetty/zsh-pandoc-completion"
zplug "zdharma/fast-syntax-highlighting"
zplug "zsh-users/zsh-autosuggestions"
zplug "zsh-users/zsh-completions"
zplug "zsh-users/zsh-history-substring-search"
#zplug voronkovich/gitigore.plugin.zsh

zplug load 

# Install plugins if there are plugins that have not been installed
if ! zplug check --verbose; then
    printf "Install? [y/N]: "
    if read -q; then
        echo; zplug install
    fi
fi

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
