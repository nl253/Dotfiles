
# ~/.zshrc

fpath+=~/.zfunc

# Load generic shell configuration (aliases, variables)
for i in ~/.shells/*.sh; do
	[[ -f $i ]] && source $i 
done 

# If ~/.home or ~/.pc detected, don't source this script!
[[ -e ~/.nozsh ]] && echo -e "~/.nozsh detected - .zshrc not sourced" && return 0

 # don't nice background tasks
setopt NO_BG_NICE
setopt NO_HUP
setopt NO_LIST_BEEP

# allow functions to have local options
setopt LOCAL_OPTIONS

 # allow functions to have local traps
setopt LOCAL_TRAPS
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

# Write the history file in the ':start:elapsed;command' format.
setopt EXTENDED_HISTORY

 # Write to the history file immediately, not when the shell exits.
setopt INC_APPEND_HISTORY

 # Share history between all sessions.
setopt SHARE_HISTORY

 # Expire a duplicate event first when trimming history.
setopt HIST_EXPIRE_DUPS_FIRST

 # Do not record an event that was just recorded again.
setopt HIST_IGNORE_DUPS

 # Delete an old recorded event if a new event is a duplicate.
setopt HIST_IGNORE_ALL_DUPS

 # Do not display a previously found event.
setopt HIST_FIND_NO_DUPS

 # Do not record an event starting with a space.
setopt HIST_IGNORE_SPACE

 # Do not write a duplicate event to the history file.
setopt HIST_SAVE_NO_DUPS

 # Do not execute immediately upon history expansion.
setopt HIST_VERIFY

# Check original command in alias completion
setopt COMPLETE_ALIASES

# Expand globs when completion
# setopt GLOB_COMPLETE
# Add "/" if completes directory
setopt MARK_DIRS

unsetopt CORRECT_ALL

setopt COMPLETE_ALIASES

# unfortunately this is necessary
setopt MENU_COMPLETE

# enable completion
zmodload zsh/complist
autoload -Uz compinit && compinit

zstyle ':completion:::::' completer _complete _approximate
zstyle ':completion:*:descriptions' format "- %d -"
# zstyle ':completion:*:corrections' format "- %d - (errors %e})"
zstyle ':completion:*:default' list-prompt '%S%M matches%s'
zstyle ':completion:*' group-name ''
zstyle ':completion:*:manuals' separate-sections true
zstyle ':completion:*:manuals.(^1*)' insert-sections true
# zstyle ':completion:*' verbose yes
zstyle -e ':completion:*:approximate:*' max-errors 'reply=( $(( ($#PREFIX + $#SUFFIX) / 3 )) )'
zstyle ':completion::approximate*:*' prefix-needed false
zstyle ':completion:*' matcher-list '' 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=*' 'l:|=* r:|=*'
zstyle ':completion:*' menu select

if [[ "${terminfo[kcbt]}" != "" ]]; then
  bindkey "${terminfo[kcbt]}" reverse-menu-complete   # [Shift-Tab] - move through the completion menu backwards
else
	bindkey '^[[Z' reverse-menu-complete
fi

autoload edit-command-line
zle -N edit-command-line
bindkey '^x^e' edit-command-line

export WORDCHARS='*?_-.[]~=/&;!#$%^(){}<>'

backward-kill-dir () {
	local WORDCHARS=${WORDCHARS/\/}
	zle backward-kill-word
}

zle -N backward-kill-dir
bindkey '^[^?' backward-kill-dir

tcsh-backward-word() {
  local WORDCHARS="${WORDCHARS:s@/@}"
  zle backward-word
}

zle -N tcsh-backward-word

# Not using Rust atm
# zplug "plugins/cargo", from:oh-my-zsh 
# zplug "plugins/rust", from:oh-my-zsh 
# zplug "plugins/z", from:oh-my-zsh

# zplug denysdovhan/spaceship-zsh-theme, use:spaceship.zsh, from:github, as:theme

# Not using Python atm
# zplug "plugins/extract", from:oh-my-zsh
# zplug "plugins/pip", from:oh-my-zsh

# NVM is not used very often
# zplug "lukechilds/zsh-nvm"

# Extra overhead?
# zplug "jreese/zsh-titles"
# zplug "zsh-users/zsh-history-substring-search"

# zplug "zdharma/fast-syntax-highlighting"
# zplug "zsh-users/zsh-autosuggestions"
# zplug "zsh-users/zsh-completions"

# zplug "nl253/Scripts", as:command, rename-to:"csv-preview", use:"csv-preview.sh"
# zplug "nl253/Scripts", as:command, rename-to:"download-dotfile", use:"download-dotfile.sh"
# zplug "nl253/Scripts", as:command, rename-to:"grf", use:"grf.sh"
# zplug "nl253/Scripts", as:command, rename-to:"p", use:"processes.sh"

# zplug "tmux-plugins/tpm", as:command, ignore:'*'

# for i in completion options; do
	# zplug "nl253/zsh-config-${i}", defer:3
# done
# zplug load 

# this is necessary because the string "vim" is present in $EDITOR zsh will attempt to set ZLE to use vi mode

bindkey -e

_stack_commands() {
    local ret=1 state
    _arguments ':subcommand:->subcommand' && ret=0

    case $state in
      subcommand)
        subcommands=(
          "build:Build the project(s) in this directory/configuration"
          "install:Build executables and install to a user path"
          "test:Build and test the project(s) in this directory/configuration"
          "bench:Build and benchmark the project(s) in this directory/configuration"
          "haddock:Generate haddocks for the project(s) in this directory/configuration"
          "new:Create a brand new project"
          "init:Initialize a stack project based on one or more stack packages"
          "solver:Use a dependency solver to try and determine missing extra-deps"
          "setup:Get the appropriate ghc for your project"
          "path:Print out handy path information"
          "unpack:Unpack one or more packages locally"
          "update:Update the package index"
          "upgrade:Upgrade to the latest stack (experimental)"
          "upload:Upload a package to Hackage"
          "dot:Visualize your project's dependency graph using Graphviz dot"
          "exec:Execute a command"
          "ghc:Run ghc"
          "ghci:Run ghci in the context of project(s)"
          "ide:Run ide-backend-client with the correct arguments"
          "runghc:Run runghc"
          "clean:Clean the local packages"
          "docker:Subcommands specific to Docker use"
        )
        _describe -t subcommands 'stack subcommands' subcommands && ret=0
    esac

    return ret
}

compdef _stack_commands stack 
# vim: foldmethod=marker sw=2 ts=2 nowrap 
