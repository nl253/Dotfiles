
# ~/.zshrc

# NOTE `$PATH` set here because :: `http://www.jacobsingh.name/content/adding-your-path-oh-my-zsh`

# OPTIONS {{{
             
# {{{ HISTORY, some in ~/.shells/variables.sh
export HISTFILE=~/.zsh_history
export SAVEHIST=10000
# }}}

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

setopt rm_star_wait                     # Improve rm *
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

# DOWNLOAD OH-MY-ZSH IF MISSING {{{
# use `curl`, fall back on `wget`
if [[ ! -e ~/.oh-my-zsh ]]; then
  if [[ -x $(which url) ]]; then
    sh -c "$(curl -fsSL https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"
  elif [[ -x $(which wget) ]]; then
    sh -c "$(wget https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh -O -)"
  else
    echo -e "Neither curl nor wget present on this system."
    echo -e "Oh-my-zsh could not be downloaded."
    echo -e "zshrc will not be fully loaded."
    echo -e "Aborting."
    return 0
  fi
fi # }}}

export ZSH=~/.oh-my-zsh # Path to your oh-my-zsh installation.

# THEME {{{
# -----
# Custom Themes :: https://github.com/robbyrussell/oh-my-zsh/wiki/Themes

# configuration for themes needs to be placed after ZSH_THEME else the settings will be overriden by defaults
 ZSH_THEME="robbyrussell"

# OTHER DECENT THEMES :: {{{
# refined # minimalist, blue prompt, subtle git info
# pygmalion jonathan half-life
# Optionally, you can set it to "random" }}}

# }}}

# FURTHER CONFIGURATION {{{
# ---------------------
# CASE_SENSITIVE="true" # case-sensitive completion.

# Hyphen-insensitive completion. Case sensitive completion must be off. _ and - will be interchangeable.
# HYPHEN_INSENSITIVE="true"

# DISABLE_AUTO_UPDATE="true" # Disable bi-weekly auto-update checks.

export UPDATE_ZSH_DAYS=7 # How often to auto-update (in days).

# DISABLE_AUTO_TITLE="true" # Disable auto-setting terminal title.

ENABLE_CORRECTION="false" # Enable command auto-correction.

COMPLETION_WAITING_DOTS="true" # Display red dots whilst waiting for completion.

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# The optional three formats: "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# HIST_STAMPS="mm/dd/yyyy"
# }}}

# PLUGINS {{{
# -------
# Plugins can be found in ~/.oh-my-zsh/plugins/*
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
#
# Description of plugins: {{{
# -----------------------
# - `sudo` will insert sudo when ESC is pressed twice
# - `gitignore` will generate `.gitignore` files when you type gi [python|java ... ]
# - `pip` utilities for python and pip : clean cache ...
# - `npm` adds aliases and completion
# - `git-extras` add completion
# - `colorize` adds a `colorize` command to color file content (it will try to guess)
# - `k` a more pretty, git aware `ls` when you press `k`
# - `tmuxinator` adds completion with description
# - `taskwarrior` adds a `t` alias for `task` and completion
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
# Custom plugins:
# --------------
# `zsh-syntax-highlighting` adds fish-like highlighting of typed words
# `zsh-completions` extra completions
# `zsh-autosuggestions` smart, fish-like autosuggestions pop up when you type
# `fast-syntax-highlighting` tweaks to `zsh-syntax-highlighting`
# `git-extra-commands` a large collection of git commands for reference : 
#  https://github.com/unixorn/git-extra-commands/blob/master/git-extra-commands.plugin.zsh
#
# }}}
plugins=(fast-syntax-highlighting zsh-syntax-highlighting zsh-autosuggestions compleat)
# }}}

# SOURCE OH_MY_ZSH {{{
[[ -e $ZSH/oh-my-zsh.sh ]] && source $ZSH/oh-my-zsh.sh
# }}}

# OVERWITE OH-MY-ZSH OPTIONS {{{
# --------------------------
# oh-my-zsh sets a lot of defaults such as LS_COLORS 
# this means we need to overwite it AFTER sourcing oh-my-zsh 
export LSCOLORS=ExFxCxdxBxegedabagacad
export LS_COLORS='di=01;34:ln=01;35:so=01;32:ex=01;31:bd=46;34:cd=43;34:su=41;30:sg=46;30:tw=42;30:ow=43;30'
# }}}

# source ~/.shells and ~/.zsh here to overwite some settings {{{
if [[ -d ~/.zsh ]] && [[ -d ~/.shells ]]; then
  for file in ~/.{shells,zsh}/* ; do  # Custom dirs with general shell configuration
    [[ -f $file ]] && source $file # all of these use POSIX compliant syntax
  done

  # CONFIG TO BE RUN ONLY FOR MY PC 
  [[ -f ~/.pc ]] && source ~/.pc.zsh
fi

# }}} # if this was sourced successfully then we have all the variables set properly

# Python virtual env manager  {{{ # 
# NOTE needs to be sourced AFTER sourcing general shells configuration
#if [[ -e ~/.pyenv/bin/pyenv ]]; then
  #eval "$(pyenv init -)"
  #source "$(pyenv root)/completions/pyenv.zsh"
  #[[ -e ~/.pyenv/plugins/pyenv-virtualenv/bin/pyenv-activate ]] && eval "$(pyenv virtualenv-init -)"
#fi
# }}}

# CUSTOM PLUGINS {{{
# --------------
# pull custom plugins if missing {{
#
# ARG1: plugin name
# ARG2: repo owner/Repo_name
# {{{
function fetch-custom-plug-gh(){  
  if [[ -x $(which git) ]] && [[ ! -e ${ZSH_CUSTOM}/plugins/$1/$1.plugin.zsh ]] ; then
    cd ${ZSH_CUSTOM}/plugins
    git clone https://github.com/$2 $1 
    cd  
    source ~/.zshrc
  fi
} 
# }}}

fetch-custom-plug-gh zsh-autosuggestions zsh-users/zsh-autosuggestions
fetch-custom-plug-gh zsh-completions zsh-users/zsh-completions
fetch-custom-plug-gh zsh-syntax-highlighting zsh-users/zsh-syntax-highlighting.git
fetch-custom-plug-gh fast-syntax-highlighting zdharma/fast-syntax-highlighting.git
fetch-custom-plug-gh git-extra-commands unixorn/git-extra-commands.git
fetch-custom-plug-gh zsh-history-substring-search zsh-users/zsh-history-substring-search

unset -f fetch-custom-plug-gh

# USER CONFIGURATION {{{
# ------------------
# export MANPATH="/usr/local/man:$MANPATH"
# export LANG=en_US.UTF-8  # You may need to manually set your language environment
# export ARCHFLAGS="-arch x86_64"  # Compilation flags
# export SSH_KEY_PATH="~/.ssh/rsa_id"  # ssh

# Preferred editor for local and remote sessions
[[ -n $SSH_CONNECTION ]] && export EDITOR='vim'
# }}}


# vim: foldmethod=marker
