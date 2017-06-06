
# ~/.zshrc

# NOTE `$PATH` set here because :: `http://www.jacobsingh.name/content/adding-your-path-oh-my-zsh`

 # UTILS {{{
 # checks if an executable is in $PATH
in-path(){ 
  for i in $(echo $PATH | sed "s/:/\n/g"); do
      if [[ -x "$i/$1" ]]; then
          return 0
      fi
  done
  return 1
}  # }}}

# OPTIONS {{{

# {{{ HISTORY
HISTFILE=~/.zsh_history
HISTSIZE=10000                          # expand history size
SAVEHIST=10000
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

# oh-my-zsh {{{
# try to install oh-my-zsh if missing
# use curl, fall back on wget
if [[ ! -e ~/.oh-my-zsh ]]; then
  if $(in-path curl); then
    sh -c "$(curl -fsSL https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"
  elif $(in-path wget); then
    sh -c "$(wget https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh -O -)"
  else
    echo -e "Neither curl nor wget present on this system,\nOh-my-zsh could not be downloaded.\n.zshrc will not be fully loaded.\nAborting."
    return 0
  fi
fi # }}}

export ZSH=~/.oh-my-zsh # Path to your oh-my-zsh installation.

# THEME {{{
# Custom Themes :: https://github.com/robbyrussell/oh-my-zsh/wiki/Themes

# configuration for themes needs to be placed after ZSH_THEME else the settings will be overriden by defaults
 ZSH_THEME="robbyrussell"

# OTHER DECENT THEMES :: {{{
# refined # minimalist, blue prompt, subtle git info
# pygmalion jonathan half-life
# Optionally, you can set it to "random" }}}

# }}}

# FURTHER CONFIGURATION {{{
# CASE_SENSITIVE="true" # Use case-sensitive completion.

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

# [[ ! -v TMUX ]] && plugins+="battery" # NOTE -v is a new construct, I've had issues with it on remote machines
# }}}

if [[ -e $ZSH/oh-my-zsh.sh ]]; then 
  source $ZSH/oh-my-zsh.sh
  unalias d
  unalias please
  unalias afind
fi

# overwite oh-my-zsh options {{{
# oh-my-zsh sets a lot of defaults such as LS_COLORS 
# this means we need to overwite it AFTER sourcing oh-my-zsh 
export LSCOLORS=ExFxCxdxBxegedabagacad
export LS_COLORS='di=01;34:ln=01;35:so=01;32:ex=01;31:bd=46;34:cd=43;34:su=41;30:sg=46;30:tw=42;30:ow=43;30'
# }}}

# source ~/.shells and ~/.zsh here to overwite some settings {{{
if [[ -d ~/.zsh ]] && [[ -d ~/.shells ]]; then

  for file in ~/.shells/* ; do  # Custom dirs with general shell configuration

    [[ -f $file ]] && source $file # all of these use POSIX compliant syntax
  done

  for file in ~/.zsh/* ; do      # Custom dirs with zsh specific configuration

    [[ -f $file ]] && source $file

  done

fi

# }}} # if this was sourced successfully then we have all the variables set properly

# Python virtual env manager  {{{ # 
# NOTE needs to be sourced AFTER sourcing general shells configuration
if [[ -e ~/.pyenv/bin/pyenv ]]; then
  eval "$(pyenv init -)"
  source "$(pyenv root)/completions/pyenv.zsh"
  [[ -e ~/.pyenv/plugins/pyenv-virtualenv/bin/pyenv-activate ]] && eval "$(pyenv virtualenv-init -)"
fi
# }}}

# CUSTOM PLUGINS {{{
# pull custom plugins if missing {{
if $(in-path git); then
  if [[ ! -e ${ZSH_CUSTOM}/plugins/zsh-autosuggestions/zsh-autosuggestions.plugin.zsh ]] ; then
    git clone "git://github.com/zsh-users/zsh-autosuggestions" "${ZSH_CUSTOM}/plugins/zsh-autosuggestions" && source ~/.zshrc
  fi
  if [[ ! -e ${ZSH_CUSTOM}/plugins/zsh-completions/zsh-completions.plugin.zsh ]] ; then
    git clone "https://github.com/zsh-users/zsh-completions" "${ZSH_CUSTOM}/plugins/zsh-completions" && source ~/.zshrc
  fi
  if [[ ! -e ${ZSH_CUSTOM}/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.plugin.zsh ]] ; then
    git clone "https://github.com/zsh-users/zsh-syntax-highlighting.git" "${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-syntax-highlighting"
    source ~/.zshrc
  fi
  if [[ ! -e ${ZSH_CUSTOM}/plugins/fast-syntax-highlighting/fast-syntax-highlighting.plugin.zsh ]] ; then
    cd ${ZSH_CUSTOM}/plugins
    git clone https://github.com/zdharma/fast-syntax-highlighting.git && cd && source ~/.zshrc
  fi
  if [[ ! -e ${ZSH_CUSTOM}/plugins/git-extra-commands/git-extra-commands.plugin.zsh ]] ; then
    cd ${ZSH_CUSTOM}/plugins && git clone https://github.com/unixorn/git-extra-commands.git git-extra-commands && cd && source ~/.zshrc
  fi
fi

if $(in-path curl); then
  # press TAB to get fzf to pop up
  if [[ ! -f ${ZSH_CUSTOM}/plugins/zsh-history-substring-search/zsh-history-substring-search.plugin.zsh ]] && $(in-path curl); then
    mkdir -p ${ZSH_CUSTOM}/plugins/zsh-history-substring-search
    curl -fLo ${ZSH_CUSTOM}/plugins/zsh-history-substring-search/zsh-history-substring-search.plugin.zsh "https://raw.githubusercontent.com/zsh-users/zsh-history-substring-search/master/zsh-history-substring-search.zsh"
  fi
fi

# SOURCE CUSTOM PLUGINS {{{
# make sure it exists
[[ -f ~/.fzf.zsh ]] && source ~/.fzf.zsh
[[ -f ${ZSH_CUSTOM}/plugins/zsh-history-substring-search/zsh-history-substring-search.plugin.zsh ]] && source ${ZSH_CUSTOM}/plugins/zsh-history-substring-search/zsh-history-substring-search.plugin.zsh
# }}}
# }}}

# USER CONFIGURATION {{{
# ------------------

# export MANPATH="/usr/local/man:$MANPATH"

# export LANG=en_US.UTF-8  # You may need to manually set your language environment

# Preferred editor for local and remote sessions
# if [[ -n $SSH_CONNECTION ]]; then
#   export EDITOR='vim'
# else
#   export EDITOR='mvim'
# fi

# export ARCHFLAGS="-arch x86_64"  # Compilation flags

# export SSH_KEY_PATH="~/.ssh/rsa_id"  # ssh

# Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.

# ------------------------------------------------ }}}

# $PROMPT
# export PROMPT=' %F{yellow}%d%f  %F{54}>>%f '
# export RPROMPT='%F{red}%?%f'


# added by travis gem
[ -f /home/norbert/.travis/travis.sh ] && source /home/norbert/.travis/travis.sh
