
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
}

# }}}

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
setopt HIST_VERIFY
setopt SHARE_HISTORY                    # share history between sessions ???
setopt EXTENDED_HISTORY                 # add timestamps to history
setopt PROMPT_SUBST
setopt COMPLETE_IN_WORD
setopt NO_BEEP
setopt BRACE_CCL                        # {a-c} -> a b c

#setopt IGNORE_EOF
# Improve rm *
setopt rm_star_wait
setopt list_types

# Compact completion
setopt auto_list
setopt auto_param_slash
setopt auto_param_keys
setopt list_packed
setopt auto_cd
setopt auto_pushd
setopt pushd_minus
setopt pushd_ignore_dups
setopt complete_aliases                 # Check original command in alias completion
setopt APPEND_HISTORY                   # adds history
setopt INC_APPEND_HISTORY SHARE_HISTORY # adds history incrementally and share it across sessions
setopt HIST_IGNORE_ALL_DUPS             # don't record dupes in history
setopt HIST_REDUCE_BLANKS
setopt hist_ignore_space                # Ignore add history if space
setopt long_list_jobs                   # Better jobs
setopt glob_complete                    # Expand globs when completion
setopt mark_dirs                        # Add "/" if completes directory

unsetopt correct_all

# }}}

# source ~/.shells and ~/.zsh {{{

if [[ -d ~/.zsh ]] && [[ -d ~/.shells ]]; then

  for file in ~/.shells/* ; do  # Custom dirs with general shell configuration
    [[ -f $file ]] && source $file # all of these use POSIX compliant syntax
  done

  for file in ~/.zsh/* ; do      # Custom dirs with zsh specific configuration
    [[ -f $file ]] && source $file
  done
fi

# }}} # if this was sourced successfully then we have all the variables set properly

# Python virtual env manager  {{{
if $(in-path); then
  export PYENV_ROOT="$HOME/.pyenv"
  export PATH="$PYENV_ROOT/bin:$PATH"
  eval "$(pyenv init -)"
  source "$(pyenv root)/completions/pyenv.zsh"
fi # }}}

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

# spaceship (Best, custom, requres installation) # {{{
if [[ ! -e ~/.oh-my-zsh/custom/themes/spaceship.zsh-theme ]]; then
  mkdir -p ~/.oh-my-zsh/custom/themes # make it in case it doesn't exist
  if $(in-path curl); then # try with curl
    curl -fLo ~/.oh-my-zsh/custom/themes/spaceship.zsh-theme "https://raw.githubusercontent.com/denysdovhan/spaceship-zsh-theme/master/spaceship.zsh"
  elif $(in-path wget); then # fall back on wget
    wget -O - 'https://raw.githubusercontent.com/denysdovhan/spaceship-zsh-theme/master/install.sh' | zsh
  else
    echo -e "\nDownloading spaceship theme failed.\n"
    return 0
  fi
fi # }}}

# configuration for themes needs to be placed after ZSH_THEME else the settings will be overriden by defaults
[[ -e ~/.oh-my-zsh/custom/themes/spaceship.zsh-theme ]] && ZSH_THEME="spaceship" || ZSH_THEME="refined"

# NOTE -v is a new construct, I've had issues with it on remote machines
# show the clock if tmux is not running # but don't show normally because tmux does it already
# [[ ! -v TMUX ]] && SPACESHIP_TIME_SHOW=true

SPACESHIP_PYENV_SYMBOL="[pyenv]"

# OTHER DECENT THEMES :: {{{
# refined # minimalist, blue prompt, subtle git info
# pygmalion
# jonathan
# half-life
# Optionally, you can set it to "random"  # also, see }}}
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
# Plugins can be found in ~/.oh-my-zsh/plugins/*
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
#
# Description of plugins: {{{
# - `sudo` will insert sudo when ESC is pressed twice
# - `gitignore` will generate `.gitignore` files when you type gi [python|java ... ]
# - `pip` utilities for python and pip : clean cache ...
# - `npm` adds aliases and completion
# - `git-extras` add completion
# - `colorize` adds a `colorize` command to color file content (it will try to guess)
# - `k` a more pretty, git aware `ls` when you press `k`
# - `tmuxinator` adds completion with description
# - `taskwarrior` adds a `t` alias for `task` and completion
# }}}

plugins=(zsh-syntax-highlighting zsh-autosuggestions compleat pip npm sudo \
  tmuxinator gitignore github git-prompt z taskwarrior git-extras colorize)

# [[ ! -v TMUX ]] && plugins+="battery" # NOTE -v is a new construct, I've had issues with it on remote machines
# }}}

source $ZSH/oh-my-zsh.sh

# CUSTOM PLUGINS {{{
# pull custom plugins if missing
if $(in-path git); then
  if [[ ! -d ${ZSH_CUSTOM}/plugins/zsh-autosuggestions ]] ; then
    git clone "git://github.com/zsh-users/zsh-autosuggestions" "${ZSH_CUSTOM}/plugins/zsh-autosuggestions"
  fi
  if [[ ! -d ${ZSH_CUSTOM}/plugins/zsh-completions ]] ; then
    git clone "https://github.com/zsh-users/zsh-completions" "${ZSH_CUSTOM}/plugins/zsh-completions"
  fi
  if [[ ! -d ${ZSH_CUSTOM}/plugins/zsh-syntax-highlighting ]] ; then
    git clone "https://github.com/zsh-users/zsh-syntax-highlighting.git" "${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-syntax-highlighting"
  fi
fi
# press TAB to get fzf to pop up
if [[ ! -f ~/.zsh/zsh-interactive-cd.plugin.zsh ]] && $(in-path curl); then
    curl -fLo ~/.zsh/zsh-interactive-cd.plugin.zsh "https://raw.githubusercontent.com/changyuheng/zsh-interactive-cd/master/zsh-interactive-cd.plugin.zsh"
fi
# }}}

# USER CONFIGURATION {{{
# ------------------

# export MANPATH="/usr/local/man:$MANPATH"

# You may need to manually set your language environment
# export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
# if [[ -n $SSH_CONNECTION ]]; then
#   export EDITOR='vim'
# else
#   export EDITOR='mvim'
# fi

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# ssh
# export SSH_KEY_PATH="~/.ssh/rsa_id"

# Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.

# ------------------------------------------------ }}}

# [[ -e ~/.xsh ]] && source ~/.xsh # for now, until I get used to zsh I am leaving this commented out

# Commenting out until the project supports readline keybindings
# [[ ! -e ~/.qfc/bin/ ]] && git clone https://github.com/pindexis/qfc $HOME/.qfc
# [[ -s "${HOME}/.qfc/bin/qfc.sh" ]] && source "${HOME}/.qfc/bin/qfc.sh"

[[ -f ~/.zsh/zsh-interactive-cd.plugin.zsh ]] && source ~/.zsh/zsh-interactive-cd.plugin.zsh # make sure it exists

[[ -f ~/.fzf.zsh ]] && source ~/.fzf.zsh

# for some reason this needs to be EXACTLY here because something is overwriting
export LSCOLORS=ExFxCxdxBxegedabagacad
export LS_COLORS='di=01;34:ln=01;35:so=01;32:ex=01;31:bd=46;34:cd=43;34:su=41;30:sg=46;30:tw=42;30:ow=43;30'

