#!/usr/bin/env zsh

# try to install oh-my-zsh using curl, fall back on wget
[[ -x /usr/bin/curl ]] && [[ ! -d ~/.oh-my-zsh ]] && sh -c "$(curl -fsSL https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"
[[ -x /usr/bin/wget ]] && [[ ! -d ~/.oh-my-zsh ]] && sh -c "$(wget https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh -O -)"

HISTFILE=~/.zsh_history
HISTSIZE=10000                          # expand history size
SAVEHIST=10000

setopt NO_BG_NICE                       # don't nice background tasks
setopt NO_HUP
setopt NO_LIST_BEEP
setopt LOCAL_OPTIONS                    # allow functions to have local options
setopt LOCAL_TRAPS                      # allow functions to have local traps
setopt HIST_VERIFY
setopt SHARE_HISTORY                    # share history between sessions ???
setopt EXTENDED_HISTORY                 # add timestamps to history
setopt PROMPT_SUBST
setopt CORRECT
setopt COMPLETE_IN_WORD
#setopt IGNORE_EOF

setopt APPEND_HISTORY                   # adds history
setopt INC_APPEND_HISTORY SHARE_HISTORY # adds history incrementally and share it across sessions
setopt HIST_IGNORE_ALL_DUPS             # don't record dupes in history
setopt HIST_REDUCE_BLANKS

export ZSH=~/.oh-my-zsh # Path to your oh-my-zsh installation.

# Theme to load. 
# DECENT :: jonathan half-life refined pygmalion
# Optionally, if you set this to "random"
# See https://github.com/robbyrussell/oh-my-zsh/wiki/Themes
ZSH_THEME="refined" # minimalist, blue prompt, subtle git info

# CASE_SENSITIVE="true" # Use case-sensitive completion.

# Hyphen-insensitive completion. Case sensitive completion must be off. _ and - will be interchangeable.
# HYPHEN_INSENSITIVE="true"

# DISABLE_AUTO_UPDATE="true" # Disable bi-weekly auto-update checks.

export UPDATE_ZSH_DAYS=7 # How often to auto-update (in days).

# DISABLE_AUTO_TITLE="true" # Disable auto-setting terminal title.

ENABLE_CORRECTION="true" # Enable command auto-correction.

COMPLETION_WAITING_DOTS="true" # Display red dots whilst waiting for completion.

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# The optional three formats: "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# HIST_STAMPS="mm/dd/yyyy"

# plugins can be found in ~/.oh-my-zsh/plugins/*
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# too many plugins slow down shell startup.

plugins=(zsh-syntax-highlighting \
        zsh-autosuggestions \
        compleat python pip \
        npm sudo history gitignore \
        globalias gnu-utils git-prompt \
        git-extras cp copyfile colorize)

source $ZSH/oh-my-zsh.sh

if [[ ! -d ${ZSH_CUSTOM}/plugins/zsh-autosuggestions ]] ; then
        git clone "git://github.com/zsh-users/zsh-autosuggestions" "${ZSH_CUSTOM}/plugins/zsh-autosuggestions"
fi
if [[ ! -d ${ZSH_CUSTOM}/plugins/zsh-completions ]] ; then
        git clone "https://github.com/zsh-users/zsh-completions" "${ZSH_CUSTOM}/plugins/zsh-completions"
fi
if [[ ! -d ${ZSH_CUSTOM}/plugins/zsh-syntax-highlighting ]] ; then
        git clone "https://github.com/zsh-users/zsh-syntax-highlighting.git" "${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-syntax-highlighting"
fi

#
# User configuration

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

# # # # # # # # # # # # # # # # # # # # # # # # #

for file in ~/.shells/* ; do  # Custom dirs with general shell configuration
  [[ -f $file ]] && source $file
done

for file in ~/.zsh/* ; do      # Custom dirs with zsh specific configuration
  [[ -f $file ]] && source $file
done

if [[ ! -e ~/.autojump/bin/autojump ]] ; then # download if missing
  git clone git://github.com/joelthelion/autojump.git
  cd autojump
  ./install.py
fi

# usage : j [fuzzy dir name] , j -a [exact name of dir to add to tracking]
[[ -e ~/.autojump/etc/profile.d/autojump.sh ]] && source ~/.autojump/etc/profile.d/autojump.sh

[[ -e ~/.xsh ]] && source ~/.xsh

# Commenting out until the project supports readline keybindings
# [[ ! -e ~/.qfc/bin/ ]] && git clone https://github.com/pindexis/qfc $HOME/.qfc
# [[ -s "${HOME}/.qfc/bin/qfc.sh" ]] && source "${HOME}/.qfc/bin/qfc.sh"

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

