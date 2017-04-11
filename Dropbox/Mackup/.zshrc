#
# THIS ZSHRC SOURCES MY BASHRC,
# where I do all I can to make is compatible with zsh
#


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

# Path to your oh-my-zsh installation.
export ZSH=/home/norbert/.oh-my-zsh

# Set name of the theme to load. Optionally, if you set this to "random"
# it'll load a random theme each time that oh-my-zsh is loaded.
# See https://github.com/robbyrussell/oh-my-zsh/wiki/Themes
ZSH_THEME="pygmalion"

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion. Case
# sensitive completion must be off. _ and - will be interchangeable.
# HYPHEN_INSENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
# DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to change how often to auto-update (in days).
export UPDATE_ZSH_DAYS=7

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
COMPLETION_WAITING_DOTS="true"

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

# ADDITONAL PLUGINS FROM GITHUB
# https://github.com/zsh-users/zsh-syntax-highlighting
#

plugins=(zsh-syntax-highlighting \
        zsh-autosuggestions \
        git \
        compleat \
        dircycle \ 
        tmuxinator \
        pyenv urltools\
        rsync python pip \
        npm man gem \
        systemd sudo \
        history gitignore \
        taskwarrior \
        globalias \
        gnu-utils git-prompt \
        git-extras \
        cp copyfile \
        colorize \
        archlinux \
        autopep8)
# look into it:
# git-flow git-hubflow

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
if [[ ! -x /usr/bin/hstr ]] ; then
        sudo pacman -S "hstr-git"
fi

if [[ ! -x ~/.autojump/bin/autojump ]] ; then
        mkdir -p /tmp/autojump
        git clone git://github.com/joelthelion/autojump.git /tmp/autojump/
        /tmp/autojump/install.py
fi

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

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.
#
# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"


source ~/.bashrc
#emulate sh -c 'source ~/.bashrc'

# # # # # # # # # # # # # # # # # # # # # # # # #
[[ -s /home/norbert/.autojump/etc/profile.d/autojump.sh ]] && source /home/norbert/.autojump/etc/profile.d/autojump.sh

