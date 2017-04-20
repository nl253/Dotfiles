
#
# .zshrc
#

# PATH set here because :: http://www.jacobsingh.name/content/adding-your-path-oh-my-zsh

for file in ~/.shells/* ; do  # Custom dirs with general shell configuration 
  [[ -f $file ]] && source $file # all of these use POSIX compliant syntax 
done

for file in ~/.zsh/* ; do      # Custom dirs with zsh specific configuration
  [[ -f $file ]] && source $file
done

# try to install oh-my-zsh using curl, fall back on wget
CURL=$(which curl)
WGET=$(which wget)
GIT=$(which git)
if [[ ! -e ~/.oh-my-zsh/ ]]; then
  if [[ -x $CURL ]] ; then
    sh -c "$(curl -fsSL https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"
  elif [[ -x $WGET ]]; then
    sh -c "$(wget https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh -O -)"
  else
    echo -e "Neither curl nor wget present on this system,\nOh-my-zsh could not be downloaded.\n.zshrc will not be fully loaded.\nAborting."
    return 0
  fi
fi

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
setopt COMPLETE_IN_WORD

#setopt IGNORE_EOF

setopt APPEND_HISTORY                   # adds history
setopt INC_APPEND_HISTORY SHARE_HISTORY # adds history incrementally and share it across sessions
setopt HIST_IGNORE_ALL_DUPS             # don't record dupes in history
setopt HIST_REDUCE_BLANKS

# setopt CORRECT
unsetopt correct_all

export ZSH=~/.oh-my-zsh # Path to your oh-my-zsh installation.

# Custom Themes :: https://github.com/robbyrussell/oh-my-zsh/wiki/Themes
# spaceship (Best, custom, requres installation)

if [[ ! -e ~/.oh-my-zsh/custom/themes/spaceship.zsh-theme ]]; then
  if [[ -x $CURL ]] ; then
    curl -o - https://raw.githubusercontent.com/denysdovhan/spaceship-zsh-theme/master/install.sh | zsh
  elif [[ -x $WGET ]]; then
    wget -O - https://raw.githubusercontent.com/denysdovhan/spaceship-zsh-theme/master/install.sh | zsh
  else
    echo -e "\nDownloading spaceship theme failed.\n"
    return 0
  fi
fi

# DECENT ::
# refined # minimalist, blue prompt, subtle git info
# pygmalion
# jonathan
# half-life
# Optionally, you can set it to "random"  # also, see

 # configuration for themes needs to be placed after ZSH_THEME else the settings will be overriden by defaults
[[ -e ~/.oh-my-zsh/custom/themes/spaceship.zsh-theme ]] && ZSH_THEME="spaceship" || ZSH_THEME="refined" 

# NOTE -v is a new construct, I've had issues with it on remote machines 
# [[ ! -v TMUX ]] && SPACESHIP_TIME_SHOW=true  # show the clock if tmux is not running # but don't show normally because tmux does it already 

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

# plugins can be found in ~/.oh-my-zsh/plugins/*
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# too many plugins slow down shell startup.

plugins=(zsh-syntax-highlighting \
  zsh-autosuggestions compleat k pip \
  npm sudo history tmuxinator gitignore \
  github git-prompt z taskwarrior \
  git-extras colorize)

# [[ ! -v TMUX ]] && plugins+="battery" # NOTE -v is a new construct, I've had issues with it on remote machines 

# Description of plugins:
# - `sudo` will insert sudo when ESC is pressed twice
# - `gitignore` will generate `.gitignore` files when you type gi [python|java ... ]
# - `pip` utilities for python and pip : clean cache ...
# - `npm` adds aliases and completion
# - `git-extras` add completion
# - `colorize` adds a `colorize` command to color file content (it will try to guess)
# - `k` a more pretty, git aware `ls` when you press `k`
# - `tmuxinator` adds completion with description
# - `taskwarrior` adds a `t` alias for `task` and completion

source $ZSH/oh-my-zsh.sh

if [[ -x $GIT ]]; then
  if [[ ! -d ${ZSH_CUSTOM}/plugins/zsh-autosuggestions ]] ; then
    git clone "git://github.com/zsh-users/zsh-autosuggestions" "${ZSH_CUSTOM}/plugins/zsh-autosuggestions"
  fi
  if [[ ! -d ${ZSH_CUSTOM}/plugins/zsh-completions ]] ; then
    git clone "https://github.com/zsh-users/zsh-completions" "${ZSH_CUSTOM}/plugins/zsh-completions"
  fi
  if [[ ! -d ${ZSH_CUSTOM}/plugins/zsh-syntax-highlighting ]] ; then
    git clone "https://github.com/zsh-users/zsh-syntax-highlighting.git" "${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-syntax-highlighting"
  fi
  if [[ ! -e ${ZSH_CUSTOM}/plugins/k ]] ; then
    git clone https://github.com/supercrabtree/k $HOME/.oh-my-zsh/custom/plugins/k # git dir lisitng with `k`
  fi
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

[[ -f ~/.bin/tmuxinator.zsh ]] && source ~/.bin/tmuxinator.zsh

# [[ -e ~/.xsh ]] && source ~/.xsh

# Commenting out until the project supports readline keybindings
# [[ ! -e ~/.qfc/bin/ ]] && git clone https://github.com/pindexis/qfc $HOME/.qfc
# [[ -s "${HOME}/.qfc/bin/qfc.sh" ]] && source "${HOME}/.qfc/bin/qfc.sh"

[[ -f ~/.fzf.zsh ]] && source ~/.fzf.zsh

