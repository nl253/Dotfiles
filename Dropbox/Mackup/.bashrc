
# ~/.bashrc

# Bash File Testing
#
# -e filename - Check for file existence, regardless of type (node, directory, socket, etc.)
# -f filename - Check for regular file existence not a directory
# -d directoryname - Check for directory Existence
# -L filename - Symbolic link
#
# -r filename - Check if file is a readable
# -w filename - Check if file is writable
# -x filename - Check if file is executable
#
#       -b filename - Block special file
#       -S filename - Check if file is socket
#       -s filename - Check if file is nonzero size
#       -c filename - Special character file
#
#       -O filename - True if file exists and is owned by the effective user id
#       -G filename set-group-id - True if file exists and is set-group-id
#       -u filename - Check if file set-user-id bit is set
#       -k filename - Sticky bit

[ -z "$PS1" ] && return   #  If not running interactively, don't do anything

[[ $- != *i* ]] && return   #  If not running interactively, don't do anything

# set variables to produce colored output later
RED="\e[31m"
CYAN="\e[96m"
DARKMAGENTA="\e[35m"
MAGENTA="\e[95m"
BLUE="\e[34mB"
GREEN="\e[32m"
WHITE="\e[97mW"
DEFCOLOR="\e[39m"
YELLOW="\e[93m"
DARKYELLOW="\e[33m"
GREY="\e[37m"
DARKGREY="\e[90m"

echo -e "${RED}~/.bashrc ${YELLOW}loaded" # indicator if it has successfully loaded

# prompt
export PS1="$(tput setaf 1)\w\n\[$(tput bold)\]\[$(tput setaf 1)\][\[$(tput setaf 3)\]\u\[$(tput setaf 2)\]@\[$(tput setaf 4)\]\h\[$(tput setaf 5)\]\[$(tput setaf 1)\]]\[$(tput setaf 7)\]\\$\[$(tput sgr0)\] "

[ -f ~/.fzf.bash ] && source ~/.fzf.bash # initialise fzf

[ -d ~/bash_it ] && [ -f ~/bash_it/.bash_it.sh ] && source ~/bash_it/bash_it.sh    # Load Bash-It
unset MAILCHECK                         # Don't check mail when opening terminal.
export SCM_CHECK=true                   # Set this to false to turn off version control status checking within the prompt for all themes
export SHORT_HOSTNAME=$(hostname -s) # Set Xterm/screen/Tmux title with only a short hostname

export HISTSIZE=20000
export HISTFILESIZE=20000
export HISTCONTROL=ignoredups
export HISTIGNORE="&:[ ]*:exit:ls:bg:fg:history:clear"
[ -x /usr/bin/irssi ] && export IRC_CLIENT='irssi'
export GREP_COLOR='1;33'

## Summary for args to less:
#   -M (-M or --LONG-PROMPT) Prompt very verbosely
#   -I (-I or --IGNORE-CASE) Searches with '/' ignore case
#   -R (-R or --RAW-CONTROL-CHARS) For handling ANSI colors
#   -F (-F or --quit-if-one-screen) Auto exit if <1 screen
#   -X (-X or --no-init) Disable termcap init & deinit

#export LESSOPEN="| /usr/bin/src-hilite-lesspipe.sh %s"
if [ -x /usr/bin/less ]; then
  export PAGER=less
  export LESS=' -R '
  [ -f "/usr/bin/source-highlight-esc.sh" ] && export LESSOPEN="| /usr/bin/source-highlight-esc.sh %s"
else
  export PAGER=more
fi
[ -x /usr/bin/bash ] && export SHELL=/usr/bin/bash || export SHELL=/usr/bin/sh
[ -x /usr/bin/google-chrome-stable ] && export BROWSER='/usr/bin/google-chrome-stable'
[ -x /usr/bin/alacritty ] && export TERMINAL='/usr/bin/alacritty' || [ -x /usr/bin/xterm ] && export TERMINAL='/usr/bin/xterm'
[ -d  /usr/lib/jvm/java-8-openjdk ] && export JAVA_HOME='/usr/lib/jvm/java-8-openjdk' && export JRE_HOME='/usr/lib/jvm/java-8-openjdk/jre'
[ -x /usr/bin/ruby ] && RUBY_BINS='~/.gem/ruby/2.4.0/bin' || RUBY_BINS=''
[ -x /usr/bin/rustc ] && RUST_BINS='~/.cargo/bin' || RUST_BINS=''
[ -x /usr/bin/php ] && PHP_BINS='~/.config/composer/vendor/bin' || PHP_BINS=""
[ -d  '~/go/bin' ] &&  GO_BINS='~/go/bin' || GO_BINS=''
[ -d  '~/bin' ] &&  MY_BINS='~/bin' || MY_BINS=''

# this is ok because if these don't exist strings will be empty
export PATH="$PATH:$RUBY_BINS:$RUST_BINS:$MY_BINS:$GO_BINS:$PHP_BINS"

[ -x /usr/bin/ranger ] && [ -f ~/.config/ranger/rc.conf ] && export RANGER_LOAD_DEFAULT_RC=false

if [ -x /usr/bin/nvim ]; then
  export EDITOR=/usr/bin/nvim
  export MANPAGER="nvim -c 'set ft=man' -"
  export NVIM_TUI_ENABLE_CURSOR_SHAPE=1
  alias vi=/usr/bin/nvim
  alias vim=/usr/bin/nvim
else # if not nvim check if vim
  if [ -x /usr/bin/vim ]; then
    alias vi=/usr/bin/vim
    export MANPAGER="vim -c 'set ft=man' -"
    export EDITOR=/usr/bin/vim
    alias nvim=/usr/bin/vim
  fi
fi

if [ -x /usr/bin/fzf ]; then
  export FZF_DEFAULT_COMMAND='ag --hidden --ignore .git -g ""'
  export FZF_DEFAULT_OPTS='--reverse --color hl:117,hl+:1,bg+:232,fg:240,fg+:246 '
  alias p=FZFpkill
fi

# Pacman
if [ -x /usr/bin/pacman ]; then
  alias pacman-recent-installations="expac --timefmt='%Y-%m-%d %T' '%l\t%n' | sort | tail -n 20"
  alias pacman-packages-by-size="expac -S -H M '%k\t%n'"
  alias pacman-reinstall-all-native-packages="sudo pacman -Qnq | pacman -S -"
  alias pacman-reinstall-all-foreign-packages="sudo pacman -Qmq | pacman -S -"
  alias pacman-remove-orphans="sudo pacman -Rns $(pacman -Qtdq)"
  [ -x /usr/bin/yaourt ] && export YAOURT_COLORS="nb=1:pkg=1:ver=1;32:lver=1;45:installed=1;42:grp=1;34:od=1;41;5:votes=1;44:dsc=0:other=1;35"
fi

# OTHER SHELLS
[ -x /usr/bin/zsh ] && alias z=zsh
[ -x $(which xonsh) ] && alias x=xonsh

# ---------
#  GIT
# ---------
[ -x /usr/bin/hub ] && eval "$(hub alias -s)" && alias g=hub
[ -x /usr/bin/tig ] && alias t=tig

setxkbmap -layout gb -option ctrl:nocaps # Caps Lock is Control on a GB keyboard

#setxkbmap -option ctrl:swapcaps # for US

echo -e "${MAGENTA}capslock remapped to ctrl"

# expands bang combinations and variables to their values - remember !$ last arg / !^ first arg / !* all args
#bind Space:magic-space # also combine these with :h (head) or :t (tail) to get path selective path expansion -> !$:h

[ -x /usr/bin/ag ] && alias ag='ag --hidden --pager="less -MIRFX"'  # search with dotfiles
[ -x /usr/bin/ranger ] && alias r='ranger'

# =======
# ALIASES
# =======

alias e=$EDITOR
alias c='jump-list'
alias l='ls -CFa'
alias l='ls -CFa'
alias la='ls -la --group-directories-first --time-style=+"%d.%m.%Y %H:%M" --color=auto -F'
alias le="ls -lo"
alias ll='ls -l --group-directories-first --time-style=+"%d.%m.%Y %H:%M" --color=auto -F'
alias ls='LC_COLLATE=C ls --color=auto --group-directories-first'

alias launcher='find /bin/ ~/.cargo/bin/ ~/.gem/ruby/2.4.0/bin/ ~/.npm/ -executable -type f -exec basename {} \; 2>/dev/null | fzf  --multi -x --bind "enter:execute({}&)"'

alias -- -='cd -'        # Go back
alias ..="cd .."
alias ...="cd ..."
alias ....="cd ...."
alias .....="cd ....."
alias ......="cd ....."
alias .......="cd ......"
alias ........="cd ......."
alias .........="cd ......."
alias ..........="cd ........."
alias diff='diff --color=auto'
alias grep='grep --color=auto'
alias fgrep='fgrep --color=auto'
alias egrep='egrep --color=auto'
alias recent-files='find ~ -amin -10 -type f | grep -P -v ".*C|cache.*" | sed -E "s/^\.\///" | fzf'
alias symlinks-at-home="find ~ -type l 2>/dev/null"
alias symlinks-pretty='for i in $(find -type l -exec echo {} \;); do echo -e " \e[36m$i  \e[39m->  \e[91m$(readlink -f $i)" ; done'
[ ! -x /usr/bin/tree ] && alias tree="find . -print | sed -e 's;[^/]*/;|____;g;s;____|; |;g'"
[ -x /usr/bin/sshfs ] && alias mount-raptor="sshfs -o transform_symlinks -o follow_symlinks nl253@raptor.kent.ac.uk: /home/norbert/Raptor"
[ -x /usr/bin/dmenu_run ] && alias dmenu_run="dmenu_run -p ' >> ' -nb black -nf white"
[ -x /usr/bin/aria2c ] && alias aria2c="aria2c --dir='~/Downloads/Torrent/'"
[ -x /usr/bin/aspell ] && alias aspell="aspell -c -l en_GB"
alias df='df --human-readable --si'
alias info='info --vi-keys'
alias freq='cut -f1 -d" " "$HISTFILE" | sort | uniq -c | sort -nr | head -n 30'
alias logout="pkill -KILL -u "
#alias j=jobs # used by autojump
alias h=history
alias untar='tar xvf'

alias show-scripts-in-bashrc='grep -P "^\S+?\(\)" ~/.bashrc | sed  "s/(//g" | sed "s/{//" | sed "s/)//g"'
#
# Readline
alias show-keybingings="bind -p | grep -v '^#\|self-insert\|^$'"

alias f="find-extension"

alias http-server="python3 -m http.server"

# ===============
# SCRIPTS
#
# NOTE
# coreutils programs will not be considered dependencies
# as they are preinstalled on practically every UNIX system

# ===============
# FUNCTION :: aliases for multi-word tig commands
# REQUIRES :: aria2c
tis(){
  tig status
}

til(){
  tig log
}

tib(){
  tig blame -C
}
# ===============
# shorthand for `tree` with hidden files and color enabled,
# ignoring `.git` directory, listing directories first.
# The output gets piped into `less` with options to preserve color and line numbers,
# unless the output is small enough for one screen.
tre(){
  tree -aC -I '.git|node_modules|bower_components' --dirsfirst "$@" | less -FRNX;
}

# ===============
# FUNCTION :: aria2 convenience configuration
# REQUIRES :: aria2c
# USAGE :: torrent <file|URL|magnet ... >
torrent(){
  [ ! -d ~/Downloads/Torrents ] && mkdir -p "~/Downloads/Torrents"
  [ ! -f ~/Downloads/Torrents] && touch "~/Downloads/Torrents/aria2.log"
  aria2c --continue --dir=~/Downloads/Torrents  --log="~/Downloads/Torrents/aria2.log" $1
}
# ========
# FUNCTION :: restore the system
# The aim of the script is to do nothing when the system is OK
# and restore the whole system when it's just been reinstalled.
#

restore-system(){

if [ ! -x /usr/bin/pacman ] ; then
  echo -e "This script is preconfigured ONLY for Arch Linux."
  #statements
fi
local NEED_TO_BE_INSTALLED=("git" "git-imerge" "git-extras" \
  "intellij-idea-community-edition" \
  "lshw" "less" "nvim" "spotify" "sncli" \
  "xf86-input-mouse" "xf86-input-synaptics" \
  "xf86-input-void" "xf86-video-intel" \
  "xmonad-contrib" "xmonad-utils" "acpid" \
  "aspell-en" "ca-certificates" "ctags" \
  "aspell" "bluej" "bashmount" "bmenu" \
  "ghc-mod" "cabal" "node" "gawk" "i3" \
  "xmonad" "autojump" "php" \
  "crontab" "psysh" "emacs" "cmake" \
  "freetype2" "fontconfig" "pkg-config" "make" \
  "csslint" "thinkfinger" "the_silver_searcher" \
  "sed" "pandoc" "openssh" "openvpn" "p7zip" "astyle" \
  "python" "tig"  "apacman" "yaourt" "tmux" \
  "aria2c" "cronie" "fdupes" "ddupes" \
  "rofi" "stylish-haskell" "tidy" "tree" \
  "xf86-input-keyboard" "xf86-input-libinput" \
  "thermald" "dropbox" "python-pip" \
  "google-chrome" "coreutils" "hub" "htop" "jdk-8" \
  "wget" "curl" "wordnet" "xclip" \
  "upower" "npm" "ruby" "gem" "timeshift" "thinkfan" \
  "xclip" "bashlint" "alsa-utils" "curl" "dos2unix" "pdftotext" \
  "perl" "shellcheck" "zsh")

for i in $NEED_TO_BE_INSTALLED; do
  [ ! -x "/usr/bin/$i" ] && sudo pacman -S --quiet  --noconfirm --needed $i
done

echo -e "checking PACMAN packages"
for i in $NEED_TO_BE_INSTALLED; do
  # check and give feedback on what's missing
  [ ! -x $(which $i) ] && echo -e $i" :: not found on the filesystem despite installation"
done

# PYTHON
# it is crucial that python is correctly set up
# mackup is dependent on it as well as things like ranger

echo -e "PYTHON"

if [ ! -x /usr/bin/pip ] && [ ! -x "/usr/bin/pip3" ] ; then
  echo "PIP and PYTHON are necessary to make this work.\nThe script will terminate, \nmake sure pip is installed to proceed"
  return 1
fi

echo -e "PYTHON and PIP detected\ninstalling PYTHON packages"

local PY=("mackup" "ranger" \
  "pudb" "neovim" "jedi" \
  "mypy" "xonsh" "xontrib-z" \
  "psutil" "nltk" "pytest" "ipython" \
  "you-get" "pandas" \
  "spacy" "sumy" "fuzzywuzzy" \
  "tensorflow" "numpy" \
  "requests" "scrapy")

for i in $PY; do
  # configuration for scripts
  # if it exists, ignore
  sudo pip install --quiet --exists-action i $i
done

echo -e "checking PIP packages"

for i in $PY; do
  [ ! -x $(which $i) ] && echo -e $i" :: not present on the system"
done

# python virtual env
echo -e "checking virtual env"

if [ ! -d '~/.pyenv' ] ; then
  echo -e "PYENV not detected\ninitiating ... "
  git clone https://github.com/pyenv/pyenv.git ~/.pyenv
  pyenv global "3.5.0"
  echo -e 'global python 3.5.0 activated'
else
  echo -e "PYENV detected continuing ..."
fi

echo -e "RUBY"

# ruby
if [ ! -x /usr/bin/gem ] || [ ! -x /usr/bin/ruby ] ; then

  echo -e "either RUBY or GEM was not detected on this filesystem"
  echo -e "make sure GEM is installed to install RUBY packages"
  echo -e "becasue RUBY is not cructial the script will continue"
  sleep 5

else

  echo -e "RUBY and GEM detected\ninstalling RUBY gems"

  local RB=(mdl sqlint rubocop)

  for i in $RB; do
    [ ! -x $(which $i) ] && sudo gem install $i
  done

  # check and give feedback on what's missing
  echo -e "checking RUBY gems"

  for i in $RB; do
    [ ! -x $(which $i) ] && echo -e $i" :: not present on the system"
  done

fi

# javascript
if [ ! -x /usr/bin/npm ] ; then

  echo "NPM not deteceted on the filesystem\nmake sure NPM is installed to install JAVSCRIPT packages"
  echo "because NPM and JAVASCRIPT aren't crucial, the script will continue "
  sleep 5

else

  echo -e "NODE and NPM detected\ninstalling NODE packages"

  local JS=("write-good" textlint \
    "git-standup" "git-stats" \
    jsonlint tern "git-fire" \
    "js-beautify" textlint)

  for i in $JS; do
    if [ ! -x $(which $i) ]; then
      sudo npm install $i -g
    fi
  done

fi


# check if ZSH is set up correctly
if [ -x /usr/bin/zsh ] ; then

  echo -e 'zsh detected on your filesystem ... '

  if  [ ! -d "~/.oh-my-zsh" ] ; then

    echo "OH-MY-ZSH not detected\ninitiating ..."

    # from oh-my-zsh [github]
    sh -c "$(curl -fsSL https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"

    # custom plugins
    [ ! -d ${ZSH_CUSTOM}/plugins/zsh-autosuggestions ] && git clone "git://github.com/zsh-users/zsh-autosuggestions" "${ZSH_CUSTOM}/plugins/zsh-autosuggestions"
    [ ! -d ${ZSH_CUSTOM}/plugins/zsh-completions ] && git clone "https://github.com/zsh-users/zsh-completions" "${ZSH_CUSTOM}/plugins/zsh-completions"
    [ ! -d ${ZSH_CUSTOM}/plugins/zsh-syntax-highlighting ] && git clone "https://github.com/zsh-users/zsh-syntax-highlighting.git" "${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-syntax-highlighting"

  fi
  
fi

# if ALACRITTY is not set up rust will be needed
# not sure about this one ...
if [ ! -x /usr/bin/alacritty ] && [ ! -x /usr/bin/rustup ]  && [ ! -x /usr/bin/rustup ] ; then
  echo -e "RUST AND ALACRITTY NOT INSTALLED\nINITALISING RUSTUP\n"
  sudo curl https://sh.rustup.rs -sSf | sh
  echo -e "CLONING ALACRITTY REPO FROM GIT"
  git clone https://github.com/jwilm/alacritty.git ~
  echo -e "CHANGING RUSTUP TOOLCHAIN TO STABLE"
  sudo rustup override set stable
  sudo rustup update stable
  sudo rustup default stable
  echo -e "cd to ${HOME}/alacritty\nBUILDING ALACRITTY ... "
  cd ~/alacritty && sudo cargo build --release 
fi

# NEO-VIM
# I chose the plugins dir to check if nvim is correctly set up, if not - clone it
if [ -x /usr/bin/nvim ] && [ ! -d "~/.config/nvim/plugins" ] && [ ! -d "~/.local/share/nvim/plugged" ]; then
  echo -e "NEOVIM NOT nINITALISED\nCLONING FROM GIT"
  # use tmp to force-write the files
  mkdir -p /tmp/nvim/ && git clone --recursive "https://github.com/nl253/VimScript" "/tmp/nvim/"
  # git won't let you overwrite anything - use cp
  cp -R /tmp/nvim/* ~/.config/nvim/
  # from vim-plug [github]
  curl -fLo ~/.local/share/nvim/site/autoload/plug.vim --create-dirs \
    https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
elif [ ! -x "/usr/bin/nvim" ] ; then
  echo "make sure neovim is installed"

fi

# TMUX
# check if tmux plugin manager dir is present
if [ -x /usr/bin/tmux ] && [ ! -d "~/.tmux/plugins/tpm" ] ; then
  echo -e "TMUX PLUGIN MANAGER NOT PRESENT\nINITALISING ..."
  git clone "https://github.com/tmux-plugins/tpm" "~/.tmux/plugins/tpm"
else
  echo "make sure tmux is installed"
fi

# at this point variables will need to be reset
echo "RESOURCING BASHRC"
source ~/.bashrc

for i in $NEED_TO_BE_INSTALLED; do
  # make sure all pacman packages are installed
  if [ ! -x $(which $i) ]; then
    echo -e $i" NOT PROPERLY INSTALLED\nQUITTING"
    return 1
  fi
done

if [ -x "/usr/bin/dropbox" ] && [ -d "~/Dropbox" ] ; then

  # this won't work
  if [ -x $(which mackup) ] && [ -L "~/.bashrc" ] && [ -L "~/.inputrc" ] ; then
    mackup restore
  else
    echo -e "you need to set up MACKUP.\nquitting."
    return 1
    # DEAL WITH DOTFILES
  fi
elif [ ! -x "/usr/bin/dropbox" ] && [ ! -d "~/Dropbox" ] ; then
  echo -e "make sure DROPBOX is set up"
  return 1
fi
#
# TODO REMOVE USELESS SOFTWARE from Manjaro [get that from their wiki]


}

# ========
# FUNCTION
# show if the current file is a symbolic link
# if so, then show what it points to, otherwise echo "FALSE"
is-symlink(){
if [ $# == 1 ]; then
  if [ $(readlink -f $1) -eq $(realpath -s $1) ]; then
    echo "FALSE"
    return 1
  elif [ $(readlink -f $1) -ne $(realpath -s $1) ]; then
    #echo -e "${GREEN}TRUE${DEFCOLOR}"
    echo -e "${CYAN}$(realpath -s $1) ${DEFCOLOR}-> ${YELLOW}$(readlink -f $1)${DEFCOLOR}"
    return 0
  fi
else
  echo 'ONLY 1 ARGUMENT IS REQURED'
  return 1
fi
}
# ==============================
# FUNCTION
# print in a JSON format a dict with your IP
# and other details about the network you are on
ipif(){
  if grep -P "(([1-9]\d{0,2})\.){3}(?2)" <<< "$1"; then
    curl ipinfo.io/"$1"
  else
    ipawk=($(host "$1" | awk '/address/ { print $NF }'))
    curl ipinfo.io/${ipawk[1]}
  fi
  echo
}
# ==============================
# FUNCTION  :: prints the 256 colors with their corresponding numbers
show-colors()
{
  (x=`tput op` y=`printf %76s`;for i in {0..256};do o=00$i;echo -e ${o:${#o}-3:3} `tput setaf $i;tput setab $i`${y// /=}$x;done)
}
# ==============================
# FUNCTION  :: shows terminal capabilities
show-term-capabilities()
{
  infocmp -1 | sed -nu 's/^[ \000\t]*//;s/[ \000\t]*$//;/[^ \t\000]\{1,\}/!d;/acsc/d;s/=.*,//p'|column -c80
}
# ==============================
# FUNCTION  :: general purpose archive extracting
# USAGE: ex <file>
# REQUIRES :: pygmentize (can be optional) :: ag for find-shell
ex ()
{
  if [ -f $1 ] && [ $# == 1 ] ; then
    case $1 in
      *.tar.bz2)   tar xjf $1   ;;
      *.tar.gz)    tar xzf $1   ;;
      *.bz2)       bunzip2 $1   ;;
      *.rar)       unrar x $1   ;;
      *.gz)        gunzip $1    ;;
      *.tar)       tar xf $1    ;;
      *.tbz2)      tar xjf $1   ;;
      *.tgz)       tar xzf $1   ;;
      *.zip)       unzip $1     ;;
      *.Z)         uncompress $1;;
      *.7z)        7z x $1      ;;
      *)           echo "'$1' cannot be extracted via ex()" ;;
    esac
  else
    echo "'$1' is not a valid file or you supplied to many args"
  fi
}
# ==============================
# FUNCTION  :: ls + cd combined
# USAGE: jump-list <directory>
jump-list()
{
  [ $# -gt 1 ] && echo 'A MAX OF 1 ARGS IS ACCEPTED' && return 1 # if more than 1 dirs are provided then return
  local dir="$1"
  local dir="${dir:=$HOME}"
  if [[ -d "$dir" ]]; then
    cd "$dir" >/dev/null; ls
  else
    echo "bash: jump-list: $dir: Directory not found"
  fi
}
# ==============================
# FUNCTION  :: finds and lists 12 files with size of more than 6kb on the whole system
# USAGE :: find-large [optional extension]
# COMMENTS :: it will literally list random file so it is useful to specify the extension
find-large()
{
  [ $# -gt 1 ] && echo 'PROVIDE 0 or 1 ARGS' && return 1
  if [ $# -eq 0 ]; then
    find / -type f -size +6k -iregex ".*" 2>/dev/null | head -n 12
  else
    find / -type f -size +6k -iregex ".*\.$1" 2>/dev/null | head -n 12
  fi
}
# ==============================
# FUNCTION  :: backup utility
#
# TODO
# make it more interactive
# bacup only necessary dotfiles
# show git diff when the file already exists and is not the same content-wise
# make the user agree before overriting if a flag is set -i like in cp
#
backup-all-dotfiles()
{
  local BACKUP="$HOME/Dropbox/Backup/"
  local BACKUP_DOTFILES=(\
    ".Xclients" \
    ".Xresources" \
    ".bash_logout" \
    ".bash_profile" \
    ".bashrc" \
    ".dir_colors.256_dark" \
    ".extend.bashrc" \
    ".extend.xinitrc" \
    ".fzf.bash" \
    ".gitconfig" \
    ".httpie/config.json" \
    ".ideavimrc" \
    ".inputrc" \
    ".moc/config" \
    ".myclirc" \
    ".netrc" \
    ".profile" \
    ".spacemacs" \
    ".tigrc" \
    ".tmux.conf" \
    ".w3m/config" \
    ".xbindkeysrc" \
    ".xinitrc" \
    ".xmonad/xmonad.hs" \
    ".xonshrc" \
    ".xprofile" \
    )
  local BACKUP_CONFIG="$BACKUP.config/"
  local BACKUP_CONFIG_DOTFILES=(\
    "xonsh/config.py" \
    "http-prompt/config.py" \
    "alacritty/alacritty.yml" \
    )
  for i in ${BACKUP_DOTFILES[*]}; do
    mkdir -p `dirname "${HOME}/${i}"` # attempt to make parent dir
    cp -i `readlink -f "${HOME}/${i}"` "${BACKUP}${i}"
  done
  for i in ${BACKUP_CONFIG_DOTFILES[*]}; do
    mkdir -p `dirname "${HOME}/.config/${i}"` # attempt to make parent dir
    cp -i  `readlink -f "${HOME}/.config/${i}"` "${BACKUP_CONFIG}${i}"
  done
}
# ==========================================
# FUNCTION  ::
# generate dictionaries based on word frequency from a certain filetype
# it produces both upper and lower case words
# writes to STDOUT, use bash redirection to save to file
# DEPENDENCIES :: dict-generator (written by me in python 3.6) ag
# USAGE: generate-dictionary <extension eg :: {sh,java,py}>
#
# TODO
# make sure it works properly
#
# ==========================================
generate-dictionary(){
# depends on ag [can be replaced with grep and regexp]
# the args needed:
# 1 : the file type
# 2 : output location
dict-generator -n 2000 -l 4 -m 12 -p upper $(ag  -g "" --$1 / 2>/dev/null | xargs) >> $2
dict-generator -n 2000 -l 4 -m 12 -p lower $(ag  -g "" --$1 / 2>/dev/null | xargs) >> $2
}
# ==========================================
# FUNCTION
# on f just list files in the CWD
# on f {filetype such as java,python,vim etc} list all files of this type from CWD recursively
# on f {filetype such as java,python,vim etc} [query] list all files of this type from CWD recursively that match the pattern
# multiple patterns can be specified

find-extension(){
# with no args will all files in CWD
[ $# == 0 ] && find -L . -maxdepth 1 -type f | sed -E 's/^\.\///' | grep -P -v '(%.+%.+)|(.*\d{4,}$)|(.*~$)|(.*(c|C)ache.*)|(.*\.git.*)|(.*\.(png)|(jpeg)|(bluej)|(ctxt)|(hg)|(svn)|(bak)|(pdf)|(jpg)|(so)|(pyc)|(obj)|(out)|(class)|(swp)|(xz)|(svn)|(swp)|(ri)$)' && return 0

if [ $# -ge 1 ] ; then

  # default depth
  local DEPTH="15"

  # check for the first (and only) arg
  case $1 in
    js | javas* | jscript | jvscirpt)
      local REGEX=".*\.js$"
      ;;
    java | jva)
      local REGEX=".*\.java$"
      ;;
    vi*)
      local REGEX=".*\.vim$"
      ;;
    erl | erlang | erlg)
      local REGEX=".*\.erl$"
      ;;
    py | python | pthn | pythn | pyth | pyt)
      local REGEX=".*\.py$"
      ;;
    lisp | ls)
      local REGEX=".*\.\(lisp\)\|\(cl\)$"
      ;;
    c)
      local REGEX=".*\.\(c\)\|\(h\)$"
      ;;
    cpp | c++)
      local REGEX=".*\.cpp$"
      ;;
    elisp | el)
      local REGEX=".*\.el$"
      ;;
    rb | ruby | rby)
      local REGEX=".*\.rb$"
      ;;
    html | HTML )
      local REGEX=".*\.html$"
      ;;
    xhtml | XHTML )
      local REGEX=".*\.xhtml$"
      ;;
    shell | sh | bash | zsh )
      local REGEX=".*\.\(sh\)\|\(zsh\)$"
      ;;
    json | jsn)
      local REGEX=".*\.json$"
      ;;
    sass | scss )
      local REGEX=".*\(sass\)\|\(scss\)$"
      ;;
    yml | yaml | yl)
      local REGEX=".*\.yml$"
      ;;
    xml | xm)
      local REGEX=".*\.xml$"
      ;;
    cof* | cffee)
      local REGEX=".*\.coffee$"
      ;;
    hs | hask* )
      local REGEX=".*\.hs$"
      ;;
    less)
      local REGEX=".*\.less$"
      ;;
    ts | typescript | tscript | tyscript | typscript)
      local REGEX=".*\.ts$"
      ;;
    css )
      local REGEX=".*\.css$"
      ;;
    org )
      local REGEX=".*\.org$"
      ;;
    rst | restr* )
      local REGEX=".*\.rst$"
      ;;
    mark* | md )
      local REGEX=".*\.md$"
      ;;
  esac

  # if 1 arg
  [ $# == 1 ] && find -L . -maxdepth $DEPTH -type f -iregex "${REGEX}" | sed -E 's/^\.\///' | grep -P -v '(%.+%.+)|(.*\d{4,}$)|(.*~$)|(.*(c|C)ache.*)|(.*\.git.*)|(.*\.(png)|(jpeg)|(bluej)|(ctxt)|(hg)|(svn)|(bak)|(pdf)|(jpg)|(so)|(pyc)|(obj)|(out)|(class)|(swp)|(xz)|(svn)|(swp)|(ri)$)' && return 0

  # if more

  # make a temporary variable
  local RE=""

  #loop omitting the first arg which will be the filetype
  # format (*this.*)|(.*that.*)|(.*other.*) ...

  for i in ${@:2}; do
    RE="${RE}\(.*${i}.*\)\|"
  done

  # for that trailing pipeline
  RE="${RE}\(ssdfdklflkjlk\)"

  # at the end append extension regex from the previous case statement
  find -L . -maxdepth 15 -type f -iregex "\("${RE}$"\)"${REGEX} | sed -E 's/^\.\///' | grep -P -v '(%.+%.+)|(.*\d{4,}$)|(.*~$)|(.*(c|C)ache.*)|(.*\.git.*)|(.*\.(png)|(jpeg)|(bluej)|(ctxt)|(hg)|(svn)|(bak)|(pdf)|(jpg)|(so)|(pyc)|(obj)|(out)|(class)|(swp)|(xz)|(svn)|(swp)|(ri)$)' && return 0

fi
}

# ==========================================

stty -ixon              # enable inc search <C-s> which is often disabled by terminal emulators

if [ ! -n "${ZSH+2}" ]; then
  complete -cf sudo
  shopt -s histappend     # Append each session's history to $HISTFILE
  shopt -s histverify     # Edit a recalled history line before executing
  shopt -s extglob        # Enable extended pattern-matching features
  shopt -s expand_aliases # Expand aliases
  shopt -s dirspell       # correct minor spelling errors
  shopt -s direxpand
  shopt -s expand_aliases
  shopt -s globstar       # ** becomes a recursive wildstar
  shopt -s cdspell        # correct minor spelling errors
  shopt -s dotglob        # Include dotfiles in pathname expansion
  shopt -s checkwinsize   # update the value of LINES and COLUMNS after each command if altered
fi

# ADDITIONAL

# =====
# GIT
# =====

# ADD
#alias ga='git add'
#alias gall='git add -A'
#alias gap='git add -p'

# BRANCH
#alias gb='git branch'
#alias gb='git branch'
#alias gba='git branch -a'
#alias gbt='git branch --track'
#alias gdel='git branch -D'

# COMMIT
#alias gc='git commit -v'
#alias gca='git commit -v -a'
#alias gcaa="git commit -a --amend -C HEAD"
#alias gci='git commit --interactive'
#alias gcm='git commit -v -m'

# CLONE
#alias gcl='git clone'

# CHECKOUT
#alias gcb='git checkout -b'
#alias gcb='git checkout -b'
#alias gco='git checkout'
#alias gcob='git checkout -b'
#alias gcom='git checkout master'
#alias gct='git checkout --track'
#alias gc='git checkout'

# DIFF
#alias gd='git diff'
#alias gds='git diff --cached'
#alias gdv='git diff -w "$@" | vim -R -'

# FETCH
#alias gmu='git fetch origin -v; git fetch upstream -v; git merge upstream/master'
#alias gf='git fetch --all --prune'
#alias gf='git fetch --prune'
#alias gfa='git fetch --all --tags --prune'
#alias gfl='git fetch --prune && git lg -15'
#alias gft='git fetch --all --prune --tags'
#alias gftv='git fetch --all --prune --tags --verbose'
#alias gfv='git fetch --all --prune --verbose'
#alias gup='git fetch && git rebase'

# LOG
#alias gll='git log --graph --pretty=oneline --abbrev-commit'
#alias gg="git log --graph --pretty=format:'%C(bold)%h%Creset%C(magenta)%d%Creset %s %C(yellow)<%an> %C(cyan)(%cr)%Creset' --abbrev-commit --date=relative"
#alias gsl="git shortlog -sn"
#alias gcount='git shortlog -sn'
#alias gnew="git log HEAD@{1}..HEAD@{0}"

# PULL
#alias gl='git pull'
#alias glum='git pull upstream master'
#alias gpp='git pull && git push'
#alias gpr='git pull --rebase'

# MERGE
#alias gm="git merge"

# PUSH
#alias gp='git push'
#alias gpo='git push origin'
#alias gpom='git push origin master'
#alias gpu='git push --set-upstream'

# REMOTE
#alias gr='git remote'
#alias gra='git remote add'
#alias grv='git remote -v'

# STATUS
#alias gs='git status -sb'
#alias gs='git status'
#alias gss='git status -s'
#alias gst='git status'

# TAG
#alias gt="git tag"
#alias gta="git tag -a"
#alias gtd="git tag -d"
#alias gtl="git tag -l"

#alias gcp='git cherry-pick'
#alias gexport='git archive --format zip --output'
#alias ggs="gg --stat"
#alias ggui="git gui"
#alias gl='git lg -15'
#alias d='git lgd -15'
#alias gll='git lg'
#alias gclean='git clean -fd'
#alias gpristine='git reset --hard && git clean -dfx'
#alias grm='git rm'
#alias gsu='git submodule update --init --recursive'
#alias gus='git reset HEAD'
