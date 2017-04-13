#!/usr/bin/env bash
#
# ~/.bashrc
#
# Bash File Testing (main bits)
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
# TODO 
# separate vimrc and init.vim files in that bash function 
# {{{
[ -z "$PS1" ] && return   #  If not running interactively, don't do anything

[[ $- != *i* ]] && return   #  If not running interactively, don't do anything

# }}}

# set variables to produce colored output later {{{
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

unset MAILCHECK                         # Don't check mail when opening terminal.
export SHORT_HOSTNAME=$(hostname -s)    # Set Xterm/screen/Tmux title with only a short hostname

export HISTSIZE=20000
export HISTFILESIZE=20000
export HISTCONTROL=ignoredups

# HISTIGNORE
# A colon-separated list of patterns used to decide which command lines should be saved on the history list.  
# It must match the complete line (no implicit `*' is appended). 
# The pattern is tested against the line after the checks specified by HISTCONTROL are applied. 
# In addition to the normal shell pattern matching characters, `&' matches the previous  history  line. 
# The pattern  matching honors the setting of the extglob shell option.
export HISTIGNORE="&:[ ]*:exit:ls:bg:fg:history:clear:jobs" # a colon separated list of items to ignore  }}}

# $IRC_CLIENT default to irssi and fall back on hexchat {{{
[ -x /usr/bin/irssi ] && export IRC_CLIENT='irssi'
[ ! -x /usr/bin/irssi ] && [ -x /usr/bin/hexchat ] && export IRC_CLIENT='hexchat' # }}}

export GREP_COLOR='1;33'

# LESS {{{

#   -M (-M or --LONG-PROMPT) Prompt very verbosely
#   -I (-I or --IGNORE-CASE) Searches with '/' ignore case
#   -R (-R or --RAW-CONTROL-CHARS) For handling ANSI colors
#   -F (-F or --quit-if-one-screen) Auto exit if <1 screen
#   -X (-X or --no-init) Disable termcap init & deinit

# if available enable syntax highlighting # fall back on more if less not available
[ -f /usr/bin/source-highlight-esc.sh ] && export LESSOPEN="| /usr/bin/source-highlight-esc.sh %s"
[ -x /usr/bin/less ] && alias less='less -x4RFsX' && export PAGER=less
[ ! -x /usr/bin/less ] && [ -x /usr/bin/more ] && export PAGER=more && alias less=more
# }}}

# REALINE aka INPUTRC # generate if not present and add configuration {{{
if [ ! -f ~/.inputrc ] ; then 
    cat /etc/inputrc >> ~/.inputrc
    echo "set expand-tilde on" >> ~/.inputrc
    echo "set skip-completed-text on" >> ~/.inputrc
    echo "set echo-control-characters off" >> ~/.inputrc
    echo "set completion-query-items 250" >> ~/.inputrc
    echo "set page-completions off" >> ~/.inputrc
    echo "set mark-symlinked-directories on" >> ~/.inputrc
    echo "set bell-style none " >> ~/.inputrc
    echo "set colored-stats on" >> ~/.inputrc
    echo "set show-all-if-ambiguous on" >> ~/.inputrc
    echo "set show-all-if-unmodified on" >> ~/.inputrc
    echo "set colored-completion-prefix on" >> ~/.inputrc
fi
# }}}

# $PATH {{{

[ -d  /usr/lib/jvm/java-8-openjdk ] && export JAVA_HOME='/usr/lib/jvm/java-8-openjdk' && export JRE_HOME='/usr/lib/jvm/java-8-openjdk/jre'
[ -d ~/.gem/rubu/2.4.0/bin ] && export PATH=${PATH}:"/.gem/ruby/2.4.0/bin"
[ -d ~/.cargo/bin ] && export PATH=${PATH}:"/.cargo/bin"
[ -d ~/.config/composer/vendor/bin ] && export PATH=${PATH}:"/.config/composer/vendor/bin"
[ -d ~/go/bin ] && export PATH=${PATH}:"/go/bin"
[ -d ~/bin ] && export PATH=${PATH}:"/bin"

# }}}

# ranger {{{

if [ ! -x /usr/bin/ranger ] ; then 
    [ ! -f /tmp/nl253/ranger/ranger.py ] && mkdir -p /tmp/nl253/ranger && git clone 'https://github.com/ranger/ranger' /tmp/nl253/ranger 
    alias ranger='/tmp/nl253/ranger/ranger.py'
    alias r='/tmp/nl253/ranger/ranger.py'
fi
[ -x /usr/bin/ranger ] && alias r='ranger'
[ -f ~/.config/ranger/rc.conf ] && export RANGER_LOAD_DEFAULT_RC=false 
# }}}

# $EDITOR 

if [ -x /usr/bin/nvim ]; then # if neovim 
    export EDITOR=/usr/bin/nvim
    alias vim=/usr/bin/nvim
    alias vi=/usr/bin/nvim

    # set up vim plugins
    if [ ! -f ~/.local/share/nvim/site/autoload/plug.vim ] ; then 
	curl -fLo ~/.local/share/nvim/site/autoload/plug.vim --create-dirs \
	    https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
    fi

else # if not neovim check if vim
    if [ -x /usr/bin/vim ]; then # if vim but not neovim
	export EDITOR=/usr/bin/vim
	alias nvim=/usr/bin/vim
	alias vi=/usr/bin/vim
    # set up vim plugins
    if [ ! -f ~/.local/share/nvim/site/autoload/plug.vim ] ; then 
	curl -fLo ~/.vim/autoload/plug.vim --create-dirs \
	    https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
    fi

    elif [ -x /usr/bin/vi ] ; then # if not neovim and not vim then fall back on vi
	export EDITOR=/usr/bin/vi
	alias vim=vi
	alias nvim=vi
    fi
fi

# generate vimrc and add all the necessary configuration

# if [ ! -f ~/.vimrc ] && [ ! -f ~/.config/nvim/init.vim ]; then 
    # make a dotfile if missing
    # use BOTH init.vim and vimrc with the majority being stored in init.vim and vim sourcing it later
    # touch ~/.vimrc
    # mkdir -p ~/.config/nvim 
    # touch ~/.config/nvim/init.vim
    # echo 'set clipboard=unnamed,unnamedplus path+=~/**' >> ~/.config/nvim/init.vim
    # echo 'let loaded_matchit = 1' >>  ~/.config/nvim/init.vim # loads addtional % functionality
    # echo 'let mapleader = " "' >> ~/.config/nvim/init.vim
    # echo 'set grepformat=%f:%l:%c:%m,%f:%l:%m' >> ~/.vimrc
   # [ -d /usr/share/dict/ ] && [ ! ~/.vim-dict ] && cat /usr/share/dict/* | sort | uniq > ~/.vim-dict && echo 'set dictionary=~/.vim-dict' >> ~/.vimrc

if [ -x /usr/bin/fzf ]; then # {{{
    alias p=FZFpkill
    export FZF_DEFAULT_OPTS='--reverse --color hl:117,hl+:1,bg+:232,fg:240,fg+:246 '
    [ -x "/usr/bin/ag" ] && export FZF_DEFAULT_COMMAND='ag --hidden --ignore .git -g ""'
    alias recent-files='find ~ -cmin -10 -type f 2>/dev/null | grep -P -v ".*C|cache.*" | grep -v chrome | grep -v ".dropbox" | grep -v "%" | fzf '
else
    alias recent-files='find ~ -cmin -10 -type f 2>/dev/null | grep -P -v ".*C|cache.*" | grep -v chrome | grep -v ".dropbox" | grep -v "%"'
fi

# }}}

# pacman {{{
if [ -x /usr/bin/pacman ]; then
    [ -x /usr/bin/expac ] && alias pacman-recent-installations="expac --timefmt='%Y-%m-%d %T' '%l\t%n' | sort | tail -n 20"
    [ -x /usr/bin/expac ] && alias pacman-packages-by-size="expac -S -H M '%k\t%n'"
    alias pacman-reinstall-all-native-packages="sudo pacman -Qnq | pacman -S -"
    alias pacman-reinstall-all-foreign-packages="sudo pacman -Qmq | pacman -S -"
    alias pacman-remove-orphans="sudo pacman -Rns $(pacman -Qtdq)"
    [ -x /usr/bin/yaourt ] && export YAOURT_COLORS="nb=1:pkg=1:ver=1;32:lver=1;45:installed=1;42:grp=1;34:od=1;41;5:votes=1;44:dsc=0:other=1;35"
fi
# }}}

# ZSH {{{
[ -x /usr/bin/zsh ] && alias z=zsh
alias x=xonsh
# }}}

# ---------
#  GIT {{{
# ---------
[ -x /usr/bin/hub ] && eval "$(hub alias -s)" && alias g=hub
[ -x /usr/bin/tig ] && alias t=tig

# }}}

# Caps Lock is Control on a GB keyboard #setxkbmap -option ctrl:swapcaps # for US
setxkbmap -layout gb -option ctrl:nocaps && echo -e "${MAGENTA}capslock remapped to ctrl${DEFCOLOR}" 

[ -x "/usr/bin/ag" ] && alias ag='ag --hidden --pager="less -MIRFX"'  # search with dotfiles

# =======
# ALIASES
# =======  {{{ 

alias e=$EDITOR
alias l='ls -CFa'
alias le="ls -lo"
alias ll='ls -l -a --group-directories-first --time-style=+"%d.%m.%Y %H:%M" --color=auto -F'
alias ls='LC_COLLATE=C ls --color=auto --group-directories-first'

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
alias symlinks-pretty='for i in $(find -type l -exec echo {} \;); do echo -e " \e[36m$i  \e[39m->  \e[91m$(readlink -f $i)" ; done'

[ ! -x /usr/bin/tree ] && alias tree="find . -print | sed -e 's;[^/]*/;|____;g;s;____|; |;g'"
[ -x /usr/bin/sshfs ] && alias mount-raptor="sshfs -o transform_symlinks -o follow_symlinks nl253@raptor.kent.ac.uk: ~/Raptor"
[ -x /usr/bin/dmenu_run ] && alias dmenu_run="dmenu_run -p ' >> ' -nb black -nf white"
[ -x /usr/bin/aspell ] && alias aspell="aspell -c -l en_GB"
[ -x /usr/bin/aria2c ] && alias aria2c="mkdir -p \"${HOME}/Downloads/Torrents/\" ; touch \"${HOME}/Downloads/Torrents/aria2.log\" ; aria2c --continue --dir=\"${HOME}/Downloads/Torrents\" --log=\"${HOME}/Downloads/Torrents/aria2.log\""

alias df='df --human-readable --si'
alias info='info --vi-keys'
alias freq='cut -f1 -d" " "$HISTFILE" | sort | uniq -c | sort -nr | head -n 30'
alias logout="pkill -KILL -u "
#alias j=jobs # used by autojump
alias untar='tar -xvf'

alias show-scripts-in-bashrc="grep -P '^\S+?\(\)' ~/.bashrc | sed  's/(//g' | sed 's/{//' | sed 's/)//g'"
#
# Readline
alias show-keybingings="bind -p | grep -v '^#\|self-insert\|^$'"

alias f="find-extension"

alias http-server="python3 -m http.server"

# =============== }}}
# SCRIPTS
#
# NOTE
# coreutils programs will not be considered dependencies
# as they are preinstalled on practically every UNIX system

# ===============
# FUNCTION :: aliases for multi-word tig commands
# REQUIRES :: tig {{{
tis(){
    tig status
}

til(){
    tig log
}

tib(){
    tig blame -C
}
# ======== }}}
# FUNCTION :: restore the system {{{
# The aim of the script is to do nothing when the system is OK
# and restore the whole system when it's just been reinstalled.
#

restore-system(){

[ ! -x /usr/bin/pacman ] && echo -e "This script is preconfigured ONLY for Arch Linux."

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
    "xclip" "bashlint" "alsa-utils" \
    "curl" "dos2unix" "pdftotext" \
    "perl" "shellcheck" "zsh")

for i in ${NEED_TO_BE_INSTALLED[*]}; do
    [ ! -x "/usr/bin/$i" ] && sudo pacman -S --quiet  --noconfirm --needed "$i"
done

echo -e "checking PACMAN packages"
for i in ${NEED_TO_BE_INSTALLED[*]}; do
    # check and give feedback on what's missing
    [ ! -x "$(which $i)" ] && echo -e "$i :: not found on the filesystem despite installation"
done

# PYTHON
# it is crucial that python is correctly set up
# mackup is dependent on it as well as things like ranger

echo -e "PYTHON"

if [ ! -x "/usr/bin/pip" ] && [ ! -x "/usr/bin/pip3" ] ; then
    echo -e "PIP and PYTHON are necessary to make this work.\nThe script will terminate, \nmake sure pip is installed to proceed"
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

for i in ${PY[*]}; do
    # configuration for scripts
    # if it exists, ignore
    sudo pip install --quiet --exists-action i "$i"
done

echo -e "checking PIP packages"

for i in ${PY[*]}; do
    [ ! -x "$(which $i)" ] && echo -e "$i :: not present on the system"
done

# python virtual env
echo -e "checking virtual env"

if [ ! -d ~/.pyenv ] && [ ! -x "$(which pyenv)" ]; then
    echo -e "PYENV not detected\ninitiating ... "
    git clone "https://github.com/pyenv/pyenv.git" ~/.pyenv
    pyenv global "3.5.0"
    echo -e 'global python 3.5.0 activated'
else
    echo -e "PYENV detected continuing ..."
fi

echo -e "RUBY"

# ruby
if [ ! -x "/usr/bin/gem" ] || [ ! -x "/usr/bin/ruby" ] ; then

    echo -e "either RUBY or GEM was not detected on this filesystem"
    echo -e "make sure GEM is installed to install RUBY packages"
    echo -e "becasue RUBY is not cructial the script will continue"
    sleep 5

else

    echo -e "RUBY and GEM detected\ninstalling RUBY gems"

    local RB=(mdl sqlint rubocop)

    for i in ${RB[*]}; do
	[ ! -x "$(which $i)" ] && sudo gem install "$i"
    done

    # check and give feedback on what's missing
    echo -e "checking RUBY gems"

    for i in ${RB[*]}; do
	[ ! -x "$(which $i)" ] && echo -e "$i :: not present on the system"
    done

fi

# javascript
if [ ! -x /usr/bin/npm ] ; then

    echo -e "NPM not deteceted on the filesystem\nmake sure NPM is installed to install JAVSCRIPT packages"
    echo -e "because NPM and JAVASCRIPT aren't crucial, the script will continue "
    sleep 5

else

    echo -e "NODE and NPM detected\ninstalling NODE packages"

    local JS=("write-good" textlint \
	"git-standup" "git-stats" \
	jsonlint tern "git-fire" \
	"js-beautify" textlint)

    for i in ${JS[*]}; do
	if [ ! -x "$(which $i)" ]; then
	    sudo npm install "$i" -g
	fi
    done

fi

# check if ZSH is set up correctly
if [ -x /usr/bin/zsh ] ; then

    echo -e 'zsh detected on your filesystem ... '

    if  [ ! -f "${HOME}/.oh-my-zsh/oh-my-zsh.sh" ] ; then

	echo -e "OH-MY-ZSH not detected\ninitiating ..."

	# from oh-my-zsh [github]
	sh -c "$(curl -fsSL https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"

	# custom plugins

	[ ! -d ~/.oh-my-zsh/custom/plugins/zsh-autosuggestions ] && git clone "git://github.com/zsh-users/zsh-autosuggestions" ~/.oh-my-zsh/custom/plugins/zsh-autosuggestions
	[ ! -d ~/.oh-my-zsh/custom/plugins/zsh-completions ] && git clone "https://github.com/zsh-users/zsh-completions" ~/.oh-my-zsh/custom/plugins/zsh-completions
	[ ! -d ~/.oh-my-zsh/custom/plugins/zsh-syntax-highlighting ] && git clone "https://github.com/zsh-users/zsh-syntax-highlighting.git" ~/.oh-my-zsh/custom/plugins/zsh-syntax-highlighting

	# source it
	zsh -c ~/.zshrc

    fi

fi

# if ALACRITTY is not set up rust will be needed
if [ ! -x /usr/bin/alacritty ] && [ ! -x /usr/bin/rustup ]  && [ ! -x /usr/bin/rustup ] ; then
    echo -e "RUST and ALACRITTY not installed\ninitalising rustup"
    sudo curl https://sh.rustup.rs -sSf | sh
    echo -e "cloning ALACRITTY repo from GIT"
    git clone https://github.com/jwilm/alacritty.git ~
    echo -e "changing RUSTUP TOOLCHAIN to STABLE"
    sudo rustup override set stable
    sudo rustup update stable
    sudo rustup default stable
    echo -e "cd to ${HOME}/alacritty\nBUILDING ALACRITTY ... "
    cd ~/alacritty && sudo cargo build --release
fi

# NEO-VIM
# I chose the plugins dir to check if nvim is correctly set up, if not - clone it
if [ -x /usr/bin/nvim ] && [ ! -f "${HOME}/.local/share/nvim/site/autoload/plug.vim" ]; then
    echo -e "NEOVIM not initalised with vim-plug\ncloning from GIT"
    # use tmp to force-write the files

    mkdir -p /tmp/nvim/
    git clone --recursive "https://github.com/nl253/VimScript" "/tmp/nvim/"

    # git won't let you overwrite anything - use cp
    cp -R /tmp/nvim/* ~/.config/nvim/

    # from vim-plug [github]
    curl -fLo ~/.local/share/nvim/site/autoload/plug.vim --create-dirs "https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim"

    # here the init.vim file will be sourced to initialise the whole setup
    # nvim -c "~/.config/nvim/init.vim"
    # not sure if it's a good idea ...

elif [ ! -x "/usr/bin/nvim" ] ; then

    echo -e "neovim not detected on the filesystem.\n the installation will continue\nbut you will have to make sure neovim is installed to use plugins"

fi

# TMUX
# check if tmux plugin manager dir is present
if [ -x /usr/bin/tmux ] && [ ! -d ~/.tmux/plugins/tpm ] ; then

    echo -e "TMUX detected but TMUX PLUGIN MANAGER not present\ninitalising ..."
    git clone "https://github.com/tmux-plugins/tpm" ~/.tmux/plugins/tpm

else

    echo -e "tmux was not detected on this filesystem\nthe script will continue\nyou will need to make sure tmux is installed"

    sleep 5

fi

# at this point variables will need to be reset
echo "RESOURCING BASHRC"
source ~/.bashrc

if [ -x /usr/bin/dropbox ] && [ -d ~/Dropbox ] ; then

    if [ -x $(which mackup) ] && [ -L ~/.bashrc ] && [ -L ~/.inputrc ] ; then

	mackup restore

    else

	echo -e "you need to set up MACKUP.\nquitting."
	return 1

    fi

elif [ ! -x "/usr/bin/dropbox" ] && [ ! -d ~/Dropbox ] ; then

    echo -e "make sure DROPBOX is set up"
    return 1

fi

}

# ============================== }}}
# FUNCTION {{{
# print in a JSON format a dict with your IP
# and other details about the network you are on
show-ip(){
if grep -P "(([1-9]\d{0,2})\.){3}(?2)" <<< "$1"; then
    curl ipinfo.io/"$1"
else
    ipawk=($(hostname "$1" | awk '/address/ { print $NF }'))
    curl ipinfo.io/${ipawk[1]}
fi
}
# ============================== }}}
# FUNCTION  :: prints the 256 colors with their corresponding numbers {{{
show-colors(){
(x=`tput op` y=`printf %76s`;for i in {0..256};do o=00$i;echo -e ${o:${#o}-3:3} `tput setaf $i;tput setab $i`${y// /=}$x;done)
}
# ============================== }}}
# FUNCTION  :: shows terminal capabilities {{{
show-term-capabilities(){
infocmp -1 | sed -nu 's/^[ \000\t]*//;s/[ \000\t]*$//;/[^ \t\000]\{1,\}/!d;/acsc/d;s/=.*,//p'|column -c80
}
# ============================== }}}
# FUNCTION  :: general purpose archive extracting {{{
# USAGE: ex <file>
# REQUIRES :: pygmentize (can be optional) :: ag for find-shell
ex (){
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

# ============================== }}}

# stty -ixon              # enable inc search <C-s> which is often disabled by terminal emulators

# make sure zsh isn't able to source it {{{
if [ ! -n "${ZSH+2}" ]; then
    complete -cf sudo
    shopt -s autocd        
    shopt -s cdspell        # correct minor spelling errors
    shopt -s checkwinsize   # update the value of LINES and COLUMNS after each command if altered
    shopt -s direxpand      # replaces directory names with expansion when <tab>
    shopt -s dirspell       # correct minor spelling errors
    shopt -s dotglob        # Include dotfiles in pathname expansion
    shopt -s checkjobs      # Include dotfiles in pathname expansion
    shopt -s extglob        # Enable extended pattern-matching features
    shopt -s nullglob        
    shopt -s globstar       # ** becomes a recursive wildstar
    shopt -s histappend     # Append each session's history to $HISTFILE
    shopt -s histverify     # Edit a recalled history line before executing
fi
# }}}

