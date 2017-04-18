#!/usr/bin/env bash
#
# ~/.bashrc
#
# {{{ If not running interactively, don't do anything
[ -z "$PS1" ] && return
[[ $- != *i* ]] && return
# }}}

# $COLORS {{{
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

PYTHON="${YELLOW}PYTHON${DEFCOLOR}"
HASKELL="${BLUE}HASKELL${DEFCOLOR}"
RUBY="${RED}RUBY${DEFCOLOR}"
JAVSCRIPT="${DARKYELLOW}JAVSCRIPT${DEFCOLOR}"

# }}}

echo -e "${RED}~/.bashrc ${YELLOW}loaded" # indicator if it has successfully loaded

# $PS1 {{{

# default (non-git) prompt
export PS1="\n\[$(tput bold)\]\[$(tput sgr0)\]\[\033[38;5;3m\]\u\[$(tput sgr0)\]\[$(tput sgr0)\]\[\033[38;5;40m\]@\[$(tput bold)\]\[$(tput sgr0)\]\[\033[38;5;31m\]\h\[$(tput sgr0)\]\[$(tput sgr0)\]\[\033[38;5;15m\] \n\[$(tput sgr0)\]\[\033[38;5;241m\]\w\[$(tput sgr0)\]\[\033[38;5;15m\] \[$(tput bold)\]\[$(tput sgr0)\]\[\033[38;5;88m\]>\[$(tput sgr0)\]\[\033[38;5;89m\]>\[$(tput sgr0)\]\[$(tput sgr0)\]\[\033[38;5;15m\] \[$(tput sgr0)\]"

export TERM=xterm-256color
unset MAILCHECK                      # Don't check mail when opening terminal.
export SHORT_HOSTNAME=$(hostname -s) # Set Xterm/screen/Tmux title with only a short hostname

# $PATH (and JAVA_HOME and JRE_HOME) {{{
# ------------------------------------------------------------------------
# FUNCTION :: add packages from all package managers to path if these paths exist along with my own scripts in ~/Scripts/
# ------------------------------------------------------------------------
[ -f ~/.config/ranger/rc.conf ] && export RANGER_LOAD_DEFAULT_RC=false
[ -d /usr/lib/jvm/java-8-openjdk ] && export JAVA_HOME='/usr/lib/jvm/java-8-openjdk' && export JRE_HOME='/usr/lib/jvm/java-8-openjdk/jre'
[ -d ~/.gem/rubu/2.4.0/bin ] && export PATH=${PATH}:"~/.gem/ruby/2.4.0/bin"
[ -d ~/.gem/rubu/2.3.0/bin ] && export PATH=${PATH}:"~/.gem/ruby/2.3.0/bin"
[ -d ~/.cargo/bin ] && export PATH=${PATH}:"~/.cargo/bin"
[ -d ~/.cabal/bin ] && export PATH="$HOME/.cabal/bin:$PATH"
[ -d ~/.config/composer/vendor/bin ] && export PATH=${PATH}:"~/.config/composer/vendor/bin"
[ -d ~/go/bin ] && export PATH=${PATH}:"~/go/bin"
[ ! -e ~/Scripts ] && mkdir -p ~/Scripts/ && git clone https://github.com/nl253/Scripts ~/Scripts/ # clone my Scripts repo 
[ -d ~/Scripts/ ] && export PATH="${PATH}:~/Scripts"
# }}}

# HISTORY {{{
export HISTSIZE=20000
export HISTFILESIZE=20000
export HISTCONTROL="ignoreboth:erasedups"
export HISTTIMEFORMAT='%s'

source_bash_completion() {
  local f
  [[ $BASH_COMPLETION ]] && return 0
  for f in /{etc,usr/share/bash-completion}/bash_completion; do
    if [[ -r $f ]]; then
      . "$f"
      return 0
    fi
  done
}
source_bash_completion

# enable bash completion in interactive shells
if ! shopt -oq posix; then
  if [ -f /usr/share/bash-completion/bash_completion ]; then
    . /usr/share/bash-completion/bash_completion
  elif [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
  fi
fi

# HISTIGNORE
# ----------------------------------------------------------------------------------------------------------
# A colon-separated list of patterns used to decide which command lines should be saved on the history list.
# It must match the complete line (no implicit `*' is appended).
# The pattern is tested against the line after the checks specified by HISTCONTROL are applied.
# In addition to the normal shell pattern matching characters, `&' matches the previous  history  line.
# The pattern  matching honors the setting of the extglob shell option.
# ----------------------------------------------------------------------------------------------------------
export HISTIGNORE="&:[ ]*:exit:cd:ls:bg:fg:history:clear:jobs" # }}}

# $IRC_CLIENT  {{{
# default to irssi and fall back on hexchat
[ -x /usr/bin/irssi ] && export IRC_CLIENT='irssi'
[ ! -x /usr/bin/irssi ] && [ -x /usr/bin/hexchat ] && export IRC_CLIENT='hexchat' # }}}

export GREP_COLOR='1;33' # makes it yellow # by default red

# $PAGER {{{
# ------------------------------------------------------------------
# if available enable syntax highlighting # fall back on more if less not available
# tries to set default pager as less and add coloring to the output if possible
# falls back on more if available
# ------------------------------------------------------------------
[ -f /usr/bin/source-highlight-esc.sh ] && export LESSOPEN="| /usr/bin/source-highlight-esc.sh %s"
[ -x /usr/bin/less ] && alias less='less -x4RFsX' && export PAGER=less
[ ! -x /usr/bin/less ] && [ -x /usr/bin/more ] && export PAGER=more && alias less=more
# }}}

# $BROWSER {{{
# -------------------------------------------------
# uses google chrome if available ie if running on a gui 
# fall back on 1. elinks 2. lynx 3. w3m
# -------------------------------------------------
if [ -x /usr/bin/google-chrome-stable ]; then
  export BROWSER=google-chrome-stable
elif [ -x /usr/bin/elinks ]; then
  export BROWSER=elinks
elif [ -x /usr/bin/lynx ]; then
  export BROWSER=lynx
elif [ -x /usr/bin/w3m ]; then
  export BROWSER=w3m
fi
# }}}

# $EDITOR  {{{
# ---------------------------------------------------------------------- 
# attempt to set to neo-vim if available, fall back on vim and then vi
# ----------------------------------------------------------------------
if [ -x /usr/bin/nvim ]; then # if neovim
  export EDITOR=/usr/bin/nvim
  alias vim=/usr/bin/nvim
  alias vi=/usr/bin/nvim

elif [ -x /usr/bin/vim ]; then # if vim but not neovim
  export EDITOR=/usr/bin/vim
  alias nvim=/usr/bin/vim
  alias vi=/usr/bin/vim
  # set up vim plugins
elif [ -x /usr/bin/vi ]; then # if not neovim and not vim then fall back on vi
  export EDITOR=/usr/bin/vi
  alias vim=vi
  alias nvim=vi
fi
# }}}

# {{{ FZF init # chech if on system # set up aliases in case it is and isn't
if [ -x /usr/bin/fzf ]; then
  export FZF_DEFAULT_OPTS="--bind='alt-e:execute($EDITOR {}),alt-r:execute([ -x/usr/bin/rifle ] && rifle {} || [ -x /usr/bin/mc ] && mc {}),ctrl-d:half-page-down,ctrl-u:half-page-up,alt-p:toggle-preview' --no-mouse --multi --black --margin 3% --prompt=' >> ' --reverse --tiebreak=end,length --color 'hl:117,hl+:1,bg+:232,fg:240,fg+:246'"
  alias fzfp='fzf --preview="[ -f {} ] && head -n 38 {} || tree -l -a --prune -L 4 -F --sort=mtime {}"'
  export FZF_DEFAULT_COMMAND='
        (git ls-tree -r --name-only HEAD ||
          find . -path "*/\.*" -prune -o -type d -print -type f -print -o -type l -print |
        sed s/^..//) 2> /dev/null'
  alias l=fzf-locate.sh
  alias fh=fzf-search-home.sh               # [F]IND [H]OME
  alias c=fzf-cd.sh                         # [C]D
  alias gc=fzf-commits.sh                   # [G]IT [L]OG
  alias gcb=fzf-checkout-branches-sorted.sh # [G]IT [C]HECKOUT [B]RANCHES
  alias gcc=fzf-checkout-commit.sh          # [G]IT [C]HECKOUT
  alias gcs=fzf-commit-sha.sh               # [G]IT [C]OMMIT [S]HA
  alias gcbt=fzf-checkout-branch-tag.sh     # [G]IT [C]HECKOUT [B]RANCH [T]AG
  alias gt=fzf-search-tags.sh               # [G]IT [T]AGS
  alias gs=fzf-stash.sh                     # [G]IT [S]TASH
  # [L]IST [R]ECENT
  [ -x /usr/bin/gdrive ] && alias gdrive-fzf='gdrive list | fzf --bind "enter:execute(echo {} | grep -P -o \"^\w+\")"'
else # non fzf solution
  [ -x /usr/bin/htop ] && alias p=htop || alias p=top # process management
  # alias gc=  # TODO provide an alternative if fzf is not available
  # alias gs=  # TODO provide an alternative if fzf is not available
  # alias gcs=  # TODO provide an alternative if fzf is not available
  # alias gt=  # TODO provide an alternative if fzf is not available
  # alias c=  # TODO provide an alternative if fzf is not available
  # alias gcb=  # TODO provide an alternative if fzf is not available
  alias gl='git log --pretty=format:"%C(yellow)%h  %Cblue%ad  %Creset%s%Cgreen  [%cn] %Cred%d" --decorate --date=relative'
fi

# }}}

# pacman aliases, yaourt colors {{{
# -----------------------------------
# REQUIRES :: pacman yaourt expac 
# -----------------------------------
if [ -x /usr/bin/pacman ]; then
  [ -x /usr/bin/expac ] && alias pacman-recent-installations="expac --timefmt='%Y-%m-%d %T' '%l\t%n' | sort | tail -n 20"
  [ -x /usr/bin/expac ] && alias pacman-packages-by-size="expac -S -H M '%k\t%n'"
  alias pacman-reinstall-all-native-packages="sudo pacman -Qnq | pacman -S -"
  alias pacman-reinstall-all-foreign-packages="sudo pacman -Qmq | pacman -S -"
  alias pacman-remove-orphans="sudo pacman -Rns $(pacman -Qtdq)"
  [ -x /usr/bin/yaourt ] && export YAOURT_COLORS="nb=1:pkg=1:ver=1;32:lver=1;45:installed=1;42:grp=1;34:od=1;41;5:votes=1;44:dsc=0:other=1;35"
fi # }}}

[ -x /usr/bin/hub ] && eval "$(hub alias -s)" && alias g=hub
[ -x /usr/bin/tig ] && alias t=tig

[ -x "/usr/bin/ag" ] && alias ag='ag --hidden --pager="less -MIRFX"' # search with dotfiles page to less with colors

# ALIASES {{{
env() { if [ ! $# = 0 ]; then command env $@; else command env | sort; fi; } # by default if no args provided sort env output
alias timer='echo "Timer started. Stop with Ctrl-D." && date && time cat && date' # stopwatch
alias sudo='sudo '                                                                # Enable aliases to be sudo’ed
alias e="$EDITOR"
alias todo="git grep -n --word-regexp --break --recurse-submodules --heading TODO"
alias path='echo -e ${PATH//:/\\n}' # split path on ":"
alias x=xonsh
[ -x /usr/bin/zsh ] && alias z=zsh
if [ -x /usr/bin/ranger ]; then
  alias r='ranger'
elif [ -x ~/.ranger/ranger.py ]; then
  alias ranger="${HOME}/.ranger/ranger.py"
  alias r="${HOME}/.ranger/ranger.py"
fi

alias map-caps-to-esc='xmodmap -e "clear lock"; xmodmap -e "keycode 0x42 = Escape"'
alias unmap-caps-from-esc='xmodmap -e "keycode 0x42 = Caps_Lock"; xmodmap -e "add lock = Caps_Lock"'
alias map-caps-lock-to-ctrl='setxkbmap -layout gb -option ctrl:nocaps && echo -e "${MAGENTA}capslock remapped to ctrl${DEFCOLOR}"'

alias le="ls -lo" # list everything 
alias ll='ls -l -a --group-directories-first --time-style=+"%d.%m.%Y %H:%M" --color=auto -F' # long listing
alias ls='LC_COLLATE=C ls --color=auto --group-directories-first' # tweak default ls 
alias lr=recent-files.sh # list recent
alias f=find-approx.sh

alias -- -='cd -' # Go back
alias ..="cd .."
alias ...="cd ../.."
alias ....="cd ../../.."
alias diff='diff --color=auto'
alias grep='grep --color=auto'
alias fgrep='fgrep --color=auto'
alias egrep='egrep --color=auto'
[ ! -x /usr/bin/tree ] && alias tree="find . -print | sed -e 's;[^/]*/;|____;g;s;____|; |;g'" # in case tree is not present on the system
[ -x /usr/bin/dmenu_run ] && alias dmenu_run="dmenu_run -p ' >> ' -nb black -nf white" # dmenu # a good alternative to rofi # this modifies the prompt and coloring
[ -x /usr/bin/aspell ] && alias aspell="aspell -c -l en_GB" 
alias df='df --human-readable --si'
alias info='info --vi-keys'
# set up logging in ~/Downloads/Torrents/aria2c.log and a default location for download of Torrents :: ~/Downloads/Torrents/
[ -x /usr/bin/aria2c ] && alias aria2c="mkdir -p ${HOME}/Downloads/Torrents/ ; touch ${HOME}/Downloads/Torrents/aria2c.log ; aria2c --continue --dir=${HOME}/Downloads/Torrents --log=${HOME}/Downloads/Torrents/aria2c.log"
alias freq='cut -f1 -d" " "$HISTFILE" | sort | uniq -c | sort -nr | head -n 30' # frequent entries from history
alias logout="pkill -KILL -u " 
 # shows links that don't point to anything
alias symlinks-pretty='for i in $(find -type l 2>/dev/null | sed -E "s/^\.\///" ); do echo -e " \e[36m$i \e[39m-> \e[91m$(readlink -f $i)" ; done' 
alias symlinks='find -type l 2>/dev/null | sed -E "s/^\.\///"' # list symlinks recursively from CWD
alias dirs='find . -type d 2>/dev/null | sed -E "s/^\.\///"' # list recursively just dirs 
alias show-term-capabilities="infocmp -1 | sed -nu 's/^[ \000\t]*//;s/[ \000\t]*$//;/[^ \t\000]\{1,\}/!d;/acsc/d;s/=.*,//p'|column -c80"
alias keybingings="bind -p | grep -v '^#\|self-insert\|^$'" # keybingings for readline
alias http-server="python3 -m http.server" # open using 0.0.0.0:{PORT}
[ -x /usr/bin/sshfs ] && alias mount-raptor="sshfs -o transform_symlinks -o follow_symlinks nl253@raptor.kent.ac.uk: ~/Raptor" # mount a remote hard-drive
# }}}

stty -ixon    # enable inc search <C-s> which is often disabled by terminal emulators
stty -ctlecho # turn off control character echoing
complete -cf sudo
complete -d cd
complete -d cd pushd

shopt -s autocd
shopt -s cdspell      # correct minor spelling errors
shopt -s checkwinsize # update the value of LINES and COLUMNS after each command if altered
shopt -s direxpand    # replaces directory names with expansion when <tab>
shopt -s dirspell     # correct minor spelling errors
shopt -s dotglob      # Include dotfiles in pathname expansion
shopt -s checkjobs    # Bash lists the status of any stopped and running jobs before exiting an interactive shell. If any jobs are running, this causes the exit to be deferred until a second exit is attempted
shopt -s extglob      # Enable extended pattern-matching features
shopt -s nullglob
shopt -s nocaseglob # matches filenames in a case-insensitive fashion when performing pathname expansion.

shopt -s globstar      # ** becomes a recursive wildstar
shopt -s histappend    # Append each session's history to $HISTFILE
shopt -s histverify    # History expansions will be verified before execution.
shopt -s histreedit    # Allow use to re-edit a faild history substitution.
bind Space:magic-space # Expand "!" history when pressing space
