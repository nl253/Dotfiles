#
# ~/.bashrc
#
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
# -b filename - Block special file
# -S filename - Check if file is socket
# -s filename - Check if file is nonzero size
# -c filename - Special character file
# 
# -O filename - True if file exists and is owned by the effective user id
# -G filename set-group-id - True if file exists and is set-group-id
# -u filename - Check if file set-user-id bit is set
# -k filename - Sticky bit

# [ -z "$PS1" ] && return

#  If not running interactively, don't do anything
[[ $- != *i* ]] && return

echo "~/.bashrc loaded"

#[[ -f ~/.extend.bashrc ]] && . ~/.extend.bashrc

export PS1="$(tput setaf 1)\w\n\[$(tput bold)\]\[$(tput setaf 1)\][\[$(tput setaf 3)\]\u\[$(tput setaf 2)\]@\[$(tput setaf 4)\]\h\[$(tput setaf 5)\]\[$(tput setaf 1)\]]\[$(tput setaf 7)\]\\$\[$(tput sgr0)\] "

[ -f ~/.fzf.bash ] && source ~/.fzf.bash

export BASH_IT="/home/norbert/.bash_it" # Path to the bash it configuration

unset MAILCHECK                         # Don't check mail when opening terminal.

export SCM_CHECK=true                   # Set this to false to turn off version control status checking within the prompt for all themes

# Set Xterm/screen/Tmux title with only a short hostname.
# Uncomment this (or set SHORT_HOSTNAME to something else),
# Will otherwise fall back on $HOSTNAME.
#export SHORT_HOSTNAME=$(hostname -s)

if [ -f $BASH_IT/bash_it.sh ]; then
  source $BASH_IT/bash_it.sh    # Load Bash-It
fi

export HISTSIZE=20000
export HISTFILESIZE=20000
export HISTCONTROL=ignoredups
export HISTIGNORE="&:[ ]*:exit:ls:bg:fg:history:clear"

export IRC_CLIENT='irssi' 

## Summary for args to less:
# less(1)
#   -M (-M or --LONG-PROMPT) Prompt very verbosely
#   -I (-I or --IGNORE-CASE) Searches with '/' ignore case
#   -R (-R or --RAW-CONTROL-CHARS) For handling ANSI colors
#   -F (-F or --quit-if-one-screen) Auto exit if <1 screen
#   -X (-X or --no-init) Disable termcap init & deinit
#
#export LESSOPEN="| /usr/bin/src-hilite-lesspipe.sh %s"
export LESSOPEN="| /usr/bin/source-highlight-esc.sh %s"
export LESS=' -R '
export PAGER=less
export SHELL='/usr/bin/bash'

if [ -x /usr/bin/google-chrome-stable ]; then 
  export BROWSER='/usr/bin/google-chrome-stable'
fi
if [ -x /usr/bin/alacritty ]; then 
  export TERMINAL='/usr/bin/alacritty'
fi
if [ -x /usr/bin/terminix ]; then 
  export ALT_TERMINAL='/usr/bin/terminix'
fi
# Programming Languages
if [ -x /usr/bin/javac ]; then 
  export JAVA_HOME='/lib/jvm/intellij-jdk'
  export JRE_HOME='/lib/jvm/intellij-jdk/jre'
fi
if [ -x /usr/bin/ruby ]; then 
  RUBY_BINS='~/.gem/ruby/2.4.0/bin'
fi
if [ -x /usr/bin/rustc ]; then 
  RUST_BINS='~/.cargo/bin'
fi
GO_BINS='~/go/bin'
MY_BINS='~/bin'
export PATH="$PATH:$RUBY_BINS:$RUST_BINS:$MY_BINS:$GO_BINS"

export RANGER_LOAD_DEFAULT_RC=false

if [ -x /usr/bin/nvim ]; then 
  export NVIM_TUI_ENABLE_CURSOR_SHAPE=1
  export MANPAGER="nvim -c 'set ft=man' -"
  export EDITOR='/usr/bin/nvim'
  alias vim=nvim
  alias x='cd ~/.config/nvim/'
fi

if [ -x /usr/bin/fzf ]; then 
  export FZF_DEFAULT_COMMAND='ag --hidden --ignore .git -g ""'
  export FZF_DEFAULT_OPTS='--reverse --color hl:117,hl+:1,bg+:232,fg:240,fg+:246 '
  alias e=FZFedit
  alias h='__fzf_history__'
  alias p=FZFpkill
  alias c='__fzf_cd__'
fi

export GREP_COLOR='1;33'

if [ -x /usr/bin/yaourt ]; then 
  export YAOURT_COLORS="nb=1:pkg=1:ver=1;32:lver=1;45:installed=1;42:grp=1;34:od=1;41;5:votes=1;44:dsc=0:other=1;35"
fi

#alias cp="cp -i"                          # confirm before overwriting something
alias dmenu_run="dmenu_run -p ' >> ' -nb black -nf white"
alias aria2c="aria2c --dir='~/Downloads/Torrent/'"
alias aspell="aspell -c -l en_GB"
#alias cal='cal | grep -C6 "$(date +%e)"'

alias df='df -h'                          # human-readable sizes
alias info='info --vi-keys'

alias freq='cut -f1 -d" " "$HISTFILE" | sort | uniq -c | sort -nr | head -n 30'
alias logout="pkill -KILL -u "
alias j=jobs
alias t='date'
alias untar='tar xvf'

# Readline
alias show-keybingings="bind -p | grep -v '^#\|self-insert\|^$'"

show-colors()
{
  (x=`tput op` y=`printf %76s`;for i in {0..256};do o=00$i;echo -e ${o:${#o}-3:3} `tput setaf $i;tput setab $i`${y// /=}$x;done)
}

show-term-capabilities()
{
  infocmp -1 | sed -nu 's/^[ \000\t]*//;s/[ \000\t]*$//;/[^ \t\000]\{1,\}/!d;/acsc/d;s/=.*,//p'|column -c80
}

# Pacman
if [ -x /usr/bin/pacman ]; then 
  alias pacman-recent-installations="expac --timefmt='%Y-%m-%d %T' '%l\t%n' | sort | tail -n 20"
  alias pacman-packages-by-size="expac -S -H M '%k\t%n'"
  alias pacman-reinstall-all-native-packages="sudo pacman -Qnq | pacman -S -"
  alias pacman-reinstall-all-foreign-packages="sudo pacman -Qmq | pacman -S -"
  alias pacman-remove-orphans="sudo pacman -Rns $(pacman -Qtdq)"
fi

# ---------
#  Git
# ---------
export GITHUB_TOKEN='78074eacb5fee2303960025acebc88704b031fb8'
## Add uncommitted and unstaged changes to the last commit
## From http://blogs.atlassian.com/2014/10/advanced-git-aliases/
## Show commits since last pull
alias git=hub
alias g=hub

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
#alias gld='git lgd -15'
#alias gll='git lg'
#alias gclean='git clean -fd'
#alias gpristine='git reset --hard && git clean -dfx'
#alias grm='git rm'
#alias gsu='git submodule update --init --recursive'
#alias gus='git reset HEAD'

setxkbmap -layout gb -option ctrl:nocaps # Caps Lock is Control on a GB keyboard

#setxkbmap -option ctrl:swapcaps # for US

echo "capslock remapped to ctrl"

# expands bang combinations and variables to their values - remember !$ last arg / !^ first arg / !* all args
#bind Space:magic-space # also combine these with :h (head) or :t (tail) to get path selective path expansion -> !$:h

if [ -x /usr/bin/ag ]; then 
  alias ag='ag --hidden --pager="less -MIRFX"'  # search with dotfiles
fi
if [ -x /usr/bin/ranger ]; then 
  alias r='ranger'
fi
alias l='ls -CFa'
alias l='ls -CFa'
alias la="ls -a"
alias le="ls -lo"
alias ll="ls -l"
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
alias recent-files='find . -amin -10 -type f | grep -P -v ".*C|cache.*" | sed -E "s/^\.\///" | fzf'
alias symlinks-at-home="find ~ -type l 2>/dev/null"
if [ ! -x /usr/bin/ag ]; then 
  alias tree="find . -print | sed -e 's;[^/]*/;|____;g;s;____|; |;g'"
fi

# REQUIRES
# --------
# pygmentize (can be optional)
# ag for find-shell
extract()
{
  local c e i

  (($#)) || return

  for i; do
    c=''
    e=1

    if [[ ! -r $i ]]; then
      echo "$0: file is unreadable: \`$i'" >&2
      continue
    fi

    case $i in
      *.t@(gz|lz|xz|b@(2|z?(2))|a@(z|r?(.@(Z|bz?(2)|gz|lzma|xz)))))
        c=(bsdtar xvf);;
      *.7z)  c=(7z x);;
      *.Z)   c=(uncompress);;
      *.bz2) c=(bunzip2);;
      *.exe) c=(cabextract);;
      *.gz)  c=(gunzip);;
      *.rar) c=(unrar x);;
      *.xz)  c=(unxz);;
      *.zip) c=(unzip);;
      *)     echo "$0: unrecognized file extension: \`$i'" >&2
        continue;;
    esac

    command "${c[@]}" "$i"
    ((e = e || $?))
  done
  return "$e"
}
cl() 
{
  local dir="$1"
  local dir="${dir:=$HOME}"
  if [[ -d "$dir" ]]; then
    cd "$dir" >/dev/null; ls
  else
    echo "bash: cl: $dir: Directory not found"
  fi
}
find-large()
{
  if [[ $1 = "" ]]; then
    1=".*"
  fi
  find / -type f -size +6k -iregex ".*\.$1" 2>/dev/null | head -n 12
}
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
find-it()
{
  find -L . -maxdepth 1 -type f -readable -iregex ".*" -exec basename {} \; | sed -E "s/^\.\///"  | fzf --bind "enter:execute(nvim {})" --preview "head -n 40 {} | pygmentize ;"
}
find-java()
{
  find -L . -iregex ".*"$1".*\.java$" -type f  | sed -E "s/^\.\///"  | fzf --bind "enter:execute(nvim {})" --preview "head -n 40 {} | pygmentize ;"
}
find-vim()
{
  find -L . -iregex ".*"$1".*\.vim$" -type f  | sed -E "s/^\.\///"  | fzf --bind "enter:execute(nvim {})" --preview "head -n 40 {} | pygmentize ;"
}
find-python()
{
  find -L . -iregex ".*"$1".*\.py$" -type f  | sed -E "s/^\.\///"  | fzf --bind "enter:execute(nvim {})" --preview "head -n 40 {}  | pygmentize ;"
}
find-ruby()
{
  find -L . -iregex ".*"$1".*\.rb$" -type f  | sed -E "s/^\.\///"  | fzf --bind "enter:execute(nvim {})" --preview "head -n 40 {} | pygmentize ;"
}
find-markdown()
{
  find -L . -iregex ".*"$1".*\.(md)|(markdown)$" -type f  | sed -E "s/^\.\///"  | fzf --bind "enter:execute(nvim {})" --preview "head -n 40 {} | pygmentize ;"
}
find-html()
{
  find -L . -iregex ".*"$1".*\.x?html$" -type f  | sed -E "s/^\.\///"  | fzf --bind "enter:execute(nvim {})" --preview "head -n 40 {} | pygmentize ;"
}
find-javascript()
{
  find -L . -iregex ".*"$1".*\.js$" -type f  | sed -E "s/^\.\///"  | fzf   --bind "enter:execute(nvim {})" --preview "head -n 40 {} | pygmentize ;"
}
find-shell()
{
  ag --sh -g ""$1  | sed -E "s/^\.\///"  | fzf --bind "enter:execute(nvim {})" --preview "head -n 40 {} | pygmentize ;"
}
find-cpp()
{
  find -L . -iregex ".*"$1".*\.cpp$" -type f  | sed -E "s/^\.\///"  | fzf --bind "enter:execute(nvim {})" --preview "head -n 40 {} | pygmentize ;"
}
find-rst()
{
  find -L . -iregex ".*"$1".*\.rst$" -type f  | sed -E "s/^\.\///"  | fzf --bind "enter:execute(nvim {})" --preview "head -n 40 {} | pygmentize ;"
}
find-json()
{
  find -L . -iregex ".*"$1".*\.rst$" -type f  | sed -E "s/^\.\///"  | fzf --bind "enter:execute(nvim {})" --preview "head -n 40 {} | pygmentize ;"
}
find-org()
{
  find -L . -iregex ".*"$1".*\.org$" -type f  | sed -E "s/^\.\///"  | fzf --bind "enter:execute(nvim {})" --preview "head -n 40 {} | pygmentize ;"
}
find-tex()
{
  find -L . -iregex ".*"$1".*\.tex$" -type f  | sed -E "s/^\.\///"  | fzf --bind "enter:execute(nvim {})" --preview "head -n 40 {} | pygmentize ;"
}
find-erlang()
{
  find -L . -iregex ".*"$1".*\.erl$" -type f  | sed -E "s/^\.\///"  | fzf --bind "enter:execute(nvim {})" --preview "head -n 40 {} | pygmentize ;"
}
find-sass()
{
  find -L . -iregex ".*"$1".*\.sass$" -type f  | sed -E "s/^\.\///"  | fzf --bind "enter:execute(nvim {})" --preview "head -n 40 {} | pygmentize ;"
}
find-css()
{
  find -L . -iregex ".*"$1".*\.css$" -type f  | sed -E "s/^\.\///"  | fzf --bind "enter:execute(nvim {})" --preview "head -n 40 {} | pygmentize ;"
}
find-php()
{
  find -L . -iregex ".*"$1".*\.php$" -type f  | sed -E "s/^\.\///"  | fzf --bind "enter:execute(nvim {})" --preview "head -n 40 {} | pygmentize ;"
}
find-yaml()
{
  find -L . -iregex ".*"$1".*\.yml$" -type f  | sed -E "s/^\.\///"  | fzf --bind "enter:execute(nvim {})" --preview "head -n 40 {} | pygmentize ;"
}
find-typescript()
{
  find -L . -iregex ".*"$1".*\.ts$" -type f  | sed -E "s/^\.\///"  | fzf --bind "enter:execute(nvim {})" --preview "head -n 40 {} | pygmentize ;"
}
find-xml()
{
  find -L . -iregex ".*"$1".*\.xml$" -type f  | sed -E "s/^\.\///"  | fzf --bind "enter:execute(nvim {})" --preview "head -n 40 {} | pygmentize ;"
}
find-haskell()
{
  find -L . -iregex ".*"$1".*\.hs$" -type f  | sed -E "s/^\.\///"  | fzf --bind "enter:execute(nvim {})" --preview "head -n 40 {} | pygmentize ;"
}
find-coffee()
{
  find -L . -iregex ".*"$1".*\.coffee$" -type f  | sed -E "s/^\.\///"  | fzf --bind "enter:execute(nvim {})" --preview "head -n 40 {} | pygmentize ;"
}
find-less()
{
  find -L . -iregex ".*"$1".*\.less$" -type f  | sed -E "s/^\.\///"  | fzf --bind "enter:execute(nvim {})" --preview "head -n 40 {} | pygmentize ;"
}

# FIND
alias f="find-it"
#alias fc="find-c"
alias fcoffee="find-coffee"
alias fcpp="find-cpp"
alias fcss="find-css"
alias ferl="find-erlang"
alias fhs="find-haskell"
alias fhtml="find-html"
alias fjava="find-java"
alias fjs="find-javascript"
alias fjson="find-json"
alias fless="find-less"
alias fmd="find-markdown"
alias forg="find-org"
alias fphp="find-php"
alias fpy="find-python"
alias frb="find-ruby"
alias frst="find-rst"
alias fsass="find-sass"
alias fsh="find-shell"
alias ftex="find-tex"
alias fts="find-typescript"
alias fvim="find-vim"
alias fxml="find-xml"
alias fyml="find-yaml"

stty -ixon              # enable inc search <C-s> which is often disabled by terminal emulators

complete -cf sudo

shopt -s histappend     # Append each session's history to $HISTFILE
shopt -s histverify     # Edit a recalled history line before executing
shopt -s extglob        # Enable extended pattern-matching features
shopt -s expand_aliases # Expand aliases
shopt -s dirspell       # correct minor spelling errors
shopt -s direxpand
shopt -s globstar       # ** becomes a recursive wildstar
shopt -s cdspell        # correct minor spelling errors
shopt -s dotglob        # Include dotfiles in pathname expansion
shopt -s checkwinsize   # update the value of LINES and COLUMNS after each command if altered

#eval `dircolors ~/.dir_colors.solarized_dark`
#eval `dircolors ~/.dir_colors.solarized_light`
if [  -f ~/.dir_colors.256_dark ]; then 
  eval `dircolors ~/.dir_colors.256_dark`
fi

ipif()
{ 
    if grep -P "(([1-9]\d{0,2})\.){3}(?2)" <<< "$1"; then
      curl ipinfo.io/"$1"
    else
      ipawk=($(host "$1" | awk '/address/ { print $NF }'))
      curl ipinfo.io/${ipawk[1]}
    fi
    echo
}
