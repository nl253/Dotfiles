#!/bin/env bash
#
# ~/.bashrc
#
# If not running interactively, don't do anything
#[[ $- != *i* ]] && return

[[ $- != *i* ]] && return
[ -z "$PS1" ] && return

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

echo -e "${RED}~/.bashrc ${YELLOW}loaded"

colors() {
  local fgc bgc vals seq0

  printf "Color escapes are %s\n" '\e[${value};...;${value}m'
  printf "Values 30..37 are \e[33mforeground colors\e[m\n"
  printf "Values 40..47 are \e[43mbackground colors\e[m\n"
  printf "Value  1 gives a  \e[1mbold-faced look\e[m\n\n"

  # foreground colors
  for fgc in {30..37}; do
    # background colors
    for bgc in {40..47}; do
      fgc=${fgc#37} # white
      bgc=${bgc#40} # black

      vals="${fgc:+$fgc;}${bgc}"
      vals=${vals%%;}

      seq0="${vals:+\e[${vals}m}"
      printf "  %-9s" "${seq0:-(default)}"
      printf " ${seq0}TEXT\e[m"
      printf " \e[${vals:+${vals+$vals;}}1mBOLD\e[m"
    done
    echo; echo
  done
}

export PS1="$(tput setaf 1)\w\n\[$(tput bold)\]\[$(tput setaf 1)\][\[$(tput setaf 3)\]\u\[$(tput setaf 2)\]@\[$(tput setaf 4)\]\h\[$(tput setaf 5)\]\[$(tput setaf 1)\]]\[$(tput setaf 7)\]\\$\[$(tput sgr0)\] "

unset MAILCHECK # Don't check mail when opening terminal.

export SCM_CHECK=false # Set this to false to turn off version control status checking within the prompt for all themes

export HISTSIZE=20000
export HISTCONTROL=ignoredups
export HISTFILESIZE=20000
export LESS=' -R '
export HISTIGNORE="&:[ ]*:exit:ls:bg:fg:history:clear"
export HISTTIMEFORMAT='%F %T '

export PAGER=less
[ -x /usr/bin/bash ] && export SHELL=/usr/bin/bash || export 


alias l='ls -CFa'
alias l='ls -CFa'
alias la='ls -la --group-directories-first --time-style=+"%d.%m.%Y %H:%M" --color=auto -F'
alias le="ls -lo"
alias ll='ls -l --group-directories-first --time-style=+"%d.%m.%Y %H:%M" --color=auto -F'
alias ls='LC_COLLATE=C ls --color=auto --group-directories-first'
alias h=history
alias e=$EDITOR
[ -x /usr/bin/aria2c ] &&  alias aria2c="aria2c --dir='~/Downloads/Torrent/'"

[ -x /usr/bin/vim ] && export EDITOR=/usr/bin/vim && alias nvim=vim 

[ ! -x /usr/bin/tree ] && alias tree="find . -print | sed -e 's;[^/]*/;|____;g;s;____|; |;g'"

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
[ -x /usr/bin/ranger ] && alias r='ranger'
alias diff='diff --color=auto'
alias grep='grep --color=auto'
alias fgrep='fgrep --color=auto'
alias egrep='egrep --color=auto'
[ -x /usr/bin/ag ] && alias ag='ag --hidden --pager="less -MIRFX"'  # search with dotfiles
alias recent-files='find . -amin -10 -type f | grep -P -v ".*C|cache.*" | sed -E "s/^\.\///"'

find-large()
{
  if [[ $1 = "" ]]; then
    1=".*"
  fi
  find / -type f -size +6k -iregex ".*\.$1" 2>/dev/null | head -n 12
}

export GREP_COLOR='1;33'

#alias cp="cp -i"                          # confirm before overwriting something

#alias cal='cal | grep -C6 "$(date +%e)"'
alias df='df --human-readable --si'
alias j=jobs
alias freq='cut -f1 -d" " "$HISTFILE" | sort | uniq -c | sort -nr | head -n 30'
alias logout="pkill -KILL -u "
#alias t='date'
alias untar='tar xvf'
[[ -x /usr/bin/aspell ]] && alias aspell="aspell -c -l en_GB"
alias info='info --vi-keys'
alias show-keybingings="bind -p | grep -v '^#\|self-insert\|^$'"
alias symlinks-at-home="find ~ -type l 2>/dev/null"
alias symlinks-pretty='for i in $(find -type l -exec echo {} \;); do echo -e " \e[36m$i  \e[39m->  \e[91m$(readlink -f $i)" ; done'

# WILL EXTRACT ANYTHING
ex ()
{
  if [ -f $1 ] ; then
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
    echo "'$1' is not a valid file"
  fi
}

# CD + LS COMBINED
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


show-colors()
{
  (x=`tput op` y=`printf %76s`;for i in {0..256};do o=00$i;echo -e ${o:${#o}-3:3} `tput setaf $i;tput setab $i`${y// /=}$x;done)
}

show-term-capabilities()
{
  infocmp -1 | sed -nu 's/^[ \000\t]*//;s/[ \000\t]*$//;/[^ \t\000]\{1,\}/!d;/acsc/d;s/=.*,//p'|column -c80
}

# ---------
#  Git
# ---------
## Add uncommitted and unstaged changes to the last commit
## From http://blogs.atlassian.com/2014/10/advanced-git-aliases/
## Show commits since last pull
alias g=git
[ -x /usr/bin/hub ] && eval "$(hub alias -s)" && alias g=hub

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

# expands bang combinations and variables to their values - remember !$ last arg / !^ first arg / !* all args
#bind Space:magic-space # also combine these with :h (head) or :t (tail) to get path selective path expansion -> !$:h
man()
{
  LESS_TERMCAP_md=$'\e[01;31m' \
    LESS_TERMCAP_me=$'\e[0m' \
    LESS_TERMCAP_se=$'\e[0m' \
    LESS_TERMCAP_so=$'\e[01;44;33m' \
    LESS_TERMCAP_ue=$'\e[0m' \
    LESS_TERMCAP_us=$'\e[01;32m' \
    command man "$@"
}
export LESS=-R
export LESS_TERMCAP_mb=$'\E[1;31m'     # begin bold
export LESS_TERMCAP_md=$'\E[1;36m'     # begin blink
export LESS_TERMCAP_me=$'\E[0m'        # reset bold/blink
export LESS_TERMCAP_so=$'\E[01;44;33m' # begin reverse video
export LESS_TERMCAP_se=$'\E[0m'        # reset reverse video
export LESS_TERMCAP_us=$'\E[1;32m'     # begin underline
export LESS_TERMCAP_ue=$'\E[0m'        # reset underline
# and so on


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

stty -ixon              # enable inc search <C-s> which is often disabled by terminal emulators
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

[ -r /usr/share/bash-completion/bash_completion ] && [ -x /usr/bin/bash ] && . /usr/share/bash-completion/bash_completion

#vim:foldlevel=-1:foldmethod=marker:foldmarker={,}:
