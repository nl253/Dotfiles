# ~/.profile sourced by all login shells

export ENV=$HOME/.config/sh/init.sh
export BASH_ENV=$HOME/.config/bash/.bashrc
export LC_ALL=en_GB.UTF-8
export LANG=en_GB.UTF-8
export EMAIL=norbertlogiewa96@gmail.com
# export RIPGREP_CONFIG_PATH=~/.config/rg/config

plantuml() (dash -c "java -jar ~/.local/share/plant-uml/plantum*.jar $@")

if [ -x $(which go 2>/dev/null) ]; then
  export PATH=$PATH:$(go env GOPATH)/bin
fi

# Don't check mail when opening terminal.
unset MAILCHECK

# $EDITOR
for i in nvim vim vi; do
  if [ -x $(command which $i 2>/dev/null) ]; then
    export EDITOR=$(command which $i 2>/dev/null) && break
  fi
done

# $PAGER
if [ -x $(command which less 2>/dev/null) ]; then
  export LESS='--RAW-CONTROL-CHARS --IGNORE-CASE --QUIET --HILITE-SEARCH --long-prompt'
  if [ -x $(command which pygmentize 2>/dev/null) ]; then
    export LESSOPEN='| pygmentize %s'
  fi
  export PAGER=less
fi

#export HISTIGNORE="&:[ ]*:pwd:exit:cd:ls:bg:fg:history:clear:jobs:git*:ls*:dirs *:vim*:nvim*:ghci*:date:ranger:alias:dirs:popd:mutt:bash*:shopt:set:env:enable:"
#export HISTTIMEFORMAT='%c'
export FIGNORE='~:.o:.swp:history:.class:cache:.pyc:.aux:.toc:.fls:.lock:.tmp:tags:.log:.hi:.so:.beam:tags:.iml:.lock:.bak:.idx:.pack'
export HH_CONFIG=hicolor # get more colors
export HISTCONTROL=ignoreboth:erasedups
export HISTFILE="$HOME/.config/sh/.history"
export HISTSIZE=20000
export HISTFILESIZE=$HISTSIZE
export SAVEHIST=$HISTSIZE
# After each command, append to the history file and reread it
export PROMPT_COMMAND="${PROMPT_COMMAND:+$PROMPT_COMMAND$'\n'}history -a; history -c; history -r"

[ -f ~/.makepkg.conf ] && export MAKEPKG_CONF=~/.makepkg.conf

# Needs to be set for ranger to load configuration
[ -f ~/.config/ranger/rc.conf ] && export RANGER_LOAD_DEFAULT_RC=false

# GUI use qt5 not qt4
export QT_SELECT=5

# $PATH
for directory in .local .yarn .stack .cabal .cargo .config/composer/vendor .local/share/fzf go node_modules; do
  [ -d "${HOME}/${directory}/bin" ] && export PATH="${HOME}/${directory}/bin:${PATH}:" 2>/dev/null
done

for directory in .config/yarn/global/node_modules/.bin; do
  [ -d "${HOME}/${directory}" ] && export PATH="${HOME}/${directory}:${PATH}:" 2>/dev/null
done

# for directory in '/usr/lib/jvm/java-9-openjdk'; do
# [ -d "${directory}/bin" ] && export PATH="${directory}/bin:${PATH}:" 2>/dev/null
# done

# PostgreSQL
if [ -x $(command which psql 2>/dev/null) ]; then
  export PGUSER=postgres
  export PGHOST=localhost
  # export PGDATABASE=testing
fi

# MySQL
if [ -x $(command which mysql 2>/dev/null) ]; then
  export MYSQL_PS1=' MySQL ~> '
  if [ $(hostname) = raptor ]; then
    export MYSQL_HOST='dragon.kent.ac.uk'
  fi
fi

# $BROWSER
for i in google-chrome-stable google-chrom-beta google-chrome-unstable firefox-developer; do
  if [ -x $(command which $i 2>/dev/null) ]; then
    export BROWSER=$(command which $i 2>/dev/null) && break
  fi
done

# Erlang
# ------
export ERL_AFLAGS="-kernel shell_history enabled"
export ERL_LIBS=/home/norbert/.local/lib/erlang

# Java
# ----
export JAVA_HOME=/usr/lib/jvm/default/

# Rust
# ----
export DEFAULT_TOOLCHAIN=nightly

# FZF (keymap)
# ------------------------
#  enter   print to STDOUT
#  ctrl-d  scroll down
#  ctrl-u  scroll up
#  alt-e   edit with $EDITOR
#  alt-d   cd
#  alt-p   preview toggle
#  alt-l   open in  less`
export FZF_DEFAULT_OPTS=" --preview-window=right:hidden --tiebreak=end --no-mouse --multi --ansi --margin 3% --filepath-word --prompt=' >> ' --reverse --tiebreak=end,length"
export FZF_DEFAULT_OPTS="$FZF_DEFAULT_OPTS --bind='alt-c:execute(cd {})' --bind='alt-v:half-page-up,ctrl-v:half-page-down,ctrl-d:half-page-down,ctrl-u:half-page-up,alt-p:toggle-preview,ctrl-n:down,ctrl-p:up'"
export FZF_DEFAULT_OPTS="$FZF_DEFAULT_OPTS --bind='alt-e:execute(\$EDITOR {})' --bind='alt-l:execute:(\$PAGER {})'"
export FZF_DEFAULT_OPTS="$FZF_DEFAULT_OPTS --color=hl:160,fg+:11,border:0,spinner:0,header:0,bg+:0,info:0"

# use git listing if in a git repo, otherwise use find to list current dir recursively
export FZF_DEFAULT_COMMAND='git ls-tree -r --name-only HEAD || find . -path "*/\.*" -prune -o -type d -print -type f -print -o -type l -print | sed s/^..//\ 2> /dev/null'

if [ -e ~/.config/ranger/scope.sh ]; then
  export FZF_DEFAULT_OPTS="$FZF_DEFAULT_OPTS"' --preview="bash ~/.config/ranger/scope.sh {} $(tput cols) $(tput lines) /tmp/ False"'

elif [ -x $(command which pygmentize 2>/dev/null) ]; then
  export FZF_DEFAULT_OPTS="$FZF_DEFAULT_OPTS"' --preview="([ -f {} ] && head -n $(tput lines) {} | pygmentize -l $(pygmentize -N {})) || ([ -d {} ] && tree -l -a --prune -L 4 -F --sort=mtime {})"'

else
  export FZF_DEFAULT_OPTS="$FZF_DEFAULT_OPTS"' --preview="[ -f {} ] && head -n $(tput lines) {} || [ -d {} ] && tree -l -a --prune -L 4 -F --sort=mtime {}"'
fi

if [ $0 = zsh ] || [ $0 = $(which zsh 2>/dev/null) ]; then
  # sh won't ever get here so [[ is fine 
  # not run with -i (interactive) but with TMUX so make interactive anyway
  [[ ! $- =~ i ]] && [[ -n $TMUX ]] && [[ -f $ZDOTDIR/.zshrc ]] && [[ -n $ZDOTDIR ]] && source $ZDOTDIR/.zshrc
elif [ $0 = bash ] || [ $0 = $(which bash 2>/dev/null) ]; then
  [[ ! $- =~ i ]] && [[ -n $TMUX ]] && [[ -f ~/.bashrc ]] && source ~/.bashrc
fi
