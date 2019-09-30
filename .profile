# ~/.profile sourced by all login shells

export ENV=$HOME/.config/sh/init.sh
export NODE_REPL_HISTORY=$HOME/.config/node/.history
export ELINKS_CONFDIR=$HOME/.config/elinks
export UV_THREADPOOL_SIZE=4
# export BASH_ENV=$HOME/.config/bash/.bashrc
export LC_ALL=en_GB.UTF-8
export LANG=en_GB.UTF-8
export EMAIL='norbertlogiewa96@gmail.com'

export GOROOT=/usr/local/share/go1.13
export GOPATH=/usr/local/share/go1.13:$HOME/go
export PATH=/usr/local/share/go1.13/bin:$PATH

# Don't check mail when opening terminal.
unset -v MAILCHECK

# $EDITOR
for i in nvim vim vi; do
  if [ -x /usr/bin/$i ]; then
    export EDITOR=/usr/bin/$i && break
  fi
done

# $PAGER
if [ -x /usr/bin/less ]; then
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

# Needs to be set for ranger to load configuration
[ -f ~/.config/ranger/rc.conf ] && export RANGER_LOAD_DEFAULT_RC=false

# GUI use qt5 not qt4
export QT_SELECT=5

# $PATH
for directory in .racket .local .yarn .stack .cabal .cargo .config/composer/vendor .local/share/fzf go node_modules /usr/local; do
  [ -d "${HOME}/${directory}/bin" ] && export PATH="${HOME}/${directory}/bin:${PATH}:" 2>/dev/null
done

for directory in .config/yarn/global/node_modules/.bin; do
  [ -d "${HOME}/${directory}" ] && export PATH="${HOME}/${directory}:${PATH}:" 2>/dev/null
done

# $BROWSER
for browser in google-chrome chromium chromium-browser firefox-developer firefox vivaldi brave palemoon; do
  for channel in snapshot nightly unstable beta stable; do
    [ -x /usr/bin/${browser}-${channel} ] && export BROWSER=/usr/bin/${browser}-${channel} && break 2
  done
  [ -x /usr/bin/$browser ] && export BROWSER=/usr/bin/$browser && break
done

# Java
# ----
export JAVA_HOME=/usr/lib/jvm/default

# Rust
# ----
export DEFAULT_TOOLCHAIN=nightly

# Python
# ------
mkdir -p /tmp/python/bytecode
# don't store __pycache__ in ~/
export PYTHONPYCACHEPREFIX=/tmp/python/bytecode
# Warn once per Python process
# export PYTHONWARNINGS=once

# FZF (keymap)
# ------------------------
#  enter   print to STDOUT
#  ctrl-d  scroll down
#  ctrl-u  scroll up
#  alt-e   edit with $EDITOR
#  alt-d   cd
#  alt-p   preview toggle
#  alt-l   open in  less`
export FZF_DEFAULT_COMMAND='command git ls-tree -r --name-only HEAD || command rg --files || command find . -path "*/\.*" -prune -o -type d -print -type f -print -o -type l -print | command sed s/^..//\ 2> /dev/null'
export FZF_DEFAULT_OPTS=" --filepath-word --history-size=10000 --history=$HOME/.config/fzf/.history --preview-window=right:hidden --tiebreak=end --no-mouse --multi --ansi --margin 3% --filepath-word --prompt=' >> ' --reverse --tiebreak=end,length"
export FZF_DEFAULT_OPTS="$FZF_DEFAULT_OPTS --bind='ctrl-y:yank,alt-c:execute(cd {})' --bind='alt-b:backward-word,alt-f:forward-word' --bind='alt-v:half-page-up,ctrl-v:half-page-down,ctrl-d:half-page-down,ctrl-u:half-page-up,alt-p:toggle-preview,ctrl-n:down,ctrl-p:up'"
export FZF_DEFAULT_OPTS="$FZF_DEFAULT_OPTS --bind='alt-e:execute(\$EDITOR {})' --bind='alt-l:execute:(\$PAGER {})'"
export FZF_DEFAULT_OPTS="$FZF_DEFAULT_OPTS --color=hl:160,fg+:11,border:0,spinner:0,header:0,bg+:0,info:0"

if [ -e ~/.config/ranger/scope.sh ]; then
  export FZF_DEFAULT_OPTS="$FZF_DEFAULT_OPTS"' --preview="command bash ~/.config/ranger/scope.sh {} $(command tput cols) $(command tput lines) /tmp/ False"'
elif [ -x $(command which pygmentize 2>/dev/null) ]; then
  export FZF_DEFAULT_OPTS="$FZF_DEFAULT_OPTS"' --preview="([ -f {} ] && command head -n $(tput lines) {} | command pygmentize -l $(pygmentize -N {})) || ([ -d {} ] && command tree -l -a --prune -L 4 -F --sort=mtime {})"'
else
  export FZF_DEFAULT_OPTS="$FZF_DEFAULT_OPTS"' --preview="[ -f {} ] && command head -n $(tput lines) {} || [ -d {} ] && tree -l -a --prune -L 4 -F --sort=mtime {}"'
fi

if [ $0 = zsh ] || [ $0 = $(which zsh 2>/dev/null) ]; then
  # sh won't ever get here so [[ is fine
  # not run with -i (interactive) but with TMUX so make interactive anyway
  [[ ! $- =~ i ]] && [[ -n $TMUX ]] && [[ -f $ZDOTDIR/.zshrc ]] && [[ -n $ZDOTDIR ]] && source $ZDOTDIR/.zshrc
elif [ $0 = bash ] || [ $0 = $(which bash 2>/dev/null) ]; then
  [[ ! $- =~ i ]] && [[ -n $TMUX ]] && [[ -f ~/.bashrc ]] && source ~/.bashrc
fi
