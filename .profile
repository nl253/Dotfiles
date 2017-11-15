# ~/.profile sourced by all login shells

export ENV=$HOME/.config/sh/init.sh

# toolchain to use for Rust
export DEFAULT_TOOLCHAIN=nightly

# Don't check mail when opening terminal.
unset MAILCHECK

# $EDITOR
for i in nvim vim vi; do
  if [ -x $(command which $i 2>/dev/null) ]; then
    export EDITOR=$(command which $i 2>/dev/null) && break
  fi
done

if [ $0 = bash ] || [ $0 = zsh ] || [ $0 = -bash ]; then
  eval $(dircolors -b)
fi

# $PAGER
if [ -x $(command which less 2>/dev/null) ]; then
  export LESS='--RAW-CONTROL-CHARS --IGNORE-CASE --QUIET --HILITE-SEARCH --long-prompt'
  if [ -x $(command which pygmentize 2>/dev/null) ]; then
    export LESSOPEN='| pygmentize %s'
  fi
  export PAGER=less
fi

export HISTFILE="$HOME/.config/sh/.shell_history"
export EXECIGNORE='{/usr,}/bin/grub*:'
export SAVEHIST=10000
export HISTSIZE=20000
export HISTFILESIZE=20000
export HISTCONTROL="ignoreboth:erasedups"
# export HISTTIMEFORMAT=""
# export TIMEFORMAT=""
export HH_CONFIG=hicolor # get more colors
export HISTIGNORE="&:[ ]*:exit:cd:ls:bg:fg:history:clear:jobs"
# colon separated list of extensions to ignore when completing
export FIGNORE='~:.o:.swp:__:history:.class:cache:.pyc:.aux:.toc:.fls:.lock:.tmp:tags'

# arch linux / manjaro
[ -f ~/.makepkg.conf ] && export MAKEPKG_CONF=~/.makepkg.conf

# needs to be set for ranger to load custom files
[ -f ~/.config/ranger/rc.conf ] && export RANGER_LOAD_DEFAULT_RC=false

# GUI use qt5 not qt4
export QT_SELECT=5

# $PATH
for directory in '.local' '.yarn' '.stack' '.cabal' '.config/composer/vendor' '.cargo' '.local/share/fzf' 'go' 'node_modules'; do
  [ -d "${HOME}/${directory}/bin" ] && export PATH="${HOME}/${directory}/bin:${PATH}:" 2>/dev/null
done

# PostgreSQL
if [ -x $(command which psql 2>/dev/null) ]; then
  export PGUSER=postgres
  export PGHOST=localhost
  export PGDATABASE=testing
fi

# $BROWSER
for i in firefox-developer firefox google-chrome-stable chromium elinks lynx w3m; do
  if [ -x $(command which $i 2>/dev/null) ]; then
    export BROWSER=$(command which $i 2>/dev/null) && break
  fi
done

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
export FZF_DEFAULT_OPTS="$FZF_DEFAULT_OPTS --bind='alt-d:execute(cd {})' --bind='ctrl-d:half-page-down,ctrl-u:half-page-up,alt-p:toggle-preview'"
export FZF_DEFAULT_OPTS="$FZF_DEFAULT_OPTS --bind='alt-e:execute(\$EDITOR {})' --bind='alt-l:execute:(\$PAGER {})'"
export FZF_DEFAULT_OPTS="$FZF_DEFAULT_OPTS --color=hl:160,fg+:11,border:0,spinner:0,header:0,bg+:0,info:0"
# use git listing if in a git repo, otherwise use find to list current dir recursively
export FZF_DEFAULT_COMMAND='git ls-tree -r --name-only HEAD || find . -path "*/\.*" -prune -o -type d -print -type f -print -o -type l -print | sed s/^..//\ 2> /dev/null'
if [ -e ~/.config/ranger/scope.sh ]; then
  export FZF_DEFAULT_OPTS="$FZF_DEFAULT_OPTS"' --preview="bash ~/.config/ranger/scope.sh {} $(tput cols) $(tput lines) /tmp/ False"'
elif [ -x $(command which pygmentize 2>/dev/null) ]; then
  export FZF_DEFAULT_OPTS="$FZF_DEFAULT_OPTS"' --preview="([ -f {} ] && head -n $(tput lines) {} | pygmentize -l $(pygmentize -N {})) || ([ -d {} ] && tree -l -a --prune -L 4 -F --sort=mtime {})"'
  # export FZF_DEFAULT_OPTS=$FZF_DEFAULT_OPTS' --preview="([ -f {} ] && head -n $(tput lines) {} | pygmentize -l $(pygmentize -N {})) || ([ -d {} ] && ls -Al {})"'
else
  export FZF_DEFAULT_OPTS="$FZF_DEFAULT_OPTS"' --preview="[ -f {} ] && head -n $(tput lines) {} || [ -d {} ] && tree -l -a --prune -L 4 -F --sort=mtime {}"'
fi
