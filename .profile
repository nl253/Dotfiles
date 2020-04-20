# ~/.profile sourced by all login shells

export PATH=/usr/bin:/usr/local/bin:/bin:/sbin:/usr/sbin:/snap/bin:/home/linuxbrew/.linuxbrew/bin
export ENV=$HOME/.config/sh/init.sh
export NODE_REPL_HISTORY=$HOME/.config/node/.history
export ELINKS_CONFDIR=$HOME/.config/elinks
export UV_THREADPOOL_SIZE=4
# export BASH_ENV=$HOME/.config/bash/.bashrc
export LC_ALL=en_GB.UTF-8
export LANG=en_GB.UTF-8
export EMAIL='norbertlogiewa96@gmail.com'

# Don't check mail when opening terminal.
unset -v MAILCHECK

# $EDITOR
for i in nvim vim vi; do
  if [ -x /usr/bin/$i ]; then
    export EDITOR=/usr/bin/$i && break
    eval "alias vim='$i'"
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
for directory in /home/linuxbrew/.linuxbrew .deno .racket .local .yarn .stack .cabal .cargo .local/share/umake .config/composer/vendor .local/share/fzf go node_modules; do
  [ -d "${HOME}/${directory}/bin" ] && export PATH="${HOME}/${directory}/bin:${PATH}:" 2>/dev/null
done

for directory in .config/yarn/global/node_modules/.bin; do
  [ -d "${HOME}/${directory}" ] && export PATH="${HOME}/${directory}:${PATH}:" 2>/dev/null
done

# $BROWSER
export BROWSER=firefox
# for browser in firefox-developer firefox google-chrome chromium chromium-browser vivaldi brave palemoon; do
  # for channel in snapshot nightly unstable beta stable; do
    # [ -x /usr/bin/${browser}-${channel} ] && export BROWSER=/usr/bin/${browser}-${channel} && break 2
  # done
  # [ -x /usr/bin/$browser ] && export BROWSER=/usr/bin/$browser && break
# done

# Rust
# ----
export DEFAULT_TOOLCHAIN=nightly
# eval $(/home/linuxbrew/.linuxbrew/bin/brew shellenv)

# test -d ~/.linuxbrew && eval $(~/.linuxbrew/bin/brew shellenv)
# test -d /home/linuxbrew/.linuxbrew && eval $(/home/linuxbrew/.linuxbrew/bin/brew shellenv)
