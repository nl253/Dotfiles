
# PATH
for i in ~/{.fzf,.pyenv,.local}/bin; do
	[[ -d $i ]] && export PATH=$i:$PATH
done

# PYENV (PATH needs to be set before this)
eval "$(pyenv virtualenv-init -)"
eval "$(pyenv init -)"

# VARIABLES

# GENERAL
export PYENV_VIRTUALENV_DISABLE_PROMPT=1

# Don't check mail when opening terminal.
unset MAILCHECK

# Set Xterm/screen/Tmux title with only a short hostname
export SHORT_HOSTNAME=$(hostname -s)

[[ -f ~/.makepkg.conf ]] && export MAKEPKG_CONF=~/.makepkg.conf
[[ -f ~/.config/ranger/rc.conf ]] && export RANGER_LOAD_DEFAULT_RC=false

 # JAVA
[[ -d /usr/lib/jvm/java-8-openjdk ]] && export JAVA_HOME='/usr/lib/jvm/java-8-openjdk' && export JRE_HOME='/usr/lib/jvm/java-8-openjdk/jre'

# }}}

# HISTORY
export HISTSIZE=20000
export HISTFILESIZE=20000
export HISTCONTROL="ignoreboth:erasedups"
export HISTTIMEFORMAT=""
export HH_CONFIG=hicolor # get more colors
export HISTIGNORE="&:[ ]*:exit:cd:ls:bg:fg:history:clear:jobs"

export GREP_COLOR='1;33' # makes it yellow # by default red
export LS_COLORS='di=01;34:ln=01;35:so=01;32:ex=01;31:bd=46;34:cd=43;34:su=41;30:sg=46;30:tw=42;30:ow=43;30'

# $PAGER
if [[ -x $(which less) ]]; then
  alias less='less -x4RFsX' && export PAGER=less
else
  [[ -x $(which more) ]] && export PAGER=more && alias less=more
fi

# $BROWSER
for i in google-chrome-stable elinks lynx w3m; do
  [[ -x $(which $i) ]] && export BROWSER=$(which $i) && break
done

# $EDITOR
for i in vim nvim vi; do
  if [[ -x $(which $i) ]]; then
    export EDITOR=$(which $i)
    [[ $i != vim ]] && eval 'alias '$i=vim
    break
  fi
done

add-to-path() {
  for directory in "$@"; do
    [[ -d $directory ]] && export PATH="$directory:$PATH:"
  done
}

add-to-path ~/.{local,cabal,cargo,gem,zplug,config/composer/vendor}/bin 2>/dev/null
add-to-path /usr/local/go/bin 2>/dev/null

unset -f add-to-path

export HISTFILE=~/.zsh_history
export SAVEHIST=10000

export YAOURT_COLORS="nb=1:pkg=1:ver=1;32:lver=1;45:installed=1;42:grp=1;34:od=1;41;5:votes=1;44:dsc=0:other=1;35"

# vim: foldmethod=marker foldlevel=0 foldmarker={{{,}}}
