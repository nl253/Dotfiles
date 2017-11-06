# VARIABLES

_add_to_path() {
  for directory in "$@"; do
    [[ -d $directory ]] && [[ ! $PATH =~ $directory ]] && export PATH="$directory:$PATH:" 2>/dev/null
  done
}
_add_to_path ~/.{local,anaconda3}/bin
_add_to_path ~/.gem/ruby/*/bin
_add_to_path ~/{.yarn,node_modules}/bin
_add_to_path ~/.{stack,cabal}/bin
_add_to_path ~/.cargo/bin
_add_to_path ~/.config/composer/vendor/bin
_add_to_path ~/.local/share/fzf/bin

# point doctuils (rst) to config file
export DOCUTILSCONFIG=~/.docutils

# use qt5 not qt4 (but only if you have qt5)
if [[ -x $(command which gt5ct 2>/dev/null) ]]; then 
	export QT_SELECT=5
fi

# POSTGRES
if [[ -x $(command which psql 2>/dev/null) ]]; then
	export PGUSER=postgres
	export PGHOST=localhost
	export PGDATABASE=testing
fi

# Don't check mail when opening terminal.
unset MAILCHECK

export JAVA_HOME=/usr/lib/jvm/java-9-openjdk
export JRE_HOME=/usr/lib/jvm/java-8-openjdk/jre

# reset
export CDPATH="${HOME}:"

for directory in ${HOME}/Documents ${HOME}/Documents/*; do
	[[ -d $directory ]] && [[ ! $CDPATH =~ $directory ]] && export CDPATH="${directory}:${CDPATH}:" 2>/dev/null
done

[[ -f ~/.makepkg.conf ]] && export MAKEPKG_CONF=~/.makepkg.conf
[[ -f ~/.config/ranger/rc.conf ]] && export RANGER_LOAD_DEFAULT_RC=false
[[ -f ~/.dir_colors ]] && eval $(dircolors ~/.dir_colors)

# HISTORY
export HISTSIZE=20000
export HISTFILESIZE=20000
export HISTCONTROL="ignoreboth:erasedups"
export HISTTIMEFORMAT=""
export HH_CONFIG=hicolor # get more colors
export HISTIGNORE="&:[ ]*:exit:cd:ls:bg:fg:history:clear:jobs"

# $PAGER
if [[ -x $(command which less 2>/dev/null) ]]; then
	export LESS='--RAW-CONTROL-CHARS --IGNORE-CASE --QUIET --HILITE-SEARCH --long-prompt'
	if [[ -x $(command which pygmentize 2>/dev/null) ]]; then
		export LESSOPEN='| pygmentize %s'
	fi
	export PAGER=less
else
  [[ -x $(command which more 2>/dev/null) ]] && export PAGER=more && alias less=more
fi

# $BROWSER
for i in firefox-developer firefox google-chrome-stable chromium elinks lynx w3m; do
  if [[ -x $(command which $i 2>/dev/null) ]]; then
    export BROWSER=$(command which $i 2>/dev/null) && break
  fi
done

# $EDITOR

for i in nvim vim vi; do
	if [[ -x $(command which $i 2>/dev/null) ]]; then
		export EDITOR=$(command which $i 2>/dev/null)
		if [[ $i != vim ]]; then
			eval "alias vim=${i}"
		fi
		break
	fi
done

export HISTFILE=~/.shell_history
export SAVEHIST=10000

if [[ -x $(command which yaourt 2>/dev/null) ]]; then
  export YAOURT_COLORS="nb=1:pkg=1:ver=1;32:lver=1;45:installed=1;42:grp=1;34:od=1;41;5:votes=1;44:dsc=0:other=1;35"
fi

# make less more friendly for non-text input files, see lesspipe(1)
[[ -x /usr/bin/lesspipe ]] && eval "$(SHELL=/bin/sh lesspipe)"

# vim: foldmethod=marker foldlevel=0 foldmarker={,} shiftwidth=2 tabstop=2
