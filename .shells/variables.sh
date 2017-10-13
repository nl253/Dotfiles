
# VARIABLES

# point doctuils (rst) to config file
export DOCUTILSCONFIG=~/.docutils

# use qt5 not qt4
export QT_SELECT=5

# POSTGRES
export PGUSER=postgres
export PGHOST=localhost
export PGDATABASE=testing

# Don't check mail when opening terminal.
unset MAILCHECK

export TERMINAL=alacritty

_add_to_cdpath() {
	for directory in "$@"; do
		[[ -d $directory ]] && [[ ! $CDPATH =~ $directory ]] && export CDPATH="$directory:$CDPATH:" 2>/dev/null
	done
}

export CDPATH="${HOME}:"

_add_to_cdpath $HOME/{Projects,Uni,.shells} $HOME/Uni/* $HOME/Uni/*/*

unset -f _add_to_cdpath

# Set Xterm/screen/Tmux title with only a short hostname
export SHORT_HOSTNAME=$(hostname -s)

export JAVA_HOME=/usr/lib/jvm/java-8-openjdk
export JRE_HOME=/usr/lib/jvm/java-8-openjdk/jre
export PATH=$PATH:${JAVA_HOME}/bin:${JRE_HOME}/bin

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
if [[ -x $(command which less) ]]; then
  alias less='less -R' && export PAGER=less
else
  [[ -x $(command which more) ]] && export PAGER=more && alias less=more
fi

# $BROWSER
for i in firefox-developer firefox google-chrome-stable chromium elinks lynx w3m; do
  if [[ -x $(command which $i 2>/dev/null) ]]; then
    export BROWSER=$(command which $i) && break
  fi
done

# $EDITOR

#for i in gvim idea pycharm; do
  #if [[ -x $(command which $i 2>/dev/null) ]]; then
    #export VISUAL=$(command which $i)
    #break
  #fi
#done

for i in vim nvim vi; do
  if [[ -x $(command which $i 2>/dev/null) ]]; then
    export EDITOR=$(command which $i)
    [[ $i != vim ]] && eval 'alias '$i=vim
    break
  fi

done

# FIXME
# if ! $(hostname) =~ Chummy; then
  # eval "alias ${EDITOR}=${EDITOR} 2>/dev/null"
# fi

export HISTFILE=~/.shell_history
export SAVEHIST=10000

if [[ -x $(command which yaourt) ]]; then
  export YAOURT_COLORS="nb=1:pkg=1:ver=1;32:lver=1;45:installed=1;42:grp=1;34:od=1;41;5:votes=1;44:dsc=0:other=1;35"
fi
# vim: foldmethod=marker foldlevel=0 foldmarker={,}
