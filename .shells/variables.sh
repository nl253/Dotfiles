
# VARIABLES

# Don't check mail when opening terminal.
unset MAILCHECK

export TERMINAL=gnome-terminal

# Set Xterm/screen/Tmux title with only a short hostname
export SHORT_HOSTNAME=$(hostname -s)

[[ -f ~/.makepkg.conf ]] && export MAKEPKG_CONF=~/.makepkg.conf
[[ -f ~/.config/ranger/rc.conf ]] && export RANGER_LOAD_DEFAULT_RC=false

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
if [[ -x $(command which less) ]]; then
  alias less='less -x4RFsX' && export PAGER=less
else
  [[ -x $(command which more) ]] && export PAGER=more && alias less=more
fi

# $BROWSER
for i in google-chrome-stable chromium elinks lynx w3m; do
  [[ -x $(command which $i 2>/dev/null) ]] && export BROWSER=$(command which $i) && break
done

# $EDITOR
for i in vim nvim vi; do
  if [[ -x $(command which $i 2>/dev/null) ]]; then
    export EDITOR=$(command which $i)
    [[ $i != vim ]] && eval 'alias '$i=vim
    break
  fi
done

export HISTFILE=~/.zsh_history
export SAVEHIST=10000

if [[ -x $(command which yaourt) ]]; then
  export YAOURT_COLORS="nb=1:pkg=1:ver=1;32:lver=1;45:installed=1;42:grp=1;34:od=1;41;5:votes=1;44:dsc=0:other=1;35"
fi
# vim: foldmethod=marker foldlevel=0 foldmarker={,}
