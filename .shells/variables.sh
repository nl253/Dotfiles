
# THIS FILE MUST USE POSIX COMPLIANT SYNTAX
# IT IS SOURCED BY BOTH `zsh` AND `bash`

# VARIABLES 

# GENERAL {{{
export TERM=xterm-256color
unset MAILCHECK                                                        # Don't check mail when opening terminal.
export SHORT_HOSTNAME=$(hostname -s)                                   # Set Xterm/screen/Tmux title with only a short hostname
[ -f ~/.makepkg.conf ] && export MAKEPKG_CONF=~/.makepkg.conf
[ -f ~/.config/ranger/rc.conf ] && export RANGER_LOAD_DEFAULT_RC=false # }}}

# $PATH (and JAVA_HOME and JRE_HOME and Python) {{{
# ------------------------------------------------------------------------
# FUNCTION :: add packages from all package managers to $PATH if these paths exist along with my own scripts in ~/Scripts/
# ------------------------------------------------------------------------
add-to-path(){
  for directory in $@; do
    if [[ -d $directory ]]; then
      export PATH="${directory}:${PATH}:"
    fi
  done
}

export PATH="/usr/local/sbin:/bin:/usr/local/bin:/usr/bin/site_perl:/usr/bin/vendor_perl:/usr/bin/core_perl:/usr/bin:"

add-to-path ~/{.local,.cabal,.cargo,.gem,go,anaconda3}/bin ~/{.bin,.config/composer/vendor/bin} /usr/local/go/bin 2>/dev/null
add-to-path ~/.gem/ruby/*/bin 2>/dev/null

unset -f add-to-path

export PATH=${PATH//::/}

# PYENV {{{
export PYENV_ROOT="$HOME/.pyenv"
export PATH="$PYENV_ROOT/bin:$PATH"
eval "$(pyenv init -)"
eval "$(pyenv virtualenv-init -)"
# }}}

[ -d /usr/lib/jvm/java-8-openjdk ] && export JAVA_HOME='/usr/lib/jvm/java-8-openjdk' && export JRE_HOME='/usr/lib/jvm/java-8-openjdk/jre'  # JAVA

# }}}

# HISTORY {{{
export HISTSIZE=20000
export HISTFILESIZE=20000
export HISTCONTROL="ignoreboth:erasedups"
export HISTTIMEFORMAT=""
export HH_CONFIG=hicolor # get more colors
export HISTIGNORE="&:[ ]*:exit:cd:ls:bg:fg:history:clear:jobs" 
# }}}

export GREP_COLOR='1;33' # makes it yellow # by default red
export LS_COLORS='di=01;34:ln=01;35:so=01;32:ex=01;31:bd=46;34:cd=43;34:su=41;30:sg=46;30:tw=42;30:ow=43;30'

# $PAGER {{{
if [[ -x /usr/bin/less ]] || [ -x /bin/less ]; then
  alias less='less -x4RFsX' && export PAGER=less
else
  [[ -x $(which more) ]] && export PAGER=more && alias less=more
fi
# }}}

# $BROWSER {{{
for i in google-chrome-stable elinks lynx w3m; do
  if [[ -x $(which $i) ]]; then 
    export BROWSER=$(which $i)
    break
  fi
done

# }}}

# $EDITOR  {{{
for i in vim nvim vi; do
  if [[ -x $(which $i) ]]; then
    export EDITOR=$(which $i)
    [[ $i != vim ]] && eval 'alias '$i=vim
    break
  fi
done
# }}}

[[ -x $(which setxkbmap) ]] && setxkbmap -layout gb  


# vim: foldmethod=marker foldlevel=0 
