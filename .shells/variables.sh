
# VARIABLES - SOURCED BY `zsh` and `bash`

# $COLORS {{{
# set variables to produce colored output later 
export RED="\e[31m"
export CYAN="\e[96m"
export DARKMAGENTA="\e[35m"
export MAGENTA="\e[95m"
export BLUE="\e[34mB"
export GREEN="\e[32m"
export WHITE="\e[97mW"
export DEFCOLOR="\e[39m"
export YELLOW="\e[93m"
export DARKYELLOW="\e[33m"
export GREY="\e[37m"
export DARKGREY="\e[90m"

PYTHON="${YELLOW}PYTHON${DEFCOLOR}"
HASKELL="${BLUE}HASKELL${DEFCOLOR}"
RUBY="${RED}RUBY${DEFCOLOR}"
JAVSCRIPT="${DARKYELLOW}JAVSCRIPT${DEFCOLOR}"
# }}}

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

function add-to-path(){
  for directory in $@; do
    if [[ -d $directory ]]; then
      export PATH="${directory}:${PATH}:"
    fi
  done
}

export PATH="/usr/local/sbin:/bin:/usr/local/bin:/usr/bin/site_perl:/usr/bin/vendor_perl:/usr/bin/core_perl:/usr/bin:"

[ ! -e ~/Scripts ] && mkdir -p ~/Scripts && git clone https://github.com/nl253/Scripts ~/Scripts                                         # MY SCRIPTS

add-to-path ~/{.local,.cabal,.cargo,.gem,go,anaconda3}/bin  ~/{Scripts,.bin,.config/composer/vendor/bin} /usr/local/go/bin ~/.gem/ruby/*/bin

unset -f add-to-path

[ -d /usr/lib/jvm/java-8-openjdk ] && export JAVA_HOME='/usr/lib/jvm/java-8-openjdk' && export JRE_HOME='/usr/lib/jvm/java-8-openjdk/jre'  # JAVA

#if [ -e ~/.pyenv/bin/pyenv ]; then
  #export PYENV_ROOT="${HOME}/.pyenv"
  #export PATH="${PYENV_ROOT}/bin:${PATH}:"
  ##export PYENV_VERSION='pypy3.5-5.7.1-beta'
  #export PYENV_VIRTUALENV_DISABLE_PROMPT=1
#fi

# }}}

# HISTORY {{{
export HISTSIZE=20000
export HISTFILESIZE=20000
export HISTCONTROL="ignoreboth:erasedups"
export HISTTIMEFORMAT=""
export HH_CONFIG=hicolor # get more colors

# HISTIGNORE
# ----------------------------------------------------------------------------------------------------------
# A colon-separated list of patterns used to decide which command lines should be saved on the history list.
# It must match the complete line (no implicit `*' is appended).
# The pattern is tested against the line after the checks specified by HISTCONTROL are applied.
# In addition to the normal shell pattern matching characters, `&' matches the previous  history  line.
# The pattern  matching honors the setting of the extglob shell option.
# ----------------------------------------------------------------------------------------------------------
export HISTIGNORE="&:[ ]*:exit:cd:ls:bg:fg:history:clear:jobs" # }}}

export GREP_COLOR='1;33' # makes it yellow # by default red

export LSCOLORS=ExFxCxdxBxegedabagacad

export LS_COLORS='di=01;34:ln=01;35:so=01;32:ex=01;31:bd=46;34:cd=43;34:su=41;30:sg=46;30:tw=42;30:ow=43;30'

# $PAGER {{{
# ------------------------------------------------------------------
# if available enable syntax highlighting # fall back on `more` if `less` not available
# tries to set default pager as `less` and add coloring to the output if possible
# falls back on `more` if available
# ------------------------------------------------------------------
if [ -x /usr/bin/less ] || [ -x /bin/less ]; then
  alias less='less -x4RFsX' && export PAGER=less
else
  [ -x /usr/bin/more ] && export PAGER=more && alias less=more
fi
# }}}

# $BROWSER {{{
# -------------------------------------------------
# uses `google chrome` if available ie if running on a gui 
# fall back on 1. `elinks` 2. `lynx` 3. `w3m`
# -------------------------------------------------
if [ -x /usr/bin/google-chrome-stable ] || [ -x /bin/google-chrome-stable ]; then
  export BROWSER=google-chrome-stable
elif [ -x /bin/elinks ] || [ -x /usr/bin/elinks ]; then
  export BROWSER=elinks
elif [ -x /bin/lynx ] || [ -x /usr/bin/lynx ]; then
  export BROWSER=lynx
elif [ -x /bin/w3m ] || [ -x /usr/bin/w3m ]; then
  export BROWSER=w3m
fi
# }}}

# $EDITOR  {{{
# ---------------------------------------------------------------------- 
# attempt to set to neo-vim if available, fall back on vim and then vi
# ----------------------------------------------------------------------
if [ -x $(which vim) ] ; then # if vim but not neovim
  export EDITOR=$(which vim)
  alias vi=$(which vim)
  # set up vim plugins
elif [ -x /usr/bin/vi ] || [ -x /bin/vi ]; then # if not neovim and not vim then fall back on vi
  export EDITOR=$(which vi)
  alias vim=$(which vi)
elif [ -x $(which nvim) ]; then 
  export EDITOR=$(which nvim)
  alias vim=$(which nvim)
fi

# }}}

[ -x /usr/bin/setxkbmap ] || [ -x /bin/setxkbmap ] && setxkbmap -layout gb  

# vim: foldmethod=marker foldlevel=0 
