
# THIS FILE MUST USE POSIX COMPLIANT SYNTAX
# IT IS SOURCED BY ALL INTERACTIVE SHELLS

# VARIABLES 
# --------

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
unset MAILCHECK                      # Don't check mail when opening terminal.
export SHORT_HOSTNAME=$(hostname -s) # Set Xterm/screen/Tmux title with only a short hostname
[ -f ~/.config/ranger/rc.conf ] && export RANGER_LOAD_DEFAULT_RC=false # }}}

# $PATH (and JAVA_HOME and JRE_HOME) {{{
# ------------------------------------------------------------------------
# FUNCTION :: add packages from all package managers to $PATH if these paths exist along with my own scripts in ~/Scripts/
# ------------------------------------------------------------------------
export PATH="/usr/local/sbin:/usr/local/bin:/usr/bin/site_perl:/usr/bin/vendor_perl:/usr/bin/core_perl:/usr/bin/:"
[[ -d ~/miniconda3/bin ]] && export PATH="${PATH}:${HOME}/miniconda3/bin"
[ -d /usr/lib/jvm/java-8-openjdk ] && export JAVA_HOME='/usr/lib/jvm/java-8-openjdk' && export JRE_HOME='/usr/lib/jvm/java-8-openjdk/jre' # JAVA
[ -d ~/.gem/ruby/2.4.0/bin ] && export PATH="${PATH}:${HOME}/.gem/ruby/2.4.0/bin" # GEM [RUBY]
[ -d ~/.gem/ruby/2.3.0/bin ] && export PATH="${PATH}:${HOME}/.gem/ruby/2.3.0/bin" # GEM [RUBY]
[ -d ~/.cargo/bin ] && export PATH="${PATH}:${HOME}/.cargo/bin" # CARGO [RUST]
[ -d ~/.cabal/bin ] && export PATH="${HOME}/.cabal/bin:${PATH}" # CABAL [HASKELL]
[ -d ~/.config/composer/vendor/bin ] && export PATH="${PATH}:${HOME}/.config/composer/vendor/bin" # COMPOSER [PHP]
[ -d ~/.local/bin ] && export PATH="${PATH}:${HOME}/.local/bin" # PIP [PYTHON]
[ -d ~/go/bin ] && export PATH="${PATH}:${HOME}/go/bin" # GO 
[ ! -e ~/Scripts ] && mkdir -p ~/Scripts && git clone https://github.com/nl253/Scripts ~/Scripts/ # clone my Scripts repo  
[ -d ~/Scripts ] && export PATH="${PATH}:${HOME}/Scripts" # MY SCRIPTS
[ -d ~/miniconda3 ] && export PATH="${HOME}/miniconda3/bin:${PATH}"
# }}}

# HISTORY {{{
export HISTSIZE=20000
export HISTFILESIZE=20000
export HISTCONTROL="ignoreboth:erasedups"
export HISTTIMEFORMAT=""
export HH_CONFIG=hicolor         # get more colors

# HISTIGNORE
# ----------------------------------------------------------------------------------------------------------
# A colon-separated list of patterns used to decide which command lines should be saved on the history list.
# It must match the complete line (no implicit `*' is appended).
# The pattern is tested against the line after the checks specified by HISTCONTROL are applied.
# In addition to the normal shell pattern matching characters, `&' matches the previous  history  line.
# The pattern  matching honors the setting of the extglob shell option.
# ----------------------------------------------------------------------------------------------------------
export HISTIGNORE="&:[ ]*:exit:cd:ls:bg:fg:history:clear:jobs" # }}}

# $IRC_CLIENT  {{{
# default to `irssi` and fall back on `hexchat`
[ -x /usr/bin/irssi ] && export IRC_CLIENT='irssi'
[ ! -x /usr/bin/irssi ] && [ -x /usr/bin/hexchat ] && export IRC_CLIENT='hexchat' # }}}

export GREP_COLOR='1;33' # makes it yellow # by default red

# $PAGER {{{
# ------------------------------------------------------------------
# if available enable syntax highlighting # fall back on `more` if `less` not available
# tries to set default pager as `less` and add coloring to the output if possible
# falls back on `more` if available
# ------------------------------------------------------------------
[ -f /usr/bin/source-highlight-esc.sh ] && export LESSOPEN="| /usr/bin/source-highlight-esc.sh %s"
[ -x /usr/bin/less ] && alias less='less -x4RFsX' && export PAGER=less
[ ! -x /usr/bin/less ] && [ -x /usr/bin/more ] && export PAGER=more && alias less=more
# }}}

# $BROWSER {{{
# -------------------------------------------------
# uses `google chrome` if available ie if running on a gui 
# fall back on 1. `elinks` 2. `lynx` 3. `w3m`
# -------------------------------------------------
if [ -x /usr/bin/google-chrome-stable ]; then
  export BROWSER=google-chrome-stable
elif [ -x /usr/bin/elinks ]; then
  export BROWSER=elinks
elif [ -x /usr/bin/lynx ]; then
  export BROWSER=lynx
elif [ -x /usr/bin/w3m ]; then
  export BROWSER=w3m
fi
# }}}

# $EDITOR  {{{
# ---------------------------------------------------------------------- 
# attempt to set to neo-vim if available, fall back on vim and then vi
# ----------------------------------------------------------------------
if [ -x /usr/bin/nvim ]; then # if neovim
  export EDITOR=/usr/bin/nvim
  alias vim=/usr/bin/nvim
  alias vi=/usr/bin/nvim

elif [ -x /usr/bin/vim ]; then # if vim but not neovim
  export EDITOR=/usr/bin/vim
  alias nvim=/usr/bin/vim
  alias vi=/usr/bin/vim
  # set up vim plugins
elif [ -x /usr/bin/vi ]; then # if not neovim and not vim then fall back on vi
  export EDITOR=/usr/bin/vi
  alias vim=vi
  alias nvim=vi
fi

#vim:set foldmethod=marker:set foldlevel=0 # }}}
