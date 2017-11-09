
# variables to be read by sh(1p)

export HISTFILESIZE=20000
export HISTCONTROL="ignoreboth:erasedups"
export HISTTIMEFORMAT=""
export HH_CONFIG=hicolor # get more colors
export HISTIGNORE="&:[ ]*:exit:cd:ls:bg:fg:history:clear:jobs"

export HISTFILE=~/.shell_history
export SAVEHIST=10000

# vim: foldmethod=marker foldlevel=0 foldmarker={,} shiftwidth=2 tabstop=2
