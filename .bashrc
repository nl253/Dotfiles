# ~/.bashrc

# NOTE THIS SCRIPT ASSUMES THAT YOU GOT THIS FILE BY CLONING https://github.com/nl253/Dot-files
# 
# NOTE IF YOU HAVE GIT, THEN RUNNING THIS SCRIPT WILL AUTOMATICALLY ATTEMPT TO CLONE:
#       - MY SCRIPTS REPO TO $HOME/Scripts/
#       - MY DICTS REPO TO $HOME/.dicts/
#       - RANGER (IF NOT PRESENT) REPO TO $HOME/.applications/ranger
#       - MY PROJECT GENERATOR (IF NOT PRESENT) REPO TO $HOME/.applications/project
#       - MY SQLITE PROMPT_TOOLKIT-BASED CLIENT (IF NOT PRESENT) REPO TO $HOME/.applications/sqlite
#
# NOTE THAT `BASHS'` BEHAVIOURS IS ALSO ALTERED BY `~/.inputrc`

#  If not running interactively, don't do anything.
[[ -z "$PS1" ]] && return 0
[[ $- != *i* ]] && return 0

# indicator if it has successfully loaded
echo -e "${HOME}/.bashrc loaded" 

# $PS1 aka PROMPT {{{
# default (non-git) prompt
export PS1="\n\[$(tput bold)\]\[$(tput sgr0)\]\[\033[38;5;3m\]\u\[$(tput sgr0)\]\[$(tput sgr0)\]\[\033[38;5;40m\]@\[$(tput bold)\]\[$(tput sgr0)\]\[\033[38;5;31m\]\h\[$(tput sgr0)\]\[$(tput sgr0)\]\[\033[38;5;15m\] \n\[$(tput sgr0)\]\[\033[38;5;241m\]\w\[$(tput sgr0)\]\[\033[38;5;15m\] \[$(tput bold)\]\[$(tput sgr0)\]\[\033[38;5;88m\]>\[$(tput sgr0)\]\[\033[38;5;89m\]>\[$(tput sgr0)\]\[$(tput sgr0)\]\[\033[38;5;15m\] \[$(tput sgr0)\]" # 
# }}}

#  SHELL OPTIONS {{{

stty -ixon    # enable inc search <C-s> which is often disabled by terminal emulators
stty -ctlecho # turn off control character echoing

shopt -s autocd
shopt -s cdspell      # correct minor spelling errors
shopt -s checkwinsize # update the value of LINES and COLUMNS after each command if altered
shopt -s direxpand    # replaces directory names with expansion when <tab>
shopt -s dirspell     # correct minor spelling errors
shopt -s dotglob      # Include dotfiles in pathname expansion
shopt -s checkjobs    # Bash lists the status of any stopped and running jobs before exiting an interactive shell. If any jobs are running, this causes the exit to be deferred until a second exit is attempted
shopt -s extglob      # Enable extended pattern-matching features
shopt -s nullglob
shopt -s nocaseglob # matches filenames in a case-insensitive fashion when performing pathname expansion.
shopt -s globstar   # ** becomes a recursive wildstar
shopt -s histappend # Append each session's history to $HISTFILE
shopt -s histverify # History expansions will be verified before execution.
shopt -s histreedit # Allow use to re-edit a faild history substitution.
# }}}

# SOURCE {{{
# source all .sh in ~/.shells/ (general shell configuration) 
# source all .sh in ~/.bash/ (bash-specific configuration)
# this will set `$PATH` and allow me to use my scripts 

for i in ~/.shells/{variables,source,setup,fzf,aliases}.sh ~/.bash/{source,functions,aliases}.sh ; do
  source $i
done

# BY HERE $PATH AND OTHER VARIABLES HAVE BEEN SET !
# `~/.shells/variables.sh` AND
# `~/.shells/aliases.sh` have been sourced

# }}}

# vim: foldmethod=marker foldlevel=0

[ -f ~/.fzf.bash ] && source ~/.fzf.bash
