# ~/.bashrc

# NOTE THIS SCRIPT ASSUMES THAT YOU GOT THIS FILE BY CLONING https://github.com/nl253/Dot-files/tree/working
# 
# NOTE IF YOU HAVE GIT, THEN RUNNING THIS SCRIPT WILL AUTOMATICALLY CLONE MY SCRIPTS REPO TO $HOME/Scripts/
#
# NOTE THAT BASHS' BEHAVIOURS IS ALSO ALTERED BY `~/.inputrc`

# TODO
# what can go wrong:
# - Scripts might not get downloaded 
# - git might not be on the system
# - it might be that neither curl nor wget is present on the system

#  If not running interactively, don't do anything.
[[ -z "$PS1" ]] && return 0
[[ $- != *i* ]] && return 0
# 

echo -e "${HOME}/.bashrc loaded" # indicator if it has successfully loaded

# $PS1 aka PROMPT 
# default (non-git) prompt
export PS1="\n\[$(tput bold)\]\[$(tput sgr0)\]\[\033[38;5;3m\]\u\[$(tput sgr0)\]\[$(tput sgr0)\]\[\033[38;5;40m\]@\[$(tput bold)\]\[$(tput sgr0)\]\[\033[38;5;31m\]\h\[$(tput sgr0)\]\[$(tput sgr0)\]\[\033[38;5;15m\] \n\[$(tput sgr0)\]\[\033[38;5;241m\]\w\[$(tput sgr0)\]\[\033[38;5;15m\] \[$(tput bold)\]\[$(tput sgr0)\]\[\033[38;5;88m\]>\[$(tput sgr0)\]\[\033[38;5;89m\]>\[$(tput sgr0)\]\[$(tput sgr0)\]\[\033[38;5;15m\] \[$(tput sgr0)\]" # 

#  SHELL OPTIONS

stty -ixon    # enable inc search <C-s> which is often disabled by terminal emulators
stty -ctlecho # turn off control character echoing
complete -d cd
# complete -cf sudo
# complete -d pushd

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
# 

# source all in ~/.shells/ # general shell configuration 
# this will set `$PATH` and allow me to use my scripts 
if [[ -d ~/.shells ]] && [[ -d ~/.bash ]]; then
  for file in ~/.shells/*; do
    [[ -f $file ]] && source "$file"
  done

  # source all in ~/.bash/ # bash-specific configuration
  for file in ~/.bash/*; do
    [[ -f $file ]] && source "$file"
  done
fi
# 

# BY HERE $PATH AND OTHER VARIABLES HAVE BEEN SET !
# `~/.shells/variables.sh` AND
# `~/.shells/aliases.sh` have been sourced

# pull scripts if needed 
if [[ ! -e ~/Scripts ]] && [[ -x $(which git) ]]; then
  echo -e "You don't appear to have scripts. They are necessary to make everything work."
  echo -e "Would you like to download them from GitHub?\nNote, this will clone them into ~/Scripts/"
  REGEX="^[Yy]es"
  read -n 3 -r -p "type [Yes/No] " RESPONSE
  if [[ $RESPONSE =~ $REGEX ]]; then
    echo -e "PULLING https://github.com/nl253/Scripts master branch\n" # Pull Scripts if not present already
    cd
    git clone --recursive https://github.com/nl253/Scripts
  else
    echo -e "OK.\nNothing to be done.\n"
  fi
fi # 

# pull scripts if needed 
if [[ ! -e ~/.dicts ]] && [[ -x $(which git) ]]; then
  echo -e "You don't appear to have dicts."
  echo -e "Would you like to download them from GitHub?\nNote, this will clone them into ~/.dicts/"
  REGEX="^[Yy]es"
  read -n 3 -r -p "type [Yes/No] " RESPONSE
  if [[ $RESPONSE =~ $REGEX ]]; then
    echo -e "PULLING https://github.com/nl253/Dictionaries master branch\n" # Pull Scripts if not present already
    git clone --recursive https://github.com/nl253/Dictionaries ~/.dicts
  else
    echo -e "OK.\nNothing to be done.\n"
  fi
fi # 

# load fzf configuration # alters $PATH
[[ -f ~/.fzf.bash ]] && source ~/.fzf.bash
# [ -e ~/.xsh ] && source ~/.xsh # leaving this one out for now


source ~/.xsh

