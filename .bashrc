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
# NOTE THAT BASHS' BEHAVIOURS IS ALSO ALTERED BY `~/.inputrc`

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
#complete -d cd
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

# BY HERE $PATH AND OTHER VARIABLES HAVE BEEN SET !
# `~/.shells/variables.sh` AND
# `~/.shells/aliases.sh` have been sourced

safe-source(){
  for i in $@; do
    [[ -f $i ]] && source $i
  done
}

safe-source ~/.fzf.bash ~/.travis/travis.sh

ensure-dir-exists(){
  for i in $@; do
    [[ ! -e $i ]] && mkdir -p $i
  done
}

ensure-dir-exists ~/.vim/{swap,backup} ~/.applications ~/.bin

# automatically link /tmp to ~/Downloads 
[[ -d ~/Downloads ]] && [[ ! -L ~/Downloads ]] && rm -rf ~/Downloads
[[ ! -e ~/Downloads ]] && ln -s /tmp ~/Downloads

fetch(){
  # arg1 : ~/${DIR RELATIVE TO ~}
  # arg2 : https://github.com/${} 
  # pull scripts if needed 
  if [[ ! -e ~/$1 ]] && [[ -x $(which git) ]]; then
    echo -e "You don't appear to have ~/${1}."
    echo -e "Would you like to download ${1} from https://github.com/${2} ?\nNote, this will clone them into ~/${1}."
    REGEX="^[Yy]es"
    read -n 3 -r -p "type [Yes/No] " RESPONSE
    if [[ $RESPONSE =~ $REGEX ]]; then
      echo -e "PULLING https://github.com/${2} master branch\n" # Pull Scripts if not present already
      mkdir -p ~/$1
      git clone https://github.com/$2 ~/$1
    else
      echo -e "OK.\nNothing to be done.\n"
    fi
  fi
}

fetch Scripts nl253/Scripts
fetch .dicts nl253/Dictionaries
fetch .applications/sqlite nl253/SQLiteREPL
fetch .applications/project nl253/ProjectGenerator
fetch .applications/ranger ranger/ranger
fetch .applications/fzf junegunn/fzf.git 

[[ ! -x $(which ranger) ]] && ln -s ~/.applications/ranger/ranger.py ~/.bin/ranger
[[ ! -x $(which project) ]] && ln -s ~/.applications/project/project ~/.bin/project
[[ ! -x $(which sqlite) ]]  && ln -s ~/.applications/sqlite/main.py ~/.bin/sqlite
[[ ! -x $(which fzf) ]]  && ln -s ~/.applications/sqlite/main.py ~/.bin/sqlite

# execute only by my PC at home
if [[ -f ~/.pc ]]; then
  tmux -c zsh
fi
