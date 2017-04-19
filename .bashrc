#!/usr/bin/env bash

# ~/.bashrc

# {{{ If not running interactively, don't do anything.
[[ -z "$PS1" ]] && return 0
[[ $- != *i* ]] && return 0
# }}}

for file in ~/.shells/*; do
  [[ -f $file ]] && source "$file" # source all in ~/.shells/ # general shell configuration 
done

for file in ~/.bash/*; do
  [[ -f $file ]] && source "$file" # source all in ~/.bash/ # bash-specific configuration
done

# BY HERE $PATH AND VARIABLES HAVE BEEN SET !

if [[ -x /usr/bin/git ]] && [[ ! -e ~/Scripts ]]; then 
  echo -e "PULLING from https://github.com/nl253/Scripts master\n" # Pull Scripts if not present already
  git clone --recursive https://github.com/nl253/Scripts ~
fi

# BY HERE WE HAVE ALL THE SCRIPTS AND VARIABLES ARE SET !
setup-vim.sh

echo -e "${RED}~/.bashrc ${YELLOW}loaded" # indicator if it has successfully loaded

[ -f ~/.fzf.bash ] && source ~/.fzf.bash # load fzf configuration

[ -e ~/.xsh ] && source ~/.xsh # ?

# $PS1 aka PROMPT {{{

# default (non-git) prompt
export PS1="\n\[$(tput bold)\]\[$(tput sgr0)\]\[\033[38;5;3m\]\u\[$(tput sgr0)\]\[$(tput sgr0)\]\[\033[38;5;40m\]@\[$(tput bold)\]\[$(tput sgr0)\]\[\033[38;5;31m\]\h\[$(tput sgr0)\]\[$(tput sgr0)\]\[\033[38;5;15m\] \n\[$(tput sgr0)\]\[\033[38;5;241m\]\w\[$(tput sgr0)\]\[\033[38;5;15m\] \[$(tput bold)\]\[$(tput sgr0)\]\[\033[38;5;88m\]>\[$(tput sgr0)\]\[\033[38;5;89m\]>\[$(tput sgr0)\]\[$(tput sgr0)\]\[\033[38;5;15m\] \[$(tput sgr0)\]" # }}}


stty -ixon    # enable inc search <C-s> which is often disabled by terminal emulators
stty -ctlecho # turn off control character echoing
complete -cf sudo
# complete -d cd
complete -d pushd

# {{{ SHELL OPTIONS
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
bind Space:magic-space # Expand "!" history when pressing space

