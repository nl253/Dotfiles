# ~/.bashrc

# IF YOU HAVE GIT, THEN RUNNING THIS SCRIPT WILL AUTOMATICALLY CLONE MY SCRIPTS REPO TO $HOME/Scripts/
# NOTE THAT BASHS' BEHAVIOURS IS ALSO ALTERED BY `~/.inputrc`

# {{{ If not running interactively, don't do anything.
[[ -z "$PS1" ]] && return 0
[[ $- != *i* ]] && return 0
# }}}

CURL=$(which curl)
GIT=$(which git)

[[ -f ~/.fzf.bash ]] && source ~/.fzf.bash # load fzf configuration # alters $PATH
# [ -e ~/.xsh ] && source ~/.xsh # leaving this one out for now

for file in ~/.shells/*; do # source all in ~/.shells/ # general shell configuration
  [[ -f $file ]] && source "$file"
done

for file in ~/.bash/*; do # source all in ~/.bash/ # bash-specific configuration
  [[ -f $file ]] && source "$file"
done

# BY HERE $PATH AND OTHER VARIABLES HAVE BEEN SET !
# `~/.shells/variables.sh` AND
# `~/.shells/aliases.sh` have been sourced

if [[ -x $GIT ]] && [[ ! -e ~/Scripts ]]; then
  echo -e "PULLING from https://github.com/nl253/Scripts master\n" # Pull Scripts if not present already
  git clone --recursive https://github.com/nl253/Scripts ~
fi

# if neither
if [[ ! -f ~/.vimrc ]] && [[ ! -f ~/.config/nvim/init.vim ]]; then
  echo -e "Neither .vimrc nor init.vim were detected on this system."
  echo -e "\nWould you like to run vim-init ?"
  REGEX="^[Yy]es"
  read -n 3 -r -p "Would you like to move it to ~/${i}.backup ? [yes/no] " RESPONSE
  [[ $RESPONSE =~ $REGEX ]] && setup-vim.sh || echo -e "OK.\nNothing to be done.\n"
fi

if [[ ! -f ~/.bash/completions/tmuxinator.bash ]] && [[ -x $CURL ]]; then # pull tmuxinator completions if not present
  mkdir -p ~/.bash/completions # in case it doesn't exist
  # get it from github
  curl -fLo ~/.bash/completions/tmuxinator.bash "https://raw.githubusercontent.com/tmuxinator/tmuxinator/master/completion/tmuxinator.bash"
  source ~/.bash/completions/tmuxinator.bash # source to be ready straight away
fi

# BY HERE WE HAVE ALL THE SCRIPTS AND VARIABLES ARE SET !

echo -e "${RED}~/.bashrc ${YELLOW}loaded" # indicator if it has successfully loaded

# $PS1 aka PROMPT {{{
# default (non-git) prompt
export PS1="\n\[$(tput bold)\]\[$(tput sgr0)\]\[\033[38;5;3m\]\u\[$(tput sgr0)\]\[$(tput sgr0)\]\[\033[38;5;40m\]@\[$(tput bold)\]\[$(tput sgr0)\]\[\033[38;5;31m\]\h\[$(tput sgr0)\]\[$(tput sgr0)\]\[\033[38;5;15m\] \n\[$(tput sgr0)\]\[\033[38;5;241m\]\w\[$(tput sgr0)\]\[\033[38;5;15m\] \[$(tput bold)\]\[$(tput sgr0)\]\[\033[38;5;88m\]>\[$(tput sgr0)\]\[\033[38;5;89m\]>\[$(tput sgr0)\]\[$(tput sgr0)\]\[\033[38;5;15m\] \[$(tput sgr0)\]" # }}}

stty -ixon    # enable inc search <C-s> which is often disabled by terminal emulators
stty -ctlecho # turn off control character echoing
complete -d cd
# complete -cf sudo
# complete -d pushd

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
