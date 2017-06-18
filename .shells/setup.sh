
# SETUP 

ensure-dir-exists(){  # {{{
  for i in $@; do
    [[ ! -e $i ]] && mkdir -p $i
  done
}   

ensure-dir-exists ~/.{bin,applications,shells,zsh,bash,shells} 
ensure-dir-exists ~/.vim/{swap,backup,undo}

unset -f ensure-dir-exists

 # }}}

# automatically link /tmp to ~/Downloads  {{{
[[ -d ~/Downloads ]] && [[ ! -L ~/Downloads ]] && rm -rf ~/Downloads
[[ ! -e ~/Downloads ]] && ln -s /tmp ~/Downloads
# }}}

# fetch necessary files and dirs # {{{{
fetch-dir(){ 
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

fetch-dotfile() {
  [ $# = 0 ] && echo -e "\n$(basename $0) : You need to specify at least 1 dotfile.\nAborting.\n\n" && return 1
  for i in "$@"; do
    if [ ! -e "${HOME}/${i}" ]; then
      echo -e "You don't appear to have ${HOME}/${i} ...\n"
      read -n 3 -r -p "Would you like to fetch it from GitHub? [yes/no] " RESPONSE
      local REGEX="^[Yy]es"
      if [[ $RESPONSE =~ $REGEX ]]; then
        echo -e "\nDownloading from https://raw.githubusercontent.com/nl253/Dot-files/master/${i} ...\n"
        curl -o "${HOME}/${i}" "https://raw.githubusercontent.com/nl253/Dot-files/master/${i}"
      else
        echo -e "Aboring.\n" && return 1
      fi
    fi
  done
}

fetch-dir Scripts nl253/Scripts 
fetch-dir .dicts nl253/Dictionaries
fetch-dir .applications/sqlite nl253/SQLiteREPL
fetch-dir .applications/project nl253/ProjectGenerator
fetch-dir .applications/ranger ranger/ranger
fetch-dir .applications/fzf junegunn/fzf.git 

fetch-dotfile .bashrc .zshrc .inputrc .gitconfig

unset -f fetch-dotfile # done using fetch-dotfile 

unset -f fetch-dir  # done using fetch-dir }}}

# set path...
source ~/.shells/variables.sh

[[ ! -x $(which ranger 2> /dev/null ) ]] && ln -s ~/.applications/ranger/ranger.py ~/.bin/ranger
[[ ! -x $(which project) ]] && ln -s ~/.applications/project/project ~/.bin/project
[[ ! -x $(which sqlite) ]]  && ln -s ~/.applications/sqlite/main.py ~/.bin/sqlite
#[[ ! -x $(which fzf) ]]  && ln -s ~/.applications/fzf/bin/fzf-tmux ~/.bin/fzf
[[ ! -x $(which extractor) ]]  && ln -s ~/Scripts/extractor.sh ~/.bin/extractor
[[ ! -x $(which download-dotfile) ]]  && ln -s ~/Scripts/download-dotfile.sh ~/.bin/download-dotfile
[[ ! -x $(which grf) ]]  && ln -s ~/Scripts/grf.sh ~/.bin/grf
