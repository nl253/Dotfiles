
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

fetch(){ # {{{{
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

unset -f fetch  # done using fetch }}}

# set path...
source ~/.shells/variables.sh

[[ ! -x $(which ranger 2> /dev/null ) ]] && ln -s ~/.applications/ranger/ranger.py ~/.bin/ranger
[[ ! -x $(which project) ]] && ln -s ~/.applications/project/project ~/.bin/project
[[ ! -x $(which sqlite) ]]  && ln -s ~/.applications/sqlite/main.py ~/.bin/sqlite
#[[ ! -x $(which fzf) ]]  && ln -s ~/.applications/fzf/bin/fzf-tmux ~/.bin/fzf
[[ ! -x $(which extractor) ]]  && ln -s ~/Scripts/extractor.sh ~/.bin/extractor
[[ ! -x $(which download-dotfile) ]]  && ln -s ~/Scripts/download-dotfile.sh ~/.bin/download-dotfile
[[ ! -x $(which grf) ]]  && ln -s ~/Scripts/grf.sh ~/.bin/grf
