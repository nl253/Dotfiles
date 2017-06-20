
# THIS FILE MUST USE POSIX COMPLIANT SYNTAX
# IT IS SOURCED BY BOTH `zsh` AND `bash`

# SETUP 

# automatically link /tmp to ~/Downloads  {{{
[[ -d ~/Downloads ]] && [[ ! -L ~/Downloads ]] && rm -rf ~/Downloads
[[ ! -e ~/Downloads ]] && ln -s /tmp ~/Downloads
# }}}

clone-repo(){  # {{{
  # arg1 : ~/${DIR RELATIVE TO ~}
  # arg2 : https://github.com/${RELATIVE TO GitHub} 
  # pull scripts if needed 
  [[ $# != 2 ]] && return 1
  if [[ ! -e ~/$2 ]] && [[ -x $(which git 2>/dev/null) ]]; then
    echo -e "You don't appear to have ~/${2}."
    echo -e "Would you like to download ${2} from https://github.com/${1} ?\nNote, this will clone them into ~/${2}."
    REGEX="^[Yy]es"
    read -n 3 -r -p "type [Yes/No] " RESPONSE
    if [[ $RESPONSE =~ $REGEX ]]; then
      echo -e "PULLING https://github.com/${2} master branch\n" # Pull Scripts if not present already
      mkdir -p ~/$2
      git clone https://github.com/$1 ~/$2
    else
      echo -e "OK.\nNothing to be done.\n"
    fi
  fi
}  # }}}

# remove dead links {{{
for link in $(ls ~/.bin); do
  if [[ ! -x ~/.bin/$link ]] || [[ -L ~/.bin/$link ]] && [[ $(readlink -e ~/.bin/$link) == "" ]]; then
    rm ~/.bin/$link
  fi
done
# }}}

# fetch-script() {{{
# FUNCTION
# --------
# args: script names
link-script(){ 
  for script in $@ ; do
    out=$(echo $script | sed -E "s/\.\w+$//")
    if [[ ! -e ~/.bin/$out ]] && [[ -f ~/.scripts/$script ]]; then 
      ln -s ~/.scripts/$script ~/.bin/$out
    fi
  done
}
# }}}

install-app(){ # {{{
  [[ $# != 4 ]] && echo "$0 : You must pass 4 args." && return 1

  clone-repo $1 $2 

  if [[ ! -x $(which $4 2>/dev/null) ]] && [[ -x ~/.applications/$3 ]] && [[ ! -e ~/.bin/$3 ]]; then 
    ln -s ~/.applications/$3 ~/.bin/$4
  fi
  
}
# }}}

# set path...
[[ ! $_VARIABLES_SOURCED ]] && source ~/.shells/variables.sh

mkdir -p ~/.{bin,applications,shells,zsh,bash,shells} ~/.vim/{swap,backup,undo}

install-app ranger/ranger .applications/ranger ranger/ranger.py ranger 
install-app nl253/ProjectGenerator .applications/project project/project project 
install-app nl253/SQLiteREPL .applications/sqlite sqlite/main.py sqlite

clone-repo nl253/Scripts Projects/Scripts
clone-repo nl253/Scripts .scripts

# {{{ fzf not found ... Install ... 
if [[ ! -x $(which fzf) ]]; then       
  cd /tmp && wget https://github.com/junegunn/fzf-bin/releases/download/0.16.8/fzf-0.16.8-linux_amd64.tgz || exit 1
  tar xfvz fzf-0.16.8-linux_amd64.tgz && mv ./fzf ~/.bin/fzf
  cd
fi
# }}}

clone-repo junegunn/fzf.git .applications/fzf

link-script {show-ip,grf,csv-preview,extractor,download-dotfile,p,env,ipython,pandoc,csv-preview}.sh  

# unset functions {{{
unset -f link-script 
unset -f clone-repo  
unset -f install-app  
# }}}

export _SETUP_SOURCED=1
