#!/bin/bas

# Download any dotfile from my Dot-files repo (github master branch) by specifying a name relative to $HOME 
# To be run by bash on a remote machine or in cases of emergency on own.
# It first checks if an existing dotfile.
# If so, then it asks if it's ok to rename it to ~/.gitconfig.backup.
# Finally if there is no risk of overwriting, then it downloads the file from github.
# -----------------------------------------------------------------------
# REQUIRES :: internet connection
# -----------------------------------------------------------------------
# DEPENDENCIES :: git curl

download-dotfile() {
  [ $# ] <1 && echo -e "$(basename $0) : You need to specify at least 1 dotfile.\nAborting.\n\n"
  for i in $@; do
    if [ -f "${HOME}/${i}" ]; then
      echo -e "An exisits ${HOME}/${i} was detected...\n"
      read -n 3 -r -p "Would you like to move it to ~/${i}.backup ? [yes/no] " RESPONSE
      local REGEX="^[Yy]es"
      if [[ $RESPONSE =~ $REGEX ]]; then
        mv ~/.gitconfig ~/.gitconfig.backup
      else
        echo -e "Aboring.\n" && return 1
      fi
    fi
    echo -e "Downloading .gitconfig from https://raw.githubusercontent.com/nl253/Dot-files/master/.gitconfig master branch...\n"
    [ ! -e ~/.gitconfig ] && curl -o ~/.gitconfig https://raw.githubusercontent.com/nl253/Dot-files/master/.gitconfig
    echo -e "Sourcing ~/.gitconfig.\n"
    source ~/.gitconfig # at the end source for convenience
  done
}

download-gitconfig
