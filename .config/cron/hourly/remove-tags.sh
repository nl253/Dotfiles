#!/usr/bin/env bash

# remove ctags tag files

log(){
	echo "[INFO] ${@}."
}

error(){
	echo "[ERROR] ${@}." 1>&2
}

# check for needed applications
for i in find rm; do
  [[ ! -x $(type -P $i) ]] && error "the executable $i could not be found, the script cannot proceed" && exit 1
done

# fail to change dir - exit
cd || (error "Could not change dir to $HOME" && exit 1)

# by convention have the name `tags`
find -type f -readable -name tags -delete
