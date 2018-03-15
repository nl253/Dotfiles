#!/usr/bin/env bash

# remove dead symlinks from ~/Documents 

log(){
	echo "[INFO] ${@}."
}

error(){
	echo "[ERROR] ${@}." 1>&2
}

# check for needed applications
for i in find rm; do
  [[ ! -x $(command which $i) ]] && error "the executable $i could not be found, the script cannot proceed" && exit 1
done

# Documents dir doesn't exist, exit
[[ ! -e ~/Documents ]] && error "Could not find ~/Documents" && exit 1

find ~/Documents -xtype l -delete
