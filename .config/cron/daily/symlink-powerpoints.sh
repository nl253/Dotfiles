#!/usr/bin/env bash

# symlink all powerpoints form ~/Documents recursively

log(){
	echo "[INFO] ${@}."
}

error(){
	echo "[ERROR] ${@}." 1>&2
}

# check for needed applications
for i in mkdir readlink basename find ln; do
  [[ ! -x $(command which $i) ]] && error "the executable $i could not be found, the script cannot proceed" && exit 1
done

# Documents dir doesn't exist, exit
[[ ! -e ~/Documents ]] && error "Could not find ~/Documents" && exit 1

# fail to change dir - exit
cd || error "Could not change dir to $HOME" && exit 1

# make dir if doesn't exist
[[ ! -e ~/Documents/PPT ]] && mkdir -p ~/Documents/PPT && log "~/Documents/PPT not found, making this dir"

# powerpoint extensios are either pptx (newer) or ppt (older)
for i in $(find ~/Documents -name '*.pptx' -or -name '*.ppt'); do
  if [[ ! -e "${HOME}/Documents/PPT/$(basename ${i})" ]] && [[ ! -h "${HOME}/Documents/PPT/$(basename ${i})" ]] && [[ ! -h $i ]]; then
		ln -s $(readlink -e $i) "${HOME}/Documents/PPT/$(basename ${i})" && log "Symlinked $(readlink -e $i) to ${HOME}/Documents/PPT/$(basename ${i})"
  fi
done
