#!/usr/bin/env bash

# symlink all pdfs form ~/Documents recursively

log(){
	echo "[INFO] ${@}."
}

error(){
	echo "[ERROR] ${@}." 1>&2
}

# check for needed applications
for i in find ln readlink basename mkdir; do
  [[ ! -x $(command which $i) ]] && error "the executable $i could not be found, the script cannot proceed" && exit 1
done

# Documents dir doesn't exist, exit
[[ ! -e ~/Documents ]] && error "Could not find ~/Documents" && exit 1

# fail to change dir - exit
cd || error "Could not change dir to $HOME" && exit 1

# make dir if doesn't exist
[[ ! -e ~/Documents/PDF ]] && mkdir -p ~/Documents/PDF && log "~/Documents/PDF not found, making this dir"

for i in $(find ~/Documents -name '*.pdf'); do
  if [[ ! -e "${HOME}/Documents/PDF/$(basename ${i})" ]] && [[ ! -L "${HOME}/Documents/PDF/$(basename ${i})" ]] && [[ ! -L $i ]]; then
    ln -s $(readlink -e $i) "${HOME}/Documents/PDF/$(basename ${i})" && log "Symlinked $(readlink -e $i) to ${HOME}/Documents/PDF/$(basename ${i})"
  fi
done
