#!/usr/bin/env bash

# set the background

log(){
	echo "[INFO] ${@}."
}

error(){
	echo "[ERROR] ${@}." 1>&2
}

# check for needed applications
for i in feh; do
  [[ ! -x $(command which $i) ]] && error "the executable $i could not be found, the script cannot proceed" && exit 1
done

[[ -f ~/Pictures/desktop.png ]] && feh --bg-fill ~/Pictures/desktop.png && log "Successfully changed the background to $HOME/Pictures/desktop.png" || error "Could not change the background to $HOME/Pictures/desktop.png. The file could not be found"
