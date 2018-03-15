#!/usr/bin/env bash

log(){
	echo "[INFO] ${@}."
}

error(){
	echo "[ERROR] ${@}." 1>&2
}

# check for needed applications
for i in pacman mkdir; do
  [[ ! -x $(command which $i) ]] && error "the executable $i could not be found, the script cannot proceed" && exit 1
done

# make dir if it doesn't exist

[[ ! -e ~/.backup/pacman ]] && mkdir -p ~/.backup/pacman && log "~/.backup/pacman could not be found, making this dir"

# list installed pacman packages (-q without version info) (-e explicitly installed)

if [[ -x $(command which pacman) ]]; then
  pacman -Qeq >~/.backup/pacman/installed.txt && log "saving installed pacman packages to $HOME/.backup/pacman/installed.txt"
fi
