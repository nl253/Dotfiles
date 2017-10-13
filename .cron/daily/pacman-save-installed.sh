#!/usr/bin/env bash

# list installed pacman packages (-q without version info) (-e explicitly installed)
if [[ -x $(command which pacman) ]]; then
  pacman -Qeq >~/.backup/pacman/installed.txt
fi
