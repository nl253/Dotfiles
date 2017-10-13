#!/usr/bin/env bash

if [[ -x $(command which pacman) ]]; then
  pacman -Qeq >~/.backup/pacman/installed.txt
fi
