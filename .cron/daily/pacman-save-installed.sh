#!/usr/bin/env bash

if [[ -x $(command which pacman) ]]; then
	pacman -Qq >~/.backup/pacman/installed.txt
fi
