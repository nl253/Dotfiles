#!/usr/bin/env bash

# Pipe entries from .gitignore to this script to filter out unused.


for entry in $(cat /dev/stdin); do
  if [[ -e $(readlink -e "${entry}") ]]; then
    if [[ -e $(readlink -e "$expanded") ]]; then
    echo $entry
  fi
  done
done
