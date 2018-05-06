#!/usr/bin/env bash

for entry in $(cat /dev/stdin); do
  if [[ -e $(readlink -e "${entry}") ]]; then
    echo $entry
  fi
done
