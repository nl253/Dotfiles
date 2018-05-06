#!/usr/bin/env bash

for entry in $(cat /dev/stdin); do
  for expanded in $(eval "echo ${entry}"); do
    if [[ -e $(readlink -e "$expanded") ]]; then
      echo $entry
    fi
  done
done
