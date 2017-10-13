#!/usr/bin/env bash

# remove ctags tag files (which by convention have the name `tags`)
for i in find rm; do
  [[ ! -x $(command which $i) ]] && return 0
done

find -type f -and -readable -and -name tags -exec rm {} +
