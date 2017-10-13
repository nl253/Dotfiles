#!/usr/bin/env bash

for i in find rm; do
  [[ ! -x $(command which $i) ]] && return 0
done

find -type f -and -readable -and -name tags -exec rm {} +
