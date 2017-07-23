
# ~/.bashrc

# `BASH'S` BEHAVIOURS IS ALSO ALTERED BY `~/.inputrc`

#  If not running interactively, don't do anything.
[[ -z "$PS1" ]] && return 0
[[ $- != *i* ]] && return 0

# source all .sh in ~/.shells/ (general shell configuration)
# source all .sh in ~/.bash/ (bash-specific configuration)
# this will set `$PATH` and allow me to use my scripts

for i in /{etc,usr/share/bash-completion}/bash_completion; do
  [[ -r $i ]] && . $i && break
done

for i in ~/.{shells,bash}/{options,variables,source,functions,setup,fzf,aliases}.sh; do
  [[ -r $i ]] && source $i
done

# vim: foldmethod=marker foldlevel=0
