# ~/.bashrc

# NOTE THIS SCRIPT ASSUMES THAT YOU GOT THIS FILE BY CLONING https://github.com/nl253/Dot-files
# 
# NOTE THAT `BASHS'` BEHAVIOURS IS ALSO ALTERED BY `~/.inputrc`

#  If not running interactively, don't do anything.
[[ -z "$PS1" ]] && return 0
[[ $- != *i* ]] && return 0

# indicator if it has successfully loaded
echo -e "${HOME}/.bashrc loaded" 

# source all .sh in ~/.shells/ (general shell configuration) 
# source all .sh in ~/.bash/ (bash-specific configuration)
# this will set `$PATH` and allow me to use my scripts 

for i in ~/.shells/{variables,source,setup,fzf,aliases}.sh ~/.bash/{options,variables,source,functions,aliases}.sh ; do
  [[ -r $i ]] && source $i
done

# vim: foldmethod=marker foldlevel=0
