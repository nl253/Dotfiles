# ~/.bashrc: executed by bash(1) for non-login shells.

# If not running interactively, don't do anything
case $- in
  *i*) ;;
  *) return ;;
esac

# Check if bash version is at least 4 to run some of my scripts.
if (($BASH_VERSINFO < 4)); then
  echo "Your bash is outdated. Install bash >= 4."
  return 0
fi

for i in $(find ~/.shells ~/.bash -name '*.sh' -not -name '_*' -type f); do
  [[ -f $i ]] && echo "souring ${i}" && source "${i}"
done 

# anaconda3 root env
[[ -f /opt/anaconda/bin/activate ]] && . /opt/anaconda/bin/activate

if ! shopt -oq posix; then
  if [[ -f /usr/share/bash-completion/bash_completion ]]; then
    . /usr/share/bash-completion/bash_completion
  elif [[ -f /etc/bash_completion ]]; then
    . /etc/bash_completion
  fi
fi 
