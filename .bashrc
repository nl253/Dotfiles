# ~/.bashrc  executed by bash(1)
# NOTe here it is moved to ~/.config/bash/bashrc.sh
# setting $BASH_ENV to this file makes it act as ~/.bashrc normally would

[[ -d ~/.config/sh ]] && export SH_DOT_DIR=$HOME/.config/sh
[[ -f $SH_DOT_DIR/init.sh ]] && source $SH_DOT_DIR/init.sh

# Check if bash version is at least 4
if ((BASH_VERSINFO < 4)); then
  echo "[ERROR] your bash is outdated, install bash >= 4"
  return 0
fi

[[ -d ~/.config/bash ]] && export BASH_DOT_DIR=$HOME/.config/bash
[[ -f $BASH_DOT_DIR/completions.sh ]] && source $BASH_DOT_DIR/completions.sh

shopt -s xpg_echo globasciiranges histappend checkjobs checkwinsize globstar extglob dotglob nocasematch nocaseglob cdspell dirspell checkhash cdable_vars

EXECIGNORE=/{usr/,}bin/{grub,pamac,x86,xdg,xfs,yuv,tiff,sndfile,quad,ntfs,nl-,mtp-,mkfs,jfs_,jack_,ip6table,iptable,idevice,gvfs,fsck,encryptfs,dvi,djv}*:
PS1="\n\[$(tput bold)\]\[$(tput sgr0)\]\[\033[38;5;3m\]\u\[$(tput sgr0)\]\[$(tput sgr0)\]\[\033[38;5;40m\]@\[$(tput bold)\]\[$(tput sgr0)\]\[\033[38;5;31m\]\h\[$(tput sgr0)\]\[$(tput sgr0)\]\[\033[38;5;15m\] \n\[$(tput sgr0)\]\[\033[38;5;241m\]\w\[$(tput sgr0)\]\[\033[38;5;15m\] \[$(tput bold)\]\[$(tput sgr0)\]\[\033[38;5;88m\]>\[$(tput sgr0)\]\[\033[38;5;89m\]>\[$(tput sgr0)\]\[$(tput sgr0)\]\[\033[38;5;15m\] \[$(tput sgr0)\]"

[[ $(hostname) =~ raptor ]] && source /etc/bash_completion

# Can also be sourced by zsh if you autoload bashcompinit, see zshcompsys(1)

[[ -x $(type -P pandoc 2>/dev/null) ]] && eval "$(pandoc --bash-completion)"
[[ -x $(type -P stack 2>/dev/null) ]] && eval "$(stack --bash-completion-script stack)"

#complete -o noquote -W "$(find ~/{Documents,.config/zsh,.config/sh,.config/bash} -type f -mmin -100 | xargs)" vim
