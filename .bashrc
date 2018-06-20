# ~/.bashrc  executed by bash(1)

[[ -d ~/.config/sh ]] && export SH_DOT_DIR=$HOME/.config/sh
[[ -f $SH_DOT_DIR/init.sh ]] && source $SH_DOT_DIR/init.sh

if ((BASH_VERSINFO < 4)); then echo "[ERROR] your bash is outdated, install bash >= 4"; return 0; fi

[[ -d ~/.config/bash ]] && export BASH_DOT_DIR=$HOME/.config/bash
[[ -f $BASH_DOT_DIR/completions.sh ]] && source $BASH_DOT_DIR/completions.sh

shopt -s xpg_echo globasciiranges histappend checkjobs checkwinsize \
  globstar extglob dotglob nocasematch nocaseglob cdspell dirspell \
  checkhash cdable_vars

PROMPT_COMMAND="history -a"

[[ -f ~/.opam/opam-init/init.sh ]] && . ~/.opam/opam-init/init.sh

#EXECIGNORE=:/{usr/,}bin/{grub*,pamac,x86,xdg,xfs,yuv,tiff,sndfile,quad,ntfs,nl-,mtp-,mkfs,jfs_,jack_,ip6table,iptable,idevice,gvfs,fsck,encryptfs,dvi,djv}*:
