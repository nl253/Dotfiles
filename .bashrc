
# ~/.bashrc: executed by bash(1) for non-login shells.

# If not running interactively, don't do anything
case $- in
	*i*) ;;
	*) return;;
esac

# set variable identifying the chroot you work in (used in the prompt below)
if [[ -z "${debian_chroot:-}" ]] && [[ -r /etc/debian_chroot ]]; then
	debian_chroot=$(cat /etc/debian_chroot)
fi

xhost +local:root > /dev/null 2>&1

# Check if bash version is at least 4 to run some of my scripts.
if ! (( $BASH_VERSINFO >= 4 )); then
    echo "Your bash is outdated. Install bash >= 4."
    return 0 
fi

for i in ~/.{shells,bash}/*.sh; do
	[[ -f $i ]] && source "${i}"
done 
