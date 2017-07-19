#!/bin/sh
#
# ~/.xinitrc
#
# Executed by startx (run your window manager from here)

if [[ -f ~/.extend.xinitrc ]];then
    . ~/.extend.xinitrc
else
    DEFAULT_SESSION=xfce4-session
fi

userresources=$HOME/.Xresources
usermodmap=$HOME/.Xmodmap
sysresources=/etc/X11/xinit/.Xresources
sysmodmap=/etc/X11/xinit/.Xmodmap

# merge in defaults and keymaps

if [ -f $sysresources ]; then
    xrdb -merge $sysresources
fi

if [ -f $sysmodmap ]; then
    xmodmap $sysmodmap
fi

if [ -f "$userresources" ]; then
    xrdb -merge "$userresources"
fi

if [ -f "$usermodmap" ]; then
    xmodmap "$usermodmap"
fi

# start some nice programs

if [ -d /etc/X11/xinit/xinitrc.d ] ; then
    for f in /etc/X11/xinit/xinitrc.d/?*.sh ; do
	[ -x "$f" ] && . "$f"
    done
    unset f
fi

get_session(){
    local dbus_args=(--sh-syntax --exit-with-session)
    case $1 in
	awesome) dbus_args+=(awesome) ;;
	bspwm) dbus_args+=(bspwm-session) ;;
	budgie) dbus_args+=(budgie-desktop) ;;
	cinnamon) dbus_args+=(cinnamon-session) ;;
	deepin) dbus_args+=(startdde) ;;
	enlightenment) dbus_args+=(enlightenment_start) ;;
	fluxbox) dbus_args+=(startfluxbox) ;;
	gnome) dbus_args+=(gnome-session) ;;
	i3|i3wm) dbus_args+=(i3 --shmlog-size 0) ;;
	jwm) dbus_args+=(jwm) ;;
	kde) dbus_args+=(startkde) ;;
	lxde) dbus_args+=(startlxde) ;;
	lxqt) dbus_args+=(lxqt-session) ;;
	mate) dbus_args+=(mate-session) ;;
	xfce) dbus_args+=(xfce4-session) ;;
	openbox) dbus_args+=(openbox-session) ;;
	*) dbus_args+=($DEFAULT_SESSION) ;;
    esac

    echo "dbus-launch ${dbus_args[*]}"
}

exec $(get_session)

feh --bg-fill  ~/Pictures/desktop.png
setxkbmap -layout gb  

#check if dropbox is running
[ ! $(pgrep dropbox >/dev/null 2>&1) && dropbox ]

xset r rate 660 25

#!/usr/bin/env bash

# Meta Keys:
# The ECMA-35 standard prefixes control sequences containing a high
# eighth-bit with <Esc> to disambiguate from unicode input, which also
# utilizes a high eigth-bit (overlapping ranges). This necessitates
# the use of timings to differentiate and <Esc>-prefixes control
# sequence and <Esc> preceding unrelated keys. Unfortunately the
# timing mechanism is unable to differentiate between macros and
# bindings from actual control sequences as in all cases timing is
# near-zero. Furthermore programs without accurate timers break
# unexpectedly and often randomly, such as vim. Neovim can be
# configured without any breakage, as can tmux. However it is simpler
# - if willing to forego working unicode input - to simply disble
# ECMA-35 and send meta-sequences as eight-bit high.
# Unfortunately URxvt simply passes eight-bit high control-sequences
# untouched, letting higher layers assume they are the beginning of
# unicode byte-sequences. This breaks these sequences. XTerm avoids
# this ambiguity by automatically converting the eight-bit high
# control sequence to appropriate byte sequences using the current
# locale. URxvt can be harcoded to mimic this behaviour, but not in a
# locale-independent fashion. If locales change, the following
# bindings will be broken.
# Summary:
#   * Specific to the current locale
#   * Breaks UTF-8 input

# read -r -d '' PyURxvtMeta8 <<-'EOF'
# 	#!/usr/bin/env python3
	
# 	import locale
	
# 	locale.setlocale(locale.LC_ALL, "")
# 	encoding = locale.getlocale()[1]
	
# 	# ASCII range
# 	for i in range(32, 128):
# 	    seq = "".join("\{:o}".format(j) for j in chr(i + (1<<7)).encode(encoding))
# 	    key = "{:#06X}".format(i)
# 	    print("urxvt*keysym.Meta-{}: {}".format(key, seq))
# EOF

# xrdb -merge <(python3 <<<"$PyURxvtMeta8")
