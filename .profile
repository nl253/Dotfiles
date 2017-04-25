#
# ~/.profile
#

echo "~/.profile loaded"

[[ "$XDG_CURRENT_DESKTOP" == "KDE" ]] || [[ "$XDG_CURRENT_DESKTOP" == "GNOME" ]] || export QT_QPA_PLATFORMTHEME="qt5ct"

[[ -f ~/.extend.profile ]] && . ~/.extend.profile


# setxkbmap -layout gb -option ctrl:nocaps # Caps Lock is Control on a GB keyboard

# UTF-8 all the way.
export LC_ALL='en_GB.UTF-8';
export LANG='en_GB';
