
# THIS FILE MUST USE POSIX COMPLIANT SYNTAX
# IT IS SOURCED BY ALL INTERACTIVE SHELLS

# ALIASES
# --------

# general  {{{
env() { if [ ! $# = 0 ]; then command env "$@"; else command env | sort; fi; }                 # by default if no args provided sort env output
alias sudo='sudo '                                                                           # Enable aliases to be sudo’ed
alias e='$EDITOR'                                                                            # quicker access to vim
alias path='echo -e ${PATH//:/\\n}'                                                          # split path on ":"
alias x=xonsh                                                                                # quicker access
if [ -x /usr/bin/ranger ] || [ -x /bin/ranger ] ; then
    alias r='ranger'
elif [ -x ~/.ranger/ranger.py ]; then
    alias ranger="${HOME}/.ranger/ranger.py"
    alias r="${HOME}/.ranger/ranger.py"
fi # }}}

# utils : timer, clipboard, {{{
[ -x /usr/bin/aspell ] || [ -x /bin/aspell ] && alias aspell="aspell -c -l en_GB"
alias timer='echo "Timer started. Stop with Ctrl-D." && date && time cat && date'            # stopwatch
if [ -x /bin/xclip ] || [ -x /usr/bin/xclip ] ; then
    alias pbcopy='xclip -selection clipboard'     # copy
    alias pbpaste='xclip -selection clipboard -o' # paste
fi
# dmenu # a good alternative to rofi # this modifies the prompt and coloring
[ -x /bin/dmenu_run ] || [ -x /usr/bin/dmenu_run ] && alias dmenu_run="dmenu_run -p ' >> ' -nb black -nf white"
alias df='df --human-readable --si'
alias info='info --vi-keys'
alias freq='cut -f1 -d" " "$HISTFILE" | sort | uniq -c | sort -nr | head -n 30'                                                # frequent entries from history
alias logout="pkill -KILL -u "
alias show-term-capabilities="infocmp -1 | sed -nu 's/^[ \000\t]*//;s/[ \000\t]*$//;/[^ \t\000]\{1,\}/!d;/acsc/d;s/=.*,//p'|column -c80"
# }}}

# keymap {{{
alias keybingings="bind -p | grep -v '^#\|self-insert\|^$'"                                                                    # keybingings for readline
[ -x /usr/bin/xmodmap ] || [ -x /bin/xmodmap ] && alias map-caps-to-esc='xmodmap -e "clear lock"; xmodmap -e "keycode 0x42 = Escape"' && alias unmap-caps-from-esc='xmodmap -e "keycode 0x42 = Caps_Lock"; xmodmap -e "add lock = Caps_Lock"'
[ -x /usr/bin/setxkbmap ] || [ -x /bin/setxkbmap ] && alias map-caps-lock-to-ctrl='setxkbmap -layout gb -option ctrl:nocaps && echo -e "${MAGENTA}capslock remapped to ctrl${DEFCOLOR}"' # }}}

# dirs and files {{{
alias -- -='cd -'                                                                             # Go back
alias ..="cd .."
alias ...="cd ../.."
alias ....="cd ../../.."
alias le="ls -lo"                                                                            # list everything
alias ll='ls -l -a --group-directories-first --time-style=+"%d.%m.%Y %H:%M" --color=auto -F' # long listing
alias ls='ls --color=auto --group-directories-first'                                          # tweak default ls
alias lr=recent-files.sh                                                                      # list recent
[ ! -x /usr/bin/tree ] && alias tree="find . -print | sed -e 's;[^/]*/;|____;g;s;____|; |;g'" # in case tree is not present on the system
alias symlinks='find -type l 2>/dev/null | sed -E "s/^\.\///"'                                                                 # list symlinks recursively from CWD
alias dirs='find . -type d 2>/dev/null | sed -E "s/^\.\///"'                                                                   # list recursively just dirs
alias files='find . -type f 2>/dev/null | sed -E "s/^\.\///"'                                                                  # list recursively just files
# NOTE: with this one you don't wanna misspell, better not write then write wrong
alias f=find-approx.sh                                                                        # f [filename] # will search recursively using dense regexp to ensure quality, slow # }}}

# pattern matching {{{
alias diff='diff --color=auto'
alias grep='grep --color=auto'
alias fgrep='fgrep --color=auto'
alias egrep='egrep --color=auto'
[ -x /usr/bin/ag ] || [ -x /bin/ag ] && alias ag='ag --hidden --pager="less -MIRFX"' # search with dotfiles page to less with colors }}}

# networking, ssh, rsync {{{
alias http-server="python3 -m http.server"                                                                                     # open using 0.0.0.0:{PORT}
[ -x /usr/bin/sshfs ] || [ -x /bin/sshfs ] && alias mount-raptor="sshfs -o transform_symlinks -o follow_symlinks nl253@raptor.kent.ac.uk: ~/Raptor" # mount a remote hard-drive
# set up logging in ~/Downloads/Torrents/aria2c.log and a default location for download of Torrents :: ~/Downloads/Torrents/
[ -x /usr/bin/aria2c ] || [ -x /bin/aria2c ] && alias aria2c="mkdir -p ${HOME}/Downloads/Torrents/ ; touch ${HOME}/Downloads/Torrents/aria2c.log ; aria2c --continue --dir=${HOME}/Downloads/Torrents --log=${HOME}/Downloads/Torrents/aria2c.log"
if [ -x /bin/rsync ] || [ -x /usr/bin/rsync ] ; then
    alias rsync-copy="rsync -avz --progress -h"
    alias rsync-move="rsync -avz --progress -h --remove-source-files"
    alias rsync-update="rsync -avzu --progress -h"
    alias rsync-synchronize="rsync -avzu --delete --progress -h" 
fi # }}}

# pacman aliases, yaourt colors {{{
# -----------------------------------
# REQUIRES :: pacman yaourt expac
# -----------------------------------
if [ -x /usr/bin/pacman ] || [ -x /bin/pacman ] ; then
    alias pacman='pacman --config "${HOME}/.pacman.conf"'
    [ -x /usr/bin/expac ] && alias pacman-recent-installations="expac --timefmt='%Y-%m-%d %T' '%l\t%n' | sort | tail -n 20"
    [ -x /usr/bin/expac ] && alias pacman-packages-by-size="expac -S -H M '%k\t%n'"
    alias pacman-reinstall-all-native-packages="sudo pacman -Qnq | pacman -S -"
    alias pacman-reinstall-all-foreign-packages="sudo pacman -Qmq | pacman -S -"
    alias pacman-remove-orphans="sudo pacman -Rns $(pacman -Qtdq)"
    [ -x /usr/bin/yaourt ] || [ -x /bin/yaourt ] && export YAOURT_COLORS="nb=1:pkg=1:ver=1;32:lver=1;45:installed=1;42:grp=1;34:od=1;41;5:votes=1;44:dsc=0:other=1;35"
fi # }}}

# git {{{
# ------------------------
# REQUIRES :: git hub tig
# ------------------------
if [ -x /bin/git ] || [ -x /usr/bin/git ] ; then
    alias todo="git grep -n --word-regexp --break --heading TODO" # look for TODOs in the current repo
    [ -x /usr/bin/hub ] || [ -x /bin/hub ] && eval "$(hub alias -s)"
    alias g=hub alias g=git
fi
#vim:set foldmethod=marker:set foldlevel=0 }}}
