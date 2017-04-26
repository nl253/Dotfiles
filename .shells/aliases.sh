# THIS FILE MUST USE POSIX COMPLIANT SYNTAX
# IT IS SOURCED BY ALL INTERACTIVE SHELLS

# ALIASES
# --------

# UTILS {{{
# checks if an executable is in $PATH
in-path() {
for i in $(echo "$PATH" | sed "s/:/\n/g"); do
    if [ -x "$i/$1" ]; then
        return 0
    fi
done
return 1
}
# }}}

# general  {{{
env() { if [ ! $# = 0 ]; then command env "$@"; else command env | sort; fi; } # by default if no args provided sort env output
alias sudo='sudo '                                                             # Enable aliases to be sudo’ed
alias e='$EDITOR'                                                              # quicker access to vim
alias path='echo -e ${PATH//:/\\n}'                                            # split path on ":"
alias x=xonsh                                                                  # quicker access
if $(in-path ranger); then
    alias r='ranger'
elif [ -x ~/.ranger/ranger.py ]; then
    alias ranger="${HOME}/.ranger/ranger.py"
    alias r="${HOME}/.ranger/ranger.py"
fi # }}}

# utils : timer, clipboard, {{{
$(in-path aspell) && alias aspell="aspell -c -l en_GB"
alias timer='echo "Timer started. Stop with Ctrl-D." && date && time cat && date' # stopwatch
if $(in-path xclip); then
    alias pbcopy='xclip -selection clipboard'     # copy
    alias pbpaste='xclip -selection clipboard -o' # paste
fi
# dmenu # a good alternative to rofi # this modifies the prompt and coloring
$(in-path dmenu_run) && alias dmenu_run="dmenu_run -p ' >> ' -nb black -nf white"
alias df='df --human-readable --si'
alias info='info --vi-keys'
alias freq='cut -f1 -d" " "$HISTFILE" | sort | uniq -c | sort -nr | head -n 30' # frequent entries from history
alias logout="pkill -KILL -u "
alias show-term-capabilities="infocmp -1 | sed -nu 's/^[ \000\t]*//;s/[ \000\t]*$//;/[^ \t\000]\{1,\}/!d;/acsc/d;s/=.*,//p'|column -c80"
# alias note=note.sh       # refer to ~/Scripts/note.sh
function pandoc(){
    if $(in-path pandoc) ; then 
        if [ $# = 1 ]; then
            pandoc -f html -t markdown_github --standalone --atx-headers --toc --ascii "$1"
        else
            command pandoc "$@"
        fi
    else
        echo "pandoc not on system." && return 0
    fi
}
if ! $(in-path vit); then
    alias vit="\$EDITOR -c 'TW'"
fi

if $(in-path libreoffice); then
    alias libreoffice="libreoffice --norestore"
fi
# }}}

# keymap {{{
if $(in-path xmodmap); then
    alias map-caps-to-esc='xmodmap -e "clear lock"; xmodmap -e "keycode 0x42 = Escape"'
    alias unmap-caps-from-esc='xmodmap -e "keycode 0x42 = Caps_Lock"; xmodmap -e "add lock = Caps_Lock"'
fi
$(in-path setxkbmap) && alias map-caps-lock-to-ctrl='setxkbmap -layout gb -option ctrl:nocaps && echo -e "${MAGENTA}capslock remapped to ctrl${DEFCOLOR}"'
# }}}

# dirs and files {{{
alias -- -='cd -' # Go back

alias ..="cd .."
alias ...="cd ../.."
alias ....="cd ../../.."
alias le="ls -lo"                                                                                                   # list everything
alias ll='ls -l -a --group-directories-first --time-style=+"%d.%m.%Y %H:%M" --color=auto -F'                        # long listing
alias ls='ls --color=auto --group-directories-first'                                                                # tweak default ls
alias lr=recent-files.sh                                                                                            # list recent
[ ! -x /usr/bin/tree ] && [ ! -x /bin/tree ] && alias tree="find . -print | sed -e 's;[^/]*/;|____;g;s;____|; |;g'" # in case tree is not present on the system
alias symlinks='find -type l 2>/dev/null | sed -E "s/^\.\///"'                                                      # list symlinks recursively from CWD
alias dirs='find . -type d 2>/dev/null | sed -E "s/^\.\///"'                                                        # list recursively just dirs
alias files='find . -type f 2>/dev/null | sed -E "s/^\.\///"'                                                       # list recursively just files
# NOTE: with this one you don't wanna misspell, better not write then write wrong
# f [filename] # will search recursively using dense regexp to ensure quality, slow
alias f=find-approx.sh
# refer to ~/Scripts/fzf-filtered-grep.sh
$(in-path fzf) && alias gr=fzf-filtered-grep.sh
# }}}

# pattern matching {{{
alias diff='diff --color=auto'
alias grep='grep --color=auto'
alias fgrep='fgrep --color=auto'
alias egrep='egrep --color=auto'
$(in-path ag) && alias ag='ag --hidden --pager="less -MIRFX"' # search with dotfiles page to less with colors }}}

# networking, ssh, rsync {{{
# -----------------------------------
# REQUIRES :: sshfs aria2c rsync python3
# -----------------------------------
alias http-server="python3 -m http.server" # open using 0.0.0.0:{PORT}
# mount a remote hard-drive
$(in-path sshfs) && alias mount-raptor="sshfs -o transform_symlinks -o follow_symlinks nl253@raptor.kent.ac.uk: ~/Raptor"
# set up logging in ~/Downloads/Torrents/aria2c.log
# and a default location for download of Torrents in ~/Downloads/Torrents/
$(in-path aria2c) && alias aria2c="mkdir -p ${HOME}/Downloads/Torrents/ ; touch ${HOME}/Downloads/Torrents/aria2c.log ; aria2c --continue --dir=${HOME}/Downloads/Torrents --log=${HOME}/Downloads/Torrents/aria2c.log"
if $(in-path rsync); then
    alias rsync-copy="rsync -avz --progress -h"
    alias rsync-move="rsync -avz --progress -h --remove-source-files"
    alias rsync-update="rsync -avzu --progress -h"
    alias rsync-synchronize="rsync -avzu --delete --progress -h"
fi # }}}

# pacman aliases, yaourt colors {{{
# -----------------------------------
# REQUIRES :: pacman yaourt expac
# -----------------------------------
if $(in-path pacman); then
    if $(in-path expac); then
        alias pacman-recent-installations="expac --timefmt='%Y-%m-%d %T' '%l\t%n' | sort | tail -n 20"
        alias pacman-packages-by-size="expac -S -H M '%k\t%n'"
    fi
    alias pacman='pacman --config "${HOME}/.pacman.conf"'
    alias pacman-reinstall-all-native-packages="sudo pacman -Qnq | pacman -S -"
    alias pacman-reinstall-all-foreign-packages="sudo pacman -Qmq | pacman -S -"
    alias pacman-remove-orphans="sudo pacman -Rns $(pacman -Qtdq)"
    $(in-path yaourt) && export YAOURT_COLORS="nb=1:pkg=1:ver=1;32:lver=1;45:installed=1;42:grp=1;34:od=1;41;5:votes=1;44:dsc=0:other=1;35"
fi # }}}

# git {{{
# ------------------------
# REQUIRES :: git hub
# ------------------------
if $(in-path git); then
    alias todo="git grep -n --word-regexp --break --heading TODO" # look for TODOs in the current repo
    if $(in-path hub); then
        eval "$(hub alias -s)"
        alias g=git
    else
        alias g=hub
    fi
fi
#vim:set foldmethod=marker:set foldlevel=0 }}}
