
# ALIASES

# SOURCED BY BOTH `zsh` AND `bash`

in-path() {
  # checks  an executable is in $PATH
  for i in $(echo "$PATH" | sed "s/:/\n/g"); do
    if [[ -x "$i/$1" ]]; then
      return 0
    fi
  done
  return 1
}

# enable color support of ls and also add handy aliases
if [[ -x /usr/bin/dircolors ]]; then
  test -r ~/.dir_colors && eval "$(dircolors -b ~/.dir_colors)" || eval "$(dircolors -b)"
fi

# Add an "alert" alias for long running commands.  Use like so:
#   sleep 10; alert
alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" \
  "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

alias j=z

# general
alias sudo='sudo '                     # Enable

if [[ -z EDITOR ]]; then
  alias e='$EDITOR'                    # quicker
  access to vim
else
  alias e=vim                          # quicker access to vim
fi

# split path on ":"
alias show-path='echo -e ${PATH//:/\\n} | sort | grep -P "^.{3,}$"'

$(in-path ranger) && alias r='ranger'

# dmenu # a good alternative to ro # this
# modifies the prompt and coloring
$(in-path dmenu_run) && alias dmenu_run="dmenu_run -p ' >> ' -nb black -nf white"

alias df='df --human-readable --si'
alias info='info --vi-keys'
alias logout="pkill -KILL -u $USER"
$(in-path ipython) && alias ipython="ipython --profile=me"
alias battery="acpi -V"
alias cp="cp --recursive --verbose --interactive --preserve=mode,ownership,timestamps"
alias show-term-capabilities=\
  "infocmp -1 | sed -nu 's/^[ \000\t]*//;s/[ \000\t]*$//;/[^ \t\000]\{1,\}/!d;/acsc/d;s/=.*,//p'|column -c80"

$(in-path libreoffice) && alias libreoffice="libreoffice --norestore"

$(in-path tmux) && alias tmux='tmux -2'

# dirs and files
alias -- -='cd -' # Go back
alias ..="cd .."
alias ...="cd ../.."
alias ....="cd ../../.."
alias ll='ls -l -a --group-directories-first --time-style=+"%d.%m.%Y %H:%M" --color=auto -F'

# long listing
alias ls='ls --color=auto --group-directories-first'
# tweak default ls
alias dirs='find . -type d 2>/dev/null | sed -E "s/^\.\///"'
# list recursively just dirs
alias files='find . -type f 2>/dev/null | sed -E "s/^\.\///"'
# list recursively just files
[[ -e ~/.gists ]] && alias gists='ls ~/.gists/*/*'

# pattern matching
alias df='df --color=auto'
alias grep='grep --color=auto'
alias fgrep='fgrep --color=auto'
alias egrep='egrep --color=auto'
alias diff='diff --color=auto'

# networking, ssh, rsync
# -----------------------------------
# REQUIRES :: sshfs aria2c rsync python3
# -----------------------------------
# open using 0.0.0.0:{PORT}
$(in-path python3) && alias http-server-python="python3 -m http.server"
# note the php server requires index.php in the
# root dir
# open using 0.0.0.0:{PORT}
$(in-path php) && alias http-server-php="php -S 0.0.0.0:8000"
$(in-path ruby) && alias http-server-ruby=\
  "ruby -rwebrick -e'WEBrick::HTTPServer.new(:Port => 8000, :DocumentRoot => Dir.pwd).start'"
# mount a remote hard-drive
$(in-path sshfs) && alias mount-raptor="mkdir -p ${HOME}/Raptor && \
  sshfs -o transform_symlinks -o follow_symlinks nl253@raptor.kent.ac.uk: ${HOME}/Raptor"

$(in-path curl) && alias my-ip='curl ipinfo.io/ip'

# set up logging in
# ~/Downloads/Torrents/aria2c.log
# and a default location for download of
# Torrents in ~/Downloads/Torrents/
if $(in-path rsync); then
  alias rsync-copy="rsync --itemize-changes --stats --partial \
    --rsh=bash --progress --recursive --times --whole-le \
    --perms --executability --verbose --human-readable  --copy-links"
fi

# pacman aliases, yaourt
# colors
# -----------------------------------
# REQUIRES
# - pacman
# - yaourt
# - expac
# -----------------------------------
if $(in-path pacman); then

  if $(in-path expac); then
    alias pacman-recent-installations="expac --timefmt='%Y-%m-%d %T' %'%l\t%n' %| sort | %tail %-n 20"
    alias pacman-packages-by-size="expac -S -H M '%k\t%n'"
  fi

  alias pacman='pacman --config "${HOME}/.pacman.conf"'
  alias pacman-reinstall-all-native-packages="pacman -Qnq | pacman -S -"
  alias pacman-reinstall-all-foreign-packages="pacman -Qmq | pacman -S -"
  alias pacman-remove-orphans="sudo pacman -Rns $(pacman -Qtdq)"
  $(in-path yaourt) && \
    export YAOURT_COLORS=\
    "nb=1:pkg=1:ver=1;32:lver=1;45:installed=1;42:grp=1;34:od=1;41;5:votes=1;44:dsc=0:other=1;35"
fi

# prcess viewing
# ------------------------
# REQUIRES
# - top
# - or
# - htop
# ------------------------
if [[ -x $(which htop) ]]; then
  alias p=htop
  # [P]ROCESSES
else
  alias p=top
fi

# git
# ------------------------
# REQUIRES
# - git
# - hub
# ------------------------
if $(in-path git); then
  alias todo="git grep -n --word-regexp --break --heading --after-context 3 TODO"
  # look for TODOs in the current repo
  if $(in-path hub); then
    alias g=hub
    eval "$(hub alias -s)"
  else
    alias g=git
  fi
fi

unset -f in-path

# Better mv, cp, mkdir
alias cp=' cp --recursive --verbose --interactive --preserve=mode,ownership,timestamps'

if [[ -x $(which psql) ]]; then
  alias psql='psql --single-line'
fi

if [[ -x $(which mycli) ]]; then
  alias mycli='mycli mysql://root@localhost/fake'
fi

if [[ -x $(which isort) ]]; then
  alias isort='isort '
fi

# vim: foldmethod=marker foldlevel=0 foldmarker={{{,}}} nowrap formatoptions=
