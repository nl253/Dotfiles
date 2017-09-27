
# ALIASES (tested on zsh and bash)

# SOURCED BY BOTH `zsh` AND `bash`

in-path(){
  # checks  an executable is in $PATH
  for i in $(echo -e ${PATH//:/\\n} | sort | uniq); do
    if [[ -x "$i/$1" ]]; then
      return 0
    fi
  done
  return 1
}

alias j=z

# general
alias sudo='sudo '                     # Enable

# split path on ":"
alias show-path='echo -e ${PATH//:/\\n} | sort | grep -P "^.{3,}$"'
alias df='df --human-readable --si --total'
alias du='du --human-readable --si --summarize --total'
alias info='info --vi-keys'
alias logout="pkill -KILL -u $USER"
$(in-path ipython) && alias ipython="ipython --profile=me"
alias battery="acpi -V"
alias cp="cp --recursive --verbose --interactive --preserve=mode,ownership,timestamps"
alias show-term-capabilities="infocmp -1 | sed -nu 's/^[ \000\t]*//;s/[ \000\t]*$//;/[^ \t\000]\{1,\}/!d;/acsc/d;s/=.*,//p'|column -c80"

$(in-path libreoffice) && alias libreoffice="libreoffice --norestore"

$(in-path tmux) && alias tmux='tmux -2'

# dirs and files
alias -- -='cd -' # Go back
alias ..="cd .."
alias ...="cd ../.."
alias ....="cd ../../.."

# long listing
alias ls='ls --color=auto --group-directories-first'

# pattern matching
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
$(in-path php) && alias http-server-php="php -S 0.0.0.0:5000"
$(in-path ruby) && alias http-server-ruby="ruby -rwebrick -e'WEBrick::HTTPServer.new(:Port => 8000, :DocumentRoot => Dir.pwd).start'"
# mount a remote hard-drive
$(in-path sshfs) && alias mount-raptor="mkdir -p ${HOME}/Raptor && sshfs -o transform_symlinks -o follow_symlinks nl253@raptor.kent.ac.uk: ${HOME}/Raptor"

$(in-path curl) && alias my-ip='curl ipinfo.io/ip'

# set up logging in
# ~/Downloads/Torrents/aria2c.log
# and a default location for download of
# Torrents in ~/Downloads/Torrents/
if $(in-path rsync); then
  alias copy="rsync --ignore-missing-args --group --xattrs --hard-links --executability --itemize-changes --stats --partial --rsh=bash --progress --recursive --times --whole-file --perms --executability --verbose --human-readable  --copy-links"
fi

# pacman aliases, yaourt colors
# -----------------------------
# REQUIRES
# ========
# pacman
# yaourt
# expac
# -----------------------------

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
    export YAOURT_COLORS="nb=1:pkg=1:ver=1;32:lver=1;45:installed=1;42:grp=1;34:od=1;41;5:votes=1;44:dsc=0:other=1;35"
fi

# prcess viewing
# ------------------------
# REQUIRES
# top || htop
# ------------------------
if $(in-path htop); then
  alias p=htop
  # [P]ROCESSES
else
  alias p=top
fi

if $(in-path git); then
  alias todo="git grep -n --word-regexp --break --heading --after-context 3 TODO"
  alias fixme="git grep -n --word-regexp --break --heading --after-context 3 FIXME"
  # look for TODOs in the current repo
  if $(in-path hub); then
    eval "$(hub alias -s)"
  fi
fi

alias cp='cp --recursive --verbose --interactive --preserve=mode,ownership,timestamps'

if $(in-path psql); then
  alias psql='psql --single-line'
fi

if $(in-path sqlite3); then
  alias sqlite3="sqlite3 -init ${HOME}/.sqliterc"
fi

unset -f in-path
# vim: foldmethod=marker foldlevel=0 foldmarker={{{,}}} nowrap formatoptions=
