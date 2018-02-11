# ~/.shinit to be run by all interactive shells

# This file is not read by bash(1) if ~/.bash_profile or ~/.bash_login exists.

# If not running interactively, don't do anything
case $- in
  *i*) ;;
  *) return ;;
esac

# normalise prompt in case somthing goes wrong
export PS1="${USER}@${HOSTNAME}${0} $ "

# reset
export CDPATH="${HOME}:"

for directory in ${HOME}/Documents ${HOME}/Documents/*; do
  [ -d $directory ] && export CDPATH="${directory}:${CDPATH}:" 2>/dev/null
done

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

export YAOURT_COLORS="nb=1:pkg=1:ver=1;32:lver=1;45:installed=1;42:grp=1;34:od=1;41;5:votes=1;44:dsc=0:other=1;35"

# general
alias sudo='sudo '

# get MIME type of a file
[ -x $(command which file) ] && alias mime-type='file --dereference --brief --mime-type -- '

# dirs and files
alias ls='ls --color=auto --group-directories-first --hide tags --hide "*{cache,history}*" --hide "{~,_}*" --hide "*~" --hide "*-{log,lock}" --hide "*.{log,class,so,beam,o,pyc,pyg,aux,toc,swp,tmp,fls,fdb_latexmk,lock}"'

alias ..="cd .."
alias ...="cd ../.."
alias ....="cd ../../.."
alias .....="cd ../../../.."

# pattern matching
for i in {f,e,}grep; do
  eval "alias ${i}='${i} --color=auto'"
done

alias diff='diff --color=auto --suppress-common-lines --side-by-side --ignore-tab-expansion --ignore-space-change --ignore-all-space --ignore-blank-lines'

# split path on ":"
alias show-path='echo -e ${PATH//:/\\n} | sort | grep -P "^.{3,}$"'
alias df='df --human-readable --si --total'
alias du='du --human-readable --si --summarize --total'
alias info='info --vi-keys'
alias logout="pkill -KILL -u \$USER"
[ -x $(command which ipython 2>/dev/null) ] && alias ipython="ipython3 --pylab=qt5 --gui=qt5"
[ -x $(command which ipython3 2>/dev/null) ] && alias ipython3="ipython3 --pylab=qt5 --gui=qt5"
[ -x $(command which acpi 2>/dev/null) ] && alias battery="acpi -V"
alias cp="cp --recursive --verbose --interactive --preserve=mode,ownership,timestamps"

if [ -x $(command which rsync 2>/dev/null) ]; then
  alias copy="rsync --ignore-missing-args --copy-links --executability --group --hard-links --human-readable  --itemize-changes --partial --perms --progress --recursive --rsh=bash --stats --times --verbose --whole-file --xattrs"
fi

alias mv="mv --verbose"
alias rm="rm --verbose"
alias show-term-capabilities="infocmp -1 | sed -nu 's/^[ \000\t]*//;s/[ \000\t]*$//;/[^ \t\000]\{1,\}/!d;/acsc/d;s/=.*,//p'|column -c80"

[ -x $(command which libreoffice 2>/dev/null) ] && alias libreoffice="libreoffice --norestore"
[ -x $(command which tmux 2>/dev/null) ] && alias tmux='tmux -2'

# Networking, Servers
if [ -x $(command which python3 2>/dev/null) ]; then
  alias http-server-python="python3 -m http.server"
  alias pip-update-packages="pip freeze --local | grep -v '^\-e' | cut -d = -f 1  | xargs -n1 pip install -U"
fi

# NOTE the php server requires index.php in the root dir
[ -x $(command which php 2>/dev/null) ] && alias http-server-php="php -S 0.0.0.0:5000"
[ -x $(command which ruby 2>/dev/null) ] && alias http-server-ruby="ruby -rwebrick -e'WEBrick::HTTPServer.new(:Port => 8000, :DocumentRoot => Dir.pwd.start'"

# mount a remote hard-drive
[ -x $(command which sshfs 2>/dev/null) ] && alias mount-raptor="mkdir -p \${HOME}/Raptor && sshfs -o transform_symlinks -o follow_symlinks nl253@raptor.kent.ac.uk: \${HOME}/Raptor"
[ -x $(command which curl 2>/dev/null) ] && alias my-ip='curl ipinfo.io/ip'

# Package Management - pacman, yaourt, expac
if [ -x $(command which pacman 2>/dev/null) ]; then
  alias pacman='pacman --config "${HOME}/.pacman.conf"'
  alias pacman-reinstall-all-native-packages="pacman -Qnq | pacman -S -"
  alias pacman-reinstall-all-foreign-packages="pacman -Qmq | pacman -S -"
  alias pacman-remove-orphans="sudo pacman -Rns \$(pacman -Qtdq)"
  if [ -x $(command which expac 2>/dev/null) ]; then
    alias pacman-recent-installations="expac --timefmt='%Y-%m-%d %T' %'%l\t%n' %| sort | %tail %-n 20"
    alias pacman-packages-by-size="expac -S -H M '%k\t%n'"
  fi
fi

# VCS
if [ -x $(command which git 2>/dev/null) ]; then
  alias g=git
  git_recursive_update() { for i in $(find -name .git -type d -exec dirname {} \;); do (cd $i && git pull); done; }
  git_foreach() { for i in $(find -name .git -type d -exec dirname {} \;); do (cd $i && $@); done; }
  if [ -x $(command which hub 2>/dev/null) ]; then
    eval "$(hub alias -s)"
    alias g=hub
  fi
fi

# Archiving
[ -x $(command which 7z 2>/dev/null) ] && alias 7z='7z -mx=9 -mmt8 -bt -bb1'

# Databases
if [ $(hostname) = raptor ]; then
  [ -x $(command which mysql 2>/dev/null) ] && alias mysql-dragon='mysql -u nl253 -p -h dragon.kent.ac.uk nl253'
  [ -x $(command which mycli 2>/dev/null) ] && alias mycli-dragon='mycli mysql://nl253@dragon.kent.ac.uk/nl253'
fi

alias pip=pip3

# $EDITOR
for i in nvim vim vi; do [ -x $(command which $i 2>/dev/null) ] && [ $i = nvim ] && eval "alias vim=${i}" && break; done
# vim: foldmethod=marker foldlevel=0 foldmarker={,} shiftwidth=2 tabstop=2 ft=sh
