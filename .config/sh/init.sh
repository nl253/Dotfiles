# ~/.shinit to be run by all interactive shells

# This file is not read by bash(1) if ~/.bash_profile or ~/.bash_login exists.

# If not running interactively, don't do anything
case $- in
  *i*) ;;
  *) return ;;
esac

# normalise prompt in case somthing goes wrong
export PS1="${USER}@"$(hostname)" ${0} >> "

# set ls colors
if [ $0 = bash ] || [ $0 = zsh ] || [ $0 = -bash ]; then
  eval $(dircolors -b)
fi

# reset
export CDPATH="${HOME}:"

for directory in ~/Documents/{,Revision,Vim,} ~/Documents/Programming/{,Functional-Programming}; do
  [ -d $directory ] && export CDPATH="${directory}:${CDPATH}:" 2>/dev/null
done

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

export YAOURT_COLORS="nb=1:pkg=1:ver=1;32:lver=1;45:installed=1;42:grp=1;34:od=1;41;5:votes=1;44:dsc=0:other=1;35"

# general
alias sudo='sudo '

# get MIME type of a file
[ -x $(command which file) ] && alias mime-type='file --dereference --brief --mime-type -- '

if [ -x $(which ghci) ]; then
  ghc_exts='-XApplicativeDo -XBangPatterns -XBinaryLiterals -XDeriveAnyClass -XDeriveFoldable -XDeriveFunctor -XDeriveGeneric -XDeriveTraversable -XEmptyDataDecls -XFlexibleContexts -XFlexibleInstances -XFunctionalDependencies -XGADTs -XKindSignatures -XLambdaCase -XMonadComprehensions -XMultiParamTypeClasses -XMultiWayIf -XNamedWildCards -XNumDecimals -XParallelListComp -XPartialTypeSignatures -XPatternGuards -XPostfixOperators -XScopedTypeVariables -XTupleSections -XTypeOperators -XViewPatterns'
  ghc_f='-fprint-potential-instances -fprint-expanded-synonyms -fdiagnostics-color=always'
  ghc_warn='-Wunused-local-binds -Wunused-foralls -Wunused-binds -Wsemigroup -Wredundant-constraints -Woverlapping-patterns -Wnoncanonical-monoid-instances -Wnoncanonical-monad-instances -Wname-shadowing -Wincomplete-uni-patterns -Wdeprecations -Wcompat'
  ghc_opts='-threaded -j4'
  alias ghci="ghc --interactive ${ghc_opts} ${ghc_f} -fprint-unicode-syntax ${ghc_warn} ${ghc_exts}"
  alias ghc="ghc ${ghc_opts} ${ghc_f} ${ghc_warn} ${ghc_exts}"
fi

# dirs and files
alias ls='ls --color=auto --group-directories-first -I tags -I "*cache*" -I "*history*" -I "~*" -I "_*" -I "*~" -I "*-log" -I "*-lock" -I "*.log" -I "*.class" -I "*.so" -I "*.beam" -I "*.o" -I "*.pyc" -I "*.pyg" -I "*.aux" -I "*.toc" -I "*.swp" -I "*.tmp" -I "*.fls" -I "*.fdb_latexmk" -I "*.lock" -I "*.hi"'

# pattern matching
for i in {f,e,}grep; do
  eval "alias ${i}='${i} --color=auto'"
done

GLOBIGNORE=*.{fls,out,aux,toc,beam,pyo,lock,tmp,bak,log,o,hi,class,so}:tags:node_modules:iml:*cache*:*history*
FIGNORE=$GLOBIGNORE

alias diff='diff --color=auto --suppress-common-lines --side-by-side --ignore-tab-expansion --ignore-space-change --ignore-all-space --ignore-blank-lines'

# split path on ":"
alias show-path='echo -e ${PATH//:/\\n} | grep -E "^.{3,}$"'
for i in df du; do alias $i="$i --human-readable --si --total"; done
alias info='info --vi-keys'
alias logout="pkill -KILL -u \$USER"
[ -x $(command which ipython 2>/dev/null) ] && alias ipython="ipython3 --pylab=qt5 --gui=qt5"
[ -x $(command which ipython3 2>/dev/null) ] && alias ipython3="ipython3 --pylab=qt5 --gui=qt5"
[ -x $(command which acpi 2>/dev/null) ] && alias battery="acpi -V"
alias cp="cp --recursive --verbose --interactive --preserve=mode,ownership,timestamps"

if [ -x $(command which rsync 2>/dev/null) ]; then
  alias copy="rsync --progress --log-file=/tmp/rsync.log --recursive --backup --backup-dir=/tmp --human-readable --times --preallocate --partial --partial-dir=/tmp/rsync-partially-copied --suffix=rsync-backup --hard-links --group --perms -xattrs --executability --copy-links --copy-dirlinks --compress --compress-level 9 --verbose"
fi

for i in mv rm; do
  alias $i="$i --verbose"
done

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

# get ip address
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

# Archiving
[ -x $(command which 7z 2>/dev/null) ] && alias 7z='7z -mx=9 -mmt8 -bt -bb1'

alias pip=pip3

# $EDITOR
for i in nvim vim vi; do [ -x $(command which $i 2>/dev/null) ] && [ $i = nvim ] && eval "alias vim=${i}" && break; done

export PS1=' $(command git --no-pager branch --color=always -vv 2>/dev/null) $(tput setaf 5)::$(tput sgr0) $(pwd)$(tput setaf 1)\n >>$(tput sgr0) '

# vim:foldmethod=marker:foldlevel=0:foldmarker={,}:shiftwidth=2:tabstop=2:
