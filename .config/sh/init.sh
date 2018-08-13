# vim:foldmethod=indent:foldlevel=1:foldmarker={,}:shiftwidth=2:tabstop=2:

# ~/.shinit to be sourced by all interactive shells

# This file is not read by bash(1) if ~/.bash_profile or ~/.bash_login exists.

# If not running interactively, don't do anything

case $- in
  *i*) ;;
  *) return ;;
esac

# normalise prompt in case somthing goes wrong
export PS1="${USER}@"$(hostname)" ${0} >> "

# set ls colors
if builtin dirs 1>/dev/null 2>/dev/null; then
  eval $(dircolors -b)
fi

# reset
export CDPATH="${HOME}:"

for directory in ~/Documents/Notes ~/Documents/Vim ~/Documents/Programming; do
  [ -d $directory ] && export CDPATH="${directory}:${CDPATH}:" 2>/dev/null
done

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

export YAOURT_COLORS="nb=1:pkg=1:ver=1;32:lver=1;45:installed=1;42:grp=1;34:od=1;41;5:votes=1;44:dsc=0:other=1;35"

# general
alias sudo='sudo '

# get MIME type of a file
[ -x $(command which file) ] && alias mime-type='file --dereference --brief --mime-type -- '

if [ -x /usr/bin/ghci ]; then
  ghc_exts='-XApplicativeDo -XBangPatterns -XBinaryLiterals -XDeriveAnyClass -XDeriveFoldable -XDeriveFunctor -XDeriveGeneric -XDeriveTraversable -XEmptyDataDecls -XFlexibleContexts -XFlexibleInstances -XFunctionalDependencies -XGADTs -XKindSignatures -XLambdaCase -XMonadComprehensions -XMultiParamTypeClasses -XMultiWayIf -XNamedWildCards -XNumDecimals -XParallelListComp -XPartialTypeSignatures -XPatternGuards -XPostfixOperators -XScopedTypeVariables -XTupleSections -XTypeOperators -XViewPatterns'
  ghc_f='-fprint-potential-instances -fprint-expanded-synonyms -fdiagnostics-color=always'
  ghc_warn='-Wunused-local-binds -Wunused-foralls -Wunused-binds -Wsemigroup -Wredundant-constraints -Woverlapping-patterns -Wnoncanonical-monoid-instances -Wnoncanonical-monad-instances -Wname-shadowing -Wincomplete-uni-patterns -Wdeprecations -Wcompat'
  ghc_opts='-threaded -j4'
  alias ghci="ghc --interactive ${ghc_opts} ${ghc_f} -fprint-unicode-syntax ${ghc_warn} ${ghc_exts}"
  alias ghc="ghc ${ghc_opts} ${ghc_f} ${ghc_warn} ${ghc_exts}"
  for var in ghc_exts ghc_f ghc_warn ghc_opts; do eval "unset -v $var"; done
fi

if [ -x /usr/bin/node ]; then
  node_libs='path fs os'
  node_experimental='modules repl-await vm-modules'
  node_cmd='node -i --no-warnings'
  for lib in $(eval "echo $node_libs"); do node_cmd="${node_cmd} --require ${lib}"; done
  for feature in $(eval "echo $node_experimental"); do node_cmd="${node_cmd} --experimental-${feature}"; done
  alias node="${node_cmd}"
  for var in node_libs node_experimental node_cmd; do eval "unset -v $var"; done
fi

# this here is very format dependent - do not change
# dirs and files
ls_opts='-I=tags -I *cache* -I *history* -I ~* -I _* -I *~ -I *-log -I *-lock -I *.log -I *.class -I *.so -I *.beam -I *.o -I *.pyc -I *.pyg -I *.aux -I *.toc -I *.swp -I *.tmp -I *.fls -I *.fdb_latexmk -I *.lock -I *.hi --color=auto --group-directories-first'
if builtin dirs 1>/dev/null 2>/dev/null && [ -x ~/.cargo/bin/exa ]; then
  # replace all occurances of ' -I ' with '|' required by exa
  alias ls="exa \"${ls_opts// -I /|}\" --git --git-ignore" 
else
  alias ls="ls $ls_opts"
fi
unset -v ls_opts

if [ -x /usr/bin/rlwrap ]; then
  for i in 'dash -i'; do
    eval "alias $(echo $i | sed -E 's/^(\S+).*/\1/')='rlwrap $i'"
  done
fi

# pattern matching
for i in fgrep egrep grep; do
  eval "alias ${i}='${i} --color=auto'"
done

if [ -x ~/.cargo/bin/rg ]; then
  alias rg='rg --pretty --threads $(grep -c ^processor /proc/cpuinfo) --context 1 --max-count 3 --no-search-zip'
fi

GLOBIGNORE='*.fls:*.out:*.aux:*.toc:*.beam:*.pyo:*.lock:*.tmp:*.bak:*.log:*.o:*.hi:*.class:*.so:tags:node_modules:iml:*cache*:*history*'
FIGNORE=$GLOBIGNORE

alias diff='diff --color=auto --suppress-common-lines --ignore-trailing-space --minimal --text --side-by-side --width=$(tput cols) --ignore-tab-expansion --ignore-space-change --ignore-all-space --ignore-blank-lines'

# split path on ":"
alias show-path='echo -e ${PATH//:/\\n} | grep -E "^.{3,}$"'
for i in df du; do alias $i="$i --human-readable --si --total"; done
alias info='info --vi-keys'
alias logout="pkill -KILL -u \$USER"
[ -x $(command which ipython 2>/dev/null) ] && alias ipython="ipython3 --pylab=qt5 --gui=qt5"
[ -x $(command which ipython3 2>/dev/null) ] && alias ipython3="ipython3 --pylab=qt5 --gui=qt5"
[ -x /usr/bin/acpi ] && alias battery="acpi -V"
alias cp="cp --recursive --verbose --interactive --preserve=mode,ownership,timestamps"

# rsync(1) is faster, more secure than cp(1)
if [ -x /usr/bin/rsync ]; then
  alias copy="rsync --progress --log-file=/tmp/rsync/rsync.log --recursive --backup --backup-dir=/tmp/rsync --human-readable --times --preallocate --partial --partial-dir=/tmp/rsync/partially-copied --suffix=rsync-backup --hard-links --group --perms -xattrs --executability --copy-links --copy-dirlinks --compress --compress-level 9"
fi

for i in cp mv rm; do
  alias $i="$i --verbose"
done

alias show-term-capabilities="infocmp -1 | sed -nu 's/^[ \000\t]*//;s/[ \000\t]*$//;/[^ \t\000]\{1,\}/!d;/acsc/d;s/=.*,//p'|column -c80"
alias bat='bat --theme TwoDark --style plain'

[ -x /usr/bin/libreoffice ] && alias libreoffice="libreoffice --norestore"
[ -x /usr/bin/tmux ] && alias tmux='tmux -2'

# Networking, Servers
if [ -x $(command which python3 2>/dev/null) ]; then
  alias http-server-python="python3 -m http.server"
  alias pip-update-packages="pip freeze --local | grep -v '^\-e' | cut -d = -f 1  | xargs -n1 pip install -U"
fi

# NOTE the php server requires index.php in the root dir
[ -x $(command which php 2>/dev/null) ] && alias http-server-php="php -S 0.0.0.0:5000"
[ -x $(command which ruby 2>/dev/null) ] && alias http-server-ruby="ruby -rwebrick -e'WEBrick::HTTPServer.new(:Port => 8000, :DocumentRoot => Dir.pwd.start'"

# get ip address
[ -x /usr/bin/curl ] && alias my-ip='curl ipinfo.io/ip'

# Package Management - pacman, yaourt, expac
if [ -x /usr/bin/pacman ]; then
  alias pacman="pacman --color always --config ${HOME}/.pacman.conf"
  alias pacman-reinstall-all-native-packages="pacman -Qnq | pacman -S -"
  alias pacman-reinstall-all-foreign-packages="pacman -Qmq | pacman -S -"
  alias pacman-remove-orphans="sudo pacman -Rns \$(pacman -Qtdq)"
  if [ -x /usr/bin/expac ]; then
    alias pacman-recent-installations="expac --timefmt='%Y-%m-%d %T' %'%l\t%n' %| sort | %tail %-n 20"
    alias pacman-packages-by-size="expac -S -H M '%k\t%n'"
  fi
fi


# Archiving
[ -x /usr/bin/7z ] && alias 7z='7z -mx=9 -mmt8 -bt -bb1'

# Python
for i in pip pydoc python; do eval "alias ${i}=${i}3"; done
alias ranger='ranger 2>/dev/null'

# $EDITOR
for i in nvim vim vi; do [ -x /usr/bin/$i ] && [ $i != vim ] && eval "alias vim=${i}" && break; done

git_basic_info='$(git --no-pager log --color=never -1 --pretty=format:"%h %cr \"%s\"" 2>/dev/null)'
git_branch_info='$(git --no-pager branch --color=never --format "%(refname:lstrip=-1)" 2>/dev/null) -> $(git --no-pager config --get remote.origin.url 2>/dev/null)@$(git --no-pager branch --color=never --format="%(upstream:lstrip=3)" 2>/dev/null) ($(git remote 2>/dev/null))'
git_branch_info="[${git_branch_info}]"
non_git_prompt='$(basename $0):/$(pwd) :: '

# bash and zsh
if builtin dirs 1>/dev/null 2>/dev/null; then

  f() {
    local cmd="command find -atime -2 -not -empty -readable -regextype posix-extended"
    local n=$#
    if ((n > 0)); then
      local cmd="$cmd "'\('
      local cmd="$cmd -iname *${1}*"
      for pattern in ${@:2}; do
        local cmd="$cmd -or -iname *${pattern}*"
      done
      local cmd="$cmd "'\)'
    fi
    eval "$cmd -not -regex '.*(/node_modules/|\\.cache|\\.cargo/registry/|/.rustup/toolchains/|libreoffice|/site-packages/|google-chrome|\\.vim/(undo|plugged|backup|views|s(essions|wap))).*' -not -regex '.*\\.(b(eam|ack)|log|tmp|/tags|fls|class|(py|s)?o|egg(-info)?|iml|hi|aux|pdf)$' 2>/dev/null"
  }

  # zsh
  if [[ -n "$ZSH_VERSION" ]]; then
    export PS1="${git_basic_info} "$'\n'"${non_git_prompt}"
  else # bash
    export PS1="${git_basic_info} \n${non_git_prompt}"
  fi

else # dash
  export PS1="${git_basic_info} ${non_git_prompt}"
fi

for var in git_basic_info git_branch_info non_git_prompt; do unset -v $var; done
