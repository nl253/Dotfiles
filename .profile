
# ~/.profile

# If not running interactively, don't do anything
case $- in
  *i*) ;;
  *) return ;;
esac

# This file is not read by bash(1) if ~/.bash_profile or ~/.bash_login exists.

# normalise prompt in case somthing goes wrong
export PS1="$USER@$HOSTNAME $0 $ "

# [[ "$XDG_CURRENT_DESKTOP" == "KDE" ]] || export QT_QPA_PLATFORMTHEME="qt5ct"

export HISTSIZE=20000
export HISTFILESIZE=20000
export HISTCONTROL="ignoreboth:erasedups"
export HISTTIMEFORMAT=""
export HH_CONFIG=hicolor # get more colors
export HISTIGNORE="&:[ ]*:exit:cd:ls:bg:fg:history:clear:jobs"

export HISTFILE=~/.shell_history
export SAVEHIST=10000

# vim: foldmethod=marker foldlevel=0 foldmarker={,} shiftwidth=2 tabstop=2

# Don't check mail when opening terminal.
unset MAILCHECK

for directory in ".local" .yarn .stack .cabal .config/composer/vendor .cargo .local/share/fzf go node_modules; do
  [ -d "${HOME}/${directory}/bin" ] && export PATH="${HOME}/${directory}/bin:${PATH}:" 2>/dev/null
done

[ -f ~/.makepkg.conf ] && export MAKEPKG_CONF=~/.makepkg.conf
[ -f ~/.config/ranger/rc.conf ] && export RANGER_LOAD_DEFAULT_RC=false

# $PAGER
if [ -x $(command which less 2>/dev/null) ]; then
  export LESS='--RAW-CONTROL-CHARS --IGNORE-CASE --QUIET --HILITE-SEARCH --long-prompt'
  if [ -x $(command which pygmentize 2>/dev/null) ]; then
    export LESSOPEN='| pygmentize %s'
  fi
  export PAGER=less
else
  [ -x $(command which more 2>/dev/null) ] && export PAGER=more && alias less=more
fi

# reset
export CDPATH="${HOME}:"

for directory in ${HOME}/Documents ${HOME}/Documents/*; do
  [ -d $directory ] && export CDPATH="${directory}:${CDPATH}:" 2>/dev/null
done

# toolchain to use for Rust
export DEFAULT_TOOLCHAIN=nightly

# use qt5 not qt4
export QT_SELECT=5

# POSTGRES
if [ -x $(command which psql 2>/dev/null) ]; then
  export PGUSER=postgres
  export PGHOST=localhost
  export PGDATABASE=testing
fi

# $BROWSER
for i in firefox-developer firefox google-chrome-stable chromium elinks lynx w3m; do
  if [ -x $(command which $i 2>/dev/null) ]; then
    export BROWSER=$(command which $i 2>/dev/null) && break
  fi
done

# $EDITOR
for i in nvim vim vi; do
  if [ -x $(command which $i 2>/dev/null) ]; then
    export EDITOR=$(command which $i 2>/dev/null)
    if [ $i != vim ]; then
      eval "alias vim=${i}"
    fi
    break
  fi
done

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

export YAOURT_COLORS="nb=1:pkg=1:ver=1;32:lver=1;45:installed=1;42:grp=1;34:od=1;41;5:votes=1;44:dsc=0:other=1;35"

for i in pandoc{-citeproc,}; do
  [ -x "/usr/local/bin/${i}" ] && eval "alias ${i}=/usr/local/bin/${i}"
done

# general
alias sudo='sudo '

# get MIME type of a file
[ -x $(command which file) ] && alias mime-type='file --dereference --brief --mime-type -- '

# dirs and files
alias ls='ls --color=auto --group-directories-first'

alias ..="cd .."
alias ...="cd ../.."
alias ....="cd ../../.."
alias .....="cd ../../../.."

# pattern matching
for i in {f,e,}grep diff; do
  eval "alias ${i}='${i} --color=auto'"
done

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
  alias copy="rsync --ignore-missing-args --group --xattrs --hard-links --executability --itemize-changes --stats --partial --rsh=bash --progress --recursive --times --whole-file --perms --executability --verbose --human-readable  --copy-links"
fi
alias mv="mv --verbose"
alias rm="rm --verbose"
alias show-term-capabilities="infocmp -1 | sed -nu 's/^[ \000\t]*//;s/[ \000\t]*$//;/[^ \t\000]\{1,\}/!d;/acsc/d;s/=.*,//p'|column -c80"

[ -x $(command which libreoffice 2>/dev/null) ] && alias libreoffice="libreoffice --norestore"
[ -x $(command which tmux 2>/dev/null) ] && alias tmux='tmux -2'

# Java
# -----------------------------------
# REQUIRES
# - mvn
# -jdk8
if [ -x $(command which javac 2>/dev/null) ] && [ -x $(command which mvn 2>/dev/null) ]; then
  alias mvn-init='mvn archetype:generate -DgroupId=com.mycompany.app -DartifactId=my-app -DarchetypeArtifactId=maven-archetype-quickstart -DinteractiveMode=false'
fi

# Networking, Servers
# -----------------------------------
# REQUIRES
# - sshfs
# - aria2c
# - rsync
# - python3
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
# -----------------------------
# REQUIRES
# - pacman
# - expac
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
# -------------------------
# REQUIRES
# - git
# - hub
[ -x $(command which git 2>/dev/null) ] && [ -x $(command which hub 2>/dev/null) ] && eval "$(hub alias -s)"

# Archiving
# --------------------------
# REQUIRES
# - 7z
[ -x $(command which 7z 2>/dev/null) ] && alias 7z='7z -mx=9 -mmt8 -bt -bb1'

# Databases
# -------------------------
# REQUIRES
# - postgresql
# - sqlite3
[ -x $(command which psql 2>/dev/null) ] && alias psql='psql --single-line'
[ -x $(command which sqlite3 2>/dev/null) ] && alias sqlite3="sqlite3 -init \${HOME}/.sqliterc"

if [ $(hostname) = "raptor" ]; then
  [ -x $(command which mysql 2>/dev/null) ] && alias mysql-dragon='mysql -u nl253 -p -h dragon.kent.ac.uk nl253'
  [ -x $(command which mycli 2>/dev/null) ] && alias mycli-dragon='mycli mysql://nl253@dragon.kent.ac.uk/nl253'
fi

# Haskell
# ----------
# REQUIRES
# - stack
for i in ghc{i,} hoogle haddock; do
  eval "alias "${i}"='stack "${i}"'"
done

# FZF init (tested on zsh and bash)

# chech if on system, set up aliases in case it is and isn't

if [ -x $(command which fzf 2>/dev/null) ]; then # {

  # KEYMAP
  # ------------------------
  # enter : print to STDOUT
  # ctrl-d : scroll down
  # ctrl-u : scroll up
  # alt-e : edit with $EDITOR
  # alt-d : cd
  # alt-r : execute `rifle` [detects what to use based on file type]
  # alt-l : open in `less`

  export FZF_DEFAULT_OPTS=" --bind='alt-d:execute(cd {})' --bind='ctrl-d:half-page-down,ctrl-u:half-page-up,alt-p:toggle-preview' --bind='alt-e:execute(\$EDITOR {})' --bind='alt-r:execute(rifle {}),alt-l:execute:($PAGER {})' --no-mouse --multi --black --margin 3% --prompt=' >> ' --reverse --tiebreak=end,length --color 'hl:107,hl+:1,bg+:234,fg:240,fg+:246'"
  export FZF_DEFAULT_COMMAND='git ls-tree -r --name-only HEAD || find . -path "*/\.*" -prune -o -type d -print -type f -print -o -type l -print | sed s/^..//\ 2> /dev/null'
  export FZF_CTRL_T_OPTS="--select-1 --exit-0"
  export FZF_CTRL_R_OPTS="--sort --exact --preview 'echo {}' --preview-window down:3:hidden --bind '?:toggle-preview'"

  # preview configured to `cat` for files and use `tree` for dirs
  # [FZF] with [P]REVIEW

  if [ -e ~/.config/ranger/scope.sh ]; then
    alias fzfp='fzf --preview="bash ~/.config/ranger/scope.sh {} $(tput cols) $(tput lines) /tmp/ False"'
  elif [ -x $(command which pygmentize 2>/dev/null) ]; then
    alias fzfp='fzf --preview="[ -f {} ] && head -n $(tput lines) {} | pygmentize -l $(pygmentize -N {}) || [ -d {} ] && tree -l -a --prune -L 4 -F --sort=mtime {}"'
  else
    alias fzfp='fzf --preview="[ -f {} ] && head -n $(tput lines) {} || [ -d {} ] && tree -l -a --prune -L 4 -F --sort=mtime {}"'
  fi
fi # }

# if running bash(1) / zsh

f() {
	if [[ $0 =~ (ba|z)sh ]]; then
    [ -f /opt/anaconda/bin/python3 ] && . /opt/anaconda/bin/activate
    eval $(dircolors -b)
  fi
}

f 2>/dev/null
