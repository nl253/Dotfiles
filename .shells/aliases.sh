
# ALIASES (tested on zsh and bash)

# SOURCED BY BOTH `zsh` AND `bash`

_in_path() {
    # checks  an executable is in $PATH
    for i in $(echo -e ${PATH//:/\\n} | sort | uniq); do
	if [[ -x "$i/$1" ]]; then
	    return 0
	fi
    done
    return 1
}

# general 
alias sudo='sudo '

# get MIME type of a file
[[ -x $(command which file) ]] && alias mime-type='file --dereference --brief --mime-type -- '

# dirs and files 
alias ls='ls --color=auto --group-directories-first'

alias -- -='cd -' # Go back
alias ..="cd .."
alias ...="cd ../.."
alias ....="cd ../../.."

# pattern matching 
for i in {f,e,}grep diff; do
	eval "alias ${i}=${i} --color=auto"
done

# utils 
# split path on ":"
alias show-path='echo -e ${PATH//:/\\n} | sort | grep -P "^.{3,}$"'
alias df='df --human-readable --si --total'
alias du='du --human-readable --si --summarize --total'
alias info='info --vi-keys'
alias logout="pkill -KILL -u \$USER"
_in_path ipython && alias ipython="ipython --profile=me"
alias battery="acpi -V"
alias cp="cp --recursive --verbose --interactive --preserve=mode,ownership,timestamps"
if _in_path rsync; then
    alias copy="rsync --ignore-missing-args --group --xattrs --hard-links --executability --itemize-changes --stats --partial --rsh=bash --progress --recursive --times --whole-file --perms --executability --verbose --human-readable  --copy-links"
fi
alias mv="mv --verbose"
alias rm="rm --verbose"
alias show-term-capabilities="infocmp -1 | sed -nu 's/^[ \000\t]*//;s/[ \000\t]*$//;/[^ \t\000]\{1,\}/!d;/acsc/d;s/=.*,//p'|column -c80"

_in_path libreoffice && alias libreoffice="libreoffice --norestore"

_in_path tmux && alias tmux='tmux -2'

# Java 
# -----------------------------------
# REQUIRES
# - mvn
# -jdk8
if _in_path javac; then
    if _in_path mvn; then
	alias mvn-init='mvn archetype:generate -DgroupId=com.mycompany.app -DartifactId=my-app -DarchetypeArtifactId=maven-archetype-quickstart -DinteractiveMode=false'
    fi
fi

# Networking, Servers 
# -----------------------------------
# REQUIRES
# - sshfs
# - aria2c
# - rsync
# - python3
# -----------------------------------
if _in_path python3; then
	alias http-server-python="python3 -m http.server" 
	alias pip-update-packages="pip freeze --local | grep -v '^\-e' | cut -d = -f 1  | xargs -n1 pip install -U"
fi

# note the php server requires index.php in the root dir
_in_path php && alias http-server-php="php -S 0.0.0.0:5000"
_in_path ruby && alias http-server-ruby="ruby -rwebrick -e'WEBrick::HTTPServer.new(:Port => 8000, :DocumentRoot => Dir.pwd.start'"

# mount a remote hard-drive
_in_path sshfs && alias mount-raptor="mkdir -p \${HOME}/Raptor && sshfs -o transform_symlinks -o follow_symlinks nl253@raptor.kent.ac.uk: \${HOME}/Raptor"
_in_path curl && alias my-ip='curl ipinfo.io/ip'

# Package Management - pacman, yaourt, expac 
# -----------------------------
# REQUIRES
# - pacman
# - yaourt
# - expac
# -----------------------------
if _in_path pacman; then
    alias pacman='pacman --config "${HOME}/.pacman.conf"'
    alias pacman-reinstall-all-native-packages="pacman -Qnq | pacman -S -"
    alias pacman-reinstall-all-foreign-packages="pacman -Qmq | pacman -S -"
    alias pacman-remove-orphans="sudo pacman -Rns \$(pacman -Qtdq)"
    if _in_path expac; then
	alias pacman-recent-installations="expac --timefmt='%Y-%m-%d %T' %'%l\t%n' %| sort | %tail %-n 20"
	alias pacman-packages-by-size="expac -S -H M '%k\t%n'"
    fi
    if _in_path yaourt; then
	export YAOURT_COLORS="nb=1:pkg=1:ver=1;32:lver=1;45:installed=1;42:grp=1;34:od=1;41;5:votes=1;44:dsc=0:other=1;35"
    fi
fi

# Proceses 
# ------------------------
# REQUIRES
# - top (or htop)
# ------------------------
for i in {h,a,}top 'ps aux'; do
    _in_path $i && alias p=$i && break
done

# VCS 
# -------------------------
# REQUIRES
# - git
# - hub
if _in_path git; then
    alias todo="git grep -n --word-regexp --break --heading --after-context 3 TODO"
    alias fixme="git grep -n --word-regexp --break --heading --after-context 3 FIXME"
    # look for TODOs in the current repo
    if _in_path hub; then
	eval "$(hub alias -s)"
    fi
fi

# Archiving
# --------------------------
# REQUIRES 
# - 7z 

if [[ -x $(command which 7z) ]]; then
	alias 7z='7z -mx=9 -mmt8 -bt -bb1'
fi

# Databases 
# -------------------------
# REQUIRES
# - postgresql
# - sqlite3
#
_in_path psql && alias psql='psql --single-line'
_in_path sqlite3 && alias sqlite3="sqlite3 -init \${HOME}/.sqliterc"
_in_path mysql && [[ $(hostname) =~ raptor ]] && alias mysql-dragon='mysql -u nl253 -p -h dragon.kent.ac.uk nl253' 
_in_path mycli && [[ $(hostname) =~ raptor ]] && alias mycli-dragon='mycli mysql://nl253@dragon.kent.ac.uk/nl253' 

# Haskell 
# ----------
# REQUIRES
# - stack
# - ghc
# - ghci
# - ghc-mod
# stack (it wraps a number of haskell-related tools, for each access ...)
for i in ghc{i,} hoogle haddock; do
    eval "alias "${i}"='stack "${i}"'"
done

unset -f _in_path
# vim: foldmethod=marker foldlevel=0 foldmarker={,} nowrap formatoptions=
