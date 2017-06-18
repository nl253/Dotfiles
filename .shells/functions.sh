
# {{{ SCRIPT OUTLINE
# -----------------------------------------------------------------------
# DESCRIPTION 
#
# -----------------------------------------------------------------------
# REQUIRES :: internet connection, git, coreutils [grep, sed, ... ]
# -----------------------------------------------------------------------
# DEPENDENCIES :: ... ... ...
# -----------------------------------------------------------------------
# USAGE EXAMPLES 
#
# download-dotfile.sh .bashrc 
# download-dotfile.sh ".gitconfig"
# download-dotfile.sh .bashrc .gitconfig .gitignore
# -----------------------------------------------------------------------
# }}}

# UTILS  {{{{
# checks if an executable is in $PATH
in-path() {
for i in $(echo "$PATH" | sed "s/:/\n/g"); do
    if [ -x "$i/$1" ]; then
        return 0
    fi
done
return 1
}
#  }}}

csv-preview(){ # {{{
    sed 's/,,/, ,/g;s/,,/, ,/g' "$@" | column -s, -t | less -#2 -N -S
} # }}}

pandoc(){  # {{{
    if $(in-path pandoc) ; then 
        if [ $# = 1 ]; then
            pandoc -f html -t markdown_github --standalone --atx-headers --toc --ascii "$1"
        else
            command pandoc "$@"
        fi
    else
        echo "pandoc not on system." && return 0
    fi
} # }}}

ipython(){ # {{{
    in-path() {
    for i in $(echo "$PATH" | sed "s/:/\n/g"); do
        if [ -x "$i/$1" ]; then
            return 0
        fi
    done
    return 1
    }
    if $(in-path ipython); then
        if [ -f .ipythonrc.py ]; then
            command ipython --config=.ipythonrc.py $@
        else
            command ipython --profile=me $@
        fi
    else
        echo "ipython not on system." && return 0
    fi
} # }}}

env(){  # {{{
    if [ ! $# = 0 ]; then 
        command env "$@"
    else command env | sort; 
    fi 
} # by default if no args provided sort env output }}}
