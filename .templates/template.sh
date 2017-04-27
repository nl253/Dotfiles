#!/bin/bash

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

# call when user is confused or the args are wrong
help() {
    echo -e "$(basename $0) : USAGE "
    echo -e "$(basename $0) : [ ... ] "
    echo -e ""
}

[ $# = 0 ] && echo -e "\n$(basename $0) : You need to specify at least 1 dotfile.\nAborting.\n\n" && return 1

# Check if necessary executables exist
if [[ ! -x ... ]] && [[ ! -x ... ]]; then
    echo -e "You cannot run this script without ...\n"
    echo -e "Please install and rerun"
fi


get-consent() { # {{{

    AGREE_REGEX="^[Yy]es"
    DISAGREE_REGEX="^[Nn]o"

    read -n 3 -r -p "PROMPT >> " RESPONSE
    if [[ $RESPONSE =~ $AGREE_REGEX ]]; then
        echo -e "You agreed.\nThe script will proceed."
        return 0
    else
        echo -e "You disagreed."
        return 1
    fi 
} # }}}

f() { # {{{

} # }}}

formatter() { # {{{

} # }}}

handler() { # {{{

} # }}}

