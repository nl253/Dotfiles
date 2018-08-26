#!/usr/bin/env bash

# ------------------------------------------------------------
# 	   		             YOUR WORKSPACE
# ------------------------------------------------------------

# GLOBAL VARIABLES (CANNOT BE DELETED)
# ------------------------------------------------------------
# You are free to add / modify the values of these globals.
# However, you cannot delete them as core functions depend on them.
DEPENDENCIES=(xargs readlink dirname basename cat find grep sed tput)
DESCRIPTION='Utility script for launching tasks.'
REQUIREMENTS=('internet connection' 'git')
ROOT=$(command dirname $(command readlink -e $0))

# >> PUBLIC FUNCTIONS
# ------------------------------------------------------------
# They will be automatically callable through the command line.
# ------------------------------------------------------------
# Make sure to add a docstring (a comment line *above* the funct 
# declaration) -- this will be shown when the help msg is displayed

# YOUR CODE GOES HERE ...

# The functions below are examples of how the script can be used.
# You are free to delete everything until `END OF YOUR WORKSPACE`.

# [PUBLIC FUNCTION]
# List dependencies.
dependencies() {
  for dep in "${DEPENDENCIES[@]}"; do
    builtin echo $dep
  done
}

# [PUBLIC FUNCTION]
# List additional requirements such as internet connection, git account etc...
requirements() {
  for req in "${REQUIREMENTS[@]}"; do
    builtin echo $req
  done
}

# [PUBLIC FUNCTION]
# List all important variables in the script.
variables() {
  builtin local vars=$(command cat $0 | command grep -Po '^\s*[A-Z][A-Z0-9_]+=' | command grep -Eo '[A-Z][A-Z0-9_]+' | command xargs)
  for var in $vars; do
    builtin local cmd="builtin echo \$$var"
    builtin printf "$var = $(builtin eval $cmd)\n"
  done
}

# [PUBLIC FUNCTION]
# Check the value of a single array variable.
check_array_variable() {
  builtin local var=$1
  for i in $(builtin eval "builtin echo \${$var[@]}"); do
    echo $i
  done
}

# [PUBLIC FUNCTION]
# Check the value of a single string variable.
check_variable() {
  builtin local var=$1
  builtin eval "builtin echo \${$var}"
}

# [PUBLIC FUNCTION]
# Display the root directory for the project as seen by the script.
root() { builtin echo $ROOT; }

# >> UTILITY (PRIVATE) FUNCTIONS
# ------------------------------------------------------------
# Make sure to declare helper (utility) functions with a
# leading `_` (underscore).
# This will prevent them from being called (private access).
# ------------------------------------------------------------

# [UTILITY FUNCTION]
#
# FEEL FREE TO DELETE IT IF YOU DON'T NEED IT
# ===========================================
# 
# Example usage:
# 
#   resp=$(_get_consent)
# 
#   if ((resp)); then 
#     echo YES
#   else
#     echo ABORTING
#   fi
#
# Use if you need some security.
# Notice the leading underscore, you cannot call it from the command line!
_get_consent() {
  # Save response to variable $RESPONSE.
  builtin read -n 1 -p "Type [Y/N] ~> " RESPONSE
  if [[ ${RESPONSE^^} == Y ]]; then
    builtin echo -e "You agreed.\nThe script will proceed." && return 0
  else
    builtin echo -e "You disagreed." && return 1
  fi
}

# [UTILITY LOGGING FUNCTIONS]
#
# FEEL FREE TO DELETE IT IF YOU DON'T NEED IT
# ===========================================
#
# Logging utility functions (coloured)
# Again, there is a leading underscore, you cannot call it from the command line!

red=$(command tput setaf 1)
green=$(command tput setaf 2)
yellow=$(command tput setaf 3)
pink=$(command tput setaf 5)
cyan=$(command tput setaf 6)
reset=$(command tput sgr0)

_info() { _log "${green}INFO${reset}" $@; }
_warn() { _log "${yellow}WARN${reset}" $@; }
_err() { _log "${red}ERROR${reset}" $@ 1>&2; }

_log() {
  builtin local level=$1
  builtin printf "[$level] ${*:2}\n"
}

# ------------------------------------------------------------------- {
# 			                END OF YOUR WORKSPACE
# -------------------------------------------------------------------
#
# THINGS IN THIS SECTION *CANNOT* BE DELETED.
#
# -------------------------------------------------------------------

# Call when user is confused or the arguments are wrong.
# Generates a help msg from declared public functions.
_help() {

  builtin local program="$(basename $0)"

  builtin echo -e "${program}\n"
  builtin echo -e "$DESCRIPTION\n"

  builtin echo "Usage:"

  builtin printf "%-4s %-35s" '' "$program help | -h | --help"
  builtin echo 'Display this help message.'

  for cmd in $(commands); do
    builtin printf "%-4s %-35s" '' "$program $cmd"
    _describe $cmd | command sed -E 's/#\s*//'
    builtin echo ''
  done
}

# [PUBLIC FUNCTION]
# List all commands that the script can be called with.
commands() { 
  command cat $0 | command grep -oP '^\s*[a-z][-a-z_]+\s*\(\)' | command grep -Eo '[a-z][-a-zA-Z_]+' | command xargs; 
}

# [PUBLIC FUNCTION]
# Check if necessary executables exist make sure you declare some $DEPENDENCIES above.
check_dependencies() {
  for dep in $DEPENDENCIES; do
    if [[ ! -x $(builtin type -P $dep) ]]; then
      builtin echo -e "You need $dep to run the script.\n"
      builtin echo -e 'NOTE it might not be in $PATH.'
      builtin return 1
    fi
  done
  builtin return 0
}

# Util function for getting the description (docstring) of a function.
_describe() { command cat $0 | command grep -Pzo '#[^\n]*?(?=\n\s*'"$1"'\s*\(\))'; }

_main() {

  # call the check_dependencies function (deps are declared above)
  check_dependencies || builtin exit 1

  # if called with --help or -h or help
  # or if no args provided then display help msg
  if (($# == 0)) || [[ "$*" =~ (--)?help|-h ]]; then
    _help
    builtin exit 0

    # trying to call a private funct (prefixed with '_')
    # shows help msg
  elif [[ $1 =~ ^_ ]]; then
    _help
    builtin exit 1

    # otherwise it's a valid call to an existing public function
  elif builtin eval "$*"; then
    builtin exit 0

  else
    _help
    builtin echo "[ERROR] unrecognised command ${*}"
    builtin exit 1
  fi

}

_main "$@"

# }

# vim:foldmarker={,}:foldmethod=marker:ts=2:sw=2:
