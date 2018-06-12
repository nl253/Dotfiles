#!/usr/bin/env bash

# ------------------------------------------------------------
# 	   		YOUR WORKSPACE
# ------------------------------------------------------------

# GLOBAL VARIABLES
# ------------------------------------------------------------
# Generic
DEPENDENCIES=(cat find grep sed g++ readlink doxygen python3)
REQUIREMENTS=("internet connection")
ROOT=$(dirname $(readlink -e $0))

# FUNCTIONS
# ------------------------------------------------------------
# They will be automatically callable through the command line.
# ------------------------------------------------------------

# list dependencies
dependencies() {
  for i in "${DEPENDENCIES[@]}"; do
    echo $i
  done
}

# additional requirements such as internet connection, git account etc...
requirements() {
  for i in "${REQUIREMENTS[@]}"; do
    echo $i
  done
}                    

DOCS_DIR=${ROOT}/docs

# build docs using doxygen
docs(){
	cd $DOCS_DIR && doxygen
}

# serve built docs using a http server built-in into Python
serve-docs(){
 cd $DOCS_DIR && python3 -m http.server
}

EXE_NAME=app
BUILD_TOOL=g++
LANG_VERSION=17
DIST_DIR=${ROOT}/dist
BUILD_FLAGS="--std c++${LANG_VERSION} -o ${DIST_DIR}/${EXE_NAME}"
SRC_DIR=${ROOT}/src

# print all source files
src-files(){
	find ${SRC_DIR} -regextype posix-extended -regex '.*\.(cpp|c|c\+\+)$'
}

# print all header files
headers(){
	find ${SRC_DIR} -regextype posix-extended -regex '.*\.(h|hpp|h\+\+)$'
}

# load all source files into $EDITOR
edit() {
  [[ -v EDITOR ]] && $EDITOR $(src-files | xargs) || echo -e '$EDITOR not set!'
}

# build the project
build(){
	eval "${BUILD_TOOL} ${BUILD_FLAGS} $(src-files | xargs)" 
}

# run compiled executable
run(){
	# build if the executable does not exist
	[[ ! -x "${DIST_DIR}/${EXE_NAME}" ]] && build
	eval "${DIST_DIR}/${EXE_NAME}"
}

# UTILITY FUNCTIONS
# ------------------------------------------------------------
# Make sure to declare helper (utility) functions with a
# leading `_` (underscore).
# This will prevent them from being called (private access).
# ------------------------------------------------------------

# Use if you need some security.
# Feel free to delete it if you don't need it.
# Notice the leading underscore, you cannot call it from the command line!
_get_consent() {
  # Save response to variable $RESPONSE.
  read -n 3 -r -p "Type [Yes/No] ~> " RESPONSE
  if [[ ${RESPONSE^^} =~ ^(YES|YEAH)$ ]]; then
    echo -e "You agreed.\nThe script will proceed." && return 0
  else
    echo -e "You disagreed. Aborting." && return 1
  fi
}
# ------------------------------------------------------------------- 
# 			END OF YOUR WORKSPACE
# ------------------------------------------------------------------- 
# {

# Call when user is confused or the args are wrong.
help() {

  local functions=$(cat $0 | grep -Po '^\s*[a-z][-a-zA-Z_]+(?=\(\))' | grep -v help | xargs)

  echo -e "\n$(basename $0)\n"
  echo -e "Usage:\n"
  echo "    $(basename $0) help | -h | --help"
  for i in $functions; do
    echo "    $(basename $0) $i"
  done
	echo ""
}

# Check if necessary executables exist
# make sure you declare some $DEPENDENCIES above
_check_dependencies() {
  for i in $DEPENDENCIES; do
    if [[ ! -x $(command which $i) ]]; then
      cat <<EOF
  You need $i to run the script.
  Pleas install $i and rerun.

  NOTE it might not be in \$PATH.
EOF
      return 1
    fi
  done
  return 0
}

_main() {

  # call the _check_dependencies function (dependencies are declared above)
  _check_dependencies || exit 1

  if (($# == 0)) || [[ "$*" =~ --help|-h|help ]]; then
    help && exit 0
  fi

  local functions=$(cat $0 | grep -Po '^\s*[a-z][-a-zA-Z_]+(?=\(\))' | xargs)

  # Iterate while there are any args left.
  while (($#)); do

    # Self-documentation, find all functions in the file that
    # DO NOT begin with an underscore (`_`).

    if [[ ! $1 =~ ^_ ]]; then

			# if you provide an arg that is CAPIT_ALISED, and such a variable is set,
			# the contend of that variable will be printed
			if [[ $1 =~ [_A-Z]+ ]]; then
				eval "echo $"$1 && shift

			# otherwise it's a function
			else
				eval $1 && shift || help && exit
			fi
    else
      help
      exit 0
    fi
  done
}

_main "$@"

# }
# vim: foldmarker={,} foldmethod=marker nospell ts=2 sw=2
