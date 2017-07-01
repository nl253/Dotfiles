
# THIS FILE MUST USE POSIX COMPLIANT SYNTAX
# IT IS SOURCED BY BOTH `zsh` AND `bash`

# SOURCE

safe-source(){
  for i in "$@"; do
    [[ -f $i ]] && source "$i"
  done
}

safe-source ~/.travis/travis.sh

unset -f safe-source  # done using safe-source
