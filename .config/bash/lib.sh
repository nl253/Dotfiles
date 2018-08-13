## Library of util functions for bash(1)

[[ ! -x /usr/bin/dirname ]] && [[ ! -x /bin/dirname ]] && dirname() { printf '%s\n' "${1%/*}"; }
[[ ! -x /usr/bin/sleep ]] && [[ ! -x /bin/sleep ]] && sleep() { read -rst "${1:-1}" -N 999; }
[[ ! -x /usr/bin/date ]] && [[ ! -x /bin/date ]] && date() { printf "%($1)T\\n" "-1"; }
[[ ! -x /usr/bin/strftime ]] && [[ ! -x /bin/strftime ]] && strftime() { printf "%($1)T\\n" "-1"; }
[[ ! -x /usr/bin/cat ]] && [[ ! -x /bin/cat ]] && cat() { mapfile -t file_data <"file"; }
[[ ! -x /usr/bin/ls ]] && [[ ! -x /bin/ls ]] && ls() { for file in *; do printf '%s\n' "$file"; done; }

upper() { printf '%s\n' "${*^^}"; }
lower() { printf '%s\n' "${*,,}"; }

split() {
  # Usage: split "string" "delimiter"
  IFS=$'\n' read -d "" -ra arr <<<"${1//$2/$'\n'}"
  printf '%s\n' "${arr[@]}"
}

trim() {
  # Usage: trim_all "   example   string    "
  set -f
  set -- $*
  printf '%s\n' "$*"
  set +f
}

reverse_array() {
  # Usage: reverse_array "array"
  shopt -s extdebug
  f() (printf '%s\n' "${BASH_ARGV[@]}")
  f "$@"
  shopt -u extdebug
}

random_array_element() {
  # Usage: random_array_element "array"
  local arr=("$@")
  printf '%s\n' "${arr[RANDOM % $#]}"
}

uuid() {
  # Usage: uuid
  C="89ab"

  for ((N = 0; N < 16; ++N)); do
    B="$((RANDOM % 256))"

    case "$N" in
      6) printf '4%x' "$((B % 16))" ;;
      8) printf '%c%x' "${C:$RANDOM%${#C}:1}" "$((B % 16))" ;;

      3 | 5 | 7 | 9)
        printf '%02x-' "$B"
        ;;

      *)
        printf '%02x' "$B"
        ;;
    esac
  done

  printf '\n'
}

extract() {
  # Usage: extract file "opening marker" "closing marker"
  while IFS=$'\n' read -r line; do
    [[ "$extract" && "$line" != "$3" ]] &&
      printf '%s\n' "$line"

    [[ "$line" == "$2" ]] && extract=1
    [[ "$line" == "$3" ]] && extract=
  done <"$1"
}

if [[ ! -x /usr/bin/basename ]] && [[ ! -x /bin/basename ]]; then
  basename() {
    # Usage: basename "path"
    : "${1%/}"
    printf '%s\n' "${_##*/}"
  }
fi

remove_array_dups() {
  # Usage: remove_array_dups "array"
  declare -A tmp_array

  for i in "$@"; do
    [[ "$i" ]] && IFS=" " tmp_array["${i:- }"]=1
  done

  printf '%s\n' "${!tmp_array[@]}"
}

if [[ ! -x /usr/bin/seq ]]; then
  seq() {
    local max=${1:-10}
    for i in {0..$max}; do
      printf '%s\n' "$i"
    done
  }
fi

if [[ ! -x /usr/bin/tail ]] && [[ ! -x /bin/tail ]]; then
  # Usage: tail "n" "file"
  tail() {
    mapfile -tn 0 line <"$2"
    printf '%s\n' "${line[@]: -$1}"
  }
fi

if [[ ! -x /usr/bin/head ]] && [[ ! -x /bin/head ]]; then
  # Usage: head "n" "file"
  head() {
    mapfile -tn "$1" line <"$2"
    printf '%s\n' "${line[@]}"
  }
fi
