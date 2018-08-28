## Library of util functions for bash(1)

split() {
  # Usage: split "string" "delimiter"
  IFS=$'\n' builtin read -d "" -ra arr <<<"${1//$2/$'\n'}"
  builtin printf '%s\n' "${arr[@]}"
}

reverse_array() {
  # Usage: reverse_array "array"
  shopt -s extdebug
  f() (builtin printf '%s\n' "${BASH_ARGV[@]}")
  f "$@"
  shopt -u extdebug
}

random_array_element() {
  # Usage: random_array_element "array"
  builtin local arr=("$@")
  builtin printf '%s\n' "${arr[RANDOM % $#]}"
}

uuid() {
  # Usage: uuid
  C="89ab"

  for ((N = 0; N < 16; ++N)); do
    B="$((RANDOM % 256))"

    case "$N" in
      6) builtin printf '4%x' "$((B % 16))" ;;
      8) builtin printf '%c%x' "${C:$RANDOM%${#C}:1}" "$((B % 16))" ;;

      3 | 5 | 7 | 9)
        builtin printf '%02x-' "$B"
        ;;

      *)
        builtin printf '%02x' "$B"
        ;;
    esac
  done

  builtin printf '\n'
}

extract() {
  # Usage: extract file "opening marker" "closing marker"
  while IFS=$'\n' builtin read -r line; do
    [[ "$extract" && "$line" != "$3" ]] &&
      builtin printf '%s\n' "$line"

    [[ "$line" == "$2" ]] && extract=1
    [[ "$line" == "$3" ]] && extract=
  done <"$1"
}


remove_array_dups() {
  # Usage: remove_array_dups "array"
  builtin declare -A tmp_array

  for i in "$@"; do
    [[ "$i" ]] && IFS=" " tmp_array["${i:- }"]=1
  done

  builtin printf '%s\n' "${!tmp_array[@]}"
}
