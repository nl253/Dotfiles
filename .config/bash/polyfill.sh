return 0

[[ ! -x /usr/bin/dirname ]] && [[ ! -x /bin/dirname ]] && dirname() { builtin printf '%s\n' "${1%/*}"; }
[[ ! -x /usr/bin/sleep ]] && [[ ! -x /bin/sleep ]] && sleep() { builtin read -rst "${1:-1}" -N 999; }
[[ ! -x /usr/bin/date ]] && [[ ! -x /bin/date ]] && date() { builtin printf "%($1)T\\n" "-1"; }
[[ ! -x /usr/bin/strftime ]] && [[ ! -x /bin/strftime ]] && strftime() { builtin printf "%($1)T\\n" "-1"; }
[[ ! -x /usr/bin/cat ]] && [[ ! -x /bin/cat ]] && cat() { mapfile -t file_data <"file"; }

if [[ ! -x /usr/bin/ls ]] && [[ ! -x /bin/ls ]]; then
  ls() {
    for file in *; do
      builtin printf '%s\n' "$file"
    done
  }
fi

if [[ ! -x /usr/bin/basename ]] && [[ ! -x /bin/basename ]]; then
  basename() {
    # Usage: basename "path"
    : "${1%/}"
    builtin printf '%s\n' "${_##*/}"
  }
fi

if [[ ! -x /usr/bin/tail ]] && [[ ! -x /bin/tail ]]; then
  # Usage: tail "n" "file"
  tail() {
    mapfile -tn 0 line <"$2"
    builtin printf '%s\n' "${line[@]: -$1}"
  }
fi

if [[ ! -x /usr/bin/head ]] && [[ ! -x /bin/head ]]; then
  # Usage: head "n" "file"
  head() {
    mapfile -tn "$1" line <"$2"
    builtin printf '%s\n' "${line[@]}"
  }
fi

if [[ ! -x /usr/bin/seq ]]; then
  seq() {
    builtin local max=${1:-10}
    for i in {0..$max}; do
      builtin printf '%s\n' "$i"
    done
  }
fi
