# $1 binary name
cached() {

  # slugify args into a single file name
  if [[ $# -eq 0 ]]; then
    builtin printf "[ERROR] was expecting an argument (program optionally with args)\n"
    builtin return 1
  elif [[ $# -eq 1 ]]; then
    builtin local cache_fname=@$(builtin command python3 -c "print('$PWD'.replace(' ', '_').replace('/', '%'))")
  else
    builtin local cache_fname=$(builtin command python3 -c 'from sys import argv; print("_".join(argv[1:]).replace(" ", "_").replace("builtin_", "").replace("command_", ""))' $@)
  fi

  # cache results
  # format is f$DAY_OF_YEAR@$HOUR
  if [[ $1 = command ]]; then
    builtin local cache_path="/tmp/cache/$2/$(builtin printf '%(%j%H)T')/$cache_fname"
  elif [[ $1 = builtin ]] && [[ $2 = command ]]; then
    builtin local cache_path="/tmp/cache/$3/$(builtin printf '%(%j%H)T')/$cache_fname"
  else
    builtin local cache_path="/tmp/cache/$1/$(builtin printf '%(%j%H)T')/$cache_fname"
  fi

  # if you already evaluated it, just print and return
  if [[ -f $cache_path ]]; then
    builtin command cat <$cache_path
    builtin return 0
  else
    # prepare to cache
    builtin command mkdir -p $(builtin command dirname $cache_path)
  fi

  # duplicate STDOUT, send to cache and print at the same time
  $* | builtin command tee $cache_path
}

_f_() {

  builtin local cmd='builtin command find -atime -2 -not -empty -readable -regextype posix-extended -not -regex ".*(/node_modules/|\\.(mypy_)?cache|\\.git/|\\.cargo/registry/|/.rustup/toolchains/|libreoffice|/site-packages/|google-chrome|\\.vim/(undo|plugged|backup|views|s(essions|wap))).*" -not -regex ".*\\.(b(eam|ack)|log|tmp|/tags|fls|class|(py|s)?o|egg(-info)?|iml|hi|aux)$"'

  if [[ $# -gt 0 ]]; then
    builtin local cmd="$cmd "'\('
    builtin local cmd="$cmd -iname \*${1}\*"
    for pattern in ${@:2}; do
      builtin local cmd="$cmd -or -iname \*${pattern}\*"
    done
    builtin local cmd="$cmd "'\)'
  fi

  builtin eval "$cmd 2>/dev/null"
}

hexadecimal() { builtin printf "%X\n" $1; }
octal() { builtin printf "%o\n" $1; }
binary() {
  n="$1"
  bit=""
  while [ "$n" -gt 0 ]; do
    bit="$((n & 1))$bit"
    : $((n >>= 1))
  done
  printf "%s\n" "$bit"
}

upper() { builtin printf '%s\n' "${*^^}"; }
lower() { builtin printf '%s\n' "${*,,}"; }
capitalise() { builtin printf '%s\n' "${*^}"; }

trim() {
  # Usage: trim_all "   example   string    "
  set -f
  set -- $*
  builtin printf '%s\n' "$*"
  set +f
}

join() {
  builtin command python3 <<EOF
from shlex import split
print('$1'.join(split('''${@:2}''')))
EOF
}

split() {
  builtin command python3 <<EOF
from re import split, M
print('\n'.join(split(r"$1", '''${*:2}''', M)))
EOF
}

replace() {
  builtin command python3 <<EOF
from re import sub
print(sub(r'$1', '$2', '''${*:3}'''))
EOF
}

count() {
  builtin command python3 <<EOF
from collections import Counter

counter = Counter('''$(cat /dev/stdin)'''.splitlines())

for key, val in counter.most_common(len(counter)):
    print(val, key)
EOF
}

gcd() {
  builtin command python3 <<EOF
from math import gcd
from shlex import split
from functools import reduce
print(reduce(gcd, map(int, split('''$*'''))))
EOF
}

sum() {
  builtin command python3 <<EOF
from shlex import split
from functools import reduce
print(reduce(lambda x, y: x + y, map(float, split('''$*'''))))
EOF
}

product() {
  builtin command python3 <<EOF
from shlex import split
from functools import reduce
print(reduce(lambda x, y: x * y, map(float, split('''$*'''))))
EOF
}

min() {
  builtin command python3 <<EOF
from shlex import split
from functools import reduce
print(reduce(min, y: x + y, map(float, split('''$*'''))))
EOF
}

max() {
  builtin command python3 <<EOF
from shlex import split
from functools import reduce
print(reduce(max, y: x + y, map(float, split('''$*'''))))
EOF
}

pi() {
  builtin command python3 <<EOF
from math import pi
print(pi)
EOF
}

e() {
  builtin command python3 <<EOF
from math import e
print(e)
EOF
}

log2() {
  builtin command python3 <<EOF
from math import log2
print(log2($1))
EOF
}

log10() {
  builtin command python3 <<EOF
from math import log10
print(log10($1))
EOF
}

mean() {
  builtin command python3 <<EOF
from statistics import mean
from shlex import split
print(mean(map(float, split('''$*'''))))
EOF
}

median() {
  builtin command python3 <<EOF
from statistics import median
from shlex import split
print(median(map(float, split('''$*'''))))
EOF
}

mode() {
  builtin command python3 <<EOF
from statistics import mode
from shlex import split
print(mode(map(float, split('''$*'''))))
EOF
}

stdev() {
  builtin command python3 <<EOF
from statistics import stdev
from shlex import split
print(stdev(map(float, split('''$*'''))))
EOF
}

permutations() {
  builtin command python3 <<EOF
from itertools import permutations
from shlex import split
for i in permutations(split('''${*:2}'''), $1):
    print(' '.join(i))
EOF
}

combinations() {
  builtin command python3 <<EOF
from itertools import combinations
from shlex import split
for i in combinations(split('''${*:2}'''), $1):
    print(' '.join(i))
EOF
}

cos() {
  builtin command python3 <<EOF
from math import cos, radians
print(cos(radians($1)))
EOF
}

sin() {
  builtin command python3 <<EOF
from math import sin, radians
print(sin(radians($1)))
EOF
}

tan() {
  builtin command python3 <<EOF
from math import tan, radians
print(tan(radians($1)))
EOF
}

for i in bc {g,}awk jupyter-nbconvert pygmentize pandoc rst2{xml,s5,odt,html,html5,html4,man,pseudoxml,xetex}{.py,} wn tokei {z,xz}{cat,diff,less} wc tree sort pydoc{3,3.5,3.6,} find fd df du curl ps rg {xz,z,}{p,e,a,f,}grep; do
  eval "alias $i='cached builtin command $i'"
done

alias f='cached _f_'
