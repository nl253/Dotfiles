# GENERIC CACHE FOR CLI UTILS

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

  builtin local cache_path="/tmp/cache"

  # get name of cmd to run
  if [[ $1 == command ]]; then
    builtin local cache_path="$cache_path/$2"
  elif [[ $1 == builtin ]] && [[ $2 == command ]]; then
    builtin local cache_path="$cache_path/$3"
  else
    builtin local cache_path="$cache_path/$1"
  fi

  # append day and time (just hour)
  builtin local cache_path="$cache_path/$(builtin printf '%(%j%H)T')/$cache_fname"

  # if you already evaluated it, just print and return
  if [[ -f $cache_path ]]; then
    builtin command cat <$cache_path
    builtin return 0
  else
    # prepare to cache
    builtin command mkdir -p $(builtin command dirname $cache_path)
  fi

  # reun duplicating STDOUT, send to cache and print at the same time
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

builtin alias f='cached _f_'

for i in bc sed {g,}awk jupyter-nbconvert pygmentize pandoc rst2{xml,s5,odt,html,html5,html4,man,pseudoxml,xetex}{.py,} wn tokei {z,xz}{cat,diff,less} wc tree sort pydoc{3,3.5,3.6,} find fd df du curl ps rg {xz,z,}{p,e,a,f,}grep; do
  builtin eval "builtin alias $i='cached builtin command $i'"
done

# CHANGE OF NUMBER BASIS

to_hex() { builtin printf "%X\n" $1; }
to_oct() { builtin printf "%o\n" $1; }
to_bin() {
  n="$1"
  bit=""
  while [ "$n" -gt 0 ]; do
    bit="$((n & 1))$bit"
    : $((n >>= 1))
  done
  builtin printf "%s\n" "$bit"
}

to_base() {
  builtin command python3 <<EOF
from string import digits, ascii_letters
digs = digits + ascii_letters
def int2base(x, base):
    if x < 0: sign = -1
    elif x == 0: return digs[0]
    else: sign = 1
    x *= sign
    digits = []
    while x:
        digits.append(digs[int(x % base)])
        x = int(x / base)
    if sign < 0: digits.append('-')
    digits.reverse()
    return ''.join(digits)
print(int2base($1, $2))
EOF
}

# STRING MANIPULATION

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

# DATA MANIPULATION

count() {
  builtin command python3 <<EOF
from collections import Counter

counter = Counter('''$(cat /dev/stdin)'''.splitlines())

for key, val in counter.most_common(len(counter)):
    print(val, key)
EOF
}

# MATH

pymath_int_funct() {
  builtin command python3 <<EOF
from math import $1
from shlex import split
from functools import reduce
print(reduce($1, map(int, split('''${*:2}'''))))
EOF
}

for f in gcd; do
  builtin eval "builtin alias $f='pymath_int_funct $f'"
done

py_float_funct() {
  builtin command python3 <<EOF
from shlex import split
from functools import reduce
print(reduce($1, map(float, split('''${*:2}'''))))
EOF
}

for f in max min pow; do
  builtin eval "builtin alias $f='py_float_funct $f'"
done

math-const() {
  builtin command python3 <<EOF
from math import $1
print($1)
EOF
}

for const in pi e tau; do
  builtin eval "builtin alias $const='math-const $const'"
done

# ARITHMETIC

pymath_bop() {
  builtin command python3 <<EOF
from shlex import split
from functools import reduce
print(reduce(lambda x, y: x $1 y, map(float, split('''${*:2}'''))))
EOF
}

builtin alias sum='pymath_bop "+"'
builtin alias difference='pymath_bop "-"'
builtin alias product='pymath_bop "*"'
builtin alias quotient='pymath_bop "/"'

pymath_uop() {
  builtin command python3 <<EOF
from math import $1
print($1($2))
EOF
}

for f in log{2,10} degrees radians sqrt exp trunc; do
  builtin eval "builtin alias $f='pymath_uop $f'"
done

# STATISTICS

pystats_funct() {
  builtin command python3 <<EOF
from statistics import $1
from shlex import split
print($1(map(float, split('''${*:2}'''))))
EOF
}

for f in mode {harmonic_,}mean median{_low,_high,_grouped,} {p,}{stdev,variance}; do
  builtin eval "builtin alias $f='pystats_funct $f'"
done

# CALCULUS

sympy-funct-pretty() {
  builtin command python3 <<EOF
from sympy import *
x, y, z = symbols('x y z')
pprint($1(${*:2}), use_unicode=True)
EOF
}

sympy-uop_list() {
builtin command python3 <<EOF
from sympy import divisors, divisor_count
for n in divisors(24):
    print(n)
EOF
}

for f in binomial_coefficients_list primefactors divisors; do
  eval "alias $f='sympy-uop_list $f'"
done

sympy-funct-ascii() {
  builtin command python3 <<EOF
from sympy import *
x, y, z = symbols('x y z')
print($1(${*:2}))
EOF
}

for f in differentiate integrate; do
  eval "alias $f='sympy-funct-pretty $f'"
done

for f in simplify expand factor trigsimp; do
  eval "alias $f='sympy-funct-ascii $f'"
done

sympy_logic_to_normal_form() {
  builtin command python3 <<EOF
from sympy.abc import x, y, z
from sympy.logic import simplify_logic
print(simplify_logic(${*:2}, form='$1'))
EOF
 }

for i in {d,c}nf; do
  eval "alias to_$i='sympy_logic_to_normal_form $i'"
done

simplify_logic() {
  builtin command python3 <<EOF
from sympy.abc import x, y, z
from sympy.logic import simplify_logic
print(simplify_logic($*))
EOF
 }

satisfiable() {
  builtin command python3 <<EOF
from sympy.abc import x, y, z
from sympy.logic.inference import satisfiable
for k, v in satisfiable($*).items():
    print(k, v)
EOF
}

# COMBINATORICS

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

# TRIGONOMETRY

trig-funct() {
  builtin command python3 <<EOF
from math import $1, radians
print($1(radians($2)))
EOF
}

plot-trig-funct() {
  for degree in {0..360..30}; do
    builtin printf "$1 %3s deg = %-10.2f\n" $degree $($1 $degree)
  done
}

for f in {a,}{sin,cos,tan}{,h}; do
  builtin eval "builtin alias $f='trig-funct $f'"
  builtin eval "builtin alias plot-$f='plot-trig-funct \"trig-funct $f\"'"
done
