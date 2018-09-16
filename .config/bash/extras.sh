function install_packages() {

  function packages() {
    builtin local package_dir="$1"
    builtin local install_cmd="$2"
    builtin local package_mgr="$(builtin echo $install_cmd | builtin command sed -nE 's/^\s*(\w+).*/\1/')"
    for package in ${@:3}; do
      if [[ ! -e "$package_dir/$package" ]]; then
        builtin echo "installing $package_mgr package $package"
        builtin eval "builtin command $install_cmd $package"
      fi
    done
  }

  packages \
    "$HOME/.local/lib/python3.$(builtin command python3 <<<'from sys import version as v; print(v[2:3])')/site-packages" \
    'pip3 --no-color --quiet --retries 2 install --progress-bar off --user --pre' \
    n{etworkx,otebook} y{outube_dl,apf} matplotlib seaborn isort {sym,num,my}py py{stache,gments,lint} p{andas,tpython} jupyter{hub,lab,_console}

  packages \
    ~/.config/yarn/global/node_modules \
    'yarn global add' \
    configurable-http-proxy

  builtin unset -f packages
}

# UTILS

# Cache output of commands.
#
# Usage:
#
#   cached [builtin [command]] <program> [arg, ...]
#
# NOTE: if the program uses STDIN then the cache cannot be used.
function cached() {
  # stringify all args
  builtin local total="${*}"

  # if combined len of args in >= 230
  # don't cache - filenames can only be so long (max is 255 bytes)
  if [[ ${#total} -ge 230 ]]; then
    $*
    builtin return $?

  elif [[ $# -eq 0 ]]; then
    builtin printf "$(tput setaf 1)[ERROR] lack of args - was expecting a program name $(tput sgr0)\n\n"
    builtin printf "Usage:\n\n"
    builtin printf "cached <program> [arg, ...]\n"
    builtin return 1

    # slugify path into file name
  elif [[ $# -eq 1 ]]; then
    builtin local cache_fname="@${PWD}"
    builtin local cache_fname=${cache_fname//\//%}
    builtin local cache_fname=${cache_fname// /_}

    # slugify args into a single file name
  else # > 1 arg
    builtin local total="${total// /_}"
    builtin local total="${total//\\n/-}"
    builtin local total="${total//\\t/%}"
    builtin local total="${total//\'/!}"
    builtin local total="${total//\"/?}"
    builtin local total="${total//\*/&}"
    builtin local total="${total//builtin/}"
    builtin local total="${total//command/}"
    builtin local cache_fname=$total
  fi

  # echo $cache_fname
  # return 0

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
    while builtin read line; do
      builtin echo $line
    done <$cache_path
    builtin return $?
  else
    # prepare to cache
    builtin command mkdir -p $(builtin printf '%s\n' "${cache_path%/*}")
  fi

  # run duplicating STDOUT, send to cache and print at the same time
  $* | builtin command tee $cache_path
  builtin return $?
}

# Look for recently used files matching basename queries
#
# Usage:
#
#   _f_ [basename_query, ...]
#
function _f_() {

  builtin local cmd='builtin command find -atime -2 -not -empty -readable -regextype posix-extended -not -regex ".*(/node_modules/|\\.(mypy_)?cache|\\.git/|\\.cargo/registry/|/.rustup/toolchains/|libreoffice|/site-packages/|google-chrome|\\.vim/(undo|plugged|backup|views|s(essions|wap))).*" -not -regex ".*\\.(b(eam|ack)|log|tmp|/tags|fls|class|(py|s)?o|egg(-info)?|iml|hi|aux)$"'

  if [[ $# -gt 0 ]]; then
    builtin local cmd="$cmd "'\('
    builtin local cmd="$cmd -iname \*${1}\*"
    for pattern in ${*:2}; do
      builtin local cmd="$cmd -or -iname \*${pattern}\*"
    done
    builtin local cmd="$cmd "'\)'
  fi

  builtin eval "$cmd 2>/dev/null"
}

builtin alias f='cached _f_'

# CHANGE OF NUMBER BASIS

# STRING MANIPULATION

# Changes case of input strings.
#
# Usage:
#
#   upper      <string> [string, ...]
#   lower      <string> [string, ...]
#   capitalise <string> [string, ...]
#
function upper() { builtin printf '%s\n' "${*^^}"; }
function lower() { builtin printf '%s\n' "${*,,}"; }
function capitalise() { builtin printf '%s\n' "${*^}"; }

# Concats input strings.
#
# E.g.: Joins words "abc" "def" into "abcdef".
#
# Usage:
#
#   concat
#
# NOTE: expects input from STDIN.
function concat() { substitute '\s+' '' </dev/stdin; }

# Print $1 times string $2.
#
# Usage:
#
#   replicate <natural_number> <string>
#
function replicate() {
  for ((i = 1; i <= $1; ++i)); do
    echo $2
  done
}

# Removes leading and trailing whitespace.
#
# Usage:
#
#   trim
#
# NOTE: expects input from STDIN.
function trim() {
  set -f
  set -- $*
  builtin printf '%s\n' "$(builtin command cat /dev/stdin)"
  set +f
}

# Joins input strings using delimiter $1.
#
# Usage:
#
#   join
#
# NOTE: expects input from STDIN.
function join() {
  builtin command python3 <<EOF
from shlex import split
print('$1'.join(split('''$(builtin command cat /dev/stdin)''')))
EOF
}

# Splits input strings on delimiter $1.
#
# Usage:
#
#   split
#
# NOTE: expects input from STDIN.
function split() {
  builtin command python3 <<EOF
from re import split, M
print('\n'.join(split(r"$1", '''$(builtin command cat /dev/stdin)''', M)))
EOF
}

# Finds all occurrences of pattern $1 in STDIN.
#
# Usage:
#
#   find_regex <pattern>
#
# NOTE: expects input from STDIN.
function find_regex() {
  builtin command python3 <<EOF
from re import finditer
for i in finditer(r'$1', '''$(builtin command cat /dev/stdin)'''):
    print(i.group(0))
EOF
}

# Partials for extracting words.
#
# Usage:
#
#   words
#   words_upper
#   words_lower
#   words_capitalised
#
# NOTE: expects input from STDIN.
builtin alias words='find_regex "\b[a-zA-Z][a-zA-Z0-9]{2,}\b"'
builtin alias wors_upper='find_regex "\b[A-Z][A-Z0-9]{2,}\b"'
builtin alias words_lower='find_regex "\b[a-z][a-z0-9]{2,}\b"'
builtin alias words_capitalised='find_regex "\b[A-Z][a-z0-9]{2,}\b"'

# Replaces all occurrences of pattern $1 with string $2 from STDIN.
#
# Usage:
#
#   find_regex <pattern> <replacement_string>
#
# NOTE: expects input from STDIN.
function substitute() {
  builtin command python3 <<EOF
from re import sub
print(sub(r'$1', '$2', '''$(builtin command cat /dev/stdin)'''))
EOF
}

# Iterates over all substrings of a given string $1.
#
# Usage:
#
#   substrings <string>
#
# NOTE: this does get very slow very quick.
function substrings() {
  builtin command python3 <<EOF
from functools import lru_cache
@lru_cache(maxsize=64)
def substr(xs): return set() if len(xs) == 0 else {xs} | substr(xs[:-1]) | substr(xs[1:])
for i in substr("""$*"""):
    print(i)
EOF
}

# DATA MANIPULATION

function fold() {
  builtin local result=$($1 $2 $3)
  for n in ${@:4}; do
    builtin local result="$($1 $n $result)"
  done
  builtin echo $result
}

# Give the first $1 terms of the fibbonacci sequence.
#
# Usage:
#
#   fibbonacci <natural_number>
#
function fibbonacci() {
  builtin command python3 <<EOF
def fibbonacci(n):
    i = 0; j = 1
    while n > 0:
        n -= 1; yield i; save_i = i; i = j; j += save_i
    return None

for i in fibbonacci($1): print(i)
EOF
}

# Groups all input tokens from STDIN by frequency of occurrence.
#
# Usage:
#
#   count
#
# NOTE: expects input from STDIN.
function count() {
  builtin command python3 <<EOF
from shlex import split
from collections import Counter

data = Counter(split('''$(builtin command cat /dev/stdin)'''))

for key, val in data.most_common(len(data)):
    print(val, key)
EOF
}

# Evaluate string $1 using every token from STDIN as argument.
#
# Usage:
#
#   map <string_cmd>
#
# NOTE: expects input from STDIN.
function map() {
  for i in $(builtin command cat /dev/stdin); do
    builtin eval "$1 $i"
  done
}

# Evaluate string $1 for every token from STDIN as argument.
#
# Usage:
#
#   foreach <string_cmd>
#
# NOTE: expects input from STDIN.
function foreach() {
  for i in $(builtin command cat /dev/stdin); do
    builtin eval "$1"
  done
}

# MATH

function pymath_int_funct() {
  builtin command python3 <<EOF
from math import $1
from shlex import split
from functools import reduce
from math import *
print(reduce($1, map(int, split('''${*:2}'''))))
EOF
}

# Greatest common divisor for input args $@.
#
# Usage:
#
#   gcd <integer> [integer, ...]
#
# NOTE: expects input from STDIN.
# builtin alias gcd='pymath_int_funct gcd'

function gcd() {
  builtin local result=$(__gcd $1 $2)
  for n in ${@:3}; do
    builtin local result="$(gcd $n $result)"
  done
  builtin echo $result
}

function __gcd() {
  if [[ $2 -eq 0 ]]; then
    builtin echo $1
  else
    builtin echo "$(__gcd $2 $(($1 % $2)))"
  fi
}

function pymath_float_funct() {
  builtin command python3 <<EOF
from shlex import split
from math import *
from functools import reduce
print(reduce($1, map(float, split('''${*:2}'''))))
EOF
}

function pymath_bop() {
  builtin command python3 <<EOF
from shlex import split
from operator import $1
from functools import reduce
print(reduce(lambda x, y: $1(x, y), map(float, split('''${*:2}'''))))
EOF
}

# Math operations on 2+ operands.
#
# Usage:
#
#   remainder  <int1> <int2> [integer, ...]
#   quotient   <num1> <num2> [integer, ...]
#   product    <num1> <num2> [integer, ...]
#   difference <num1> <num2> [integer, ...]
#   sum        <num1> <num2> [integer, ...]
#   max        <num1> <num2> [integer, ...]
#   min        <num1> <num2> [integer, ...]
#   pow        <base> <exponent>
#
for f in max min pow; do
  builtin eval "builtin alias $f='pymath_float_funct $f'"
done
builtin alias sum='pymath_bop add'
builtin alias remainder='pymath_bop mod'
builtin alias difference='pymath_bop sub'
builtin alias product='pymath_bop mul'
builtin alias quotient='pymath_bop truediv'

function series() {
  builtin local size=${2:-100}
  builtin local dtype=${3:-uint32}
  builtin command python3 <<EOF
from math import *
for n in map(lambda x: $1, range(1, $size + 1)): print(n)
EOF
}

function finite_difference() {
  builtin local order=${1:-1}
  builtin command python3 <<EOF
import numpy as np
from shlex import split
from functools import reduce

def compose(*xs): return reduce(lambda f1, f2: lambda x: f1(f2(x)), xs)

array = compose(
    lambda array: np.diff(array, ${order}),
    np.array,
    list,
    lambda n: map(float, n),
    split)('''$(builtin command cat /dev/stdin)''')

for n in array: print(n)
EOF

}

function math_const() {
  builtin command python3 <<EOF
from math import $1
print($1)
EOF
}

# Math constants.
#
# Usage:
#
#   e
#   pi
#   tau
#
for const in pi e tau; do
  builtin eval "builtin alias $const='math_const $const'"
done

function pymath_uop() {
  builtin command python3 <<EOF
from math import *
print($1($2))
EOF
}

# Generic logarithm of $1 to the base of $2.
#
# Usage:
#
#   log <number> <base>
#
function log() { pymath_uop log "$1, $2"; }

# Math operations on a single operand (i.e. unary functions).
#
# Usage:
#
#   log2    <number>
#   log10   <number>
#   degrees <number>
#   radians <number>
#   sqrt    <number>
#   trunc   <number>
#
for f in log{2,10} degrees radians sqrt exp trunc; do
  builtin eval "builtin alias $f='pymath_uop $f'"
done

# STATISTICS

function pystats_funct() {
  builtin command python3 <<EOF
from statistics import $1
from math import *
from shlex import split
print($1(map(float, split('''$(builtin command cat /dev/stdin)'''))))
EOF
}

# Statistical functions on many data points.
#
# Usage:
#
#   harmonic_mean
#   mean
#   median
#   median_grouped
#   median_high
#   median_low
#   mode
#   pstdev
#   pvariance
#   stdev
#   variance
#
# NOTE: expects input from STDIN.
for f in mode {harmonic_,}mean median{_low,_high,_grouped,} {p,}{stdev,variance}; do
  builtin eval "builtin alias $f='pystats_funct $f'"
done

# CALCULUS

# Solve a single-variable polynomial equation using Netwon's Method.
#
# Usage:
#
#   solve_polynomial <function_string>
#
# NOTE: expects the independent variable to be called 'x'.
function solve_polynomial() {
  builtin command python3 <<EOF
from math import *
from time import perf_counter

def solve(f, f_, guess = 1.0, timeout = 1.5, verbose = False):
    start = perf_counter()
    while (perf_counter() - start) < timeout:
        new = guess - f(guess) / f_(guess)
        if verbose: print(guess)
        if guess == new: return guess, perf_counter() - start
        else: guess = new
    return None

# try 3 times by tweaking the guess value
# also flip the sign
def run_it(tries = 3, guess = 1.0):
    result = solve(lambda x: $1, lambda x: $(sympy_funct_ascii diff $1), guess)
    if result:
        (n, took) = result
        print("%s" % "x = %.2f" % n)
        print("%s" % "took %.8f seconds" % took)
      elif tries == 0: print("no solution")
      else: run_it(tries - 1, -(guess + guess * 0.01))

run_it()
EOF
}

function sympy_uop_list() {
  builtin command python3 <<EOF
from sympy import $1
for n in $1(${*:2}):
    print(n)
EOF
}

# Number-theoretic utils.
#
# Usage:
#
#   binomial_coefficients_list
#   primefactors
#   divisors
#
for f in binomial_coefficients_list primefactors divisors; do
  eval "alias $f='sympy_uop_list $f'"
done

function sympy_funct_pretty() {
  builtin command python3 <<EOF
from sympy import *
x, y, z = symbols('x y z')
from math import pi, e, tau
pprint($1(${*:2}), use_unicode=True)
EOF
}

function sympy_funct_ascii() {
  builtin command python3 <<EOF
from sympy import *
x, y, z = symbols('x y z')
from math import pi, e, tau
print($1(${*:2}))
EOF
}

# Symbolic manipulation.
#
# Usage:
#
#   differentiate <formula_string>
#   integrate     <formula_string>
#   simplify      <formula_string>
#   expand        <formula_string>
#   factor 			  <formula_string>
#   trigsimp 		  <formula_string>
#
# Unicode versions of the above:
#
#   differentiate_pretty <formula_string>
#   integrate_pretty     <formula_string>
#   simplify_pretty      <formula_string>
#   expand_pretty        <formula_string>
#   factor_pretty 			 <formula_string>
#   trigsimp_pretty 		 <formula_string>
#
# NOTE: unicode versions cannot be fed as input into another function.
#
# NOTE: name clashes with diff(1) (hence separate alias)
alias differentiate='sympy_funct_ascii diff'
alias differentiate_pretty='sympy_funct_pretty diff'

for f in integrate simplify expand factor trigsimp integrate; do
  eval "alias ${f}='sympy_funct_ascii $f'"
  eval "alias ${f}_pretty='sympy_funct_pretty $f'"
done

function indefinite_integral() {
  builtin shopt -s expand_aliases
  builtin local F="lambda x: $(integrate $1)"
  builtin local a=$2
  builtin local b=$3
  builtin command python3 <<EOF
from math import *
print(($F)($b) - ($F)($a))
EOF
  builtin shopt -u expand_aliases
}

function sympy_logic_to_normal_form() {
  builtin command python3 <<EOF
from sympy.abc import x, y, z
from sympy.logic import simplify_logic
print(simplify_logic(${*:2}, form='$1'))
EOF
}

# Convert a logical formula to normal form.
#
# Usage:
#
#   to_dnf <formula_string>
#   to_cnf <formula_string>
#
for i in {d,c}nf; do
  eval "alias to_$i='sympy_logic_to_normal_form $i'"
done

# Simplify a logical formula.
#
# Usage:
#
#   simplify_logic <formula_string>
#
function simplify_logic() {
  builtin command python3 <<EOF
from sympy.abc import x, y, z
from sympy.logic import simplify_logic
print(simplify_logic($*))
EOF
}

# Checks whether the formula has an assignment of
# True or False to variables x, y, z that would make it True.
#
# Usage:
#
#   satisfiable <formula_string>
#
function satisfiable() {
  builtin command python3 <<EOF
from sympy.abc import x, y, z
from sympy.logic.inference import satisfiable
for k, v in satisfiable($*).items():
    print(k, v)
EOF
}

# COMBINATORICS

# Generate permutations of length $1 from set of items ${@:2}.
#
# Usage:
#
#   permutations <natural_number> <item> [item, ...]
#
function permutations() {
  builtin command python3 <<EOF
from itertools import permutations
from shlex import split
for i in permutations(split('''${*:2}'''), $1):
    print(' '.join(i))
EOF
}

# Generate combinations of length $1 from set of items ${@:2}.
#
# Usage:
#
#   combinations <natural_number> <item> [item, ...]
#
function combinations() {
  builtin command python3 <<EOF
from itertools import combinations
from shlex import split
for i in combinations(split('''${*:2}'''), $1):
    print(' '.join(i))
EOF
}

# TRIGONOMETRY

function trig_funct() {
  builtin command python3 <<EOF
from math import $1, radians
from math import *
print($1(radians($2)))
EOF
}

function plot_trig_funct() {
  for degree in {0..360..30}; do
    builtin printf "$1 %3s deg = %-10.2f\n" $degree $($1 $degree)
  done
}

# Trigonometric functions.
#
# Usage:
#
#   sin <degrees>
#   cos <degrees>
#   tan <degrees>
#
#   asin <sin_of_angle>
#   acos <cos_of_angle>
#   atan <tan_of_angle>
#
#   plot_sin
#   plot_cos
#   plot_tan
#
#   plot_asin
#   plot_acos
#   plot_atan
#
for f in {a,}{sin,cos,tan}; do
  builtin eval "builtin alias $f='trig_funct $f'"
  builtin eval "builtin alias plot_${f}='plot_trig_funct \"trig_funct $f\"'"
done

# init cache for commands that don't change their output (too often)
for i in wn tokei {z,xz}diff pydoc{3,3.5,3.6,} fd d{u,f} curl ps; do
  builtin eval "builtin alias $i='cached command $i'"
done

for i in substrings fibbonacci sympy_uop_list pymath_{b,u}op pymath_{int,float}_funct to_{hex,oct,bin,base} satisfiable {permut,combin}ations; do
  builtin eval "builtin alias $i='cached $i'"
done

function to_hex() { builtin printf "%X\n" $1; }
function to_oct() { builtin printf "%o\n" $1; }
function to_bin() {
  n="$1"
  bit=""
  while [ "$n" -gt 0 ]; do
    bit="$((n & 1))$bit"
    : $((n >>= 1))
  done
  builtin printf "%s\n" "$bit"
}

# Converts to a base (hexadecimal, octal, binary, any other - see python code below).
#
# Usage:
#
#   to_hex  <number>
#   to_bin  <number>
#   to_oct  <number>
#   to_base <number> <base>
#
function to_base() {
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
