#!/usr/bin/env bash

# Dependencies
# ------------
# * Very likely to have: cat column grep head ls OR tree man tar
# * Might need to install:
# ** Distro package manager (yum, pacman, apt etc.): 7z atool OR bsdtar elinks exiftool file jq pandoc highlight unrar pdftotext
# ** NPM / Yarn: js-beautify jupyter
#
# TO CHECK DEPS RUN IN BASH: for i in $(builtin command grep -Eo 'command builtin \w+' < $THIS_DIR/scope.sh | builtin command grep -Eo '\w+$'); do [[ ! -x $(builtin command type -P $i) ]] && echo "$i not installed"; done
#
# - If the option `use_preview_script` is set to `true`,
#   then this script will be called and its output will be displayed in ranger.
# - ANSI color codes are supported.
# - STDIN is disabled, so interactive scripts won't work properly.
#
# code | meaning    | action
# -----+------------+--------------------------------------
# 0    | success    | Display stdout as preview
# 1    | no preview | Display no preview at all
# 2    | plain text | Display the plain content of the file
# 3    | fix width  | Don't reload when width changes
# 4    | fix height | Don't reload when height changes
# 5    | fix both   | Don't ever reload
# 6    | image      | Display the image  "${IMAGE_CACHE_PATH}"  points to as an image preview
# 7    | image      | Display the file directly as an image
# ---------------------------------------------------------
FILE_PATH="${1}"
# Width of the preview pane (number of fitting characters)
PV_WIDTH="${2}"
# Height of the preview pane (number of fitting characters)
PV_HEIGHT="${3}"
# Full path that should be used to cache image preview
IMAGE_CACHE_PATH="${4}"
# 'True' if image previews are enabled, 'False' otherwise.
PV_IMAGE_ENABLED="${5}"
FILE_NAME=$(basename "$FILE_PATH")
FULL_EXTENSION="${FILE_NAME#*.}"
EXTENSION="${FILE_NAME##*.}"

hour=$(command bash -c "builtin printf '%(%H)T'")

if [[ $hour =~ ^0 ]]; then
  hour=${hour:1}
fi

if [[ $hour -lt 7 ]] || [[ $hour -ge 15 ]]; then
  COLORS=molokai
else
  COLORS=github
fi

unset -v hour

head() { builtin command head -n $PV_HEIGHT $FILE_PATH; }

elinks() { builtin command elinks -no-references -no-numbering -dump -dump-width ${PV_WIDTH} </dev/stdin; }

preview_stdin() {
  if builtin command highlight --kw-case=upper --line-range=1-${PV_HEIGHT} --style=${COLORS} --stdout --out-format=xterm256 --line-length=${PV_WIDTH} --replace-tabs=4 --line-numbers --syntax=${1} </dev/stdin 2>/dev/null; then
    builtin return 0
  elif builtin command cat </dev/stdin; then
    builtin return 0
  else
    builtin return 1
  fi
}

preview() {
  if head | preview_stdin $1; then
    builtin return 0
  elif head; then
    builtin return 0
  else
    builtin return 1
  fi
}

## $1 from language
## this function always reads from stdin if $1 arg is set
pandoc() {
  if builtin command pandoc --self-contained --html-q-tags --ascii --mathml --columns=${PV_WIDTH} --highlight-style=breezedark -t html5 -f $@ </dev/stdin | elinks; then
    builtin return 0
  else
    builtin return 1
  fi
}

from_shebang() {
  builtin local fst_ln=$(builtin command head -n 1 "$FILE_PATH")

  if [[ $fst_ln =~ ^#! ]]; then
    # regex handles: #!/usr/bin/env  <prog> [-] flag*
    #                #!/usr/sbin/env <prog> [-] flag*
    #                #!/bin/env      <prog> [-] flag*
    #                #!/bin/<prog>          [-] flag*
    #                #!/sbin/env     <prog> [-] flag*
    #                #!/sbin/<prog>         [-] flag*
    #                #!/usr/bin/<prog>      [-] flag*
    #                #!/usr/sbin/<prog>     [-] flag*
    #                #!<prog>               [-] flag*
    builtin local prog=$(builtin echo $fst_ln | builtin command sed -E 's/^#!\s*(\/(usr\/)?s?bin\/(env\s+)?)?([0-9a-zA-Z][-\.0-9a-zA-Z_]{2,})(\s+-\S*)*\s*$/\4/')
    if preview $prog 2>/dev/null; then
      builtin return 0
    fi
  fi

  builtin return 1
}

case "$EXTENSION" in

  md | m*down | Rmd)
    if head | pandoc markdown+line_blocks+gfm_auto_identifiers+compact_definition_lists+fancy_lists+all_symbols_escapable+superscript+subscript+implicit_figures+footnotes+four_space_rule-emoji 2>/dev/null; then
      builtin exit 0
    elif preview markdown 2>/dev/null; then
      builtin exit 0
    else
      builtin exit 2
    fi
    ;;


  *html | hbs)
    if elinks <"$FILE_PATH" 2>/dev/null; then
      builtin exit 0
    elif preview html 2>/dev/null; then
      builtin exit 0
    else
      builtin exit 2
    fi
    ;;

  json)
    if builtin command jq -C <"$FILE_PATH" 2>/dev/null; then
      builtin exit 0
    elif preview json 2>/dev/null; then
      builtin exit 0
    else
      builtin exit 2
    fi
    ;;

  pdf)
    if builtin command pdftotext -nopgbrk -f 1 -l 3 "$FILE_PATH" - | builtin command grep -Eo '^.{4,}$'; then
      builtin exit 0
    elif builtin command pdftotext -nopgbrk -f 1 -l 3 "$FILE_PATH" -; then
      builtin exit 0
    else
      builtin exit 1
    fi
    ;;

  sh | bash* | zsh*)
    if shfmt -s -i 2 -ci <"$FILE_PATH" | preview_stdin sh 2>/dev/null; then
      builtin exit 0
    elif preview sh 2>/dev/null; then
      builtin exit 0
    else
      builtin exit 2
    fi
    ;;

  xml)
    if head | builtin command html-beautify | preview_stdin xml 2>/dev/null; then
      builtin exit 0
    elif head | preview_stdin xml 2>/dev/null; then
      builtin exit 0
    elif preview xml 2>/dev/null; then
      builtin exit 0
    else
      builtin exit 2
    fi
    ;;

  csv)
    if head | builtin command column --separator ',' --table 2>/dev/null; then
      builtin exit 0
    elif preview 2>/dev/null; then
      builtin exit 0
    else
      builtin exit 2
    fi
    ;;

  conf | cnf | cfg | toml | desktop)
    if preview ini 2>/dev/null; then
      builtin exit 0
    else
      builtin exit 2
    fi
    ;;

  h | hpp | c)
    if preview c 2>/dev/null; then
      builtin exit 0
    else
      builtin exit 2
    fi
    ;;

  rs)
    if preview_stdin rust <"$FILE_PATH" 2>/dev/null; then
      builtin exit 0
    elif preview rust 2>/dev/null; then
      builtin exit 0
    else
      builtin exit 2
    fi
    ;;

  rst | txt)
    if head | pandoc rst 2>/dev/null; then
      builtin exit 0
    elif preview rst 2>/dev/null; then
      builtin exit 0
    else
      builtin exit 2
    fi
    ;;

  org)
    if head | pandoc org 2>/dev/null; then
      builtin exit 0
    elif preview org 2>/dev/null; then
      builtin exit 0
    else
      builtin exit 2
    fi
    ;;

  ipynb)
    if builtin command jupyter nbconvert --stdout --to html "$FILE_PATH" | elinks 2>/dev/null; then
      builtin exit 0
    elif head | builtin command js-beautify | preview_stdin json 2>/dev/null; then
      builtin exit 0
    elif head | builtin command js-beautify 2>/dev/null; then
      builtin exit 0
    else
      builtin exit 2
    fi
    ;;

  docx)
    if pandoc docx <"$FILE_PATH" 2>/dev/null; then
      builtin exit 0
    else
      builtin exit 1
    fi
    ;;

  tex | lhs)
    if pandoc latex <"$FILE_PATH" 2>/dev/null; then
      builtin exit 0
    elif preview tex 2>/dev/null; then
      builtin exit 0
    else
      builtin exit 2
    fi
    ;;

  1)
    if MANWIDTH=$PV_WIDTH builtin command man -- "$FILE_PATH" 2>/dev/null; then
      builtin exit 0
    else
      builtin exit 1
    fi
    ;;

  scm | rkt* | ss | ls*p | [ce]l)
    if preview lisp 2>/dev/null; then
      builtin exit 0
    else
      builtin exit 2
    fi
    ;;

  puml | dot | gv)
    if preview java 2>/dev/null; then
      builtin exit 0
    else
      builtin exit 2
    fi
    ;;

  tsv)
    if head | builtin command column --separator "\t" --table 2>/dev/null; then
      builtin exit 0
    elif preview 2>/dev/null; then
      builtin exit 0
    else
      builtin exit 2
    fi
    ;;
esac

case "$FULL_EXTENSION" in

  tar.gz)
    if builtin command tar ztf "$FILE_PATH" 2>/dev/null; then
      builtin exit 5
    else
      builtin exit 1
    fi
    ;;

  tar.xz)
    if builtin command tar Jtf "$FILE_PATH" 2>/dev/null; then
      builtin exit 5
    else
      builtin exit 1
    fi
    ;;

  tar.bz2)
    if builtin command tar jtf "$FILE_PATH" 2>/dev/null; then
      builtin exit 5
    else
      builtin exit 1
    fi
    ;;

  zip | gz | tar | jar | 7z | bz2)
    # Avoid password prompt by providing empty password
    if builtin command 7z l -p -- "$FILE_PATH" 2>/dev/null; then
      builtin exit 5
    else
      builtin exit 1
    fi
    ;;

  [blx]z)
    if builtin command atool --list -- "$FILE_PATH" 2>/dev/null; then
      builtin exit 5
    elif builtin command bsdtar --list --file "$FILE_PATH" 2>/dev/null; then
      builtin exit 5
    else
      builtin exit 1
    fi
    ;;

  rar)
    if builtin command unrar lt -p- -- "${FILE_PATH}" 2>/dev/null; then
      builtin exit 5
    else
      builtin exit 1
    fi
    ;;

esac

case $(builtin command file --dereference --brief --mime-type -- "$FILE_PATH") in

  # Text files
  text/* | application/*xml | application/*script)
    # check if no extension
    # if so look at the first line for shebang
    if [[ "$FULL_EXTENSION" == "$FILE_NAME" ]] && from_shebang 2>/dev/null; then
      builtin exit 0
    elif preview_stdin "$EXTENSION" <"$FILE_PATH" 2>/dev/null; then
      builtin exit 0
    else
      builtin exit 2
    fi
    ;;

  inode/directory)
    if builtin command tree -l -a --prune -L 4 -F --sort=mtime "$FILE_PATH" 2>/dev/null; then
      builtin exit 0
    elif builtin command ls --color=always -pNsh1goG "$FILE_PATH" 2>/dev/null; then
      builtin exit 0
    else
      builtin exit 1
    fi
    ;;

  # Image
  image/*)
    builtin exit 7
    ;;
esac

builtin command exiftool "$FILE_PATH" 2>/dev/null
builtin command file --dereference --brief -- "$FILE_PATH" 2>/dev/null
builtin exit 5
# vim:foldmethod=indent:
