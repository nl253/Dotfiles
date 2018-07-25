#!/usr/bin/env bash

# Dependencies
# ---------------------------
# - GNU coreutils
# - 7z
# - perl
# - pygmentize
# - tar
# - elinks
# - exiftool
# - unrar
# - img2txt
# - jupyter (python package)
# - atool
# - bsdtar
# - transmission
# - odt2txt
# - pdftotext
# - astyle or clang
# - gofmt (part of golang)
# - js-beautify (includes css-beautify and html-beautify)
# - rst2html5.py (ships with the docutils python package)
# - pandoc
# - javap (part of jdk)
# - tree

# If the option `use_preview_script` is set to `true`,
# then this script will be called and its output will be displayed in ranger.
# ANSI color codes are supported.
# STDIN is disabled, so interactive scripts won't work properly

# Meanings of exit codes:
# code | meaning    | action of ranger
# -----+------------+-------------------------------------------
# 0    | success    | Display stdout as preview
# 1    | no preview | Display no preview at all
# 2    | plain text | Display the plain content of the file
# 3    | fix width  | Don't reload when width changes
# 4    | fix height | Don't reload when height changes
# 5    | fix both   | Don't ever reload
# 6    | image      | Display the image  "${IMAGE_CACHE_PATH}"  points to as an image preview
# 7    | image      | Display the file directly as an image

# Script arguments
FILE_PATH="${1}"        # Full path of the highlighted file
PV_WIDTH="${2}"         # Width of the preview pane (number of fitting characters)
PV_HEIGHT="${3}"        # Height of the preview pane (number of fitting characters)
IMAGE_CACHE_PATH="${4}" # Full path that should be used to cache image preview
PV_IMAGE_ENABLED="${5}" # 'True' if image previews are enabled, 'False' otherwise.
FILE_NAME=$(basename "$FILE_PATH")
EXTENSION="${FILE_NAME#*.}"

head() {
  if command head -n $PV_HEIGHT "$FILE_PATH"; then
    return 0
  else 
    return 1
  fi
}

pandoc() {
  if command pandoc --self-contained --html-q-tags --ascii --mathml --columns=${PV_WIDTH} --highlight-style=breezedark -t html5 $@; then
    return 0
  else 
    return 1
  fi
}

elinks() {
  command elinks -no-references -no-numbering -dump -dump-color-mode 4 -dump-width ${PV_WIDTH} </dev/stdin
}

preview-stdout() {
  if command source-highlight -s $1 </dev/stdin | elinks; then
    return 0
  else 
    return 1
  fi
}

preview() {
  if head | preview-stdout $1; then
    return 0 
  elif head; then
    return 0
  else
    return 1
  fi
}

report() {
  cat <<EOF
$(ls -l "$FILE_PATH")

MIME:            $(command file --dereference --brief --mime-type -- "$FILE_PATH")

EOF
  exit 5
}

## $1 from language
## this function always reads from stdin if $1 arg is set
pandoc_preview() {
  if [[ -n $1 ]] && pandoc -f $1 </dev/stdin | elinks; then
    return 0
  elif pandoc "$FILE_PATH" | elinks; then
    return 0
  else
    return 1
  fi
}

preview_img() {
  if [[ -x $(command which img2txt 2>/dev/null) ]]; then
    img2txt --gamma=0.6 --width="$PV_WIDTH" -- "$FILE_PATH"
  fi
}

## $1 c | cpp | java
preview_cfamily() {
  # local filetype=$(command pygmentize -N "$FILE_PATH")
  if [[ -x $(command which astyle 2>/dev/null) ]]; then
    command astyle \
      --max-code-length="$PV_WIDTH" \
      --style=google \
      --remove-braces \
      --indent=spaces \
      --pad-comma \
      --pad-oper \
      --pad-header \
      --mode="$1" <"$FILE_PATH" | preview-stdout $1
  elif [[ -x $(command which clang 2>/dev/null) ]]; then
    command clang-format -style=Google <"$FILE_PATH" | preview-stdout $1
  else
    preview $1
  fi
}

guess_shebang() {
  # guess from shebang
  if [[ $(eval "command head -n 1 $FILE_PATH") =~ '#!' ]]; then
    local executable=$(command head -n 1 "$FILE_PATH" | grep -Eo '\w+$')
    preview $executable
    exit 5
  fi
  ((RANGER_LEVEL < 1)) && exit 2 || preview
}

preview_pdf() {
  command pdftotext -nopgbrk -f 1 -l 3 "$FILE_PATH" -
  exit 5
}

handle_extension() {

  case "$EXTENSION" in

    java | cpp | c)
      if preview_cfamily $EXTENSION; then
        exit 0
      else
        exit 2
      fi
      ;;

    h | hpp | cc | gv | dot)
      if preview_cfamily c; then 
        exit 0
      else
        exit 2
      fi
      ;;

    *html)
      if elinks <"$FILE_PATH"; then 
        exit 0
      else
        exit 2
      fi
      ;;

    json)
      if jq -C < "$FILE_PATH"; then
        exit 0
      elif js-beautify < "$FILE_PATH" | preview-stdout json; then 
        exit 0
      else
        exit 2
      fi
      ;;

    md | m*down | Rmd)
      if head | pandoc_preview markdown+line_blocks+gfm_auto_identifiers+compact_definition_lists+fancy_lists+all_symbols_escapable+superscript+subscript+implicit_figures+footnotes+four_space_rule-emoji; then 
        exit 0
      else
        exit 2
      fi
      ;;

    ipynb)
      if command jupyter nbconvert --stdout --to html "$FILE_PATH" | elinks; then
        exit 0
      elif head | js-beautify; then
        exit 0
      else
        exit 2
      fi
      ;;

    csv)
      if head | column --separator ',' --table; then
        exit 0 
      elif preview; then 
        exit 0
      else 
        exit 2
      fi
      ;;

    js | ts)
      if head | js-beautify; then
        exit 0
      else 
        exit 2
      fi
      ;;

    sh | .bash* | .zsh* | .profile)
      if shfmt -s -i 2 -ci <"$FILE_PATH" | preview-stdout shell; then
        exit 0
      else 
        exit 2
      fi
      ;;

    org)
      if head | pandoc_preview org; then
        exit 0
      else 
        exit 2
      fi
      ;;

    css)
      if head | css-beautify | preview-stdout css; then 
        exit 0
      else 
        exit 2
      fi
      ;;

    xml)
      if head | html-beautify | preview-stdout xml; then 
        exit 0
      else 
        exit 2
      fi
      ;;

    pdf)
      if preview_pdf; then
        exit 0
      else 
        exit 1
      fi
      ;;

    puml | dot)
      if preview java; then
        exit 0
      else 
        exit 2
      fi
      ;;

    go)
      if ([[ -x $(command which gofmt 2>/dev/null) ]] && command gofmt -s || cat) <"$FILE_PATH" | preview-stdout go; then 
        exit 0
      else
        exit 2
      fi
      ;;

    hs)
      if ([[ -x $(command which hindent 2>/dev/null) ]] && command hindent --line-length $PV_WIDTH --indent-size 4 --sort-imports || cat) <"$FILE_PATH" | preview-stdout haskell; then
        exit 0
      else
        exit 2
      fi
      ;;

    rst)
      if head | pandoc_preview rst; then
        exit 0
      else
        exit 2
      fi
      ;;

    docx)
      if pandoc_preview docx <"$FILE_PATH"; then
        exit 0
      else 
        exit 1
      fi
      ;;

    tex | lhs)
      if pandoc_preview latex <"$FILE_PATH"; then
        exit 0
      else
        exit 2
      fi
      ;;

    conf | cnf | cfg | toml | desktop)
      if preview ini; then 
        exit 0
      else
        exit 2
      fi
      ;;

    ?.gz | 1)
      if MANWIDTH=$PV_WIDTH man -- "$FILE_PATH"; then
        exit 0
      else
        exit 1
      fi
      ;;

    *)
      if [[ $EXTENSION == '' ]]; then
        return 0
      elif [[ $(source-highlight --lang-list | grep -Eo '^\w+') =~ $EXTENSION ]] && preview "$EXTENSION"; then
        exit 0 
      else
        return 1
      fi
      ;;

  esac
}

handle_mime() {

  case $(command file --dereference --brief --mime-type -- "$FILE_PATH") in

    # Text files
    text/* | application/*xml | application/*script)

      # check if no extension
      if [[ $EXTENSION == $FILE_NAME ]]; then
        # if so look at the first line for shebang
        guess_shebang && exit 0
      else
        exit 2
      fi
      ((RANGER_LEVEL < 2)) && exit 2 || preview
      ;;

    inode/directory)
      command tree -l -a --prune -L 4 -F --sort=mtime "$FILE_PATH" && exit 0
      command ls --color=always "$FILE_PATH" -pNsh1goG && exit 0
      exit 1
      ;;

    # Image
    image/*)
      if ((RANGER_LEVEL < 1)); then
        preview_img
      fi
      exit 7
      ;;
  esac
}

handle_archive() {

  # echo -e "archive preview\n________________\n"

  case "$EXTENSION" in

    zip | gz | tar | jar | 7z | bz2)
      # Avoid password prompt by providing empty password
      command 7z l -p -- "$FILE_PATH" && exit 5
      exit 1
      ;;

    bz | lz | xz)
      command atool --list -- "$FILE_PATH" && exit 5
      command bsdtar --list --file "$FILE_PATH" && exit 5
      exit 1
      ;;

    tar.gz)
      command tar ztf "$FILE_PATH" && exit 5
      exit 1
      ;;

    tar.xz)
      command tar Jtf "$FILE_PATH" && exit 5
      exit 1
      ;;

    tar.bz2)
      command tar jtf "$FILE_PATH" && exit 5
      exit 1
      ;;

    rar)
      [[ -x $(command unrar 7z 2>/dev/null) ]] && command unrar lt -p- -- "${FILE_PATH}" && exit 5
      exit 1
      ;;

  esac
}

handle_fallback() {

  cat <<EOF

$(report)

======================================================================

$(exiftool "$FILE_PATH")

======================================================================

$(command file --dereference --brief -- "$FILE_PATH" | $fmt)

EOF
  exit 5
}

handle_extension
handle_mime
handle_archive
handle_fallback

exit 1
# vim: nowrap
