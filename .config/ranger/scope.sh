#!/usr/bin/env bash

# set -o noclobber -o noglob -o nounset -o pipefail
# IFS=$'\n'

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

# This script is considered a configuration file and must be updated manually.
# It will be left untouched if you upgrade ranger.

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
EXTENSION="${FILE_PATH#*.}"
FILE_NAME=$(basename "${FILE_PATH}")
head="command head -n ${PV_HEIGHT} --"

# Essential
[[ -x $(command which elinks 2>/dev/null) ]] && HAS_ELINKS=1 && export browser="command elinks -no-references -no-numbering -dump -dump-color-mode 4 -dump-width ${PV_WIDTH}" || HAS_ELINKS=0
[[ -x $(command which js-beautify 2>/dev/null) ]] && HAS_JSBEAUTIFY=1 || HAS_JSBEAUTIFY=0
[[ -x $(command which pandoc 2>/dev/null) ]] && HAS_PANDOC=1 && export pandoc="command pandoc --self-contained -t html5" || HAS_PANDOC=0
[[ -x $(command which highlight 2>/dev/null) ]] && HAS_HIGHLIGHT=1 || HAS_PYGMENTS=0

# Settings
if [[ -x $(command which highlight) ]]; then
  HAS_HIGHLIGHT=1
  export highlight="command highlight --stdout --reformat=google --out-format=xterm256"
else
  HAS_HIGHLIGHT=0
fi

if [[ -x $(command which pygmentize 2>/dev/null) ]]; then
  HAS_PYGMENTS=1
  HIGHLIGHT_SIZE_MAX=262143 # 256KiB
  HIGHLIGHT_TABWIDTH=8
  HIGHLIGHT_STYLE=pablo
  PYGMENTIZE_STYLE=autumn
  [[ "$(tput colors)" -ge 256 ]] && PYGMENTIZE_FORMAT=terminal256 || PYGMENTIZE_FORMAT=terminal
  export pygmentize="command pygmentize -O style=default,bg=dark,lineos=1 -F gobble,highlight,codetagify -f ${PYGMENTIZE_FORMAT}"
else
  HAS_PYGMENTS=0
fi

colorize_stdout() {

	# if language not set, guess using pygments or use extension
	if [[ -v 1 ]]; then
	  local language=$1
	else
		if ((HAS_PYGMENTS)); then
			local language=$(pygmentize -N "${FILE_PATH}")
		else
			local language=$EXTENSION
		fi
	fi

  if ((HAS_HIGHLIGHT)); then
    $highlight -S $language < /dev/stdin
  elif ((HAS_PYGMENTS)); then
    $pygmentize -l $language < /dev/stdin
  else
    cat < /dev/stdin
  fi
	exit 5
}

colorize_head() {
	if [[ -v 1 ]]; then
		$head "${FILE_PATH}" | colorize_stdout $1
	else
		$head "${FILE_PATH}" | colorize_stdout
	fi
}

# most generic, no syntax highlighting
preview() {
  $head "${FILE_PATH}" && exit 5 || exit 1
}

preview_go() {
  ([[ -x $(command which gofmt 2>/dev/null) ]] && command gofmt -s || cat) < "$FILE_PATH" | colorize_stdout go && exit 5
}

preview_js() {
  $head "$FILE_PATH" | ($HAS_JSBEAUTIFY && command js-beautify || cat) | colorize_stdout javascript && exit 5
}

preview_css() {
  $head "$FILE_PATH" | ($HAS_JSBEAUTIFY && command css-beautify || cat) | colorize_stdout css && exit 5
}

preview_sh() {
  ([[ -x $(command which shfmt 2>/dev/null) ]] && shfmt -s -i 2 -ci || cat) <"$FILE_PATH" | colorize_stdout sh && exit 5
}

preview_class() {
  [[ -x $(command which javac 2>/dev/null) ]] && command javap "$FILE_PATH" | colorize_stdout java && exit 5
}

preview_rst() {
  if [[ -x $(command which rst2html5.py 2>/dev/null) ]]; then
    $head "$FILE_PATH" |
      rst2html5.py --math-output=LaTeX --link-stylesheet --quiet --smart-quotes=yes --stylesheet=${HOME}/.docutils/docutils.css |
      $browser && exit 5
  elif ((HAS_PANDOC)); then
    $head "$FILE_PATH" | $pandoc -f rst | $browser && exit 5
  else
    $head "$FILE_PATH" | colorize_stdout rst && exit 5
  fi
}

preview_md() {
  $head "${FILE_PATH}" | ($HAS_PANDOC && $pandoc -f gfm | $browser && exit 5 || colorize_head markdown)
}

preview_jupyter() {
  if [[ -x $(command which jupyter 2>/dev/null) ]]; then
    command jupyter nbconvert --stdout --to html "${FILE_PATH}" | $browser && exit 5
  else
    preview_js
  fi
}

preview_html() {
  $browser "${FILE_PATH}" && exit 5
}

preview_img() {
  img2txt --gamma=0.6 --width="${PV_WIDTH}" -- "$FILE_PATH" && exit 5
  exiftool "$FILE_PATH" && exit 5
  exit 1
}

preview_xml() {
  $head "${FILE_PATH}" |
    ($HAS_JSBEAUTIFY && command html-beautify || cat) |
    colorize_stdout xml && exit 5
}

preview_ini() {
  $head "${FILE_PATH}" | colorize_stdout ini && exit 5
}

preview_docx() {
  # unzip, strip tags
  if ((HAS_PANDOC)); then
    $pandoc "$FILE_PATH" | $browser && exit 5
  elif [[ -x $(command which unzip 2>/dev/null) ]]; then
    command unzip -p "$FILE_PATH" word/document.xml |
      command sed -e 's/<\/w:p>/\n\n/g; s/<[^>]\{1,\}>//g; s/[^[:print:]\n]\{1,\}//g' |
      command fmt -u -w "$PV_WIDTH" |
      $head -n $PV_HEIGHT && exit 5
  fi
  exit 1
}

preview_cfamily() {
  local filetype=$(command pygmentize -N "$FILE_PATH")
  if [[ -x $(command which astyle 2>/dev/null) ]]; then
    command astyle --mode="$filetype" <"$FILE_PATH" | colorize_stdout "$filetype" && exit 5
  elif [[ -x $(command which clang 2>/dev/null) ]]; then
    command clang-format <"$FILE_PATH" | colorize_stdout "$filetype" && exit 5
  else
    # command pygmentize -f "${PYGMENTIZE_FORMAT}" -S "${filetype}" && exit 5
    $head "$FILE_PATH" | colorize_stdout "${filetype}" && exit 5
  fi
}

preview_js() {
  if ((HAS_JSBEAUTIFY)); then
    $head "${FILE_PATH}" | command js-beautify | colorize_stdout json && exit 5
  else
    $head "${FILE_PATH}" | colorize_stdout json && exit 5
  fi
}

preview_dir() {
  if [[ -d ${FILE_PATH} ]]; then
    if [[ -x $(command which tree 2>/dev/null) ]]; then
      command tree -l -a --prune -L 4 -F --sort=mtime "${FILE_PATH}" && exit 5
    else
      command ls "${FILE_PATH}" -1log
    fi
  fi
}

preview_org() {
  if ((HAS_PANDOC)); then
    $head "${FILE_PATH}" | $pandoc -f org | $browser && exit 5
  fi
}

preview_csv() {
  $head "${FILE_PATH}" | column --separator ',' --table && exit 5
  exit 1
}

preview_sqlite() {
  if [[ -x $(command which sqlite3 2>/dev/null) ]]; then
    IFS=$' \n'
    echo -e "\nPreview of SQLite3 database ${FILE_NAME}\n"
    for table_name in $(command sqlite3 --init '' --no$header "${FILE_PATH}" .tables | xargs); do
      command sqlite3 -column -$header -init '' "${FILE_PATH}" 'SELECT * FROM '"${table_name} LIMIT 3"
      echo -e "\n"
    done
    exit 5
  fi
}

preview_pptx() {
  echo -e $(basename "${FILE_PATH}")"\n----------------------------------------------------\n"
  command unzip -p "${FILE_PATH}" | command sed -e 's/<\/w:p>/\n\n/g; s/<[^>]\{1,\}>//g; s/[^[:print:]\n]\{1,\}//g' | perl -pe 's/[^[:ascii:]]//g' | fmt -u -w $PV_WIDTH | column | sed -E -e 's/\. ([A-Z])|(\*)|(\-)/\n\n\1/g' && exit 5
  exit 1
}

guess_pygments() {
  # let pygments guess
  local guess=$(pygmentize -N "${FILE_PATH}")

  if [[ $guess =~ 'text' ]]; then
    preview
  else
    $head "${FILE_PATH}" | colorize_stdout $guess && exit 5 || exit 1
  fi
}

guess_shebang() {
  # guess from shebang
  if [[ $(eval "$head ${FILE_PATH}") =~ '#!' ]]; then
    local executable=$($head -n 1 "${FILE_PATH}" | grep -Eo '\w+$')
    if [[ -x $(command which $executable 2>/dev/null) ]]; then
      colorize_head $executable
    fi
  fi
}

preview_pdf() {
  # Preview as text conversion
  command pdftotext -l 5 -nopgbrk -q -- "${FILE_PATH}" - | grep -Eo '[[:print:][:alpha:]]{5,}' && exit 5
  command exiftool "${FILE_PATH}" && exit 5
  exit 1
}

handle_code() {

  # save time,
  # don't attempt to render non-text files,
  # don't attempt to pygmentize without it installed
  ! ((HAS_PYGMENTS)) && return
  ! [[ $(command file --dereference --brief -- "${FILE_PATH}") =~ text|(xml|script)$ ]] && return

  case "${EXTENSION}" in

    # java | cpp | c | h | hpp | cs | c++ | hh | hxx | cp)
    java | cpp | c | h)
      preview_cfamily
      ;;

    js | ts | json)
      preview_js
      ;;

    sh)
      preview_sh
      ;;

    # PHP
    php)
      preview_php
      ;;

    css)
      preview_css
      ;;

    # XML formats
    # xml | iml | ?cls | plist | back | xbel | fo | urdf | sdf | xacro | uml | aird | notation | project | svg | rng | page | docbook | ui | glade | gir)
    xml)
      preview_xml
      ;;

    puml)
      $head "${FILE_PATH}" | colorize_stdout java && exit 5
      ;;

    go)
      preview_go
      ;;

  esac
}

handle_extension() {

  case "${EXTENSION}" in

    csv)
      preview_csv
      ;;

    pdf)
      preview_pdf
      ;;

    docx)
      preview_docx
      ;;

    pptx)
      preview_pptx
      ;;

    sqlite*)
      preview_sqlite
      ;;

    # automatically decompile Java's *.class files + highlight
    class)
      preview_class
      ;;

    # OpenDocument
    odt | ods | odp | sxw)
      # Preview as text conversion
      command odt2txt "${FILE_PATH}" && exit 5 || exit 1
      ;;

    ?.gz)
      MANWIDTH=${PV_WIDTH} man -- "$FILE_PATH"
      ;;

    # BitTorrent
    torrent)
      [[ -x $(command which transmission) ]] && command transmission-show -- "${FILE_PATH}" && exit 5 || exit 1
      ;;

  esac
}

handle_website() {
  ! ((HAS_ELINKS)) && return

  case "${EXTENSION}" in

    md | m*down)
      preview_md
      ;;

    *html)
      preview_html
      ;;

    rst)
      preview_rst
      ;;

    org)
      preview_org
      ;;

    ipynb)
      preview_jupyter
      ;;

  esac
}

handle_mime() {

  case $(command file --dereference --brief --mime-type -- "${FILE_PATH}") in

    # Text files
    text/* | application/*xml | application/*script)

      if ! ((HAS_PYGMENTS)); then
        preview && exit 5 || exit 1
      fi

      guess_shebang

      case "${FILE_NAME,,}" in

        # must be checked for BEFORE *.txt
        # requirements.txt | humans.txt | robots.txt)
        # preview_ini
        # ;;

        # Generic text files
        *.txt)
          preview
          ;;

        # *.conf | *config | *.cfg | .*ignore* | *.cnf | *.toml | *.MF | *.desktop | .flake8 | *.yapf)
        *.conf | *config | *.cfg | .*ignore* | *.cnf | *.toml | *.desktop)
          preview_ini
          ;;

        license | readme | change* | contrib* | building | roadmap)
          preview_rst
          ;;

        *)
          guess_pygments
          ;;

      esac
      ;;

    inode/directory)
      preview_dir
      ;;

    # Image
    image/*)
      preview_img
      ;;
  esac
}

handle_archive() {

  case "${EXTENSION}" in

    # zip | gz | tar | jar | 7z | bz2 | rpm | deb | cpio | deb | arj)
    zip | gz | tar | jar | 7z | bz2)

      # Avoid password prompt by providing empty password
      [[ -x $(command which 7z 2>/dev/null) ]] && command 7z l -p -- "${FILE_PATH}" && exit 5 || exit 1
      ;;

    # a | ace | alz | arc | arj | bz | cab | lha | lz | lzh | lzma | lzo | rz | t7z | tbz | tbz2 | tgz | tlz | txz | tZ | tzo | war | xpi | xz | Z)
    bz | lz | xz)
      [[ -x $(command atool 7z 2>/dev/null) ]] && command atool --list -- "${FILE_PATH}" && exit 5 || exit 1
      [[ -x $(command bsdtar 7z 2>/dev/null) ]] && command bsdtar --list --file "${FILE_PATH}" && exit 5 || exit 1
      ;;

    tar.gz)
      command tar ztf "${FILE_PATH}" && exit 5 || exit 1
      ;;

    tar.xz)
      command tar Jtf "${FILE_PATH}" && exit 5 || exit 1
      ;;

    tar.bz2)
      command tar jtf "${FILE_PATH}" && exit 5 || exit 1
      ;;

    rar)
      [[ -x $(command unrar 7z 2>/dev/null) ]] && command unrar lt -p- -- "${FILE_PATH}" && exit 5 || exit 1
      exit 1
      ;;

  esac
}

handle_fallback() {
  echo ""
  exiftool "${FILE_PATH}"
  echo -e "\n${FILE_PATH}\n" && command file --dereference --brief -- "${FILE_PATH}" | command fmt -u -w $PV_WIDTH && exit 5
}

handle_code
handle_website
handle_archive
handle_extension
handle_mime
handle_fallback

exit 1
# vim: nowrap
