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
FILE_NAME=$(basename "$FILE_PATH")
EXTENSION="${FILE_NAME#*.}"
head="command head -n $PV_HEIGHT --"
fmt="command fmt -u -w $PV_WIDTH"

# Essential
[[ -x $(command which elinks 2>/dev/null) ]] && HAS_ELINKS=1 && export browser="command elinks -no-references -no-numbering -dump -dump-color-mode 4 -dump-width $PV_WIDTH" || HAS_ELINKS=0
[[ -x $(command which js-beautify 2>/dev/null) ]] && HAS_JSBEAUTIFY=1 || HAS_JSBEAUTIFY=0
[[ -x $(command which pandoc 2>/dev/null) ]] && HAS_PANDOC=1 && export pandoc="command pandoc --self-contained -t html5" || HAS_PANDOC=0
[[ -x $(command which highlight 2>/dev/null) ]] && HAS_HIGHLIGHT=1 || HAS_PYGMENTS=0

HAS_SYNTAX_HL=0
HAS_PYGMENTS=0
HAS_PYGMENTS=0

# Settings
if [[ -x $(command which highlight) ]]; then
  HAS_HIGHLIGHT=1
  export highlight="command highlight --line-numbers --stdout --out-format=xterm256"
fi

if [[ -x $(command which pygmentize 2>/dev/null) ]]; then
  HAS_PYGMENTS=1
	HAS_SYNTAX_HL=1
  HIGHLIGHT_SIZE_MAX=262143 # 256KiB
  HIGHLIGHT_TABWIDTH=8
  HIGHLIGHT_STYLE=pablo
  PYGMENTIZE_STYLE=autumn
  [[ "$(tput colors)" -ge 256 ]] && PYGMENTIZE_FORMAT=terminal256 || PYGMENTIZE_FORMAT=terminal
  export pygmentize="command pygmentize -O style=default,bg=dark,lineos=1 -F gobble,highlight,codetagify -f ${PYGMENTIZE_FORMAT}"
fi

report(){
	cat <<EOF
REPORT 
------

FILE_PATH: $FILE_PATH
FILE_NAME: $FILE_NAME
EXTENSION: $EXTENSION
MIME:     $(command file --dereference --brief --mime-type -- "$FILE_PATH")
-------------------------------
pygmentize:  $pygmentize
highlight:   $highlight
browser:     $browser
fmt:         $fmt
head:        $head
pandoc:      $pandoc
--------------------------------
HAS_SYNTAX_HL:   $HAS_SYNTAX_HL
HAS_PYGMENTS:    $HAS_PYGMENTS
HAS_PANDOC:      $HAS_PANDOC
HAS_JSBEAUTIFY:  $HAS_JSBEAUTIFY
HAS_ELINKS:      $HAS_ELINKS
--------------------------------
PV_WIDTH:   $PV_WIDTH
PV_HEIGHT:  $PV_HEIGHT

EOF
	exit 5
}

colorize_stdout() {

	! ((HAS_SYNTAX_HL)) && cat < /dev/stdin && exit 5
	
	if [[ -v 1 ]]; then
	  local language=$1
	else

		if ((HAS_PYGMENTS)); then
			local language=$(pygmentize -N "$FILE_PATH")
		else
			local language=$EXTENSION
		fi

		if ((HAS_HIGHLIGHT)) || [[ $2 =~ highl ]]; then
			$highlight -S $language < /dev/stdin && exit 5
		elif ((HAS_PYGMENTS)) || [[ $2 =~ pygme ]]; then
			$pygmentize -l $language < /dev/stdin && exit 5
		fi
  fi
	preview && exit 5
	echo "[ERROR] Could not highlight file $FILE_PATH with langugage ${language}."
	exit 5
}

colorize_head() {
	$head "$FILE_PATH" | colorize_stdout $1 $2 
	exit 5
}

# most generic, no syntax highlighting
preview() {
  $head "$FILE_PATH"  
	exit 5
}

preview_go() {
  ([[ -x $(command which gofmt 2>/dev/null) ]] && command gofmt -s || cat) < "$FILE_PATH" | colorize_stdout go 
	exit 5
}

preview_js() {
  $head "$FILE_PATH" | ($HAS_JSBEAUTIFY && command js-beautify || cat) | colorize_stdout javascript 
	exit 5
}

preview_css() {
  $head "$FILE_PATH" | ($HAS_JSBEAUTIFY && command css-beautify || cat) | colorize_stdout css 
	exit 5
}

preview_sh() {
  ([[ -x $(command which shfmt 2>/dev/null) ]] && shfmt -s -i 2 -ci || cat) <"$FILE_PATH" | colorize_stdout sh 
	exit 5
}

preview_class() {
  [[ -x $(command which javac 2>/dev/null) ]] && command javap "$FILE_PATH" | colorize_stdout java 
	exit 5
}

preview_rst() {
  if [[ -x $(command which rst2html5.py 2>/dev/null) ]]; then
    $head "$FILE_PATH" |
      rst2html5.py --math-output=LaTeX --link-stylesheet --quiet --smart-quotes=yes --stylesheet=${HOME}/.docutils/docutils.css |
      $browser
  elif ((HAS_PANDOC)); then
    $head "$FILE_PATH" | $pandoc -f rst | $browser
  else
		colorize_head rst 
  fi
	exit 5
}

preview_md() {
	# report
	$head "$FILE_PATH" | ( ((HAS_PANDOC)) && $pandoc -f gfm | $browser || colorize_head markdown) 
	exit 5
}

preview_jupyter() {
  if [[ -x $(command which jupyter 2>/dev/null) ]]; then
    command jupyter nbconvert --stdout --to html "${FILE_PATH}" | $browser 
  else
    preview_js
  fi
	exit 5
}

preview_html() {
  $browser "$FILE_PATH" 
	exit 5
}

preview_img() {
  img2txt --gamma=0.6 --width="$PV_WIDTH" -- "$FILE_PATH" && exit 5
  exiftool "$FILE_PATH"
	exit 5
}

preview_xml() {
  $head "$FILE_PATH" |
	( ((HAS_JSBEAUTIFY)) && command html-beautify || cat) |
    colorize_stdout xml  
	exit 5
}

preview_docx() {
  # unzip, strip tags
  if ((HAS_PANDOC)); then
    $pandoc -f docx "$FILE_PATH" | $browser 
  elif [[ -x $(command which unzip 2>/dev/null) ]]; then
    command unzip -p "$FILE_PATH" word/document.xml | command sed -e 's/<\/w:p>/\n\n/g; s/<[^>]\{1,\}>//g; s/[^[:print:]\n]\{1,\}//g' | $fmt | $head
  fi
	exit 5
}

preview_cfamily() {
  # local filetype=$(command pygmentize -N "$FILE_PATH")
	if [[ $EXTENSION =~ java|c|cpp ]]; then
		local filetype=$EXTENSION
	else
		local filetype=$(command pygmentize -N "$FILE_PATH")
	fi
  if [[ -x $(command which astyle 2>/dev/null) ]]; then
    command astyle --mode="$filetype" <"$FILE_PATH" | colorize_stdout "$filetype" $1
  elif [[ -x $(command which clang 2>/dev/null) ]]; then
    command clang-format <"$FILE_PATH" | colorize_stdout "$filetype" $1
  else
		colorize_head "$filetype" $1
  fi
	exit 5
}

preview_js() {
	$head "$FILE_PATH" | ( ((HAS_JSBEAUTIFY)) && command js-beautify || cat) | colorize_stdout javascript 
	exit 5
}

preview_dir() {
  if [[ -d "$FILE_PATH" ]]; then
    if [[ -x $(command which tree 2>/dev/null) ]]; then
      command tree -l -a --prune -L 4 -F --sort=mtime "$FILE_PATH"
    else
      command ls "$FILE_PATH" -1log
    fi
		exit 5
  fi
}

preview_org() {
		$head "$FILE_PATH" | 
			( ((HAS_PANDOC)) && $pandoc -f org | $browser || ((HAS_PYGMENTS)) && colorize_head org) 
		exit 5
}

preview_csv() {
  $head "$FILE_PATH" | column --separator ',' --table || preview 
	exit 5
}

preview_sqlite() {
  if [[ -x $(command which sqlite3 2>/dev/null) ]]; then
		cat <<EOF

SQLite3 database $FILE_NAME $(basename $(dirname $FILE_PATH))
-----------------------------------------------

EOF
    for table_name in $(command sqlite3 --init '' --no-header "$FILE_PATH" .tables | xargs); do
      command sqlite3 -column -$header -init '' "$FILE_PATH" 'SELECT * FROM '"$table_name LIMIT 3"
      echo -e "\n"
    done
    exit 5
  fi
}

preview_pptx() {
	cat <<EOF

PowerPoint ($EXTENSION) - $FILE_NAME $(basename $(dirname $FILE_PATH))
----------------------------------------------------

EOF
  command unzip -p "${FILE_PATH}" | 
		command sed -e 's/<\/w:p>/\n\n/g; s/<[^>]\{1,\}>//g; s/[^[:print:]\n]\{1,\}//g' | 
		perl -pe 's/[^[:ascii:]]//g' | 
		$fmt | 
		column | 
		sed -E -e 's/\. ([A-Z])|(\*)|(\-)/\n\n\1/g' | 
		grep -E '[[:print:][:alpha:]]{5,}' 
	exit 5
}

guess_pygments() {
  # let pygments guess
  local guess=$(pygmentize -N "${FILE_PATH}")

  if [[ $guess = text ]]; then
    preview 
  else
    $head "$FILE_PATH" | colorize_stdout $guess 
  fi
	exit 5 
}

guess_shebang() {
  # guess from shebang
  if [[ $(eval "$head $FILE_PATH") =~ '#!' ]]; then
    local executable=$($head -n 1 "$FILE_PATH" | grep -Eo '\w+$')
    if [[ -x $(command which $executable 2>/dev/null) ]]; then
      colorize_head $executable && exit 5
    fi
  fi
	preview 
	exit 5
}

preview_pdf() {
  # Preview as text conversion
  command pdftotext -l 5 -nopgbrk -q -- "$FILE_PATH" -
	exit 5
}

handle_extension() {

	echo -e "[INFO] extension handle"

  case "$EXTENSION" in

    # java | cpp | c | h | hpp | cs | c++ | hh | hxx | cp)
    java | cpp | c | h)
      preview_cfamily pygments && exit 5
      ;;

    js | json | ts)
      preview_js && exit 5
      ;;

    sh)
      preview_sh && exit 5
      ;;

    md | m*down)
      preview_md && exit 5
      ;;

    css)
      preview_css && exit 5
      ;;

    pptx)
      preview_pptx && exit 5
      ;;

    pdf)
      preview_pdf && exit 5
      ;;

    xml)
      preview_xml && exit 5
      ;;

    docx)
      preview_docx && exit 5
      ;;

    puml)
      colorize_head java pygments && exit 5
      ;;

    *html)
      preview_html && exit 5
      ;;

    rst)
      preview_rst && exit 5
      ;;

    org)
      preview_org && exit 5
      ;;

    ipynb)
      preview_jupyter && exit 5
      ;;

    go)
      preview_go && exit 5
      ;;

    csv)
      preview_csv && exit 5
      ;;

		conf | cnf | cfg | toml | desktop)
			preview_ini && exit 5
      ;;

    ?.gz)
      MANWIDTH=$PV_WIDTH man -- "$FILE_PATH" && exit 5
      ;;

    sqlite*)
      preview_sqlite && exit 5
      ;;

    # # automatically decompile Java's *.class files + highlight
    # class)
      # preview_class
      # ;;

    # # OpenDocument
    # odt | ods | odp | sxw)
      # # Preview as text conversion
      # command odt2txt "${FILE_PATH}" && exit 5 || exit 1
      # ;;

    # # BitTorrent
    # torrent)
      # [[ -x $(command which transmission) ]] && command transmission-show -- "${FILE_PATH}" && exit 5 || exit 1
      # ;;

  esac
}

handle_mime() {

	echo -e "[INFO] MIME handle"

  case $(command file --dereference --brief --mime-type -- "$FILE_PATH") in

		# Text files
    text/* | application/*xml | application/*script)
			[[ $EXTENSION = $FILE_NAME ]] && guess_shebang && exit 5
			( ((HAS_SYNTAX_HL)) && colorize_head || preview) && exit 5
			;;

    inode/directory)
      preview_dir && exit 5
      ;;

    # Image
    image/*)
      preview_img && exit 5
      ;;
  esac
}

handle_archive() {

	echo -e "[INFO] archive HANDLE"

	case "$EXTENSION" in

    zip | gz | tar | jar | 7z | bz2)
      # Avoid password prompt by providing empty password
      [[ -x $(command which 7z 2>/dev/null) ]] && command 7z l -p -- "$FILE_PATH" && exit 5 
			exit 1
      ;;

    bz | lz | xz)
      [[ -x $(command atool 7z 2>/dev/null) ]] && command atool --list -- "$FILE_PATH" && exit 5
      [[ -x $(command bsdtar 7z 2>/dev/null) ]] && command bsdtar --list --file "$FILE_PATH" && exit 5
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

----------------------------------------------------------------------

$(exiftool "$FILE_PATH")

----------------------------------------------------------------------

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
