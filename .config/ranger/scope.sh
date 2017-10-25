#!/usr/bin/env bash

set -o noclobber -o noglob -o nounset -o pipefail
IFS=$'\n'

# Dependencies
# ------------
# - GNU coreutils
# - 7z
# - pygmentize
# - tar
# - unrar
# - atool 
# - bsdtar
# - transmission
# - odt2txt
# - pdftotext
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
# 6    | image      | Display the image `$IMAGE_CACHE_PATH` points to as an image preview
# 7    | image      | Display the file directly as an image

# Script arguments
FILE_PATH="${1}"        # Full path of the highlighted file
PV_WIDTH="${2}"         # Width of the preview pane (number of fitting characters)
PV_HEIGHT="${3}"        # Height of the preview pane (number of fitting characters)
IMAGE_CACHE_PATH="${4}" # Full path that should be used to cache image preview
PV_IMAGE_ENABLED="${5}" # 'True' if image previews are enabled, 'False' otherwise.

FILE_EXTENSION="${FILE_PATH##*.}"
FILE_EXTENSION_LOWER="${FILE_EXTENSION,,}"

[[ $(command which pygmentize) ]] && HAS_PYGMENTS=1 || HAS_PYGMENTS=0

# Settings
HIGHLIGHT_SIZE_MAX=262143 # 256KiB
HIGHLIGHT_TABWIDTH=8
HIGHLIGHT_STYLE='pablo'
PYGMENTIZE_STYLE='autumn'

[[ "$(tput colors)" -ge 256 ]] && PYGMENTIZE_FORMAT='terminal256' || PYGMENTIZE_FORMAT='terminal'

handle_extension() {
  case "${FILE_EXTENSION_LOWER}" in

    # Archive
    a | ace | alz | arc | arj | bz | bz2 | cab | cpio | deb | gz | jar | lha | lz | lzh | lzma | lzo | rpm | rz | t7z | tar | tbz | tbz2 | tgz | tlz | txz | tZ | tzo | war | xpi | xz | Z | zip)

      command atool --list -- "${FILE_PATH}" && exit 5
      command bsdtar --list --file "${FILE_PATH}" && exit 5
      exit 1
      ;;

		tar.gz)
			command tar -ztf "${FILE_PATH}"
		;;

		tar.bz2)
			command tar -jtf "${FILE_PATH}"
		;;

		gzip | bzip2 | 7z)
      # Avoid password prompt by providing empty password
      command 7z l -p -- "${FILE_PATH}" && exit 5
      exit 1
      ;;

    rar)
      # Avoid password prompt by providing empty password
      command unrar lt -p- -- "${FILE_PATH}" && exit 5
      exit 1
      ;;

    # PDF
    pdf)
      # Preview as text conversion
      command pdftotext -l 10 -nopgbrk -q -- "${FILE_PATH}" - && exit 5
      command exiftool "${FILE_PATH}" && exit 5
      exit 1
      ;;

    # BitTorrent
    torrent)
      command transmission-show -- "${FILE_PATH}" && exit 5
      exit 1
      ;;

    # OpenDocument
    odt | ods | odp | sxw)
      # Preview as text conversion
      command odt2txt "${FILE_PATH}" && exit 5
      exit 1
      ;;

    # HTML
    html | xhtml)
      # Preview as text conversion
      command w3m -dump "${FILE_PATH}" && exit 5
      command lynx -dump -- "${FILE_PATH}" && exit 5
      command elinks -dump "${FILE_PATH}" && exit 5
      ;;

		css)
			if [[ $(command which css-beautify) ]] && (($HAS_PYGMENTS)); then
				command head -n "${PV_HEIGHT}" -- "${FILE_PATH}" | command css-beautify | command pygmentize -f "${PYGMENTIZE_FORMAT}" -l css
				exit 5
			fi
			;;

		js | ts)
			if [[ $(command which js-beautify) ]] && (($HAS_PYGMENTS)); then
				command head -n "${PV_HEIGHT}" -- "${FILE_PATH}" | command js-beautify | command pygmentize -f "${PYGMENTIZE_FORMAT}" -l javascript 
				exit 5
			fi
			;;

		json)
			if [[ $(command which js-beautify) ]] && (($HAS_PYGMENTS)); then
				command head -n "${PV_HEIGHT}" -- "${FILE_PATH}" | command js-beautify | command pygmentize -f "${PYGMENTIZE_FORMAT}" -l json 
				exit 5
			fi
			;;

		java | cpp | c | h | hpp | cs | c++ | hh| hxx | cp)
			if [[ $(command which astyle) ]] && (($HAS_PYGMENTS)); then
				local filetype=$(pygmentize -N "${FILE_PATH}") 
				command cat $FILE_PATH | command astyle --mode="${filetype}" | command pygmentize -f "${PYGMENTIZE_FORMAT}" -l "${filetype}" || break
				exit 5
			fi
			;;

		docx)
			# unzip, strip tags
			if [[ -x $(command which unzip 2>/dev/null) ]]; then
				command unzip -p ${FILE_PATH} word/document.xml | command sed -e 's/<\/w:p>/\n\n/g; s/<[^>]\{1,\}>//g; s/[^[:print:]\n]\{1,\}//g' | command fmt -u -w ${PV_WIDTH} | command head -n ${PV_HEIGHT} 
				exit 5
			fi
			;;

		md | m*down)

			if [[ -x $(command which pandoc 2>/dev/null) ]] && [[ -x $(command which elinks 2>/dev/null) ]]; then
				pandoc --self-contained -f markdown_github -t html ${FILE_PATH} | elinks -dump
				exit 5
			fi
		;;

		rst)
			if [[ -x ~/.local/bin/rst2html5.py ]] && [[ -x $(command which elinks 2>/dev/null) ]]; then
				~/.local/bin/rst2html5.py --smart-quotes=yes --math-output='MathJax' --stylesheet='' ${FILE_PATH} | elinks -dump
				exit 5
			fi
		;;

		# Generic text files
		txt)
      if (($HAS_PYGMENTS)); then
				if [[ $(basename $FILE_PATH) =~ requirements ]] || [[ $(basename $(dirname $FILE_PATH)) =~ requirements ]]; then
					command head -n "${PV_HEIGHT}" -- "${FILE_PATH}" | command pygmentize -f "${PYGMENTIZE_FORMAT}" -l dosini
				else
					command head -n "${PV_HEIGHT}" -- "${FILE_PATH}" | fmt -u -w ${PV_WIDTH} | command pygmentize -f "${PYGMENTIZE_FORMAT}" -l rst
				fi
        exit 5
      fi
      ;;

		# PHP
		php)
      if (($HAS_PYGMENTS)); then
        command head -n "${PV_HEIGHT}" -- "${FILE_PATH}" | command pygmentize -f "${PYGMENTIZE_FORMAT}" -l html+php
        exit 5
      fi
      ;;

		toml | conf | MF | cnf | desktop)
      if (($HAS_PYGMENTS)); then
        command head -n "${PV_HEIGHT}" -- "${FILE_PATH}" | command pygmentize -f "${PYGMENTIZE_FORMAT}" -l dosini
        exit 5
      fi
      ;;

    # XML formats
    iml | ucls | plist | back | xbel | fo | urdf | sdf | xacro)
			if (($HAS_PYGMENTS)) && [[ -x $(command which html-beautify 2>/dev/null) ]]; then
				command head -n "${PV_HEIGHT}" -- "${FILE_PATH}" | command html-beautify | command pygmentize -f "${PYGMENTIZE_FORMAT}" -l xml
				exit 5
      fi
      ;;

		puml)
      if (($HAS_PYGMENTS)); then
        command head -n "${PV_HEIGHT}" -- "${FILE_PATH}" | command pygmentize -f "${PYGMENTIZE_FORMAT}" -l java
        exit 5
      fi
			;;

		sqlite*)

			if [[ -x $(command which sqlite3 2>/dev/null) ]]; then
				local no_tables=$(command sqlite3 -noheader -init '' ${FILE_PATH} .tables | grep -Ec '^[-a-zA-Z0-9_]+')
				if (($no_tables == 1)); then
					local table_name=$(command sqlite3 -init "" ${FILE_PATH} .tables | head -n 1)
					echo -e "\nPreview of SQLite3 database $(basename ${FILE_PATH})"
					echo -e "\nTable ${table_name}\n"
					command sqlite3 -column -header -init '' ${FILE_PATH} 'SELECT * FROM '"${table_name} LIMIT ${PV_HEIGHT}"
				else
					echo -e "\nPreview of SQLite3 database $(basename ${FILE_PATH})"
					echo -e "\nTables\n"
					command sqlite3 -init '' ${FILE_PATH} .tables
				fi
			fi
			exit 5
			;;

		# automatically decompile Java's *.class files + highlight
    class)
      if [[ -x $(command which javap 2>/dev/null) ]]; then

        if (($HAS_PYGMENTS)); then
          command javap "${FILE_PATH}" | pygmentize -l java

        else
          command javap "${FILE_PATH}"
        fi

        exit 5
      fi
      ;;

  esac
}

handle_mime() {
  local mimetype="${1}"
  case "${mimetype}" in

    # Text
    text/* | *xml* | *html* | *json* | *script*)

      if (($HAS_PYGMENTS)); then
        command head -n "${PV_HEIGHT}" -- "${FILE_PATH}" | command pygmentize -f "${PYGMENTIZE_FORMAT}" -l $(pygmentize -N "${FILE_PATH}")
        exit 5
      else
        exit 2
      fi
      ;;

    inode/directory)

      [[ -d "${FILE_PATH}" ]] && command tree -l -a --prune -L 4 -F --sort=mtime "${FILE_PATH}" && exit 5
      ;;

  esac
}

handle_fallback() {
  echo -e "${FILE_PATH}\n"
  command file --dereference --brief -- "${FILE_PATH}" | command fmt -w -U ${PV_WIDTH} && exit 5
}

handle_extension
handle_mime $(command file --dereference --brief --mime-type -- "${FILE_PATH}")
handle_fallback

exit 1

# vim: nowrap
