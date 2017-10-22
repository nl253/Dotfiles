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
    a | ace | alz | arc | arj | bz | bz2 | cab | cpio | deb | gz | jar | lha | lz | lzh | lzma | lzo | rpm | docx | rz | t7z | tar | tbz | tbz2 | tgz | tlz | txz | tZ | tzo | war | xpi | xz | Z | zip)

      atool --list -- "${FILE_PATH}" && exit 5
      bsdtar --list --file "${FILE_PATH}" && exit 5
      exit 1
      ;;

		tar.gz)
			tar -ztf "${FILE_PATH}"
		;;

		tar.bz2)
			tar -jtf "${FILE_PATH}"
		;;

		gzip | bzip2 | 7z)
      # Avoid password prompt by providing empty password
      7z l -p -- "${FILE_PATH}" && exit 5
      exit 1
      ;;

    rar)
      # Avoid password prompt by providing empty password
      unrar lt -p- -- "${FILE_PATH}" && exit 5
      exit 1
      ;;

    # PDF
    pdf)
      # Preview as text conversion
      pdftotext -l 10 -nopgbrk -q -- "${FILE_PATH}" - && exit 5
      exiftool "${FILE_PATH}" && exit 5
      exit 1
      ;;

    # BitTorrent
    torrent)
      transmission-show -- "${FILE_PATH}" && exit 5
      exit 1
      ;;

    # OpenDocument
    odt | ods | odp | sxw)
      # Preview as text conversion
      odt2txt "${FILE_PATH}" && exit 5
      exit 1
      ;;

    # HTML
    htm | html | xhtml)
      # Preview as text conversion
      w3m -dump "${FILE_PATH}" && exit 5
      lynx -dump -- "${FILE_PATH}" && exit 5
      elinks -dump "${FILE_PATH}" && exit 5
      ;;

		# PHP
		php)
      if (($HAS_PYGMENTS)); then
        head -n "${PV_HEIGHT}" -- "${FILE_PATH}" | pygmentize -f "${PYGMENTIZE_FORMAT}" -l html+php
        exit 5
      fi
      ;;

		toml | conf | MF | cnf | desktop)
      if (($HAS_PYGMENTS)); then
        head -n "${PV_HEIGHT}" -- "${FILE_PATH}" | pygmentize -f "${PYGMENTIZE_FORMAT}" -l dosini
        exit 5
      fi
      ;;

    # XML formats
    iml | ucls | plist | back | xbel | fo)
      if (($HAS_PYGMENTS)); then
        head -n "${PV_HEIGHT}" -- "${FILE_PATH}" | pygmentize -f "${PYGMENTIZE_FORMAT}" -l xml
        exit 5
      fi
      ;;

		# automatically decompile Java's *.class files + highlight
    class)
      if [[ -x $(command which javap 2>/dev/null) ]]; then

        if (($HAS_PYGMENTS)); then
          javap "${FILE_PATH}" | pygmentize -l java

        else
          javap "${FILE_PATH}"
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
        head -n "${PV_HEIGHT}" -- "${FILE_PATH}" | pygmentize -f "${PYGMENTIZE_FORMAT}" -l $(pygmentize -N "${FILE_PATH}")
        exit 5
      else
        exit 2
      fi
      ;;

    inode/directory)

      [[ -d "${FILE_PATH}" ]] && tree -l -a --prune -L 4 -F --sort=mtime "${FILE_PATH}" && exit 5
      ;;

  esac
}

# handle_filename() {

	# case $FILE_PATH in
	# "$quit")
			# echo "Exiting."
			# break
			# ;;
	# *)
			# echo "You picked $filename ($REPLY)"
			# ;;
	# esac
# }

handle_fallback() {
  echo -e "${FILE_PATH}\n"
  file --dereference --brief -- "${FILE_PATH}" | fmt && exit 5
}

handle_extension
handle_mime $(file --dereference --brief --mime-type -- "${FILE_PATH}")
handle_fallback

exit 1

# vim: nowrap
