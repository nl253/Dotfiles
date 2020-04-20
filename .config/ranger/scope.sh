#!/usr/bin/env bash

set -o noclobber -o noglob -o nounset -o pipefail
IFS=$'\n'

FILE_PATH="${1}"     # Full path of the highlighted file
PV_WIDTH="${2}"      # Width of the preview pane (number of fitting characters)
PV_HEIGHT="${3}"     # Height of the preview pane (number of fitting characters)
IMAGE_CACHE_PATH="${4}"  # Full path that should be used to cache image preview
# PV_IMAGE_ENABLED="${5}"  # 'True' if image previews are enabled, 'False' otherwise.
PV_IMAGE_ENABLED=True

FILE_EXTENSION="${FILE_PATH##*.}"
FILE_EXTENSION_LOWER="$(printf "%s" "${FILE_EXTENSION}" | tr '[:upper:]' '[:lower:]')"

## Settings
HIGHLIGHT_SIZE_MAX=262143  # 256KiB
HIGHLIGHT_TABWIDTH=${HIGHLIGHT_TABWIDTH:-2}
HIGHLIGHT_STYLE=${HIGHLIGHT_STYLE:-molokai}
HIGHLIGHT_OPTIONS="--replace-tabs=${HIGHLIGHT_TABWIDTH} --reformat=google --line-numbers --no-trailing-nl --kw-case=upper --style=${HIGHLIGHT_STYLE} ${HIGHLIGHT_OPTIONS:-}"
PYGMENTIZE_STYLE=${PYGMENTIZE_STYLE:-autumn}

handle_extension() {
  case "${FILE_EXTENSION_LOWER}" in
    htm|html|xhtml)
      ## Preview as text conversion
      w3m -dump "${FILE_PATH}" && exit 5
      lynx -dump -- "${FILE_PATH}" && exit 5
      elinks -dump "${FILE_PATH}" && exit 5
      pandoc -s -t markdown -- "${FILE_PATH}" && exit 5
      ;;
    json)
      jq --color-output . "${FILE_PATH}" && exit 5
      python -m json.tool -- "${FILE_PATH}" && exit 5
      ;;
    csv)
      xsv slice --start 0 --end "${PV_HEIGHT}" -- "${FILE_PATH}" | xsv fmt | xsv table 2>/dev/null && exit 5;;
    7z)
      ## Avoid password prompt by providing empty password
      7z l -p -- "${FILE_PATH}" && exit 5
      exit 1;;
    bz|bz2|deb|gz|jar|lz|lzh|lzma|lzo|tar|tbz|tbz2|tgz|tlz|txz|xz|zip)
      atool --list -- "${FILE_PATH}" && exit 5
      bsdtar --list --file "${FILE_PATH}" && exit 5
      exit 1;;
    pdf)
      ## Preview as text conversion
      pdftotext -l 10 -nopgbrk -q -- "${FILE_PATH}" - | \
        fmt -w "${PV_WIDTH}" && exit 5
      mutool draw -F txt -i -- "${FILE_PATH}" 1-10 | \
        fmt -w "${PV_WIDTH}" && exit 5
      exiftool "${FILE_PATH}" && exit 5
      exit 1;;
    # rar)
      # ## Avoid password prompt by providing empty password
      # unrar lt -p- -- "${FILE_PATH}" && exit 5
      # exit 1;;
    xlsx)
      ## Preview as csv conversion
      ## Uses: https://github.com/dilshod/xlsx2csv
      xlsx2csv -- "${FILE_PATH}" && exit 5
      exit 1;;
  esac
}

handle_image() {
  ## Size of the preview if there are multiple options or it has to be
  ## rendered from vector graphics. If the conversion program allows
  ## specifying only one dimension while keeping the aspect ratio, the width
  ## will be used.
  local DEFAULT_SIZE="1920x1080"

  local mimetype="${1}"
  case "${mimetype}" in

    ## Image
    image/*)
      local orientation
      orientation="$( identify -format '%[EXIF:Orientation]\n' -- "${FILE_PATH}" )"
      ## If orientation data is present and the image actually
      ## needs rotating ("1" means no rotation)...
      if [[ -n "$orientation" && "$orientation" != 1 ]]; then
        ## ...auto-rotate the image according to the EXIF data.
        convert -- "${FILE_PATH}" -auto-orient "${IMAGE_CACHE_PATH}" && exit 6
      fi

      ## `w3mimgdisplay` will be called for all images (unless overriden
      ## as above), but might fail for unsupported types.
      exit 7;;

    ## PDF
    # application/pdf)
    #   pdftoppm -f 1 -l 1 \
    #        -scale-to-x "${DEFAULT_SIZE%x*}" \
    #        -scale-to-y -1 \
    #        -singlefile \
    #        -jpeg -tiffcompression jpeg \
    #        -- "${FILE_PATH}" "${IMAGE_CACHE_PATH%.*}" \
    #     && exit 6 || exit 1;;
  esac
}

handle_mime() {
  local mimetype="${1}"
  case "${mimetype}" in

    ## Text
    text/* | */xml)
      ## Syntax highlight
      if [[ "$( stat --printf='%s' -- "${FILE_PATH}" )" -gt "${HIGHLIGHT_SIZE_MAX}" ]]; then
        exit 2
      fi
      if [[ "$( tput colors )" -ge 256 ]]; then
        local pygmentize_format='terminal256'
        local highlight_format='xterm256'
      else
        local pygmentize_format='terminal'
        local highlight_format='ansi'
      fi
      env HIGHLIGHT_OPTIONS="${HIGHLIGHT_OPTIONS}" highlight \
        --out-format="${highlight_format}" \
        --force -- "${FILE_PATH}" && exit 5
      env COLORTERM=8bit bat --color=always --style="plain" \
        -- "${FILE_PATH}" && exit 5
      pygmentize -f "${pygmentize_format}" -O "style=${PYGMENTIZE_STYLE}"\
        -- "${FILE_PATH}" && exit 5
      exit 2;;


    ## Image
    image/*)
      ## Preview as text conversion
      # img2txt --gamma=0.6 --width="${PV_WIDTH}" -- "${FILE_PATH}" && exit 4
      exiftool "${FILE_PATH}" && exit 5
      exit 1;;

    ## Video and audio
    video/* | audio/*)
      mediainfo "${FILE_PATH}" && exit 5
      exiftool "${FILE_PATH}" && exit 5
      exit 1;;

    ## XLS
    *ms-excel)
      ## Preview as csv conversion
      ## xls2csv comes with catdoc:
      ##   http://www.wagner.pp.ru/~vitus/software/catdoc/
      xls2csv -- "${FILE_PATH}" && exit 5
      exit 1;;
  esac
}

MIMETYPE="$( file --dereference --brief --mime-type -- "${FILE_PATH}" )"
if [[ "${PV_IMAGE_ENABLED}" == 'True' ]]; then
  handle_image "${MIMETYPE}"
fi
handle_extension
handle_mime "${MIMETYPE}"
echo '----- File Type Classification -----' && file --dereference --brief -- "${FILE_PATH}" && exit 5
exit 1
