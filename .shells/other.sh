#alias freq='cut -f1 -d" " "$HISTFILE" | sort | uniq -c | sort -nr | head -n 30' # frequent entries from history
#alias timer='echo "Timer started. Stop with Ctrl-D." && date && time cat && date' # stopwatch
#$(in-path aspell) && alias aspell="aspell -c -l en_GB"
# utils : timer, clipboard, 
#$(in-path xclip) && alias pbcopy='xclip -selection clipboard' && alias pbpaste='xclip -selection clipboard -o' 
# keymap  {{{
#if $(in-path xmodmap); then
    #alias map-caps-to-esc='xmodmap -e "clear lock"; xmodmap -e "keycode 0x42 = Escape"'
    #alias unmap-caps-from-esc='xmodmap -e "keycode 0x42 = Caps_Lock"; xmodmap -e "add lock = Caps_Lock"'
#fi
#$(in-path setxkbmap) && alias map-caps-lock-to-ctrl='setxkbmap -layout gb -option ctrl:nocaps && echo -e "${MAGENTA}capslock remapped to ctrl${DEFCOLOR}"'
##  }}}
