# $ZDOTDIR/.zshrc read by zsh(1) when invoked as an interactive shell

[[ -d ~/.config/sh      ]] && SHDOTDIR=~/.config/sh
[[ -d ~/.config/bash    ]] && BASHDOTDIR=~/.config/bash
[[ -f $SHDOTDIR/init.sh ]] && source $SHDOTDIR/init.sh

#PS1='%~ %F{magenta}λ%F{white} '
WORDCHARS=''
FPATH=~/.config/zsh/zfunc:$FPATH
DIRSTACKSIZE=10
READNULLCMD='less'

# completion
autoload -Uz compinit && \
  compinit && \
  zmodload zsh/complete && \
  zmodload zsh/complist 

autoload -U select-word-style
zle -N select-word-style

autoload -Uz edit-command-line
zle -N edit-command-line
bindkey '^x^e' edit-command-line

autoload -Uz vcs_info
zstyle ':vcs_info:*' enable git

zstyle ':completion:*:*:-subscript-:*' tag-order indexes parameters
zstyle ':completion:*:*:kill:*:processes' list-colors "=(#b) #([0-9]#)*=$color[cyan]=$color[red]"
zstyle ':completion::*:(-command-|export):*' fake-parameters ${${${_comps[(I)-value-*]#*,}%%,*}:#-*-}
zstyle ':completion:*' use-cache on
zstyle ':completion:*' cache-path ~/.config/zsh/.cache
zstyle ':completion:*' completer _complete _match _approximate
zstyle ':completion:*' list-colors "${(@s.:.)LS_COLORS}"
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}' 'e:|[._-]=* e:|=*' 'l:|=* e:|=*'
#zstyle ':completion:*' matcher-list '' 'm:{a-zA-Z}={A-Za-z}' 'r:|=*' 'l:|=* r:|=*'
#zstyle ':completion:*' matcher-list '' 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=*' 'l:|=* r:|=*'
zstyle ':completion:*' menu select
zstyle ':completion:*:corrections' format "- %d - (errors %e})"
zstyle ':completion:*:default' list-prompt '%S%M matches%s'
zstyle ':completion:*:descriptions' format "    # %d"
zstyle ':completion::approximate*:*' prefix-needed false
zstyle ':completion:match-word:*' insert-unambiguous true
zstyle -e ':completion:*:approximate:*' max-errors 'reply=( $(( ($#PREFIX + $#SUFFIX) / 3 )) )'

bindkey -M menuselect '^[[Z' reverse-menu-complete
if [[ "${terminfo[kcbt]}" != "" ]]; then
  bindkey "${terminfo[kcbt]}" reverse-menu-complete   # [Shift-Tab] - move through the completion menu backwards
else
  bindkey '^[[Z' reverse-menu-complete
fi
  
zstyle ':completion:*:*:*:*:processes' command "ps -u $USER -o pid,comm -w -w"

# ignore uninteresting files
for i in files paths all-files; do 
  zstyle ":completion:*:$i" ignored-patterns 'tags' '*.swp' '*.bak' '*history*' '*cache*' '*.pyc' '*.class' '*.o' '*.so' '*.fls' '*.lock' '*.iml' '*.aux' '*.hi' '*.beam' '*.toc' '*.fdb_latexmk'
done

# ignore functions that start with an underscore (typically private to a script)
zstyle ':completion:*:functions' ignored-patterns '_*'

# Don't complete uninteresting users
zstyle ':completion:*:*:*:users' ignored-patterns \
  adm amanda at avahi avahi-autoipd beaglidx bin cacti canna clamav daemon \
  dbus distcache dnsmasq dovecot fax games gdm gkrellmd gopher hacluster \
  haldaemon halt hsqldb ident junkbust kdm ldap lp mail mailman mailnull \
  man messagebus  mldonkey mysql nagios named netdump grub news nfsnobody \
  nobody nscd ntp nut nx obsrun openvpn operator pcap polkitd postfix postgres \
  privoxy pulse pvm quagga radvd rpc rpcuser rpm rtkit scard squid statd svn \
  sync tftp grub-bios-setup grub-editenv grub-file grub-fstest grub-glue-efi \
  grub-install usbmux uucp vcsa wwwrun xfs '_*' 

# $ZDOTDIR/options.zsh shell options for zsh(1)

# this is necessary because the string "vim" is present in $EDITOR zsh will attempt to set ZLE to use vi mode
setopt AUTO_LIST
setopt AUTO_PUSHD
setopt BARE_GLOB_QUAL
setopt BRACE_CCL
setopt CDABLE_VARS
setopt CHASE_DOTS
setopt COMPLETE_ALIASES
setopt COMPLETE_IN_WORD
setopt CSH_NULLCMD
setopt EMACS
setopt EQUALS
setopt EXTENDED_GLOB
setopt GLOB_DOTS
setopt GLOB_STAR_SHORT
setopt HIST_EXPIRE_DUPS_FIRST
setopt HIST_FCNTL_LOCK
setopt HIST_FIND_NO_DUPS
setopt HIST_IGNORE_ALL_DUPS
setopt HIST_IGNORE_DUPS
setopt HIST_IGNORE_SPACE
setopt HIST_NO_FUNCTIONS
setopt HIST_NO_STORE
setopt HIST_REDUCE_BLANKS
setopt HIST_SAVE_NO_DUPS
setopt HIST_SUBST_PATTERN
setopt HIST_VERIFY
setopt INC_APPEND_HISTORY
setopt INTERACTIVE_COMMENTS
setopt KSH_GLOB
setopt LONG_LIST_JOBS
setopt MAGIC_EQUAL_SUBST
setopt MARK_DIRS
setopt MENU_COMPLETE
setopt NO_BEEP
setopt NO_BG_NICE
setopt NO_HIST_BEEP
setopt NO_HUP
setopt NO_LIST_BEEP
setopt POSIX_IDENTIFIERS
setopt PROMPT_SUBST
setopt PUSHD_IGNORE_DUPS
setopt PUSHD_TO_HOME
setopt RC_EXPAND_PARAM
setopt SHARE_HISTORY
setopt SHORT_LOOPS

# vim: foldmethod=marker sw=2 ts=2 nowrap 
