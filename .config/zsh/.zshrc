
# $ZDOTDIR/.zshrc refer to zshoptions(1)

[[ -f ~/.config/sh/init.sh ]] && source ~/.config/sh/init.sh
FPATH+=~/.config/zsh/zfunc
PS1='$(echo "\n$(dirs)\n$(pwd)") >> ' 

autoload -z edit-command-line
zle -N edit-command-line
bindkey '^x^e' edit-command-line

autoload -Uz compinit && compinit
autoload -Uz bashcompinit && bashcompinit &&
[[ -f $ZDOTDIR/../bash/completions.sh ]] && source $ZDOTDIR/../bash/completions.sh
zmodload zsh/complete
zmodload zsh/complist

WORDCHARS='"*?_|-.[]~=/&;!#$%^(){}<>'

backward-kill-dir() {
	local WORDCHARS=${WORDCHARS/\/}
	zle backward-kill-word
}

tcsh-backward-word() {
	local WORDCHARS="${WORDCHARS:s@/@}"
	zle backward-word
}

zle -N backward-kill-dir
bindkey '^[^?' backward-kill-dir
zle -N tcsh-backward-word

# zstyle ':completion:*' group-name ''
# zstyle ':completion:::::' completer _complete _approximate
zstyle ':completion:*:*:-subscript-:*' tag-order indexes parameters
zstyle ':completion:*:*:kill:*:processes' list-colors "=(#b) #([0-9]#)*=$color[cyan]=$color[red]"
zstyle ':completion::*:(-command-|export):*' fake-parameters ${${${_comps[(I)-value-*]#*,}%%,*}:#-*-}
zstyle ':completion:*' use-cache on
zstyle ':completion:*' cache-path ~/.config/zsh/.cache
zstyle ':completion:*' completer _complete _match _approximate
zstyle ':completion:*' list-colors "${(@s.:.)LS_COLORS}"
zstyle ':completion:*' matcher-list '' 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=*' 'l:|=* r:|=*'
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
zstyle ':completion:*:functions' ignored-patterns '_*'
 
# Don't complete uninteresting users
zstyle ':completion:*:*:*:users' ignored-patterns \
        adm amanda at avahi avahi-autoipd beaglidx bin cacti canna \
        clamav daemon dbus distcache dnsmasq dovecot fax games gdm \
        gkrellmd gopher hacluster haldaemon halt hsqldb ident junkbust kdm \
        ldap lp mail mailman mailnull man messagebus  mldonkey mysql nagios \
        named netdump grub news nfsnobody nobody nscd ntp nut nx obsrun openvpn \
        operator pcap polkitd postfix postgres privoxy pulse pvm quagga radvd \
        rpc rpcuser rpm rtkit scard squid statd svn sync tftp \
				grub-bios-setup grub-editenv grub-file grub-fstest grub-glue-efi grub-install \
				usbmux uucp vcsa wwwrun xfs '_*' 

[[ -f $ZDOTDIR/options.zsh ]] && source $ZDOTDIR/options.zsh
# vim: foldmethod=marker sw=2 ts=2 nowrap 
