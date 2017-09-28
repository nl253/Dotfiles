
# ~/.zshrc

fpath+=~/.zsh/zfunc

# Load generic shell configuration (aliases, variables)
for i in ~/.shells/*.sh; do
	[[ -f $i ]] && source $i 
done 

# If ~/.home or ~/.pc detected, don't source this script!
for i in ~/.{home,pc}; do
	[[ -e $i ]] && echo -e "detected ${i} - .zshrc not sourced" && return 0
done

# Not using Rust atm
# zplug "plugins/cargo", from:oh-my-zsh 
# zplug "plugins/rust", from:oh-my-zsh 
# zplug "plugins/z", from:oh-my-zsh

# zplug denysdovhan/spaceship-zsh-theme, use:spaceship.zsh, from:github, as:theme

# Not using Python atm
# zplug "plugins/extract", from:oh-my-zsh
# zplug "plugins/pip", from:oh-my-zsh

# NVM is not used very often
# zplug "lukechilds/zsh-nvm"

# Extra overhead?
# zplug "jreese/zsh-titles"
# zplug "zsh-users/zsh-history-substring-search"

# zplug "zdharma/fast-syntax-highlighting"
# zplug "zsh-users/zsh-autosuggestions"
# zplug "zsh-users/zsh-completions"

# zplug "nl253/Scripts", as:command, rename-to:"csv-preview", use:"csv-preview.sh"
# zplug "nl253/Scripts", as:command, rename-to:"download-dotfile", use:"download-dotfile.sh"
# zplug "nl253/Scripts", as:command, rename-to:"grf", use:"grf.sh"
# zplug "nl253/Scripts", as:command, rename-to:"p", use:"processes.sh"

# zplug "tmux-plugins/tpm", as:command, ignore:'*'

# for i in completion options; do
	# zplug "nl253/zsh-config-${i}", defer:3
# done

setopt monitor

bindkey -e

# zplug load

# vim: foldmethod=marker sw=2 ts=2 nowrap 
