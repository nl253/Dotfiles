
# ~/.zshrc

[[ ! -d ~/.gists/ ]] && mkdir -p ~/.gists/
for i in 122b12050f5fb267e75f 7001839 8172796 8294792; do
	if [[ ! -d ~/.gists/$i ]]; then
			git clone "https://gist.github.com/${i}.git" ~/.gists/$i
	fi
done

[[ -e ~/.pc ]] && return 0

[[ ! -e ~/.zplug ]] && curl -sL --proto-redir -all,https https://raw.githubusercontent.com/zplug/installer/master/installer.zsh | zsh

source ~/.zplug/init.zsh

zplug 'zplug/zplug', hook-build:'zplug --self-manage'

zplug "plugins/cargo", from:oh-my-zsh
zplug "plugins/z", from:oh-my-zsh

zplug denysdovhan/spaceship-zsh-theme, use:spaceship.zsh, from:github, as:theme

zplug "plugins/extract", from:oh-my-zsh
zplug "plugins/pip", from:oh-my-zsh

zplug "jreese/zsh-titles"
zplug "zdharma/fast-syntax-highlighting"
zplug "zsh-users/zsh-autosuggestions"
zplug "zsh-users/zsh-completions"
zplug "zsh-users/zsh-history-substring-search"

zplug "ranger/ranger", as:command, use:"ranger.py", rename-to:'ranger'

zplug "nl253/Scripts", as:command, rename-to:"csv-preview", use:"csv-preview.sh"
zplug "nl253/Scripts", as:command, rename-to:"download-dotfile", use:"download-dotfile.sh"
zplug "nl253/Scripts", as:command, rename-to:"grf", use:"grf.sh"
zplug "nl253/Scripts", as:command, rename-to:"p", use:"processes.sh"

zplug "nl253/SQLiteREPL", as:command, rename-to:"sqlite", use:"main.py", if:"(( $(python --version | grep -Eo '[0-9]\.[0-9]\.[0-9]' | sed -E 's/\.//g') >= 360 ))"
zplug "nl253/ProjectGenerator", as:command, use:"project", if:"(( $(python --version | grep -Eo '[0-9]\.[0-9]\.[0-9]' | sed -E 's/\.//g') >= 360 ))"
zplug "nl253/DictGen", as:command, use:"dict-gen", if:"[[ -x $(which python3) ]]"

zplug "junegunn/fzf-bin", from:gh-r, as:command, rename-to:"fzf", use:"*linux*amd64*"

zplug "tmux-plugins/tpm", as:command, ignore:'*'

#zplug "getpelican/pelican-plugins", as:command, ignore:'*'

for i in {variables,source,functions,aliases,options,fzf}; do
	if [[ -e ~/Projects/ZshPlugins/$i ]]; then
		zplug "~/Projects/ZshPlugins/${i}", from:local, use:'*.zsh', defer:3
	else
		zplug "nl253/zsh-config-${i}", defer:3
	fi
done

zplug "pyenv/pyenv", as:command, hook-load:"source ${HOME}/.zsh/variables.zsh", ignore:'*'

zplug load 

setopt monitor
# vim: foldmethod=marker sw=2 ts=2
