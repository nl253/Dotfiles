
# ~/.zshrc

[[ ! -e ~/.zplug ]] && curl -sL --proto-redir -all,https https://raw.githubusercontent.com/zplug/installer/master/installer.zsh | zsh

source ~/.zplug/init.zsh

zplug 'zplug/zplug', hook-build:'zplug --self-manage'

zplug "jreese/zsh-titles"
zplug "zdharma/fast-syntax-highlighting"
zplug "zsh-users/zsh-autosuggestions"
zplug "zsh-users/zsh-completions"
zplug "zsh-users/zsh-history-substring-search"

zplug "ranger/ranger", as:command, use:"ranger.py", rename-to:'ranger'

zplug "nl253/Scripts", as:command, rename-to:"csv-preview", use:"csv-preview.sh"
zplug "nl253/Scripts", as:command, rename-to:"extractor", use:"extractor.sh"
zplug "nl253/Scripts", as:command, rename-to:"download-dotfile", use:"download-dotfile.sh"
zplug "nl253/Scripts", as:command, rename-to:"grf", use:"grf.sh"
zplug "nl253/Scripts", as:command, rename-to:"p", use:"processes.sh"

zplug "nl253/SQLiteREPL", as:command, rename-to:"sqlite", use:"main.py", if:"(( $(python --version | grep -Eo '[0-9]\.[0-9]\.[0-9]' | sed -E 's/\.//g') >= 360 ))"
zplug "nl253/ProjectGenerator", as:command, use:"project", if:"(( $(python --version | grep -Eo '[0-9]\.[0-9]\.[0-9]' | sed -E 's/\.//g') >= 360 ))"
zplug "nl253/DictGen", as:command, use:"dict-gen", if:"[[ -x $(which python3) ]]"

zplug "junegunn/fzf-bin", from:gh-r, as:command, rename-to:"fzf", use:"*linux*amd64*"

zplug "tmux-plugins/tpm", as:command, ignore:'*'

zplug "~/.shells", from:local, use:"{variables,source,fzf,aliases}.sh"
zplug "~/.zsh", from:local, ignore:'*.sh', use:"{variables,source,functions,aliases,options}.zsh"

zplug "pyenv/pyenv", as:command, hook-load:"source ${HOME}/.zsh/variables.zsh", ignore:'*'
#zplug voronkovich/gitigore.plugin.zsh

zplug load 
setopt monitor
# vim: foldmethod=marker
