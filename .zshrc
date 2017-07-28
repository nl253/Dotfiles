
# ~/.zshrc

export GOPATH=/usr/lib64/go

for i in ~/.pc; do
	[[ -e $i ]] && echo -e "detected ${i} - .zshrc not sourced" && return 0
done

for i in ~/.home; do
	[[ ! -e $i ]] && echo -e "not detected ${i} - .zshrc not sourced" && return 0
done

export PYTHON_PACKAGES=(ranger ipython pylint pycodestyle yapf yamllint isort proselint profiling pytest pudb3 youtube-dl)
export NODE_PACKAGES=(jshint js-beautify stylelint textlint write-good csslint tern eslint remark stylus coffee coffeelint prettier)
export GISTS=("122b12050f5fb267e75f" "7001839" "8172796" "8294792")
export RUBY_GEMS=(travis)
export RUST_CRATES=(rustfmt racer mdbook)
# export RBENV_VERSION="2.4.0"
# export PYENV_VERSION="3.6.1"
export DEFAULT_TOOLCHAIN=nightly-x86_64-unknown-linux-gnu

[[ ! -e ~/.zplug ]] && curl -sL --proto-redir -all,https https://raw.githubusercontent.com/zplug/installer/master/installer.zsh | zsh

source ~/.zplug/init.zsh

zplug 'zplug/zplug', hook-build:'zplug --self-manage' 

zplug "plugins/cargo", from:oh-my-zsh 
zplug "plugins/rust", from:oh-my-zsh 
zplug "plugins/z", from:oh-my-zsh

zplug denysdovhan/spaceship-zsh-theme, use:spaceship.zsh, from:github, as:theme

zplug "plugins/extract", from:oh-my-zsh
zplug "plugins/pip", from:oh-my-zsh

zplug "jreese/zsh-titles"
zplug "zdharma/fast-syntax-highlighting"
zplug "zsh-users/zsh-autosuggestions"
zplug "zsh-users/zsh-completions"
zplug "zsh-users/zsh-history-substring-search"

zplug "nl253/Scripts", as:command, rename-to:"csv-preview", use:"csv-preview.sh"
zplug "nl253/Scripts", as:command, rename-to:"download-dotfile", use:"download-dotfile.sh"
zplug "nl253/Scripts", as:command, rename-to:"grf", use:"grf.sh"
zplug "nl253/Scripts", as:command, rename-to:"p", use:"processes.sh"

#zplug "nl253/SQLiteREPL", as:command, rename-to:"sqlite", use:"main.py", if:"(( $(python --version | grep -Eo '[0-9]\.[0-9]\.[0-9]' | sed -E 's/\.//g') >= 360 ))"
zplug "nl253/ProjectGenerator", as:command, use:"project", if:"(( $(python --version | grep -Eo '[0-9]\.[0-9]\.[0-9]' | sed -E 's/\.//g') >= 360 ))"
zplug "nl253/DictGen", as:command, use:"dict-gen", if:"[[ -x $(which python3) ]]"

zplug "junegunn/fzf-bin", from:gh-r, as:command, rename-to:"fzf", use:"*linux*amd64*"
zplug "tmux-plugins/tpm", as:command, ignore:'*'
zplug "getpelican/pelican-plugins", as:command, ignore:'*'
zplug "getpelican/pelican-themes", as:command, ignore:'*'

for i in {completion,variables,source,functions,aliases,options,fzf,packages}; do
	if [[ -e ~/Projects/ZshPlugins/$i ]]; then
		zplug "~/Projects/ZshPlugins/${i}", from:local, use:'*.zsh', defer:3
	else
		zplug "nl253/zsh-config-${i}", defer:3
	fi
done

zplug load 

setopt monitor

# PYTHON

for i in 'better_exceptions' 'faker' 'numpy' 'pandas' 'ipdb' 'jedi'; do
	[[ ! -e ~/.local/lib/python3.6/site-packages/${i} ]] && pip install --user $i
done

[[ ! -e ~/.local/bin/vint ]] && pip install --user vim-vint

# vim: foldmethod=marker sw=2 ts=2 nowrap
