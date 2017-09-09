
# ~/.zshrc

fpath+=~/.zsh/zfunc

export GOPATH=/usr/lib64/go

# for i in ~/.{shells,zsh}/*.sh; do

for i in ~/.shells/*.sh; do
	[[ -f $i ]] && source $i 
done 

for i in ~/.{home,pc}; do
	[[ -e $i ]] && echo -e "detected ${i} - .zshrc not sourced" && return 0
done

export DOCUTILSCONFIG=~/.docutils
export PYTHON_PACKAGES=(\
	yapf jedi SQLAlchemy yamllint isort \
	ipython flake8 vulture pycallgraph mypy pylint ranger-fm \
	proselint profiling pytest psycopg2 mycli docutils \
	requests Jinja2 Django Flask youtube-dl cookiecutter)
export NODE_PACKAGES=(write-good heroku-cli yo js-beautify tern gitbook textlint)
export GISTS=('122b12050f5fb267e75f' '7001839' '8172796' '8294792')
export RUBY_GEMS=(travis sass) # scss_lint
export RUST_CRATES=(rustfmt racer mdbook cargo-count cargo-find tokei)
export DEFAULT_TOOLCHAIN=nightly-x86_64-unknown-linux-gnu

[[ ! -e ~/.zplug ]] && curl -sL --proto-redir -all,https \
	https://raw.githubusercontent.com/zplug/installer/master/installer.zsh | zsh

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

zplug "tmux-plugins/tpm", as:command, ignore:'*'

for i in completion options packages; do
	if [[ -e ~/Projects/Zsh/zsh-${i} ]]; then
		zplug ~/Projects/Zsh/zsh-${i}, from:local, use:'*.zsh', defer:3
	else
		zplug "nl253/zsh-config-${i}", defer:3
	fi
done

zplug load 

setopt monitor

bindkey -e

[[ $- == *i* ]] && source "${HOME}/.fzf/shell/completion.zsh" 2> /dev/null
[[ -f ~/.fzf/shell/key-bindings.zsh ]] && source ~/.fzf/shell/key-bindings.zsh 

# vim: foldmethod=marker sw=2 ts=2 nowrap
