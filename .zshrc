
# ~/.zshrc

if [[ -e ~/.pc ]] || [[ ! -e ~/.home ]] || [[ ! -x $(which git) ]] || [[ ! -x $(which curl) ]] || [[ ! -x $(which python3) ]]; then
	echo -e "requirements not satified .zshrc not sourced" && return 0
fi

[[ ! -d ~/.gists/ ]] && mkdir -p ~/.gists/

for i in "122b12050f5fb267e75f" "7001839" "8172796" "8294792"; do
	if [[ ! -d ~/.gists/$i ]]; then
			git clone "https://gist.github.com/${i}.git" ~/.gists/$i
	fi
done

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

zplug "nl253/Scripts", as:command, rename-to:"csv-preview", use:"csv-preview.sh"
zplug "nl253/Scripts", as:command, rename-to:"download-dotfile", use:"download-dotfile.sh"
zplug "nl253/Scripts", as:command, rename-to:"grf", use:"grf.sh"
zplug "nl253/Scripts", as:command, rename-to:"p", use:"processes.sh"

zplug "nl253/SQLiteREPL", as:command, rename-to:"sqlite", use:"main.py", if:"(( $(python --version | grep -Eo '[0-9]\.[0-9]\.[0-9]' | sed -E 's/\.//g') >= 360 ))"
zplug "nl253/ProjectGenerator", as:command, use:"project", if:"(( $(python --version | grep -Eo '[0-9]\.[0-9]\.[0-9]' | sed -E 's/\.//g') >= 360 ))"
zplug "nl253/DictGen", as:command, use:"dict-gen", if:"[[ -x $(which python3) ]]"

zplug "junegunn/fzf-bin", from:gh-r, as:command, rename-to:"fzf", use:"*linux*amd64*"
zplug "tmux-plugins/tpm", as:command, ignore:'*'
zplug "getpelican/pelican-plugins", as:command, ignore:'*'
zplug "getpelican/pelican-themes", as:command, ignore:'*'

for i in {variables,source,functions,aliases,options,fzf}; do
	if [[ -e ~/Projects/ZshPlugins/$i ]]; then
		zplug "~/Projects/ZshPlugins/${i}", from:local, use:'*.zsh', defer:3
	else
		zplug "nl253/zsh-config-${i}", defer:3
	fi
done

zplug load 

setopt monitor

# PYTHON

[[ ! -e ~/.pyenv ]] && curl -L https://raw.githubusercontent.com/pyenv/pyenv-installer/master/bin/pyenv-installer | bash

export PYENV_ROOT=~/.pyenv
export PATH="${PATH}:${PYENV_ROOT}/bin"
eval "$(pyenv init -)" 
eval "$(pyenv virtualenv-init -)"

[[ ! -e ${PYENV_ROOT}/plugins/pyenv-virtualenv ]] && git clone https://github.com/pyenv/pyenv-virtualenv.git ${PYENV_ROOT}/plugins/pyenv-virtualenv

[[ ! $(pyenv versions) =~ '3\.6\.1' ]] && pyenv install 3.6.1

export PYENV_VERSION="3.6.1"

for i in ranger ipython pylint flake8 pycodestyle yapf yamllint isort proselint profiling pytest pudb3 youtube-dl; do
	[[ ! -x $(which $i) ]] && [[ ! -e ~/.local/bin/$i ]] && pip install --user $i
done

[[ ! -e ~/.local/bin/vint ]] && pip install --user git+http://www.github.com/Kuniwak/vint.git

for i in 'better_exceptions' 'faker' 'numpy' 'pandas' 'ipdb' 'jedi'; do
	[[ ! -e ~/.local/lib/python3.6/site-packages/${i} ]] && pip install --user $i
done

# RUBY

[[ ! -e ~/.rbenv ]] && git clone https://github.com/rbenv/rbenv.git ~/.rbenv && cd ~/.rbenv && src/configure && make -C src && cd

export PATH="${HOME}/.rbenv/bin:${PATH}"
eval "$(rbenv init -)"

[[ ! -e ~/.rbenv/plugins/ruby-build ]] && git clone https://github.com/rbenv/ruby-build.git ~/.rbenv/plugins/ruby-build

export RBENV_VERSION="2.4.0"

for i in travis; do
	[[ ! -x $(which $i) ]] && gem install $i
done

# JAVASCRIPT 

if [[ -x $(which npm) ]]; then
	for i in jshint js-beautify stylelint textlint write-good csslint tern eslint remark stylus coffee coffeelint prettier; do
		[[ ! -x $(which $i) ]] && npm install $i
	done
fi

# RUST

[[ ! -x $(which rustc) ]] && curl https://sh.rustup.rs -sSf | sh && rustup toolchain install nightly && rustup default nightly

for i in rustfmt racer; do
	[[ ! -x $(which i) ]] && [[ ! -e ~/.cargo/bin/${i} ]] && cargo install $i
done

# vim: foldmethod=marker sw=2 ts=2 nowrap
