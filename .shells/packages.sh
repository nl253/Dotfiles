: ' {
ADDITIONAL POSSIBLE VARIABLES
-----------------------------
$RUBY_VERSION
$GO_VERSION
$GVM_ROOT=~/.gvm
$RBENV_ROOT=~/.rbenv
$RUBY_VERSION=~/.rbenv
$GIST_DIR=~/.gists
$GOPATH
$GOROOT
$DEFAULT_TOOLCHAIN=stable-x86_64-unknown-linux-gnu
$RUST_SRC_PATH=~/.multirust/toolchains/${DEFAULT_TOOLCHAIN}/lib/rustlib/src/rust/src

ADDITIONAL NOTES
----------------
Some languages may not build without the correct libraries such as gcc-libs or libc++.
If you dont have superuser privilidges then there is little you can do.
If you do have them, simply use the OS package manger to install them.

SUDO
----
If you dont mind running sudo commands you might want to set HAS_ADMIN=1,
which will also allow to sync pacman packages in $PACMAN_PACKAGES.
}'

# 1 or 0
HAS_ADMIN=0

PACMAN_PACKAGES=('ranger' 'htop' 'apache' 'aria2' 'aspell' 'autoconf'
	'automake' 'bash' 'bash-completion' 'bashmount' 'bc' 'binutils' 'bison' 'clang'
	'composer' 'coreutils' 'cronie' 'ctags' 'curl' 'elinks' 'erlang' 'flex' 'fzf' 'gawk'
	'gcc' 'gcc-libs' 'gimp' 'go' 'gparted' 'grep' 'hexchat' 'highlight' 'htop' 'hub'
	'hugo' 'jdk8-openjdk' 'jdk9-openj9-bin' 'jq' 'jre8-openjdk'
	'jre8-openjdk-headless' 'json-glib' 'jupyter-nbformat' 'jupyter-notebook'
	'less' 'lsb-release' 'lshw' 'lua' 'make' 'man-db' 'man-pages' 'mariadb' 'mariadb-clients' 'mathjax'
	'mongodb' 'mongodb-tools' 'mutt' 'ncurses' 'ngrep' 'nodejs' 'npm' 'openssh' 'parted' 'perl' 'php'
	'php-apache' 'php-cgi' 'php-codesniffer' 'php-mongodb' 'php-pgsql' 'php-phpdbg' 'php-sqlite' 'phpmyadmin'
	'phppgadmin' 'postgresql' 'postgresql-libs' 'powertop' 'ranger' 'redis' 'rsync' 'ruby' 'ruby-docs' 'sed'
	'shfmt' 'speedtest-cli' 'spotify-stable' 'sqlite' 'subversion' 'tar' 'thunderbird' 'tmux' 'tracker'
	'transmission-cli' 'tree' 'zsh')

# declare used python packages
PYTHON_PACKAGES=(
	'yapf' 'jedi' 'SQLAlchemy' 'yamllint' 'isort'
	'ipython' 'pylama' 'flake8' 'vulture' 'pycallgraph' 'mypy' 'pylint' 'ranger-fm'
	'proselint' 'profiling' 'pytest' 'psycopg2' 'mycli' 'docutils'
	'requests' 'Jinja2' 'Django' 'Flask' 'youtube-dl' 'cookiecutter')

# declare used Node.js packages
NODE_PACKAGES=('jest' 'typescript' 'write-good' 'htmlhint'
	'jsonlint' 'heroku-cli' 'yo' 'js-beautify' 'standard' 'uglify-es'
	'tern' 'gitbook' 'textlint' 'express-generator')

# declare used Php packages
PHP_PACKAGES=()

# declare used Haskell packages
HASKELL_PACKAGES=('pandoc' 'happy' 'shellcheck')

# declare used Go packages
GO_PACKAGES=()

# declare gists to keep in $GIST_DIR
GISTS=('122b12050f5fb267e75f' '7001839' '8172796' '8294792')

# declare used ruby gems
RUBY_GEMS=('travis' 'jekyll' 'sass' 'sqlint' 'mdl' 'scss_lint')

# declare used Rust crates
RUST_CRATES=('rustfmt' 'racer' 'mdbook' 'cargo-count' 'cargo-find' 'tokei')

# toolchain to use for Rust
export DEFAULT_TOOLCHAIN=nightly-x86_64-unknown-linux-gnu

# ---------------------------------------------------------------

# global requirements to intialise all package mangers
REQUIREMENTS=('curl' 'git')

: '
CHECKLIST {
---------
- [X] Python
- [X] Haskell
- [X] Ruby
- [X] Node
- [X] Go
- [X] Rust
- [X] Php
} '

: '{{
UTILS
-----

_add_to_path
============

Safely append dirs to $PATH.

args: directories you want to add to $PATH

NOTE
----
They WILL be checked to see if they exist.

_in_array
=========

args:
  $1 the item you want to check
  the rest is the contentn of the "array"

EXAMPLE
-------

_in_array c x y z s k c

returns 0

_in_path
========

Safely check if the item is in $PATH.

args:
  $1 item you want to check for.

}'

_add_to_path() {
	for directory in "$@"; do
		[[ -d $directory ]] && [[ ! $PATH =~ $directory ]] && export PATH="$directory:$PATH:" 2>/dev/null
	done
}

_in_array() {
	for i in "${@:1}"; do
		[[ $i == $2 ]] && return 0
	done
	return 1
}

_in_path() {
	# checks  an executable is in $PATH
	for i in $(echo "$PATH" | sed "s/:/\n/g"); do
		[[ -x "$i/$1" ]] && return 0
	done
	return 1
}

# }

_gists() {
	[[ -z $GIST_DIR ]] && export GIST_DIR=~/.gists
	if [[ -n $GISTS ]]; then
		[[ ! -e $GIST_DIR ]] && mkdir -p $GIST_DIR
		for i in "${GISTS[@]}"; do
			if [[ ! -e ${GIST_DIR}/${i} ]]; then
				git clone "https://gist.github.com/${i}.git" "${GIST_DIR}/${i}"
			fi
		done
	fi
}

_init_python() {

	_add_to_path ~/.local/bin

	# Don't change the order!
	[[ -z $PYENV_ROOT ]] && export PYENV_ROOT=~/.pyenv
	[[ -z $PYENV_VERSION ]] && export PYENV_VERSION=3.6.1
	_add_to_path "${PYENV_ROOT}/bin"

	eval "$(pyenv init -)"
	eval "$(pyenv virtualenv-init -)"
}

_install_python(){

	# install pyenv if missing
	[[ ! -e $PYENV_ROOT ]] && git clone https://github.com/pyenv/pyenv.git "${PYENV_ROOT}"

	# install the viruatlenv plugin if missing
	if [[ ! -e "${PYENV_ROOT}/plugins/pyenv-virtualenv" ]]; then
		git clone https://github.com/pyenv/pyenv-virtualenv.git "${PYENV_ROOT}/plugins/pyenv-virtualenv"
	fi

	if [[ $(pyenv versions) =~ $(echo $PYENV_VERSION | sed -E 's/\./\\./g') ]]; then
		pyenv install $PYENV_VERSION 2>/dev/null
	fi

}

_packages_python() {
	if [[ -n $PYTHON_PACKAGES ]] && _in_path pip; then
		# DO STUFF
		local packages=$(pip list --format=legacy | sed -E 's/\(.*\)//')
		for i in "${PYTHON_PACKAGES[@]}"; do
			_in_array "${i}" "${packages}" && pip install --retries 3 --timeout 10 --pre --user "${i}" 2>/dev/null
		done
	fi
}

_init_ruby() {
	[[ -z $RUBY_VERSION ]] && export RUBY_VERSION=2.4.1
	[[ -z $RBENV_ROOT ]] && export RBENV_ROOT=~/.rbenv
	_add_to_path "${RBENV_ROOT}/bin"
	_add_to_path "${RBENV_ROOT}/bin"
	_add_to_path ~/.gem/ruby/*/bin
	eval "$(rbenv init -)"
}

_packages_ruby() {
	if [[ -n $RUBY_GEMS ]] && _in_path gem; then
		local packages=$(gem list | sed -E 's/\(.*\)//')
		for i in "${RUBY_GEMS[@]}"; do
			_in_array "${i}" "${packages}" && gem install "${i}" 2>/dev/null
		done
	fi
}

_install_ruby() {
	if [[ ! -e $RBENV_ROOT ]]; then
		git clone https://github.com/rbenv/rbenv.git "${RBENV_ROOT}"
	fi
	if [[ ! -e "${RBENV_ROOT}/plugins/ruby-build" ]]; then
		git clone https://github.com/rbenv/ruby-build.git "${RBENV_ROOT}/plugins/ruby-build"
		[[ $HAS_ADMIN == 1 ]] && sudo ~/.rbenv/plugins/ruby-build/install.sh
	fi
	if [[ $(rbenv versions) =~ $(echo $RUBY_VERSION | sed -E 's/\./\\./g') ]]; then
		rbenv install $RUBY_VERSION
	fi
}

_init_node() {
	_add_to_path ~/{.yarn,node_modules}/bin
}

_install_node() {
	if ! _in_path node; then
		if _in_path curl; then
			curl -o- https://raw.githubusercontent.com/creationix/nvm/v0.33.4/install.sh | bash
		elif _in_path wget; then
			wget -qO- https://raw.githubusercontent.com/creationix/nvm/v0.33.4/install.sh | bash
		fi
	fi
}

_packages_node() {

    if _in_path yarn; then
        local install_command="yarn global add"
        local packages=$(yarn global list 2>/dev/null)

    elif _in_path npm; then
        if [[ $HAS_ADMIN == 1 ]]; then
            local packages=$(sudo npm list --global --depth=0 2>/dev/null)
            local install_command="sudo npm install -g"
        else
            local packages=$(npm list --depth=0 2>/dev/null)
            local install_command="npm install"
        fi
    else
        return 1
    fi

    for i in "${NODE_PACKAGES[@]}"; do
        if ! _in_array "${i}" "${packages}"; then
            eval "${install_command} ${i}"
        fi
    done
}

_init_haskell() {
	_add_to_path ~/.{stack,cabal}/bin
}

_install_haskell() {
    # look for stack
	# curl needed, wget as fallback,
	if ! _in_path stack; then
		if _in_path curl; then
			curl -sSL https://get.haskellstack.org/ | sh
		elif _in_path wget; then
			wget -qO- https://get.haskellstack.org/ | sh
		fi
	fi
}

_packages_haskell() {
	for i in "${HASKELL_PACKAGES[@]}"; do
		stack install "${i}"
	done
}

_packages_rust() {
	if [[ -n $RUST_CRATES ]] && _in_path cargo; then
		for i in "${RUST_CRATES[@]}"; do
			if ! _in_path i && [[ ! -e ~/.cargo/bin/${i} ]]; then
				rustup run "${DEFAULT_TOOLCHAIN}" cargo install --all-features "${i}" 2>/dev/null
			fi
		done
	fi
}

_init_rust() {
	[[ -z $DEFAULT_TOOLCHAIN ]] && export DEFAULT_TOOLCHAIN=stable-x86_64-unknown-linux-gnu
	[[ -z $RUST_SRC_PATH ]] && export RUST_SRC_PATH=~/.multirust/toolchains/${DEFAULT_TOOLCHAIN}/lib/rustlib/src/rust/src
	_add_to_path ~/.cargo/bin
}

_install_rust(){
	# install Rust using rustup if Rust is missing
	if ! _in_path rustup && _in_path curl; then
		curl https://sh.rustup.rs -sSf | sh
		rustup toolchain install "${DEFAULT_TOOLCHAIN}"
		rustup default "${DEFAULT_TOOLCHAIN}"
	fi
}

_init_go() {
	[[ -z $GO_VERSION ]] && export GO_VERSION=1.9
	export GOPATH=$HOME/Projects/Go
	_add_to_path {$GOROOT,$GOPATH,/usr/local/go}/bin
}

_install_go() {
	if [[ ! -e ~/.gvm ]]; then
		curl -s -S -L https://raw.githubusercontent.com/moovweb/gvm/master/binscripts/gvm-installer | bash
	fi
	if [[ ! $(gvm version) == $GO_VERSION ]]; then
		nvm install "${GO_VERSION}"
	fi
}

_packages_go() {
	echo ""
}

_init_php() {
	_add_to_path ~/.config/composer/vendor/bin
}

_packages_php() {
	echo ""
}

_install_php() {
	echo ""
}

_packages_pacman() {

    # only run on systems that have pacman
	if _in_path pacman && _in_path xargs && [[ -n $PACMAN_PACKAGES ]] && [[ "${HAS_ADMIN}" == 1 ]]; then
	    # query local database, quiet mode, just names
		local packages=$(pacman -Qq | xargs)
		# break immediately on any isse to avoid endless loops
		for i in "${PACMAN_PACKAGES[@]}"; do
			if ! _in_array "${i}" "${packages}"; then
				sudo pacman -S "${i}" || break
			fi
		done
	fi
}

main() {

    for i in "${REQUIREMENTS[@]}"; do
        if ! _in_path $i; then
            exit 1
        fi
    done

	local _cwd=$(pwd)

	cd

	for language in python ruby haskell go node php; do
		eval "_init_${language}"
	done

	cd "${_cwd}"
}

install_package_managers(){
  _packages_pacman
  _gists
  for i in python ruby haskell php go rust node; do
    eval "_init_${i}"
    eval "_install_${i}"
    eval "_init_${i}" # this IS necessary!
    eval "_packages_${i}"
  done
}

main

# vim: nowrap foldmarker={,} foldmethod=marker
