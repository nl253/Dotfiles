
export PACMAN_PACKAGES=("ranger" "htop" "apache" "aria2" "aspell" "autoconf"
	"automake" "bash" "bash-completion" "bashmount" "bc" "binutils" "bison" "clang"
	"composer" "coreutils" "cronie" "ctags" "curl" "elinks" "erlang" "flex" "fzf" "gawk"
	"gcc" "gcc-libs" "gimp" "go" "gparted" "grep" "hexchat" "highlight" "htop" "hub"
	"hugo" "java-environment-common" "java-runtime-common" "jdk8-openjdk" "jdk9-openj9-bin"
	"jq" "jre8-openjdk" "jre8-openjdk-headless" "json-glib" "jupyter-nbformat" "jupyter-notebook"
	"less" "lsb-release" "lshw" "lua" "make" "man-db" "man-pages" "mariadb" "mariadb-clients" "mathjax"
	"mongodb" "mongodb-tools" "mutt" "ncurses" "ngrep" "nodejs" "npm" "openssh" "parted" "perl" "php"
	"php-apache" "php-cgi" "php-codesniffer" "php-mongodb" "php-pgsql" "php-phpdbg" "php-sqlite" "phpmyadmin"
	"phppgadmin" "postgresql" "postgresql-libs" "powertop" "ranger" "redis" "rsync" "ruby" "ruby-docs" "sed"
	"shfmt" "speedtest-cli" "spotify-stable" "sqlite" "subversion" "tar" "thunderbird" "tmux" "tracker"
	"transmission-cli" "tree" "zsh")

# stack
# -----
# gcc, make, libffi, zlib, libgmp and libtinfo

# declare used python pacakges
export PYTHON_PACKAGES=(
	"yapf" "jedi" "SQLAlchemy" "yamllint" "isort"
	"ipython" "flake8" "vulture" "pycallgraph" "mypy" "pylint" "ranger-fm"
	"proselint" "profiling" "pytest" "psycopg2" "mycli" "docutils"
	"requests" "Jinja2" "Django" "Flask" "youtube-dl" "cookiecutter")

# declare used Node.js pacakges
export NODE_PACKAGES=("jest" "typescript" "write-good" "htmlhint"
	"jsonlint" "heroku-cli" "yo" "js-beautify" "standard" "uglify-es"
	"tern" "gitbook" "textlint" "express-generator")

# declare used Php pacakges
export PHP_PACKAGES=()

# declare used Haskell pacakges
export HASKELL_PACKAGES=()

# declare used Go pacakges
export GO_PACKAGES=()

# declare gists to keep in $GIST_DIR
export GISTS=('122b12050f5fb267e75f' '7001839' '8172796' '8294792')

# declare used ruby gems
export RUBY_GEMS=("travis" "jekyll" "sass" "sqlint" "mdl" "scss_lint")

# declare used Rust crates
export RUST_CRATES=("rustfmt" "racer" "mdbook" "cargo-count" "cargo-find" "tokei")

# toolchain to use for Rust
export DEFAULT_TOOLCHAIN=nightly-x86_64-unknown-linux-gnu

# ---------------------------------------------------------------

REQUIREMENTS=("curl" "git")


# CHECKLIST
# - [X] Python
# - [X] Haskell
# - [X] Ruby
# - [X] Node
# - [X] Go
# - [X] Rust
# - [X] Php

# UTILS
_add_to_path() {
	for directory in "$@"; do
		[[ -d $directory ]] && ! [[ $PATH =~ $directory ]] && export PATH="$directory:$PATH:" 2>/dev/null
	done
}

# $1 item
_in_array() {
	for i in "${@:1}"; do
		[[ $i == $2 ]] && return 0
	done
	return 1
}


_in_path() {
	# checks  an executable is in $PATH
	for i in $(echo "$PATH" | sed "s/:/\n/g"); do
		if [[ -x "$i/$1" ]]; then
			return 0
		fi
	done
	return 1
}

for i in "${REQUIREMENTS[@]}"; do
	if ! _in_path $i; then
		exit 1
	fi
done

_gists() {
	[[ -n $GIST_DIR ]] && export GIST_DIR=~/.gists
	if [[ -n $GISTS ]]; then
		[[ ! -e $GIST_DIR ]] && mkdir -p $GIST_DIR
		for i in "${GISTS[@]}"; do
			if [[ ! -e ${GIST_DIR}/${i} ]]; then
				git clone "https://gist.github.com/${i}.git" "${GIST_DIR}/${i}"
			fi
		done
	fi
}

_init_python(){

  _add_to_path ~/.local/bin

  # Don't change the order!
  [[ -z $PYENV_ROOT ]] && export PYENV_ROOT=~/.pyenv
  [[ -z $PYENV_VERSION ]] && export PYENV_VERSION=3.6.1

  # install pyenv if missing
  [[ ! -e $PYENV_ROOT ]] && git clone https://github.com/pyenv/pyenv.git $PYENV_ROOT

  # install the viruatlenv plugin if missing
  if [[ ! -e "${PYENV_ROOT}/plugins/pyenv-virtualenv" ]]; then
    git clone https://github.com/pyenv/pyenv-virtualenv.git "${PYENV_ROOT}/plugins/pyenv-virtualenv"
  fi

  if [[ "$(pyenv versions)" =~ "$(echo $PYENV_VERSION | sed -E 's/\./\\./g')" ]]; then
    pyenv install $PYENV_VERSION 2>/dev/null
  fi

  _add_to_path "${PYENV_ROOT}/bin"

  eval "$(pyenv init -)"
  eval "$(pyenv virtualenv-init -)"
}

_packages_python() {
	if [[ -n $PYTHON_PACKAGES ]] && _in_path pip; then
		# DO STUFF
		packages=$(pip list --format=legacy | sed -E 's/\(.*\)//')
		for i in "${PYTHON_PACKAGES[@]}"; do
			_in_array "${i}" "${packages}" && pip install --retries 3 --timeout 10 --pre --user "${i}" 2>/dev/null
		done
	fi
}

_init_ruby() {
	[[ -n $RBENV_VERSION ]] && export RBENV_VERSION=2.4.1
	[[ -n $RBENV_ROOT ]] && export RBENV_ROOT=~/.rbenv
	if [[ ! -e "${RBENV_ROOT}/plugins/ruby-build" ]]; then
		git clone https://github.com/rbenv/ruby-build.git "${RBENV_ROOT}/plugins/ruby-build"
		~/.rbenv/plugins/ruby-build/install.sh
	fi
	_add_to_path  "${RBENV_ROOT}/bin"
	_add_to_path ~/.gem/ruby/*/bin
	eval "$(rbenv init -)"
	if [[ $(rbenv versions) =~ $(echo $RBENV_VERSION | sed -E 's/\./\\./g') ]]; then
		rbenv install $RBENV_VERSION
	fi
}

_packages_ruby() {
	if [[ -n $RUBY_GEMS ]] && $(_in_path gem); then
		packages=$(gem list | sed -E 's/\(.*\)//')
		for i in "${RUBY_GEMS[@]}"; do
			_in_array "${i}" "${packages}"  && gem install "${i}" 2>/dev/null
		done
	fi
}

_init_node() {
	if ! _in_path node; then
		if _in_path curl; then
			curl -o- https://raw.githubusercontent.com/creationix/nvm/v0.33.4/install.sh | bash
		elif _in_path wget; then
			wget -qO- https://raw.githubusercontent.com/creationix/nvm/v0.33.4/install.sh | bash
		fi
	fi
	_add_to_path ~/{.yarn,node_modules}/bin
}

_packages_node() {
	if _in_path yarn; then
		if [[ -n $NODE_PACKAGES ]]; then
			yarn_packages=$(yarn global list 2>/dev/null)
			for i in "${NODE_PACKAGES[@]}"; do
				! _in_array "${i}" "${yarn_packages}" && yarn global add "${i}" 2>/dev/null
			done
		fi
	elif _in_path npm; then
		if [[ -n ${NODE_PACKAGES[*]} ]]; then
			npm_packages=$(npm list --depth=0 2>/dev/null)
			for i in "${NODE_PACKAGES[@]}"; do
				! _in_array "${i}" "${npm_packages}" && npm install "${i}" 2>/dev/null
			done
		fi
	fi
}

_init_haskell() {
	# curl needed, wget as fallback
	if ! _in_path stack; then
		if _in_path curl; then
			curl -sSL https://get.haskellstack.org/ | sh
		elif _in_path wget; then
			wget -qO- https://get.haskellstack.org/ | sh
		fi
	fi
	_add_to_path ~/.{stack,cabal}/bin
}

_packages_haskell() {
	for i in "${HASKELL_PACKAGES[@]}"; do
		stack install "${i}"
	done
}

_packages_rust() {
	if [[ -n $RUST_CRATES ]] && _in_path cargo; then
		for i in ${RUST_CRATES[@]}; do
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

	# install Rust using rustup if Rust is missing
	if ! _in_path rustc && _in_path curl; then
		curl https://sh.rustup.rs -sSf | sh
		rustup toolchain install "${DEFAULT_TOOLCHAIN}"
		rustup default "${DEFAULT_TOOLCHAIN}"
	fi

}

_init_go() {
	export GOPATH=$HOME/Projects/Go
	_add_to_path {$GOROOT,$GOPATH,/usr/local/go}/bin
}

_packages_go(){
  echo ""
}

_init_php() {
	_add_to_path ~/.config/composer/vendor/bin
}

_packages_php(){
  echo ""
}

_packages_pacman() {

	# query local database, quiet mode, just names
    if _in_path pacman && _in_path xargs && [[ -n $PACMAN_PACKAGES ]]; then

      pacman_packages=$(pacman -Qq | xargs)

      # break immediately on any isse to avoid endless loops
      for i in "${PACMAN_PACKAGES[@]}"; do
          if ! _in_array "${i}" "${PACMAN_PACKAGES}"; then
              sudo pacman -S "${i}" || break
          fi
      done
    fi
}

main() {
	cd
	_cwd=$(pwd)

	_packages_pacman

	for language in python ruby haskell go node php; do
		eval "_init_${language}"
		eval "_packages_${language}"
	done

	_gists

	cd "${_cwd}"
}

main

# vim: nowrap foldmarker={,} foldmethod=marker
