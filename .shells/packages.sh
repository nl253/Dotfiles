: ' {
ADDITIONAL POSSIBLE VARIABLES
-----------------------------
$DEFAULT_TOOLCHAIN=stable-x86_64-unknown-linux-gnu
$GIST_DIR=~/.gists
$GOPATH
$GOROOT
$GO_VERSION
$GVM_ROOT=~/.gvm
$RBENV_ROOT=~/.rbenv
$RUBY_VERSION
$RUBY_VERSION=~/.rbenv
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

ASYNC
-----
To the extent that is possible commands are executed in the background.
However, functions that alter the env cannot be exectued in this way because they run in separate
subshells and the parent process never inherits the changes made by those subprocesses.

"Variables in a subshell are not visible outside the block of code in the subshell.
They are not accessible to the parent process, to the shell that launched the subshell.
These are, in effect, variables local to the child process."

source http://tldp.org/LDP/abs/html/subshells.html

INSTALL
-------
After installation each function calls the corresponding _init_* function
to intialise variables so that a new shell doesnt need to be spawned.

}'

# 1 or 0
HAS_ADMIN=1

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

_check_requirements() {

	# global requirements to intialise all package mangers
	REQUIREMENTS=('curl' 'git' 'xargs')

	# if any of the requirements aren't met, exit
	for i in "${REQUIREMENTS[@]}"; do
		if ! _in_path $i; then
            echo -e "[ERROR] Requirement ${i} not satisfied. Aborting." >&2
			exit 1
		fi
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
                # in the background
				git clone "https://gist.github.com/${i}.git" "${GIST_DIR}/${i}" &
            else
                echo -e "[INFO] Gist ${i} already present." >&2
			fi

		done
    else
        echo -e "[INFO] GISTS not set. Nothing to do." >&2
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
	[[ ! -e $PYENV_ROOT ]] && git clone https://github.com/pyenv/pyenv.git "${PYENV_ROOT}" &

    # if the above async process (git) hasn't (yet) created those dirs, make them
    [[ ! -e $PYENV_ROOT/plugins ]] && mkdir -p "${PYENV_ROOT}/plugins"

	# install the viruatlenv plugin if missing
	if [[ ! -e "${PYENV_ROOT}/plugins/pyenv-virtualenv" ]]; then
		git clone https://github.com/pyenv/pyenv-virtualenv.git "${PYENV_ROOT}/plugins/pyenv-virtualenv" &
      else
        echo -e "[INFO] Pyenv virtualenv plugin already present. Nothing to do." >&2
	fi

	if [[ $(pyenv versions) =~ $(echo $PYENV_VERSION | sed -E 's/\./\\./g') ]]; then
		pyenv install $PYENV_VERSION 2>/dev/null
    else
        echo -e "[INFO] Pyenv has the correct version of python. Nothing to do." >&2
	fi

    # intialise pyenv init
    _init_python
}

_packages_python() {
	if [[ -n $PYTHON_PACKAGES ]] && _in_path pip; then
		local packages=$(pip list --format=legacy | sed -E 's/\(.*\)//')
		for i in "${PYTHON_PACKAGES[@]}"; do
			if _in_array "${i}" "${packages}"; then
				pip install --retries 3 --timeout 10 --pre --user "${i}" &
			fi
		done

    else
        echo -e "[WARN] Either python not installed or you didn't specify any packages to install." >&2
	fi
	# await last loop
	wait $!
}

_init_ruby() {
	[[ -z $RUBY_VERSION ]] && export RUBY_VERSION=2.4.1
	[[ -z $RBENV_ROOT ]] && export RBENV_ROOT=~/.rbenv
	_add_to_path "${RBENV_ROOT}/bin"
	_add_to_path ~/.gem/ruby/*/bin
	eval "$(rbenv init -)"
}

_packages_ruby() {
	if [[ -n $RUBY_GEMS ]] && _in_path gem; then
		local packages=$(gem list | sed -E 's/\(.*\)//')
		for i in "${RUBY_GEMS[@]}"; do
			if _in_array "${i}" "${packages}"; then
				gem install "${i}" &
			fi
		done

    else
        echo -e "[WARN] Either ruby not installed or you didn't specify any gems to install." >&2
	fi
	# await last loop
	wait $!
}

_install_ruby() {
	if [[ ! -e $RBENV_ROOT ]]; then
		git clone https://github.com/rbenv/rbenv.git "${RBENV_ROOT}" &
	fi
	if [[ ! -e "${RBENV_ROOT}/plugins/ruby-build" ]]; then
        wait $!
		git clone https://github.com/rbenv/ruby-build.git "${RBENV_ROOT}/plugins/ruby-build" &
		if [[ $HAS_ADMIN == 1 ]]; then
          wait $!
          sudo ~/.rbenv/plugins/ruby-build/install.sh &
        fi
	fi
	if [[ $(rbenv versions) =~ $(echo $RUBY_VERSION | sed -E 's/\./\\./g') ]]; then
        wait $!
		rbenv install $RUBY_VERSION
	fi

    # run rbenv init
    _init_ruby
}

_init_node() {
    if [[ -s "/home/norbert/.gvm/scripts/gvm" ]]; then
        source "/home/norbert/.gvm/scripts/gvm"
    fi
    # for yarn and npm
	_add_to_path ~/{.yarn,node_modules}/bin
}

_install_node() {
	if ! _in_path node; then
		curl -o- https://raw.githubusercontent.com/creationix/nvm/v0.33.4/install.sh | bash
		_init_node
	fi
}

_packages_node() {

    if _in_path yarn; then
        local install_command="yarn global add"
        local packages=$(yarn global list 2>/dev/null)

    else
        echo "[ERROR] Yarn not installed! Node.js packages not synced." >&2
        return 1
    fi

    for i in "${NODE_PACKAGES[@]}"; do
        if ! _in_array "${i}" "${packages}"; then
            eval "${install_command} ${i}" &
        fi
    done

    # await last loop
    wait $!
}

_init_haskell() {

	_add_to_path ~/.{stack,cabal}/bin

	if _in_path stack && [[ -n $BASH ]]; then
		# enable stack bash completion
		eval "$(stack --bash-completion-script stack)"
	else
		echo "[ERROR] Stack not installed! Haskell not initialised." >&2
	fi
}

_install_haskell() {
	if ! _in_path stack; then
      curl -sSL https://get.haskellstack.org/ | sh
      _init_haskell
    else
      echo "[INFO] Stack seems to be installed. No need to install haskell." >&2
	fi
}

_packages_haskell() {

	if _in_path stack; then
      for i in "${HASKELL_PACKAGES[@]}"; do
          stack install "${i}" &
      done
    else
      echo -e "[ERROR] Attempting to use stack to install haskell packages while stack is not installed.
            Install stack first and rerun." >&2
      return 1
    fi

    # await last loop
    wait $!
}

_packages_rust() {
	if [[ -n $RUST_CRATES ]] && _in_path cargo; then
		for i in "${RUST_CRATES[@]}"; do
			if ! _in_path i && [[ ! -e ~/.cargo/bin/${i} ]]; then
				rustup run "${DEFAULT_TOOLCHAIN}" cargo install --all-features "${i}" 2>/dev/null &
			fi
		done
    else
      echo -e "[ERROR] Rust doesn't seem to be installed.
            Cargo not present so packages could not be installed.
            Install Rust using rustup and rerun." >&2
	fi

    # await last loop
    wait $!
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
		rustup toolchain install "${DEFAULT_TOOLCHAIN}" &
		rustup default "${DEFAULT_TOOLCHAIN}" &
        _init_rust
    else
        echo -e "[INFO] Rust is already installed.  Nothing to do." >&2
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
        _init_go
    else
        echo -e "[INFO] GVM (golang) is already installed.  Nothing to do." >&2
	fi

	if [[ ! $(gvm version) == $GO_VERSION ]]; then
		gvm install "${GO_VERSION}"
        _init_go
    else
        echo -e "[INFO] GVM has the correct GO_VERSION. Nothing to do." >&2
	fi
}

_packages_go() {

	#for i in "${GO_PACKAGES[@]}"; do
	  #go install
	#done
    echo "" &
}

_init_php() {
	_add_to_path ~/.config/composer/vendor/bin
}

_packages_php() {
	echo "" &
}

_install_php() {
	echo "" &
}

_packages_pacman() {

    # only run on systems that have pacman
	if _in_path pacman && [[ -n $PACMAN_PACKAGES ]] && [[ "${HAS_ADMIN}" == 1 ]]; then
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

    _check_requirements

	local _cwd=$(pwd)

	cd

    # do it in the background
	for language in python ruby haskell go node php; do
		eval "_init_${language}"
	done

	cd "${_cwd}"
}

install_package_managers(){

  _check_requirements

  _packages_pacman

  coproc _gists

  for i in python ruby haskell php go rust node; do
  # FIXME have the _install_* call init
  # this repetition IS necessary!
    eval "_init_${i}" # must not be run by a subshell - it sets vars
    eval "_install_${i}"
    eval "_packages_${i}" &
  done
}

main

# unset created functions
for i in main; do
  eval "unset -f ${i}"
done

# vim: nowrap foldmarker={,} foldmethod=marker
