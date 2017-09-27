# point doctuils (rst) to config file
export DOCUTILSCONFIG=~/.docutils

# declare used python pacakges
export PYTHON_PACKAGES=(\
	"yapf" "jedi" "SQLAlchemy" "yamllint" "isort" \
	"ipython" "flake8" "vulture" "pycallgraph" "mypy" "pylint" "ranger-fm" \
	"proselint" "profiling" "pytest" "psycopg2" "mycli" "docutils" \
	"requests" "Jinja2" "Django" "Flask" "youtube-dl" "cookiecutter")

# declare used Node.js pacakges
export NODE_PACKAGES=("jest" "typescript" "write-good" "htmlhint" \
  "jsonlint" "heroku-cli" "yo" "js-beautify" "standard" "uglify-es" \
  "tern" "gitbook" "textlint" "express-generator")

# declare gists to keep in $GIST_DIR
export GISTS=('122b12050f5fb267e75f' '7001839' '8172796' '8294792')

# declare used ruby gems
export RUBY_GEMS=("travis" "jekyll" "sass" "sqlint" "mdl" "scss_lint")

# declare used Rust crates
export RUST_CRATES=("rustfmt" "racer" "mdbook" "cargo-count" "cargo-find" "tokei")

# toolchain to use for Rust
export DEFAULT_TOOLCHAIN=nightly-x86_64-unknown-linux-gnu

# ---------------------------------------------------------------

sync-path(){

  add-to-path(){
    for directory in "$@"; do
      [[ -d $directory ]] && [[ ! $PATH =~ $directory ]] && export PATH="$directory:$PATH:" 2>/dev/null
    done
  }

  [[ -z $GIST_DIR ]] && export GIST_DIR=~/.gists


  [[ -z $DEFAULT_TOOLCHAIN ]] && export DEFAULT_TOOLCHAIN=stable-x86_64-unknown-linux-gnu
  [[ -z $RUST_SRC_PATH ]] && export RUST_SRC_PATH=~/.multirust/toolchains/${DEFAULT_TOOLCHAIN}/lib/rustlib/src/rust/src

  # Don't change the order!
  [[ -z $PYENV_ROOT ]] && export PYENV_ROOT=~/.pyenv
  [[ -z $PYENV_VERSION ]] && export PYENV_VERSION=3.6.1
  export PATH="${PATH}:${PYENV_ROOT}/bin"
  eval "$(pyenv init -)"
  eval "$(pyenv virtualenv-init -)"

  export GOPATH=$HOME/Projects/Go

  add-to-path {$GOROOT,$GOPATH,/usr/local/go}/bin # GO
  add-to-path ~/.gem/ruby/*/bin                   # RUBY
  add-to-path ~/{.yarn,node_modules}/bin          # NODE
  add-to-path ~/.cargo/bin                        # RUST
  add-to-path ~/.local/bin                        # PYTHON
  add-to-path ~/.config/composer/vendor/bin       # PHP
  add-to-path ~/.{stack,cabal}/bin                # HASKELL

  unset -f add-to-path
}

sync-path

function sync-packages(){
  cwd=$(pwd)
  cd

  in-path() {
  # checks  an executable is in $PATH
    for i in $(echo "$PATH" | sed "s/:/\n/g"); do
       if [[ -x "$i/$1" ]]; then
          return 0
        fi
      done
    return 1
  }

  # GISTS
  if [[ ! -z $GISTS ]]; then
    [[ ! -e $GIST_DIR ]] && mkdir -p $GIST_DIR
    for i in $GISTS; do
      if [[ ! -e ${GIST_DIR}/${i} ]]; then
        git clone "https://gist.github.com/${i}.git" "${GIST_DIR}/${i}"
      fi
    done
  fi

  # PYTHON

  [[ ! -e $PYENV_ROOT ]] && git clone https://github.com/pyenv/pyenv.git $PYENV_ROOT

  if [[ ! -e ${PYENV_ROOT}/plugins/pyenv-virtualenv ]]; then
    git clone https://github.com/pyenv/pyenv-virtualenv.git ${PYENV_ROOT}/plugins/pyenv-virtualenv
  fi


  if [[ ! $(pyenv versions) =~ $(echo $PYENV_VERSION | sed -E 's/\./\\./g') ]]; then
    pyenv install $PYENV_VERSION 2>/dev/null
  fi

  if [[ ! -z $PYTHON_PACKAGES ]] && $(in-path pip); then
      # DO STUFF
      packages=$(pip list --format=legacy | sed -E 's/\(.*\)//')
      for i in $PYTHON_PACKAGES; do
          [[ ! $packages =~ $i ]] && pip install --retries 3 --timeout 10 --pre --user $i 2>/dev/null
      done
  fi

  # FIXME
  # ----------------------------------------------------------------------------------- {
  # RUBY

  # ! [[ -z RBENV_ROOT ]] && export RBENV_ROOT=~/.rbenv

  # if [[ ! -e ${RBENV_ROOT}/plugins/ruby-build ]]; then
    # git clone https://github.com/rbenv/ruby-build.git ${RBENV_ROOT}/plugins/ruby-build
    # ~/.rbenv/plugins/ruby-build/install.sh
  # fi

  # export PATH="${RBENV_ROOT}/bin:${PATH}"
  # eval "$(rbenv init -)"

  # ! [[ -z RBENV_VERSION ]] && export RBENV_VERSION=2.4.1

  # if [[ ! $(rbenv versions) =~ $(echo $RBENV_VERSION | sed -E 's/\./\\./g') ]]; then
    # rbenv install $RBENV_VERSION
  # fi
  # ----------------------------------------------------------------------------------- }

  if [[ ! -z $RUBY_GEMS ]] && $(in-path gem); then

    packages=$(gem list | sed -E 's/\(.*\)//')

    for i in $RUBY_GEMS; do
      [[ ! $packages =~ $i ]] && gem install $i 2>/dev/null
    done

  fi

  # NODE

  # make sure npm is in path
  if $(in-path npm); then

    # add it's bin dir to $PATH

    npm_packages=$(npm list --depth=0 2>/dev/null)

    # use yarn instead of npm
    [[ ! $npm_packages =~ yarn ]] && npm install yarn 2>/dev/null

    if [[ ! -z $NODE_PACKAGES ]]; then

      yarn_packages=$(yarn global list 2>/dev/null)

      for i in $NODE_PACKAGES; do
        [[ ! $yarn_packages =~ $i ]] && yarn global add $i 2>/dev/null
      done
    fi

  fi

  # RUST

  # install Rust using rustup if Rust is missing
  if ! $(in-path rustc); then
    curl https://sh.rustup.rs -sSf | sh
    rustup toolchain install $DEFAULT_TOOLCHAIN
    rustup default $DEFAULT_TOOLCHAIN
  fi

  if [[ ! -z $RUST_CRATES ]] && $(in-path cargo); then
    for i in $RUST_CRATES; do
      if ! $(in-path i) && [[ ! -e ~/.cargo/bin/${i} ]]; then
        rustup run $DEFAULT_TOOLCHAIN cargo install --all-features $i 2>/dev/null
      fi
    done
  fi

  unset -f in-path

  cd $cwd

  sync-path
}

# vim: nowrap foldmarker={,} foldmethod=marker
