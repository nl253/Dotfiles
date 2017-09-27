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
