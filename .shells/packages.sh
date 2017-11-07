

# [[ -e ~/AppData/ ]] && return 0
return 0

PACMAN_PACKAGES=('ranger' 'htop' 'apache' 'aria2' 'aspell' 
	'autoconf' 'automake' 'vlc' 'bash-completion' 'bashmount' 
	'bc' 'binutils' 'bison' 'clang' 'base-devel' 
	'composer' 'rubber' 'coreutils' 'cronie' 'atool' 
	'universal-ctags-git' 'texlive-core' 'texlive-bin'
	'curl' 'elinks' 'erlang' 'flex' 'fzf' 'gawk' 'gcc' 'gcc-libs' 
	'gimp' 'go' 'gparted' 'grep' 'hexchat' 'highlight' 'htop' 
	'hub' 'hugo' 'jdk9-openjdk' 'jq' 'json-glib' 'less' 'graphviz'
	'lsb-release' 'lshw' 'lua' 'make' 'man-db' 'man-pages' 'mariadb' 
	'mariadb-clients' 'mathjax' 'mongodb' 'mongodb-tools' 'mutt' 
	'ncurses' 'ngrep' 'nodejs' 'npm' 'openssh' 'parted' 'perl' 'php'
	'postgresql' 'postgresql-libs' 'powertop' 'redis' 
	'rsync' 'ruby' 'ruby-docs' 'sed' 'speedtest-cli' 
	'spotify' 'sqlite' 'subversion' 'tar' 'thunderbird' 
	'tmux' 'tracker' 'tree')

PYTHON_PACKAGES=('Cython' 'Jinja2' 'PyYAML' 'SQLAlchemy' 'Sphinx' 'bokeh' 'cookiecutter'
	'cpplint' 'docopt' 'docutils' 'flake8' 'jedi' 'jupyter' 'lxml' 'matplotlib' 'mycli'
	'mypy' 'neovim' 'networkx' 'nltk' 'numpy' 'pandas' 'profiling' 'prompt-toolkit' 'proselint'
	'psycopg2' 'pycallgraph' 'pydocstyle' 'pyflakes' 'pygments' 'pylama' 'pylint' 'pytest'
	'radon' 'ranger-fm' 'scipy' 'seaborn' 'sphinx' 'sqlalchemy' 'sympy'
	'tensorflow' 'vulture' 'yamllint' 'yapf' 'youtube-dl')

NODE_PACKAGES=('jest' 'typescript' 'write-good' 'htmlhint' 'jsonlint' 
	'heroku-cli' 'yo' 'js-beautify' 'uglify-es' 'tern' 'textlint')

PHP_PACKAGES=()

HASKELL_PACKAGES=('pandoc' 'happy' 'shellcheck') 

GO_PACKAGES=('shfmt') # go get ...

GISTS=('122b12050f5fb267e75f' '7001839' '8172796' '8294792')

RUBY_GEMS=('travis' 'jekyll' 'sass' 'sqlint' 'mdl' 'scss_lint')

RUST_CRATES=('rustfmt' 'racer' 'mdbook' 'cargo-count' 'cargo-find' 'tokei')

# ---------------------------------------------------------------

# vim: nowrap foldmarker={,} foldmethod=marker sw=2 ts=2
