
# toolchain to use for Rust
export DEFAULT_TOOLCHAIN=nightly

# use qt5 not qt4
export QT_SELECT=5

for directory in ".local" .yarn .stack .cabal .config/composer/vendor .cargo .local/share/fzf go node_modules; do
  [ -d "${HOME}/${directory}/bin" ] && export PATH="${HOME}/${directory}/bin:${PATH}:" 2>/dev/null
done

# POSTGRES
if [ -x $(command which psql 2>/dev/null) ]; then
  export PGUSER=postgres
  export PGHOST=localhost
  export PGDATABASE=testing
fi

# $EDITOR
for i in nvim vim vi; do
  if [ -x $(command which $i 2>/dev/null) ]; then
    export EDITOR=$(command which $i 2>/dev/null)
    if [ $i != vim ]; then
      eval "alias vim=${i}"
    fi
    break
  fi
done

# $BROWSER
for i in firefox-developer firefox google-chrome-stable chromium elinks lynx w3m; do
  if [ -x $(command which $i 2>/dev/null) ]; then
    export BROWSER=$(command which $i 2>/dev/null) && break
  fi
done
