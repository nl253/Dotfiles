# toolchain to use for Rust
export DEFAULT_TOOLCHAIN=nightly

# Don't check mail when opening terminal.
unset MAILCHECK


export HISTFILE=~/.shell_history
export SAVEHIST=10000
export HISTSIZE=20000
export HISTFILESIZE=20000
export HISTCONTROL="ignoreboth:erasedups"
export HISTTIMEFORMAT=""
export HH_CONFIG=hicolor # get more colors
export HISTIGNORE="&:[ ]*:exit:cd:ls:bg:fg:history:clear:jobs"

[ -f ~/.makepkg.conf ] && export MAKEPKG_CONF=~/.makepkg.conf
[ -f ~/.config/ranger/rc.conf ] && export RANGER_LOAD_DEFAULT_RC=false

# use qt5 not qt4
export QT_SELECT=5

for directory in '.local' '.yarn' '.stack' '.cabal' '.config/composer/vendor' '.cargo' '.local/share/fzf' 'go' 'node_modules' .stack/snapshots/x86_64-linux-*/lts-9.12/8.2.1 .stack/compiler-tools/x86_64-linux-*/ghc-8.2.1 .stack/programs/x86_64-linux/ghc-*-8.2.1; do
  [ -d "${HOME}/${directory}/bin" ] && export PATH="${HOME}/${directory}/bin:${PATH}:" 2>/dev/null
done

# POSTGRES
if [ -x $(command which psql 2>/dev/null) ]; then
  export PGUSER=postgres
  export PGHOST=localhost
  export PGDATABASE=testing
fi

# $BROWSER
for i in firefox-developer firefox google-chrome-stable chromium elinks lynx w3m; do
  if [ -x $(command which $i 2>/dev/null) ]; then
    export BROWSER=$(command which $i 2>/dev/null) && break
  fi
done

if [ -x $(command which fzf 2>/dev/null) ]; then

  # KEYMAP
  # ------------------------
  # enter : print to STDOUT
  # ctrl-d : scroll down
  # ctrl-u : scroll up
  # alt-e : edit with $EDITOR
  # alt-d : cd
  # alt-r : execute `rifle` [detects what to use based on file type]
  # alt-l : open in `less`

  export FZF_DEFAULT_OPTS=" --bind='alt-d:execute(cd {})' --bind='ctrl-d:half-page-down,ctrl-u:half-page-up,alt-p:toggle-preview' --bind='alt-e:execute(\$EDITOR {})' --bind='alt-r:execute(rifle {}),alt-l:execute:($PAGER {})' --no-mouse --multi --black --margin 3% --prompt=' >> ' --reverse --tiebreak=end,length --color 'hl:107,hl+:1,bg+:234,fg:240,fg+:246'"
  export FZF_DEFAULT_COMMAND='git ls-tree -r --name-only HEAD || find . -path "*/\.*" -prune -o -type d -print -type f -print -o -type l -print | sed s/^..//\ 2> /dev/null'
  export FZF_CTRL_T_OPTS="--select-1 --exit-0"
  export FZF_CTRL_R_OPTS="--sort --exact --preview 'echo {}' --preview-window down:3:hidden --bind '?:toggle-preview'"

fi
