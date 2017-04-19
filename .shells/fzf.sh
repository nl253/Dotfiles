# {{{ FZF init # chech if on system # set up aliases in case it is and isn't

if [ ! -x /usr/bin/fzf ] && [ ! -e ~/.fzf/install ] ; then                          # executeable not found ...
  [ -e ~/.fzf ] && rm -rf ~/.fzf                                                    # if there is such a dir, remove
  git clone --depth 1 https://github.com/junegunn/fzf.git ~/.fzf && ~/.fzf/install  # download and install
fi

if [ -x /usr/bin/fzf ] || [ -e ~/.fzf/bin/fzf ] ; then

  export FZF_DEFAULT_OPTS="--bind='ctrl-d:half-page-down,ctrl-u:half-page-up,alt-p:toggle-preview' --bind='alt-e:execute(\$EDITOR {})' --bind='alt-r:execute(rifle {}),alt-l:execute:(command less -RX {})' --no-mouse --multi --black --margin 3% --prompt=' >> ' --reverse --tiebreak=end,length --color 'hl:117,hl+:1,bg+:232,fg:240,fg+:246'"
  # enter : print to STDOUT
  # ctrl-d : scroll down
  # ctrl-u : scroll up
  # alt-e : edit with $EDITOR
  # alt-r : execute `rifle` [detects what to use based on file type]
  # alt-l : open in `less`

  export FZF_DEFAULT_COMMAND='(git ls-tree -r --name-only HEAD || find . -path "*/\.*" -prune -o -type d -print -type f -print -o -type l -print | sed s/^..//) 2> /dev/null'
  alias fzfp='fzf --preview="[ -f {} ] && head -n 38 {} || tree -l -a --prune -L 4 -F --sort=mtime {}"' # [FZF] with [P]REVIEW

  alias l=fzf-locate.sh       # [L]OCATE
  # alias fh=fzf-search-home.sh # [F]IND [H]OME
  alias c=fzf-cd.sh           # [C]D
  alias p=fzf-pkill.sh        # [P]ROCESSES 

  [ -x /usr/bin/gdrive ] && alias gdrive-fzf='gdrive list | fzf --bind "enter:execute(echo {} | grep -P -o \"^\w+\")"'

else # non fzf solution

  [ -x /usr/bin/htop ] && alias p=htop || alias p=top   # [P]ROCESSES 

fi

# }}}
