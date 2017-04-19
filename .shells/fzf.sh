
# {{{ FZF init # chech if on system # set up aliases in case it is and isn't
if [ -x /usr/bin/fzf ]; then
  export FZF_DEFAULT_OPTS="--bind='enter:execute( [ -f {} ] && $EDITOR {} || [ -r {} ] && $EDITOR),alt-y:execute(echo {} | xclip -selection clipboard),alt-r:execute([ -x/usr/bin/rifle ] && rifle {} || [ -x /usr/bin/mc ] && mc {}),alt-e:execute($EDITOR {}),ctrl-x:execute(rm -i {}),ctrl-d:half-page-down,ctrl-u:half-page-up,alt-p:toggle-preview' --no-mouse --multi --black --margin 3% --prompt=' >> ' --reverse --tiebreak=end,length --color 'hl:117,hl+:1,bg+:232,fg:240,fg+:246'"
  alias fzfp='fzf --preview="[ -f {} ] && head -n 38 {} || tree -l -a --prune -L 4 -F --sort=mtime {}"'
  export FZF_DEFAULT_COMMAND='(git ls-tree -r --name-only HEAD || find . -path "*/\.*" -prune -o -type d -print -type f -print -o -type l -print | sed s/^..//) 2> /dev/null'
  alias l=fzf-locate.sh       # [L]OCATE
  alias fh=fzf-search-home.sh # [F]IND [H]OME
  alias c=fzf-cd.sh           # [C]D
  alias p=fzf-pkill.sh        # [P]ROCESSES 
  [ -x /usr/bin/gdrive ] && alias gdrive-fzf='gdrive list | fzf --bind "enter:execute(echo {} | grep -P -o \"^\w+\")"'
else # non fzf solution
  [ -x /usr/bin/htop ] && alias p=htop || alias p=top # process management
fi

# }}}
