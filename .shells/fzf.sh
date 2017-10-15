
# FZF init (tested on zsh and bash)

# chech if on system, set up aliases in case it is and isn't

if [[ -x $(command which fzf 2>/dev/null) ]]; then # {

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

  # preview configured to `cat` for files and use `tree` for dirs
  # [FZF] with [P]REVIEW
  #if [[ -x $(command which pygmentize 2>/dev/null) ]]; then
  alias fzfp='fzf --preview="[[ -f {} ]] && head -n 38 {} | pygmentize -l $(pygmentize -N {}) || [[ -d {} ]] && tree -l -a --prune -L 4 -F --sort=mtime {}"'
  #else
    #alias fzfp='fzf --preview="[ -f {} ] && head -n 38 {} || tree -l -a --prune -L 4 -F --sort=mtime {}"'
  #fi

  [[ -e ~/.gists ]] && alias gists="find ~/.gists/*/ -type f -and -not -path '**.git**' | fzfp"

fi # }

# vim: nowrap formatoptions=
