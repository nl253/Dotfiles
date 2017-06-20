
# THIS FILE MUST USE POSIX COMPLIANT SYNTAX
# IT IS SOURCED BY BOTH `zsh` AND `bash`

# FZF init 

# chech if on system 
# set up aliases in case it is and isn't

[[ ! $_SETUP_SOURCED ]] && source ~/.shells/setup.sh

if [[ -x $(which fzf) ]]; then # {{{

   # h - repeat history {{{
  h() { 
    print -z $( ([ -n "$ZSH_NAME" ] && fc -l 1 || history) | fzf +s --tac | sed 's/ *[0-9]* *//')
  } # }}}

  # tmux ftpane - switch pane (@george-b) {{{
  ftpane() {
    local panes current_window current_pane target target_window target_pane
    panes=$(tmux list-panes -s -F '#I:#P - #{pane_current_path} #{pane_current_command}')
    current_pane=$(tmux display-message -p '#I:#P')
    current_window=$(tmux display-message -p '#I')

    target=$(echo "$panes" | grep -v "$current_pane" | fzf +m --reverse) || return

    target_window=$(echo $target | awk 'BEGIN{FS=":|-"} {print$1}')
    target_pane=$(echo $target | awk 'BEGIN{FS=":|-"} {print$2}' | cut -c 1)

    if [[ $current_window -eq $target_window ]]; then
      tmux select-pane -t ${target_window}.${target_pane}
    else
      tmux select-pane -t ${target_window}.${target_pane} &&
      tmux select-window -t $target_window
    fi
  } # }}}

  export FZF_DEFAULT_OPTS="--bind='alt-d:execute(cd {})' --bind='ctrl-d:half-page-down,ctrl-u:half-page-up,alt-p:toggle-preview' --bind='alt-e:execute(\$EDITOR {})' --bind='alt-r:execute(rifle {}),alt-l:execute:(command less -RX {})' --no-mouse --multi --black --margin 3% --prompt=' >> ' --reverse --tiebreak=end,length --color 'hl:117,hl+:1,bg+:232,fg:240,fg+:246'"

  # KEYMAP
  # enter : print to STDOUT
  # ctrl-d : scroll down
  # ctrl-u : scroll up
  # alt-e : edit with $EDITOR
  # alt-d : cd
  # alt-r : execute `rifle` [detects what to use based on file type]
  # alt-l : open in `less`

  export FZF_DEFAULT_COMMAND='(git ls-tree -r --name-only HEAD || find . -path "*/\.*" -prune -o -type d -print -type f -print -o -type l -print | sed s/^..//) 2> /dev/null'
  export FZF_CTRL_T_OPTS="--select-1 --exit-0"

  export FZF_CTRL_R_OPTS="--sort --exact --preview 'echo {}' --preview-window down:3:hidden --bind '?:toggle-preview'"

  # preview configured to `cat` for files and use `tree` for dirs
  alias fzfp='fzf --preview="[ -f {} ] && head -n 38 {} || tree -l -a --prune -L 4 -F --sort=mtime {}"' # [FZF] with [P]REVIEW

else # non fzf solution 

  if [[ -x $(which htop) ]]; then
    alias p=htop 
  else 
    alias p=top # [P]ROCESSES 
  fi

fi # }}}

export _FZF_SOURCED=1
