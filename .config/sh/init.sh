# ~/.shinit to be sourced by all POSIX-compliant interactive shells
#
# NOTE: This file is not read by bash(1) if ~/.bash_profile or ~/.bash_login exists.

# If not running interactively, don't do anything
case $- in
  *i*) ;;
  *) return ;;
esac

alias emacs='emacs -nw'

# normalise prompt in case somthing goes wrong
export PS1="${USER}@"$(hostname)" ${0} >> "

# reset
export CDPATH="${HOME}:"

for dir in ~/Code/JS; do
  [ -d $dir ] && export CDPATH="${dir}:${CDPATH}:" 2>/dev/null
done

# when working in   
for dir in ../../../../../node_modules/.bin  ../../../../node_modules/.bin ../../../node_modules/.bin ../../node_modules/.bin ../node_modules/.bin ./node_modules/.bin; do
  export PATH="${dir}:${PATH}"
done

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# export YAOURT_COLORS="nb=1:pkg=1:ver=1;32:lver=1;45:installed=1;42:grp=1;34:od=1;41;5:votes=1;44:dsc=0:other=1;35"

# get MIME type of a file
[ -x /usr/bin/file ] && alias mime-type='command file --dereference --brief --mime-type -- '

if [ -x /usr/bin/node ]; then
  node_libs='path fs os'
  node_experimental='modules repl-await vm-modules worker'
  node_cmd='command node -i --no-warnings'
  for lib in $(eval "echo $node_libs"); do node_cmd="${node_cmd} --require ${lib}"; done
  for feature in $(eval "echo $node_experimental"); do node_cmd="${node_cmd} --experimental-${feature}"; done
  alias node="${node_cmd}"
  for var in node_libs node_experimental node_cmd; do eval "unset -v $var"; done
fi

# this here is very format dependent - do not change
# dirs and files
ls_opts='-I=tags -I *cache* -I *.synctex* -I *history* -I ~* -I _* -I *~ -I *-log -I *-lock -I *.log -I *.class -I *.so -I *.beam -I *.o -I *.pyc -I *.pyg -I *.aux -I *.toc -I *.swp -I *.tmp -I *.fls -I *.fdb_latexmk -I *.lock -I *.hi --color=auto --group-directories-first'
if builtin dirs 1>/dev/null 2>/dev/null && [ -x ~/.cargo/bin/exa ]; then
  # replace all occurances of ' -I ' with '|' required by exa
  alias ls="command exa \"${ls_opts// -I /|}\" --git --git-ignore"
else
  alias ls="command ls $ls_opts"
fi
unset -v ls_opts

[ -x ~/.cargo/bin/bat ] && alias cat='bat' 

# pattern matching
alias grep='command grep -E -I --color=auto'

if [ -x ~/.cargo/bin/rg ]; then
  alias rg='command rg --hidden --pretty --no-heading --threads $(grep -c ^processor /proc/cpuinfo) --context 1 --max-count 3 --no-search-zip'
fi

GLOBIGNORE='*.fls:*.out:*.aux:*.toc:*.beam:*.pyo:*.lock:*.tmp:*.bak:*.log:*.o:*.hi:*.class:*.so:tags:node_modules:iml:*cache*:*history*'
FIGNORE=$GLOBIGNORE

alias diff='command diff --color=auto --suppress-common-lines --ignore-trailing-space --minimal --text --side-by-side --width=$(tput cols) --ignore-tab-expansion --ignore-space-change --ignore-all-space --ignore-blank-lines'

for i in df du; do alias $i="command $i --human-readable --si --total"; done
alias info='command info --vi-keys'
alias pgrep='command pgrep --list-full'
alias logout="command pkill -KILL -u \$USER"
[ -x /usr/bin/acpi ] && alias battery="command acpi -V"
alias cp="cp --recursive --verbose --interactive --preserve=mode,ownership,timestamps"

# rsync(1) is faster, more secure than cp(1)
if [ -x /usr/bin/rsync ]; then
  alias copy="command rsync --progress --log-file=/tmp/rsync/rsync.log --recursive --backup --backup-dir=/tmp/rsync --human-readable --times --preallocate --partial --partial-dir=/tmp/rsync/partially-copied --suffix=rsync-backup --hard-links --group --perms -xattrs --executability --copy-links --copy-dirlinks --compress --compress-level 9"
fi

for i in cp mv rm; do
  alias $i="command $i --verbose"
done

alias setclip="xclip -selection c"

alias show-term-capabilities="command infocmp -1 | command sed -nu 's/^[ \000\t]*//;s/[ \000\t]*$//;/[^ \t\000]\{1,\}/!d;/acsc/d;s/=.*,//p'|column -c80"
alias bat='command bat --theme TwoDark --style plain'

[ -x /usr/bin/libreoffice ] && alias libreoffice="libreoffice --norestore 2>/dev/null 1>/dev/null &"
# alias tmux='tmux -2'

# Networking, Servers
# NOTE the php server requires index.php in the root dir
[ -x /usr/bin/php ] && alias http-server-php="command php -S 0.0.0.0:5000"
[ -x /usr/bin/ruby ] && alias http-server-ruby="command ruby -rwebrick -e'WEBrick::HTTPServer.new(:Port => 8000, :DocumentRoot => Dir.pwd.start'"

# because kitty is not recognised as a terminal emulator
[ -x /usr/bin/ssh ] && alias ssh='TERM=xterm command ssh'
[ -x /usr/bin/kitty ] && [ -x ~/.local/bin/kitty ] && alias kitty="~/.local/kitty.app/bin/kitty --config ~/.config/kitty/kitty-dark.conf"

# Python
alias pip-update-all="command pip3 freeze --local | command grep -v '^\\-e' | command cut -d = -f 1  | command xargs -n1 pip install -U"
alias pip-uninstall="for i in \$(command pip3 list --user --not-required | command sed -n -E -e 's/^(\\S+)\\s+.*/\\1/' -e '3,\$p' | command fzf); do command pip3 uninstall -y \$i; done"

[ -x ~/.local/bin/isympy ] && alias isympy='command isympy -I -p unicode'

[ -x /usr/bin/python3 ] && alias python='command python3'

for i in pip pydoc 'http.server'; do
  eval "alias $i='command python3 -m $i'"
  eval "alias ${i}3='command python3 -m $i'"
done

alias ranger='command ranger 2>/dev/null'

# get ip address
[ -x /usr/bin/curl ] && alias my-ip='command curl ipinfo.io/ip'

# Archiving
[ -x /usr/bin/7z ] && alias 7z='command 7z -mx=9 -mmt8 -bt -bb1'

# $EDITOR
for i in nvim vim vi; do 
  [ -x /usr/bin/$i ] && [ $i != vim ] && eval "alias vim='command ${i}'" && break
done

git_basic_info='$(command git branch -vv --no-color | command grep "*" | command head -n 1 | command sed -E -e "s/\\*?\\s+/ /g" -e "s/^ //" -e "s/(.{,$(command expr $(command tput cols) - 3)}).*?/\1/")'
# git_branch_info='$(command git --no-pager branch --color=never --format "%(refname:lstrip=-1)" 2>/dev/null) -> $(command git --no-pager config --get remote.origin.url 2>/dev/null)@$(command git --no-pager branch --color=never --format="%(upstream:lstrip=3)" 2>/dev/null) ($(command git remote 2>/dev/null))'
# git_branch_info="[${git_branch_info}]"
non_git_prompt='$(command basename $0):/$PWD :: '

# bash and zsh
if builtin dirs 1>/dev/null 2>/dev/null; then

  # set ls colors
  builtin eval $(command dircolors -b)

  export PS1="${git_basic_info} \n${non_git_prompt}"

else # dash
  export PS1="${git_basic_info} ${non_git_prompt}"
fi

for var in git_basic_info git_branch_info non_git_prompt; do unset -v $var; done
# vim:foldmethod=indent:foldmarker={,}:shiftwidth=2:tabstop=2:foldlevel=4:
