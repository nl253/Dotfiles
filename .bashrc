# ~/.bashrc  executed by bash(1)

[[ -n $ENV ]] && . $ENV

if ((BASH_VERSINFO < 4)); then
  echo "[ERROR] your bash is outdated, install bash >= 4"
  return 0
fi

for file in completions; do
  . ~/.config/bash/$file.sh
done

shopt -s xpg_echo globasciiranges histappend checkjobs checkwinsize \
  globstar extglob dotglob nocasematch nocaseglob cdspell dirspell \
  checkhash cdable_vars

PROMPT_COMMAND="history -a"

[ -f ~/.fzf.bash ] && source ~/.fzf.bash
