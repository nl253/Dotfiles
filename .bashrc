# ~/.bashrc  executed by bash(1)

[[ -n $ENV ]] && . $ENV

nvm () {
  if builtin command nvm &>/dev/null; then
    builtin command nvm $@
  else
    [[ -s "$NVM_DIR/nvm.sh" ]] && \. "$NVM_DIR/nvm.sh"
    [[ -s "$NVM_DIR/bash_completion" ]] && \. "$NVM_DIR/bash_completion"
    nvm $@
  fi
}

for dir in /usr/share/bash-completion /etc; do
  if [[ -f "${dir}/bash_completion" ]]; then
    . "${dir}/bash_completion"
    builtin break
  fi
done

PROMPT_COMMAND="history -a"

aws() { 
  builtin local result=$(builtin command aws "$@")
  builtin echo "$result" | jq . || builtin echo "$result"
}

for base in ~/.config/bash; do
  for file in completions vars; do
    [[ -f ${base}/${file}.sh ]] && . "${base}/${file}.sh"
  done
done

# builtin eval "$(starship init bash)" || builtin echo '[ERROR] starship is not installed'

if ((BASH_VERSINFO < 4)); then
  builtin echo "[ERROR] your bash is outdated, install bash >= 4"
  builtin return 0
else
  builtin shopt -s \
    xpg_echo \
    globasciiranges \
    histappend \
    checkjobs \
    checkwinsize \
    globstar \
    extglob \
    dotglob \
    nocasematch \
    nocaseglob \
    cdspell \
    dirspell \
    checkhash \
    cdable_vars
fi

