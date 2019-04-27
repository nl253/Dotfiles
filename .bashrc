# ~/.bashrc  executed by bash(1)

[[ -n $ENV ]] && . $ENV

if ((BASH_VERSINFO < 4)); then
  echo "[ERROR] your bash is outdated, install bash >= 4"
  return 0
fi

for file in completions; do
  [[ -f ~/.config/bash/$file.sh ]] && . ~/.config/bash/$file.sh
done

shopt -s xpg_echo globasciiranges histappend checkjobs checkwinsize \
  globstar extglob dotglob nocasematch nocaseglob cdspell dirspell \
  checkhash cdable_vars

PROMPT_COMMAND="history -a"

if [[ -f /usr/share/bash-completion/bash_completion ]]; then
  . /usr/share/bash-completion/bash_completion 
  for c in pgrep psql 7z man gzip curl pdftotext apt{,titude,-get} aspell chown chgrp dot feh find file git host{,name} java{,c} kill{,all} python{,3} R ssh sqlite3 whatis; do
    if ([[ -f /usr/bin/$c ]] || [[ -f /usr/local/bin/$c ]] || [[ -f /bin/$c ]]) && [[ -f /usr/share/bash-completion/completions/$c ]]; then 
      . /usr/share/bash-completion/completions/$c
    fi
  done
fi
