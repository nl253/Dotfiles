## Completions for bash(1). 
## 
## Can also be sourced by zsh if you autoload bashcompinit, see zshcompsys(1).
# [[ -x $(builtin type -P pandoc 2>/dev/null) ]] && builtin eval "$(command pandoc --bash-completion)"
[[ -x $(builtin type -P node 2>/dev/null) ]] && builtin eval "$(command node --completion-bash)" && builtin eval "$(command npm completion)"
[[ -x $(builtin type -P pip3 2>/dev/null) ]] && builtin eval "$(command python3 -m pip completion --bash)"

_vim() {
  local cur="${COMP_WORDS[COMP_CWORD]}"
  if [[ "${COMP_WORDS[COMP_CWORD - 1]}" =~ n?vim ]] && [[ $cur == -* ]]; then
    local comms="--help -h -E -s -d -R -Z -m -M  -b -n -r --cmd -u --noplugin -p -o -O + -c -S -s -w -W --startuptime -i"
    COMPREPLY=($(compgen -W "${comms}" -- ${cur}))
  fi
  return 0
}

complete -F _vim -A file vim nvim

_read() {
  local cur="${COMP_WORDS[COMP_CWORD]}"
  if [[ "${COMP_WORDS[COMP_CWORD - 1]}" == read ]]; then
    local comms="-a -d -i -n -N -p"
    COMPREPLY=($(compgen -W "${comms}" -- ${cur}))
  fi
  return 0
}

complete -F _read read

[[ $(command hostname) =~ raptor ]] && source /etc/bash_completion
# vim:ft=sh:
