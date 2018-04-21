# Can also be sourced by zsh if you autoload bashcompinit, see zshcompsys(1)

[[ -x $(type -P pandoc 2>/dev/null) ]] && eval "$(pandoc --bash-completion)"
[[ -x $(type -P stack 2>/dev/null) ]] && eval "$(stack --bash-completion-script stack)"

_go() {
  local cur="${COMP_WORDS[COMP_CWORD]}"
  case "${COMP_WORDS[COMP_CWORD - 1]}" in
    "go")
      local comms="build clean doc env bug fix fmt generate get install list run test tool version vet"
      COMPREPLY=($(compgen -W "${comms}" -- ${cur}))
      ;;
    *)
      local files=$(command find ${PWD} -mindepth 1 -maxdepth 1 -type f -iname "*.go" -exec basename {} \;)
      local dirs=$(command find ${PWD} -mindepth 1 -maxdepth 1 -type d -not -name ".*" -exec basename {} \;)
      local repl="${files} ${dirs}"
      COMPREPLY=($(compgen -W "${repl}" -- ${cur}))
      ;;
  esac
  return 0
}

complete -F _go go

_vim() {
  local cur="${COMP_WORDS[COMP_CWORD]}"
  if [[ "${COMP_WORDS[COMP_CWORD - 1]}" =~ n?vim ]] && [[ $cur == -* ]]; then
    local comms="-E -s -d -R -Z -m -M  -b -n -r --cmd -u --noplugin -p -o -O + -c -S -s -w -W --startuptime -i"
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

[[ $(hostname) =~ raptor ]] && source /etc/bash_completion

