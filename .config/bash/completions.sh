## Completions for bash(1). 
## 
## Can also be sourced by zsh if you autoload bashcompinit, see zshcompsys(1).

[[ -x $(type -P pandoc 2>/dev/null) ]] && eval "$(command pandoc --bash-completion)"
[[ -x $(type -P stack 2>/dev/null) ]] && eval "$(command stack --bash-completion-script stack)"
[[ -x $(type -P pip3 2>/dev/null) ]] && eval "$(command pip3 completion --bash)"

_cargo() {
  local cur prev words cword
  _get_comp_words_by_ref cur prev words cword

  COMPREPLY=()

  # Skip past - and + options to find the command.
  local nwords=${#words[@]}
  local cmd_i cmd
  for ((cmd_i = 1; cmd_i < $nwords; cmd_i++)); do
    if [[ ! "${words[$cmd_i]}" =~ ^[+-] ]]; then
      cmd="${words[$cmd_i]}"
      break
    fi
  done

  local vcs='git hg none'
  local color='auto always never'
  local msg_format='human json'

  local opt_help='-h --help'
  local opt_verbose='-v --verbose'
  local opt_quiet='-q --quiet'
  local opt_color='--color'
  local opt_common="$opt_help $opt_verbose $opt_quiet $opt_color"
  local opt_pkg='-p --package'
  local opt_feat='--features --all-features --no-default-features'
  local opt_mani='--manifest-path'
  local opt_jobs='-j --jobs'
  local opt_force='-f --force'
  local opt_test='--test --bench'
  local opt_lock='--frozen --locked'

  local opt___nocmd="$opt_common -V --version --list"
  local opt__bench="$opt_common $opt_pkg $opt_feat $opt_mani $opt_lock $opt_jobs $opt_test --message-format --target --lib --bin --example --no-run"
  local opt__build="$opt_common $opt_pkg $opt_feat $opt_mani $opt_lock $opt_jobs $opt_test --message-format --target --lib --bin --example --release"
  local opt__check="$opt_common $opt_pkg $opt_feat $opt_mani $opt_lock $opt_jobs $opt_test --message-format --target --lib --bin --example --release"
  local opt__clean="$opt_common $opt_pkg $opt_mani $opt_lock --target --release"
  local opt__doc="$opt_common $opt_pkg $opt_feat $opt_mani $opt_lock $opt_jobs --message-format --bin --lib --target --open --no-deps --release"
  local opt__fetch="$opt_common $opt_mani $opt_lock"
  local opt__generate_lockfile="${opt__fetch}"
  local opt__git_checkout="$opt_common $opt_lock --reference --url"
  local opt__help="$opt_help"
  local opt__init="$opt_common $opt_lock --bin --lib --name --vcs"
  local opt__install="$opt_common $opt_feat $opt_jobs $opt_lock $opt_force --bin --branch --debug --example --git --list --path --rev --root --tag --vers"
  local opt__locate_project="$opt_mani -h --help"
  local opt__login="$opt_common $opt_lock --host"
  local opt__metadata="$opt_common $opt_feat $opt_mani $opt_lock --format-version --no-deps"
  local opt__new="$opt_common $opt_lock --vcs --bin --lib --name"
  local opt__owner="$opt_common $opt_lock -a --add -r --remove -l --list --index --token"
  local opt__package="$opt_common $opt_mani $opt_lock $opt_jobs --allow-dirty -l --list --no-verify --no-metadata"
  local opt__pkgid="${opt__fetch} $opt_pkg"
  local opt__publish="$opt_common $opt_mani $opt_lock $opt_jobs --allow-dirty --dry-run --host --token --no-verify"
  local opt__read_manifest="$opt_help $opt_verbose $opt_mani $opt_color --no-deps"
  local opt__run="$opt_common $opt_feat $opt_mani $opt_lock $opt_jobs --message-format --target --bin --example --release"
  local opt__rustc="$opt_common $opt_pkg $opt_feat $opt_mani $opt_lock $opt_jobs $opt_test --message-format --profile --target --lib --bin --example --release"
  local opt__rustdoc="$opt_common $opt_pkg $opt_feat $opt_mani $opt_lock $opt_jobs $opt_test --message-format --target --lib --bin --example --release --open"
  local opt__search="$opt_common $opt_lock --host --limit"
  local opt__test="$opt_common $opt_pkg $opt_feat $opt_mani $opt_lock $opt_jobs $opt_test --message-format --all --doc --target --lib --bin --example --no-run --release --no-fail-fast"
  local opt__uninstall="$opt_common $opt_lock --bin --root"
  local opt__update="$opt_common $opt_pkg $opt_mani $opt_lock --aggressive --precise"
  local opt__verify_project="${opt__fetch}"
  local opt__version="$opt_help $opt_verbose $opt_color"
  local opt__yank="$opt_common $opt_lock --vers --undo --index --token"

  if [[ $cmd_i -ge $nwords-1 ]]; then
    # Completion before or at the command.
    if [[ "$cur" == -* ]]; then
      COMPREPLY=($(compgen -W "${opt___nocmd}" -- "$cur"))
    elif [[ "$cur" == +* ]]; then
      COMPREPLY=($(compgen -W "$(_toolchains)" -- "$cur"))
    else
      COMPREPLY=($(compgen -W "$__cargo_commands" -- "$cur"))
    fi
  else
    case "${prev}" in
      --vcs)
        COMPREPLY=($(compgen -W "$vcs" -- "$cur"))
        ;;
      --color)
        COMPREPLY=($(compgen -W "$color" -- "$cur"))
        ;;
      --message-format)
        COMPREPLY=($(compgen -W "$msg_format" -- "$cur"))
        ;;
      --manifest-path)
        _filedir toml
        ;;
      --bin)
        COMPREPLY=($(compgen -W "$(_bin_names)" -- "$cur"))
        ;;
      --test)
        COMPREPLY=($(compgen -W "$(_test_names)" -- "$cur"))
        ;;
      --bench)
        COMPREPLY=($(compgen -W "$(_benchmark_names)" -- "$cur"))
        ;;
      --example)
        COMPREPLY=($(compgen -W "$(_get_examples)" -- "$cur"))
        ;;
      --target)
        COMPREPLY=($(compgen -W "$(_get_targets)" -- "$cur"))
        ;;
      help)
        COMPREPLY=($(compgen -W "$__cargo_commands" -- "$cur"))
        ;;
      *)
        local opt_var=opt__${cmd//-/_}
        COMPREPLY=($(compgen -W "${!opt_var}" -- "$cur"))
        ;;
    esac
  fi

  # compopt does not work in bash version 3

  return 0
}

complete -F _cargo cargo

__cargo_commands=$(cargo --list 2>/dev/null | tail -n +2)

_locate_manifest() {
  local manifest=$(cargo locate-project 2>/dev/null)
  # regexp-replace manifest '\{"root":"|"\}' ''
  echo ${manifest:9:-2}
}

# Extracts the values of "name" from the array given in $1 and shows them as
# command line options for completion
_get_names_from_array() {
  local manifest=$(_locate_manifest)
  if [[ -z $manifest ]]; then
    return 0
  fi

  local last_line
  local -a names
  local in_block=false
  local block_name=$1
  while read line; do
    if [[ $last_line == "[[$block_name]]" ]]; then
      in_block=true
    else
      if [[ $last_line =~ .*\[\[.* ]]; then
        in_block=false
      fi
    fi

    if [[ $in_block == true ]]; then
      if [[ $line =~ .*name.*\= ]]; then
        line=${line##*=}
        line=${line%%\"}
        line=${line##*\"}
        names+=($line)
      fi
    fi

    last_line=$line
  done <$manifest
  echo "${names[@]}"
}

# Gets the bin names from the manifest file
_bin_names() {
  _get_names_from_array "bin"
}

# Gets the test names from the manifest file
_test_names() {
  _get_names_from_array "test"
}

# Gets the bench names from the manifest file
_benchmark_names() {
  _get_names_from_array "bench"
}

_get_examples() {
  local files=($(dirname $(_locate_manifest))/examples/*.rs)
  local names=("${files[@]##*/}")
  local names=("${names[@]%.*}")
  # "*" means no examples found
  if [[ "${names[@]}" != "*" ]]; then
    echo "${names[@]}"
  fi
}

_get_targets() {
  local CURRENT_PATH
  if [ $(uname -o) == "Cygwin" -a -f "$PWD"/Cargo.toml ]; then
    CURRENT_PATH=$PWD
  else
    CURRENT_PATH=$(_locate_manifest)
  fi
  if [[ -z "$CURRENT_PATH" ]]; then
    return 1
  fi
  local TARGETS=()
  local FIND_PATHS=("/")
  local FIND_PATH LINES LINE
  while [[ "$CURRENT_PATH" != "/" ]]; do
    FIND_PATHS+=("$CURRENT_PATH")
    CURRENT_PATH=$(dirname $CURRENT_PATH)
  done
  for FIND_PATH in ${FIND_PATHS[@]}; do
    if [[ -f "$FIND_PATH"/.cargo/config ]]; then
      LINES=($(grep "$FIND_PATH"/.cargo/config -e "^\[target\."))
      for LINE in ${LINES[@]}; do
        TARGETS+=($(sed 's/^\[target\.\(.*\)\]$/\1/' <<<$LINE))
      done
    fi
  done
  echo "${TARGETS[@]}"
}

_toolchains() {
  local result=()
  local toolchains=$(rustup toolchain list)
  local channels="nightly|beta|stable|[0-9]\.[0-9]{1,2}\.[0-9]"
  local date="[0-9]{4}-[0-9]{2}-[0-9]{2}"
  while read line; do
    # Strip " (default)"
    line=${line%% *}
    if [[ "$line" =~ ^($channels)(-($date))?(-.*) ]]; then
      if [[ -z ${BASH_REMATCH[3]} ]]; then
        result+=("+${BASH_REMATCH[1]}")
      else
        # channel-date
        result+=("+${BASH_REMATCH[1]}-${BASH_REMATCH[3]}")
      fi
      result+=("+$line")
    fi
  done <<<"$toolchains"
  echo "${result[@]}"
}

# _go() {
# local cur="${COMP_WORDS[COMP_CWORD]}"
# case "${COMP_WORDS[COMP_CWORD - 1]}" in
# "go")
# local comms="build clean doc env bug fix fmt generate get install list run test tool version vet"
# COMPREPLY=($(compgen -W "${comms}" -- ${cur}))
# ;;
# *)
# local files=$(command find ${PWD} -mindepth 1 -maxdepth 1 -type f -iname "*.go" -exec basename {} \;)
# local dirs=$(command find ${PWD} -mindepth 1 -maxdepth 1 -type d -not -name ".*" -exec basename {} \;)
# local repl="${files} ${dirs}"
# COMPREPLY=($(compgen -W "${repl}" -- ${cur}))
# ;;
# esac
# return 0
# }

# not using go for now
# complete -F _go go

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

_yarn_completion() {
  local curr prev opts commands comp get_scripts get_dependencies get_global_dependencies yarn_config

  COMPREPLY=()
  curr="${COMP_WORDS[COMP_CWORD]}"
  prev="${COMP_WORDS[COMP_CWORD - 1]}"
  yarn_config="$HOME/.config/yarn"
  commands="access add autoclean bin cache check config create exec generate-lock-entry global help import info init install licenses link list login logout outdated owner pack publish remove run tag team unlink upgrade upgrade-interactive version versions why workspace"
  opts="-h --help -v --version --verbose --offline --prefer-offline --strict-semver --json --ignore-scripts --har --ignore-platform --ignore-engines --ignore-optional --force --skip-integrity-check --check-files --no-bin-links --flat --prod --production --no-lockfile --pure-lockfile --frozen-lockfile --link-duplicates --link-folder --global-folder --modules-folder --preferred-cache-folder --cache-folder --mutex --emoji -s --silent --cwd --proxy --https-proxy --registry --no-progress --network-concurrency --network-timeout --non-interactive --scripts-prepend-node-path --no-node-version-check"

  # Node scripts
  get_scripts="const pkg = require('./package.json'); \
    const scripts = pkg.scripts ? Object.keys(pkg.scripts) : [];
  console.log(scripts.join(' '))"
  get_dependencies="const pkg = require('./package.json'); \
    const deps = pkg.dependencies ? Object.keys(pkg.dependencies) : [];
  const devDeps = pkg.devDependencies ? Object.keys(pkg.devDependencies) : []; \
    console.log(deps.concat(devDeps).join(' '));"
  get_global_dependencies="const path = require('path'); \
    const yarnPath = '.config/yarn/global/package.json';
  const pkg = require(path.join(process.env.HOME, yarnPath)); \
    const deps = pkg.dependencies ? Object.keys(pkg.dependencies) : [];
  console.log(deps.join(' '));"

  # Options
  if [[ "$curr" == -* ]]; then
    case "${COMP_WORDS[1]}" in
      add)
        opts="$opts -W --ignore-workspace-root-check -D --dev -P --peer -O --optional -E --exact -T --tilde"
        ;;
      autoclean)
        opts="$opts -I --init -F --force"
        ;;
      cache)
        opts="$opts --pattern"
        ;;
      check)
        opts="$opts --integrity --verify-tree"
        ;;
      generate-lock-entry)
        opts="$opts --use-manifest --resolved --registry"
        ;;
      global)
        opts="$opts --prefix --latest"
        ;;
      init)
        opts="$opts -y --yes -p --private"
        ;;
      list)
        opts="$opts --depth --pattern"
        ;;
      pack)
        opts="$opts -f --filename"
        ;;
      publish)
        opts="$opts --new-version --message --no-git-tag-version --access --tag"
        ;;
      upgrade | upgrade-interactive)
        opts="$opts -S --scope -L --latest -E --exact -P --pattern -T --tilde -C --caret"
        ;;
      version)
        opts="$opts --new-version --message --no-git-tag-version"
        ;;
    esac

    COMPREPLY=($(compgen -W "$opts" -- "$curr"))
    return 0
  fi

  # Command expected
  if [[ $COMP_CWORD == 1 ]]; then
    COMPREPLY=($(compgen -W "$commands" -- "$curr"))
    return 0
  fi

  # First argument to commands
  if [[ $COMP_CWORD == 2 ]]; then
    case "${COMP_WORDS[1]}" in
      access)
        comp="public restricted grant revoke ls-packages ls-collaborators edit"
        ;;
      cache)
        comp="list dir clean"
        ;;
      config)
        comp="set get delete list current"
        ;;
      create)
        comp="react-app react-native-app next-app vue-app elm-app xp-app"
        ;;
      global)
        comp="add bin dir list remove upgrade upgrade-interactive"
        ;;
      help)
        comp="$commands"
        ;;
      licenses)
        comp="list generate-disclaimer"
        ;;
      link | unlink)
        if [[ -d "$yarn_config/link" ]]; then
          comp=$(find "$yarn_config/link" -type l | sed -e "s#$yarn_config/link/##")
        fi
        ;;
      owner)
        comp="add remove list"
        ;;
      info | outdated | remove | upgrade)
        if [[ -f "./package.json" ]]; then
          comp=$(node -e "$get_dependencies")
        fi
        ;;
      run)
        if [[ -f "./package.json" ]]; then
          comp=$(node -e "$get_scripts")
        fi
        ;;
      tag)
        comp="add remove list"
        ;;
      team)
        comp="create destroy add remove list"
        ;;
      why)
        if [[ -d "./node_modules" ]]; then
          comp=$(ls node_modules)
        fi
        ;;
    esac

    COMPREPLY=($(compgen -W "$comp" -- "$curr"))
    return 0
  fi

  # Commands with more than one argument
  case "${COMP_WORDS[1]}" in
    global)
      case "${COMP_WORDS[2]}" in
        remove | upgrade)
          if [[ -f "$yarn_config/global/package.json" ]]; then
            comp=$(node -e "$get_global_dependencies")
          fi
          ;;
      esac
      ;;
    link | unlink)
      if [[ -d "$yarn_config/link" ]]; then
        comp=$(find "$yarn_config/link" -type l | sed -e "s#$yarn_config/link/##")
      fi
      ;;
    outdated | remove | upgrade)
      if [[ -f "./package.json" ]]; then
        comp=$(node -e "$get_dependencies")
      fi
      ;;
  esac

  COMPREPLY=($(compgen -W "$comp" -- "$curr"))
  return 0
}

complete -o default -F _yarn_completion yarn

_git-journal() {
  local i cur prev opts cmds
  COMPREPLY=()
  cur="${COMP_WORDS[COMP_CWORD]}"
  prev="${COMP_WORDS[COMP_CWORD - 1]}"
  cmd=""
  opts=""

  for i in ${COMP_WORDS[@]}; do
    case "${i}" in
      git-journal)
        cmd="git-journal"
        ;;

      help)
        cmd+="__help"
        ;;
      p)
        cmd+="__p"
        ;;
      prepare)
        cmd+="__prepare"
        ;;
      s)
        cmd+="__s"
        ;;
      setup)
        cmd+="__setup"
        ;;
      v)
        cmd+="__v"
        ;;
      verify)
        cmd+="__verify"
        ;;
      *) ;;

    esac
  done

  case "${cmd}" in
    git-journal)
      opts=" -a -g -s -u -h -V -p -n -e -t -o  --all --generate --short --skip-unreleased --help --version --path --tags-count --template --output  <revision range>  prepare setup verify help  p  s  v"
      if [[ ${cur} == -* || ${COMP_CWORD} -eq 1 ]]; then
        COMPREPLY=($(compgen -W "${opts}" -- ${cur}))
        return 0
      fi
      case "${prev}" in

        --path)
          COMPREPLY=($(compgen -f ${cur}))
          return 0
          ;;
        -p)
          COMPREPLY=($(compgen -f ${cur}))
          return 0
          ;;
        --tags-count)
          COMPREPLY=($(compgen -f ${cur}))
          return 0
          ;;
        -n)
          COMPREPLY=($(compgen -f ${cur}))
          return 0
          ;;
        -e)
          COMPREPLY=($(compgen -f ${cur}))
          return 0
          ;;
        --template)
          COMPREPLY=($(compgen -f ${cur}))
          return 0
          ;;
        -t)
          COMPREPLY=($(compgen -f ${cur}))
          return 0
          ;;
        --output)
          COMPREPLY=($(compgen -f ${cur}))
          return 0
          ;;
        -o)
          COMPREPLY=($(compgen -f ${cur}))
          return 0
          ;;
        *)
          COMPREPLY=()
          ;;
      esac
      COMPREPLY=($(compgen -W "${opts}" -- ${cur}))
      return 0
      ;;

    git__journal__help)
      opts=" -h -V  --help --version  "
      if [[ ${cur} == -* || ${COMP_CWORD} -eq 2 ]]; then
        COMPREPLY=($(compgen -W "${opts}" -- ${cur}))
        return 0
      fi
      case "${prev}" in

        *)
          COMPREPLY=()
          ;;
      esac
      COMPREPLY=($(compgen -W "${opts}" -- ${cur}))
      return 0
      ;;
    git__journal__p)
      opts=" -h -V  --help --version  <COMMIT_MSG> <TYPE> "
      if [[ ${cur} == -* || ${COMP_CWORD} -eq 2 ]]; then
        COMPREPLY=($(compgen -W "${opts}" -- ${cur}))
        return 0
      fi
      case "${prev}" in

        *)
          COMPREPLY=()
          ;;
      esac
      COMPREPLY=($(compgen -W "${opts}" -- ${cur}))
      return 0
      ;;
    git__journal__prepare)
      opts=" -h -V  --help --version  <COMMIT_MSG> <TYPE> "
      if [[ ${cur} == -* || ${COMP_CWORD} -eq 2 ]]; then
        COMPREPLY=($(compgen -W "${opts}" -- ${cur}))
        return 0
      fi
      case "${prev}" in

        *)
          COMPREPLY=()
          ;;
      esac
      COMPREPLY=($(compgen -W "${opts}" -- ${cur}))
      return 0
      ;;
    git__journal__s)
      opts=" -h -V  --help --version  "
      if [[ ${cur} == -* || ${COMP_CWORD} -eq 2 ]]; then
        COMPREPLY=($(compgen -W "${opts}" -- ${cur}))
        return 0
      fi
      case "${prev}" in

        *)
          COMPREPLY=()
          ;;
      esac
      COMPREPLY=($(compgen -W "${opts}" -- ${cur}))
      return 0
      ;;
    git__journal__setup)
      opts=" -h -V  --help --version  "
      if [[ ${cur} == -* || ${COMP_CWORD} -eq 2 ]]; then
        COMPREPLY=($(compgen -W "${opts}" -- ${cur}))
        return 0
      fi
      case "${prev}" in

        *)
          COMPREPLY=()
          ;;
      esac
      COMPREPLY=($(compgen -W "${opts}" -- ${cur}))
      return 0
      ;;
    git__journal__v)
      opts=" -h -V  --help --version  <COMMIT_MSG> "
      if [[ ${cur} == -* || ${COMP_CWORD} -eq 2 ]]; then
        COMPREPLY=($(compgen -W "${opts}" -- ${cur}))
        return 0
      fi
      case "${prev}" in

        *)
          COMPREPLY=()
          ;;
      esac
      COMPREPLY=($(compgen -W "${opts}" -- ${cur}))
      return 0
      ;;
    git__journal__verify)
      opts=" -h -V  --help --version  <COMMIT_MSG> "
      if [[ ${cur} == -* || ${COMP_CWORD} -eq 2 ]]; then
        COMPREPLY=($(compgen -W "${opts}" -- ${cur}))
        return 0
      fi
      case "${prev}" in

        *)
          COMPREPLY=()
          ;;
      esac
      COMPREPLY=($(compgen -W "${opts}" -- ${cur}))
      return 0
      ;;
  esac
}

complete -F _git-journal -o bashdefault -o default git-journal

[[ $(command hostname) =~ raptor ]] && source /etc/bash_completion

# vim:ft=sh:
