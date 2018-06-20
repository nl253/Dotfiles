# Load the environment variables
. /home/norbert/.opam/opam-init/variables.sh > /dev/null 2> /dev/null || true

# Load the auto-complete scripts
if tty -s >/dev/null 2>&1; then
  . /home/norbert/.opam/opam-init/complete.zsh > /dev/null 2> /dev/null || true
fi

# Load the opam-switch-eval script
if tty -s >/dev/null 2>&1; then
  . /home/norbert/.opam/opam-init/switch_eval.sh > /dev/null 2> /dev/null || true
fi

