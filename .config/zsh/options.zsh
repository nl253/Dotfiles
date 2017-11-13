
# $ZDOTDIR/options.zsh shell options (shopts) for zsh(1)

# this is necessary because the string "vim" is present in $EDITOR zsh will attempt to set ZLE to use vi mode
setopt EMACS
setopt AUTO_PUSHD
setopt PROMPT_SUBST
setopt CHASE_DOTS
setopt PUSHD_TO_HOME
setopt BRACE_CCL
setopt CDABLE_VARS
setopt COMPLETE_ALIASES
setopt COMPLETE_IN_WORD
setopt GLOB_DOTS
setopt HIST_EXPIRE_DUPS_FIRST
setopt HIST_FCNTL_LOCK
setopt HIST_FIND_NO_DUPS
setopt HIST_IGNORE_ALL_DUPS
setopt HIST_IGNORE_DUPS
setopt HIST_IGNORE_SPACE
setopt HIST_NO_FUNCTIONS
setopt HIST_NO_STORE
setopt HIST_REDUCE_BLANKS
setopt HIST_SAVE_NO_DUPS
setopt HIST_SUBST_PATTERN
setopt HIST_VERIFY
setopt INC_APPEND_HISTORY
setopt LONG_LIST_JOBS
setopt MARK_DIRS
setopt NO_BEEP
setopt NO_BG_NICE
setopt NO_HIST_BEEP
setopt NO_HUP
setopt NO_LIST_BEEP
setopt POSIX_IDENTIFIERS
setopt PUSHD_IGNORE_DUPS
setopt SHARE_HISTORY

# setopt ALWAYS_TO_END
# setopt AUTO_CONTINUE
# setopt AUTO_PARAM_KEYS
# setopt BAD_PATTERN
# setopt BANG_HIST
# setopt COMBINING_CHARS
# setopt EXTENDED_HISTORY
# setopt GLOB_ASSIGN
# setopt GLOB_STAR_SHORT
# setopt GLOB_SUBST
# setopt KSH_GLOB
# setopt LOCAL_OPTIONS
# setopt LOCAL_TRAPS
# setopt MENU_COMPLETE
# setopt PRINT_EXIT_VALUE
# setopt RC_EXPAND_PARAM
# setopt WARN_CREATE_GLOBAL
