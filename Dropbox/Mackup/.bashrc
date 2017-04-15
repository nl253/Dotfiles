#!/usr/bin/env bash
#
# ~/.bashrc
#
# {{{  #  If not running interactively, don't do anything
[ -z "$PS1" ] && return
[[ $- != *i* ]] && return
# }}}

# colors # set variables to produce colored output later {{{
RED="\e[31m"
CYAN="\e[96m"
DARKMAGENTA="\e[35m"
MAGENTA="\e[95m"
BLUE="\e[34mB"
GREEN="\e[32m"
WHITE="\e[97mW"
DEFCOLOR="\e[39m"
YELLOW="\e[93m"
DARKYELLOW="\e[33m"
GREY="\e[37m"
DARKGREY="\e[90m"
# }}}

echo -e "${RED}~/.bashrc ${YELLOW}loaded" # indicator if it has successfully loaded

# $PS1 # prompt just for bash {{{
[ ! -n "${ZSH+2}" ] && export PS1="$(tput setaf 1)\w\n\[$(tput bold)\]\[$(tput setaf 1)\][\[$(tput setaf 3)\]\u\[$(tput setaf 2)\]@\[$(tput setaf 4)\]\h\[$(tput setaf 5)\]\[$(tput setaf 1)\]]\[$(tput setaf 7)\]\\$\[$(tput sgr0)\] " # }}}

unset MAILCHECK                      # Don't check mail when opening terminal.
export SHORT_HOSTNAME=$(hostname -s) # Set Xterm/screen/Tmux title with only a short hostname

# $BROWSER {{{

if [ -x /usr/bin/google-chrome-stable ]; then
        export BROWSER=google-chrome-stable
elif [ -x /usr/bin/elinks ]; then
        export BROWSER=elinks
elif [ -x /usr/bin/lynx ]; then
        export BROWSER=lynx
elif [ -x /usr/bin/w3m ]; then
        export BROWSER=w3m
fi

# }}}

# HISTORY {{{
export HISTSIZE=20000
export HISTFILESIZE=20000
export HISTCONTROL=ignoredups

# HISTIGNORE
# A colon-separated list of patterns used to decide which command lines should be saved on the history list.
# It must match the complete line (no implicit `*' is appended).
# The pattern is tested against the line after the checks specified by HISTCONTROL are applied.
# In addition to the normal shell pattern matching characters, `&' matches the previous  history  line.
# The pattern  matching honors the setting of the extglob shell option.
export HISTIGNORE="&:[ ]*:exit:ls:bg:fg:history:clear:jobs" # a colon separated list of items to ignore  }}}

# $IRC_CLIENT default to irssi and fall back on hexchat {{{
[ -x /usr/bin/irssi ] && export IRC_CLIENT='irssi'
[ ! -x /usr/bin/irssi ] && [ -x /usr/bin/hexchat ] && export IRC_CLIENT='hexchat' # }}}

export GREP_COLOR='1;33' # makes it yellow # by default red

# $PAGER less {{{

# if available enable syntax highlighting # fall back on more if less not available

[ -f /usr/bin/source-highlight-esc.sh ] && export LESSOPEN="| /usr/bin/source-highlight-esc.sh %s"
[ -x /usr/bin/less ] && alias less='less -x4RFsX' && export PAGER=less
[ ! -x /usr/bin/less ] && [ -x /usr/bin/more ] && export PAGER=more && alias less=more

# }}}

generate-inputrc() { # generate if not present and add configuration {{{
        [ -f ~/.inputrc ] && echo -e "Inputrc already exists in ~/.inputrc.\nNothing to do.\nAborting." && return 1
        echo "Inputrc not detecting.\nGenerating ..."
        cat /etc/inputrc >>~/.inputrc # copy defaults
        echo "set expand-tilde on" >>~/.inputrc
        echo "set skip-completed-text on" >>~/.inputrc
        echo "set echo-control-characters off" >>~/.inputrc
        echo "set completion-query-items 250" >>~/.inputrc
        echo "set page-completions off" >>~/.inputrc
        echo "set mark-symlinked-directories on" >>~/.inputrc
        echo "set bell-style none " >>~/.inputrc
        echo "set colored-stats on" >>~/.inputrc
        echo "set show-all-if-ambiguous on" >>~/.inputrc
        echo "set show-all-if-unmodified on" >>~/.inputrc
        echo "set colored-completion-prefix on" >>~/.inputrc
}

# }}}

# $PATH (and JAVA_HOME and JRE_HOME) {{{
[ -f ~/.config/ranger/rc.conf ] && export RANGER_LOAD_DEFAULT_RC=false
[ -d /usr/lib/jvm/java-8-openjdk ] && export JAVA_HOME='/usr/lib/jvm/java-8-openjdk' && export JRE_HOME='/usr/lib/jvm/java-8-openjdk/jre'
[ -d ~/.gem/rubu/2.4.0/bin ] && export PATH=${PATH}:"~/.gem/ruby/2.4.0/bin"
[ -d ~/.cargo/bin ] && export PATH=${PATH}:"~/.cargo/bin"
[ -d ~/.cabal/bin ] && export PATH="$HOME/.cabal/bin:$PATH"
[ -d ~/.config/composer/vendor/bin ] && export PATH=${PATH}:"~/.config/composer/vendor/bin"
[ -d ~/go/bin ] && export PATH=${PATH}:"~/go/bin"
[ -d ~/Scripts/ ] && export PATH="${PATH}:~/Scripts"
# }}}

# $EDITOR  {{{
if [ -x /usr/bin/nvim ]; then # if neovim
        export EDITOR=/usr/bin/nvim
        alias vim=/usr/bin/nvim
        alias vi=/usr/bin/nvim

elif [ -x /usr/bin/vim ]; then # if vim but not neovim
        export EDITOR=/usr/bin/vim
        alias nvim=/usr/bin/vim
        alias vi=/usr/bin/vim
        # set up vim plugins
elif [ -x /usr/bin/vi ]; then # if not neovim and not vim then fall back on vi
        export EDITOR=/usr/bin/vi
        alias vim=vi
        alias nvim=vi
fi
# }}}

open-it() { # {{{
        [ ! -r "$1" ] && return 1
        if [ -f "$1" ]; then
                $EDITOR "$1"
        elif [ -d "$1" ]; then
                [ -x /usr/bin/ranger ] && ranger "$1"
                [ -x ~/.ranger/ranger.py ] && ~/.ranger/ranger.py "$1"
        fi
} # }}}

if [ -x /usr/bin/fzf ]; then # {{{ FZF init # chech if on system # set up aliases in case it is and isn't
        [ -x /usr/bin/gdrive ] && alias gdrive-fzf='gdrive list | fzf --bind "enter:execute(echo {} | grep -P -o \"^\w+\")"'
        export FZF_DEFAULT_OPTS='--reverse --color hl:117,hl+:1,bg+:232,fg:240,fg+:246 '
        [ -x "/usr/bin/ag" ] && export FZF_DEFAULT_COMMAND='ag --hidden --ignore .git -g ""'

        FZFlocate() {
                [ $# = 0 ] && return 1
                locate $@ 2>/dev/null | grep -P -v "^/(dev)|(tmp)|(mnt)|(root)" | grep -P -v "\.(png)|(jpeg)|(bluej)|(ctxt)|(jpg)|(so)|(pyc)|(obj)|(out)|(class)|(swp)|(xz)|(ri)|(~)|(\d{4,})$" | grep -P -v -i ".*\%|(cache)|(chrome)|(timeshift).*" | fzf --bind "enter:execute($EDITOR {})"
        }

        FZFcheckout-branches-sorted() { # checkout git branch (including remote branches), sorted by most recent commit, limit 30 last branches
                local branches branch
                branches=$(git for-each-ref --count=30 --sort=-committerdate refs/heads/ --format="%(refname:short)") \
                        && branch=$(echo "$branches" \
                                | fzf-tmux -d $((2 + $(wc -l <<<"$branches"))) +m) \
                                && git checkout $(echo "$branch" | sed "s/.* //" | sed "s#remotes/[^/]*/##")
        }

        # FUNCTION :: vague file find use agrep
        # DEPENDENCIES :: agrep
        find-approx() {
                [ $# = 0 ] && echo -e "You have to provide 1 argument.\nAborting." && return 1
                cd ~
                [ ! -x /usr/bin/fzf ] && find ~ -readable -type f 2>/dev/null | agrep $1 | grep -P -v "(\d{4,}$)|(~$)" | grep -P -v "^/(dev)|(tmp)|(mnt)|(root)" | grep -P -v "\.((png)|(jpeg)|(bluej)|(ctxt)|(jpg)|(so)|(pyc)|(obj)|(out)|(class)|(swp)|(xz)|(ri))$" | grep -v "%" | grep -v -i "cache" | grep -v elpa | grep -v -i "chrome" | grep -v IdeaIC | grep -v -i "timeshift" | sort | uniq | sed "s/\/home\/norbert\///" | grep -v -i "Trash"
                [ -x /usr/bin/fzf ] && find ~ -readable -type f 2>/dev/null | agrep $1 | grep -P -v "(\d{4,}$)|(~$)" | grep -P -v "^/(dev)|(tmp)|(mnt)|(root)" | grep -P -v "\.((png)|(jpeg)|(bluej)|(ctxt)|(jpg)|(so)|(pyc)|(obj)|(out)|(class)|(swp)|(xz)|(ri))$" | grep -v "%" | grep -v -i "cache" | grep -v elpa | grep -v -i "chrome" | grep -v IdeaIC | grep -v -i "timeshift" | sort | uniq | sed "s/\/home\/norbert\///" | grep -v -i "Trash" | fzf --bind "enter:execute: $EDITOR {} \;"
        }

        FZFcheckout-branch-tag() {
                local tags branches target
                tags=$(
                        git tag | awk '{print "\x1b[31;1mtag\x1b[m\t" $1}'
                ) || return
                branches=$(
                        git branch --all | grep -v HEAD \
                                | sed "s/.* //" | sed "s#remotes/[^/]*/##" \
                                | sort -u | awk '{print "\x1b[34;1mbranch\x1b[m\t" $1}'
                ) || return
                target=$(
                        (
                                echo "$tags"
                                echo "$branches"
                        ) \
                                | fzf-tmux -l30 -- --no-hscroll --ansi +m -d "\t" -n 2
                ) || return
                git checkout $(echo "$target" | awk '{print $2}')
        }

        FZFcommits() {
                git log --graph --color=always \
                        --format="%C(auto)%h%d %s %C(black)%C(bold)%cr" "$@" \
                        | fzf --ansi --no-sort --reverse --tiebreak=index --bind=ctrl-s:toggle-sort \
                                --bind "ctrl-m:execute:
	(grep -o '[a-f0-9]\{7\}' | head -1 |
	xargs -I % sh -c 'git show --color=always % | less -R') << 'FZF-EOF'
	{}
	FZF-EOF"
        }

        FZFcommit-sha() {
                local commits commit
                commits=$(git log --color=always --pretty=oneline --abbrev-commit --reverse) \
                        && commit=$(echo "$commits" | fzf --tac +s +m -e --ansi --reverse) \
                                && echo -n -e $(echo "$commit" | sed "s/ .*//")
        }

        # FZFstash - list of your stashes
        # [enter] shows you the contents of the stash
        # [ctrl-d] shows a diff of the stash against your current HEAD
        # [ctrl-b] checks the stash out as a branch, for easier merging
        FZFstash() {
                local out q k sha
                while out=$(
                        git stash list --pretty="%C(yellow)%h %>(14)%Cgreen%cr %C(blue)%gs" \
                                | fzf --ansi --no-sort --query="$q" --print-query \
                                        --expect=ctrl-d,ctrl-b
                ); do
                        mapfile -t out <<<"$out"
                        q="${out[0]}"
                        k="${out[1]}"
                        sha="${out[-1]}"
                        sha="${sha%% *}"
                        [[ -z "$sha" ]] && continue
                        if [[ "$k" == 'ctrl-d' ]]; then
                                git diff $sha
                        elif [[ "$k" == 'ctrl-b' ]]; then
                                git stash branch "stash-$sha" $sha
                                break
                        else
                                git stash show -p $sha
                        fi
                done
        }

        FZFcd() { # quickly change dir
                local dir
                dir=$(find ${1:-.} -type d 2>/dev/null | fzf +m) && cd "$dir"
        }

        FZFctags() { # search ctags
                local line
                [ -e tags ] \
                        && line=$(
                                awk 'BEGIN { FS="\t" } !/^!/ {print toupper($4)"\t"$1"\t"$2"\t"$3}' tags \
                                | cut -c1-80 | fzf --nth=1,2
                        ) && ${EDITOR:-vim} $(cut -f3 <<<"$line") -c "set nocst" \
                                -c "silent tag $(cut -f2 <<<"$line")"
        }

        FZFcheckout-commit() {
                local commits commit
                commits=$(git log --pretty=oneline --abbrev-commit --reverse) \
                        && commit=$(echo "$commits" | fzf --tac +s +m -e) \
                                && git checkout $(echo "$commit" | sed "s/ .*//")
        }

        alias l=FZFlocate
        alias p=FZFpkill                      # [P]ROCESS
        alias c=FZFcd                         # [C]D
        alias gl=FZFcommits                   # [G]IT [L]OG
        alias gcs=FZFcommit-sha               # [G]IT [C]OMMIT [S]HA
        alias gc=FZFcheckout-commit           # [G]IT [C]HECKOUT
        alias gcb=FZFcheckout-branches-sorted # [G]IT [C]HECKOUT [B]RANCHES
        alias gcbt=FZFcheckout-branch-tag     # [G]IT [C]HECKOUT [B]RANCH [T]AG
        alias gt=FZFctags                     # [G]IT [T]AGS
        alias gs=FZFstash                     # [G]IT [S]TASH
        # [L]IST [R]ECENT
        alias lr='find ~ -cmin -10 -type f 2>/dev/null | grep -P -v ".*C|cache.*" | grep -v chrome | grep -v ".dropbox" | grep -v "%" | fzf --bind "enter:execute: $EDITOR {} \;"'
else # non fzf solution
        alias lr='find ~ -cmin -10 -type f 2>/dev/null | grep -P -v ".*C|cache.*" | grep -v chrome | grep -v ".dropbox" | grep -v "%"'
        [ -x /usr/bin/htop ] && alias p=htop || alias p=top # process management
        # alias gc=  # TODO provide an alternative if fzf is not available
        # alias gs=  # TODO provide an alternative if fzf is not available
        # alias gcs=  # TODO provide an alternative if fzf is not available
        # alias gt=  # TODO provide an alternative if fzf is not available
        # alias c=  # TODO provide an alternative if fzf is not available
        # alias gcb=  # TODO provide an alternative if fzf is not available
        alias gl='git log --pretty=format:"%C(yellow)%h  %Cblue%ad  %Creset%s%Cgreen  [%cn] %Cred%d" --decorate --date=relative'
fi

# }}}

# pacman {{{
if [ -x /usr/bin/pacman ]; then
        [ -x /usr/bin/expac ] && alias pacman-recent-installations="expac --timefmt='%Y-%m-%d %T' '%l\t%n' | sort | tail -n 20"
        [ -x /usr/bin/expac ] && alias pacman-packages-by-size="expac -S -H M '%k\t%n'"
        alias pacman-reinstall-all-native-packages="sudo pacman -Qnq | pacman -S -"
        alias pacman-reinstall-all-foreign-packages="sudo pacman -Qmq | pacman -S -"
        alias pacman-remove-orphans="sudo pacman -Rns $(pacman -Qtdq)"
        [ -x /usr/bin/yaourt ] && export YAOURT_COLORS="nb=1:pkg=1:ver=1;32:lver=1;45:installed=1;42:grp=1;34:od=1;41;5:votes=1;44:dsc=0:other=1;35"
fi
# }}}

# git {{{
[ -x /usr/bin/hub ] && eval "$(hub alias -s)" && alias g=hub
[ -x /usr/bin/tig ] && alias t=tig

setup-git() {
        [ -x /usr/bin/git-extras ] && git extras update
        [ ! -x /usr/bin/git-extras ] && curl -sSL http://git.io/git-extras-setup | sudo bash /dev/stdin
        #[ ! -x /usr/bin/git-fire ] &&
        #[ ! -x /usr/bin/git-imerge ] &&
        #[ ! -x /usr/bin/git-stats ] &&
}
# }}}

# remap-capslock {{{
# Caps Lock is Control on a GB keyboard #setxkbmap -option ctrl:swapcaps # for US
setxkbmap -layout gb -option ctrl:nocaps && echo -e "${MAGENTA}capslock remapped to ctrl${DEFCOLOR}"
# }}}

[ -x "/usr/bin/ag" ] && alias ag='ag --hidden --pager="less -MIRFX"' # search with dotfiles page to less with colors

# ALIASES {{{

alias e="$EDITOR"
alias todo=todo-detect
alias x=xonsh

alias le="ls -lo"
alias ll='ls -l -a --group-directories-first --time-style=+"%d.%m.%Y %H:%M" --color=auto -F'
alias ls='LC_COLLATE=C ls --color=auto --group-directories-first'
alias f=find-approx

alias -- -='cd -' # Go back
alias ..="cd .."
alias ...="cd ..."
alias ....="cd ...."
alias .....="cd ....."
alias ......="cd ....."
alias .......="cd ......"
alias ........="cd ......."
alias .........="cd ......."
alias ..........="cd ........."
alias diff='diff --color=auto'
alias grep='grep --color=auto'
alias fgrep='fgrep --color=auto'
alias egrep='egrep --color=auto'
[ ! -x /usr/bin/tree ] && alias tree="find . -print | sed -e 's;[^/]*/;|____;g;s;____|; |;g'" # in case tree is not present on the system
[ -x /usr/bin/dmenu_run ] && alias dmenu_run="dmenu_run -p ' >> ' -nb black -nf white"        # dmenu # a good alternative to rofi
[ -x /usr/bin/aspell ] && alias aspell="aspell -c -l en_GB"                                   # set up logging in ~/Downloads/Torrents/aria2c.log and a default location for download of Torrents :: ~/Downloads/Torrents/
alias df='df --human-readable --si'
alias info='info --vi-keys'
[ -x /usr/bin/aria2c ] && alias aria2c="mkdir -p \"${HOME}/Downloads/Torrents/\" ; touch \"${HOME}/Downloads/Torrents/aria2c.log\" ; aria2c --continue --dir=\"${HOME}/Downloads/Torrents\" --log=\"${HOME}/Downloads/Torrents/aria2c.log\""
alias freq='cut -f1 -d" " "$HISTFILE" | sort | uniq -c | sort -nr | head -n 30'
alias logout="pkill -KILL -u "
alias todo-detect='for i in $( find ~ -mtime -1 -type f 2>/dev/null | grep -P -v ".*C|cache.*" | grep -v chrome | grep -v "bash_history" | grep -v ".dropbox" | grep -v "%" | grep -v ".git" | grep -v "vim/plugged" | sed -E -r '/^.{,7}$/d' ); do ag --vimgrep TODO $i ; done | sed "s/\/home\/norbert/~/" | grep TODO'
alias symlinks-pretty='for i in $(find -type l -exec echo {} \;); do echo -e " \e[36m$i  \e[39m->  \e[91m$(readlink -f $i)" ; done'
alias show-term-capabilities="infocmp -1 | sed -nu 's/^[ \000\t]*//;s/[ \000\t]*$//;/[^ \t\000]\{1,\}/!d;/acsc/d;s/=.*,//p'|column -c80"
#alias j=jobs # used by autojump
alias untar='tar -xvf'
alias scripts-in-bashrc="grep -P '^\S+?\(\)' ~/.bashrc | sed  's/(//g' | sed 's/{//' | sed 's/)//g'"
alias keybingings="bind -p | grep -v '^#\|self-insert\|^$'" # Readline
alias http-server="python3 -m http.server"
[ -x /usr/bin/sshfs ] && alias mount-raptor="sshfs -o transform_symlinks -o follow_symlinks nl253@raptor.kent.ac.uk: ~/Raptor" # mount a remote hard-drive
# }}}

setup-pyvirtualenv() { # {{{

        # python virtual env
        echo -e "checking virtual env"

        if [ ! -x /usr/bin/pyenv ] && [ ! -x ~/.pyenv/bin/pyenv ]; then
                echo -e "PYENV not detected\ninitiating ... "
                git clone "https://github.com/pyenv/pyenv.git" ~/.pyenv
                pyenv install "3.5.0"
                pyenv global "3.5.0"
                echo -e 'global python 3.5.0 activated'

        elif [ -x /usr/bin/pyenv ] || [ -x ~/.pyenv/bin/pyenv ]; then
                # if pyenv present on system initiate python 3.5.0
                echo -e "PYENV detected continuing ..."
                local PYENV_VERSION=$(pyenv version-name)
                [ ! "${PYENV_VERSION}" = "3.5.0" ] && pyenv install "3.5.0" && pyenv global "3.5.0"
        fi

} # }}}

setup-zsh() { # {{{ # to be run on a remote machine while logged in without superuser privilidges.

        [ ! -x /usr/bin/zsh ] && echo echo -e 'zsh not detected on your filesystem ... \nAborting' && return 1

        echo -e 'zsh detected on your filesystem ... \nSetting up z alias and checking for oh-my-zsh'

        [ -f ~/.oh-my-zsh/oh-my-zsh.sh ] && echo -e 'oh-my-zsh detected.\nNothing to be done.\nAborting.' && return 1

        echo -e "OH-MY-ZSH not detected\ninitiating ..."
        [ -e ~/.zshrc ] && echo -e ".zshrc and oh-my-zsh wasn't downloaded becuse an existing .zshrc is in your home directory.\nEither delete or backup.\nAborting." && return 1

        echo -e 'Downloading oh-my-zsh'

        # from oh-my-zsh [github]
        sh -c "$(curl -fsSL https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"

        echo -e 'oh-my-zsh replaced your .zshrc with its own version and backed it up to ~/.zshrc.pre-oh-my-zsh.\nThis script will replace it with my preconfigured zshrc and move this to ~/.zshrc-oh-my-zsh-preconfigured-defaults.'

        # custom plugins
        echo -e 'Attempting to download custom plugins for oh-my-zsh.'
        [ ! -d ~/.oh-my-zsh/custom/plugins/zsh-autosuggestions ] && git clone "git://github.com/zsh-users/zsh-autosuggestions" ~/.oh-my-zsh/custom/plugins/zsh-autosuggestions
        [ ! -d ~/.oh-my-zsh/custom/plugins/zsh-completions ] && git clone "https://github.com/zsh-users/zsh-completions" ~/.oh-my-zsh/custom/plugins/zsh-completions
        [ ! -d ~/.oh-my-zsh/custom/plugins/zsh-syntax-highlighting ] && git clone "https://github.com/zsh-users/zsh-syntax-highlighting.git" ~/.oh-my-zsh/custom/plugins/zsh-syntax-highlighting

        echo -e 'Moving oh-my-zsh preconfigured zshrc to ~/.zshrc-oh-my-zsh-preconfigured-defaults.\nReplacing with my custom .zshrc.'
        mv ~/.zshrc ~/.zshrc-oh-my-zsh-preconfigured-defaults

        curl https://raw.githubusercontent.com/nl253/Dot-files/master/Dropbox/Mackup/.zshrc >>~/.zshrc

        # source it
        echo -e 'resourcing .zshrc.'
        zsh -c ~/.zshrc
}
alias z=zsh
# }}}

install-go-packages() { # {{{
        local GO=("github.com/mvdan/sh/cmd/shfmt")

        for i in $GO ; do
                go get -u "$i"
        done
} # }}}

install-cabal-packages() { # {{{
        local CAB=(ShellCheck)
        
        cabal update

        for i in ${CAB[*]}; do
                [ ! -e "$HOME/.cabal/bin/$i" ] && cabal install
        done
}

install-pip-packages() { # {{{

        # PYTHON
        # it is crucial that python is correctly set up
        # mackup is dependent on it as well as things like ranger

        echo -e "${YELLOW}PYTHON${DEFCOLOR}"
        if [ ! -x /usr/bin/pip ] && [ ! -x /usr/bin/pip3 ]; then # necessary for mackup
                echo -e "PIP and PYTHON are necessary to make this work.\nThe script will terminate, \nmake sure pip is installed to proceed"
                return 1
        fi

        echo -e "PYTHON and PIP detected\ninstalling PYTHON packages"

        local PIP=(
                "mackup" "yapf" "newspaper" "textblob" "ranger" "requests" "scrapy"
                "pudb" "neovim" "jedi" "mypy" "spacy" "xonsh" "xontrib-z"
                "psutil" "nltk" "pytest" "ipython" "sumy" "fuzzywuzzy"
                "you-get" "pandas" "tensorflow" "numpy" "flake8" "vint")

        for i in ${PIP[*]}; do
                pip install --user --quiet --exists-action i "$i"
        done

        # set up bash completion for pip if it doesn't exist
        # [ ! -e /etc/bash_completion.d/pip ] && sudo pip completion --bash >> /etc/bash_completion.d/pip # sudo won't work for redirects

} # }}}

install-npm-packages() { # {{{

        [ ! -x /usr/bin/npm ] && echo -e "NPM not deteceted on the filesystem\nmake sure NPM is installed to install JAVSCRIPT packages" && return 1

        echo -e "NODE and NPM detected\ninstalling NODE packages"

        local NPM=(proselint
                "write-good" "remark" "remark-cli" textlint
                "git-standup" "git-stats" "git-extras"
                tern "git-fire"
                "js-beautify" 
                jsonlint csslint tidy writegood)

        for i in ${NPM[*]}; do
                npm install "$i"
        done

} # }}}

install-gem-packages() { # {{{
        local GEM=()


        echo -e "${RED}RUBY${DEFCOLOR}"

        # RUBY
        if [ ! -x /usr/bin/gem ] || [ ! -x /usr/bin/ruby ]; then
                echo -e "either RUBY or GEM was not detected on this filesystem"
                echo -e "make sure GEM is installed to install RUBY packages"
                echo -e "becasue RUBY is not cructial the script will continue"
                sleep 5 # sleep for long enough to see

        else # if both gem and ruby found

                echo -e "RUBY and GEM detected\ninstalling RUBY gems"

                local RB=(mdl sqlint rubocop) # ruby gems

                for i in ${GEM[*]}; do
                        gem install "$i"
                done
        fi


}

# }}}

install-alacritty() { # {{{
        if [ ! -x /usr/bin/alacritty ] && [ ! -x /usr/bin/rustup ] && [ ! -x /usr/bin/rustup ]; then
                echo -e "RUST and ALACRITTY not installed\ninitalising rustup"
                sudo curl https://sh.rustup.rs -sSf | sh
                echo -e "cloning ALACRITTY repo from GIT"
                git clone https://github.com/jwilm/alacritty.git ~
                echo -e "changing RUSTUP TOOLCHAIN to STABLE"
                sudo rustup override set stable
                sudo rustup update stable
                sudo rustup default stable
                echo -e "cd to ${HOME}/alacritty\nBUILDING ALACRITTY ... "
                cd ~/alacritty && sudo cargo build --release
        else
                return 1
        fi
}
# }}}

install-tmux-plugs() { # {{{
        # check if tmux plugin manager dir is present
        if [ -x /usr/bin/tmux ] && [ ! -d ~/.tmux/plugins/tpm ]; then
                echo -e "TMUX detected but TMUX PLUGIN MANAGER not present\ninitalising ..."
                git clone "https://github.com/tmux-plugins/tpm" ~/.tmux/plugins/tpm
        elif [ -x /usr/bin/tmux ] && [ -d ~/.tmux/plugins/tpm ]; then
                echo -e "TMUX detected along with TMUX PLUGIN MANAGER\nNothing to do.\nAborting."
        elif [ ! -x /usr/bin/tmux ]; then
                return 1
                echo -e "Tmux is not installed.\nAborting."
                sleep 5
        fi
} # }}}

install-ranger() { # {{{
        if [ ! -x /usr/bin/ranger ] && [ ! -e ~/.ranger ]; then # check if ranger is installed, if not use a git-workaround
                [ ! -f ~/.ranger/ranger.py ] && mkdir -p ~/.ranger && git clone 'https://github.com/ranger/ranger' ~/.ranger/
        fi # if present set up an alias
}

[ -x /usr/bin/ranger ] && alias r='ranger'
[ -x ~/.ranger/ranger.py ] && alias r=~/.ranger/ranger.py && alias ranger=~/.ranger/ranger.py

# }}}

install-vim-plug() { # vim plugins {{{
        if [ -x /usr/bin/nvim ] && [ ! -f ~/.local/share/nvim/site/autoload/plug.vim ]; then # if vim but not neovim
               curl -fLo ~/.local/share/nvim/site/autoload/plug.vim --create-dirs \
                        https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
        elif [ -x /usr/bin/vim ] && [ ! -f ~/.local/share/nvim/site/autoload/plug.vim ]; then # if vim but not neovim
                curl -fLo ~/.vim/autoload/plug.vim --create-dirs \
                        https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
        else
                [ ! -x /usr/bin/nvim ] && [ ! -x /usr/bin/vim ] && echo -e "Neither vim nor neovim is installed on the system.\nAborting." && return 1
                echo -e "Vim-plug already set up.\nNothing to do here."
                return 1
        fi
} # }}}

download-vimrc() { # download from the master branch  {{{
        if [ -e ~/.vimrc ] && [ ! -x /usr/bin/nvim ] ; then  
                mv ~/.vimrc ~/.vimrc.backup && echo -e 'Detected an existing .vimrc file.\nIt was moved to ~/.vimrc.backup.'
        elif [ -e ~/.config/nvim/init.vim ] && [ -x /usr/bin/nvim ] ; then 
                mv ~/.config/nvim/init.vim ~/.config/nvim/init.vim.backup && echo -e 'Detected an existing .init.vim file.\nIt was moved to ~/.config/nvim/init.vim.backup.'
        fi
        if [ -x /usr/bin/nvim ] && [ ! -f /.config/nvim/init.vim ]; then # if vim but not neovim
                curl -o --create-dirs ~/.config/nvim/init.vim https://raw.githubusercontent.com/nl253/Dot-files/master/Dropbox/Mackup/.config/nvim/init.vim >> 
        elif [ -x /usr/bin/vim ] && [ ! -f ~/.vimrc ]; then # if vim but not neovim
                curl -o --create-dirs ~/.vimrc https://raw.githubusercontent.com/nl253/Dot-files/master/Dropbox/Mackup/.config/nvim/init.vim 
        else
                [ ! -x /usr/bin/nvim ] && [ ! -x /usr/bin/vim ] && echo -e "Neither vim nor neovim is installed on the system.\nAborting." && return 1
        fi
} # }}}

download-bashrc() { # download from the master branch  {{{
        if [ -e ~/.bashrc ] ; then 
                mv ~/.bashrc ~/.bashrc.backup  || echo -e "Something went wrong" && return 1
                echo -e "Existing .bashrc was detected on the system.\nIt was moved to ~/.bashrc.backup." 
        fi
        [ ! -e ~/.bashrc ] && curl -o --create-dirs ~/.bashrc https://raw.githubusercontent.com/nl253/Dot-files/master/Dropbox/Mackup/.bashrc
} # }}}

download-gitconfig() { # {{{
        [ ! -x /usr/bin/git ] && echo -e 'Git not detected on this system.\nAborting.' && return 1
        if [ -e ~/.gitconfig ] ; then 
                mv ~/.gitconfig ~/.gitconfig.backup  || echo -e "Something went wrong" && return 1
                echo -e "Existing .gitconfig was detected on the system.\nIt was moved to ~/.gitconfig.backup." 
        fi
        [ ! -f ~/.gitconfig ] && curl -o --create-dirs  ~/.gitconfig https://raw.githubusercontent.com/nl253/Dot-files/master/Dropbox/Mackup/.gitconfig
} # }}}

# transfer-dotfiles {{{ # TODO test with --dry-run
# FUNCTION :: transfer the necessary {dot}files to a remote machine (sftp server)
# NARGS 1 : [ssh address in the style nl253@raptor.kent.ac.uk]

download-scripts() {
        [ ! -d ~/Scripts ] && mkdir -p ~/Scripts
        git clone --recursive https://github.com/nl253/Scripts ~/Scripts/
}

download-personal() {
        [ ! -d ~/nl253 ] && mkdir -p ~/nl253
        git clone --recursive https://github.com/nl253/Notes ~/nl253
}

download-dotfiles() { # to be run on a remote machine, on a local machine it would be called from a sys-restore script
        download-bashrc
        download-gitconfig
        download-vimrc
} # }}}

# remote-setup {{{
# FUNCTION :: a high level function that aims setup everything on a remote machine
# You must be logged into that machine to run it.
remote-setup() {
        install-ranger
        install-vim-plug
        download-dotfiles
        download-scripts
        generate-inputrc
        setup-zsh
} # }}}

# todo-detect {{{
# ---------------
# FUNCTION :: detects 'TODO's in recently modified files [24h]
# avoids chrome .dropbox % (backup) .git vim/plugged (where vim plugins are stored) and {c,C}ache
# sed abbreviates /home/norbert to ~ to make the output shorter
# that seemingly redundant grep at the end highlights 'TODO' output
# ---------------
# DEPENDENCIES :: ag
# ---------------
alias todo-detect='for i in $( find ~ -mtime -1 -type f 2>/dev/null | grep -P -v ".*C|cache.*" | grep -v chrome | grep -v "bash_history" | grep -v ".dropbox" | grep -v "%" | grep -v ".git" | grep -v "vim/plugged" | grep -v "*.local/lib/*" | sed -E -r "/^.{,7}$/d" ); do ag --vimgrep TODO $i ; done | sed "s/\/home\/norbert/~/" | grep TODO'
# }}}

setup-onedrive() { # {{{
        if [ -x /usr/bin/onedrive ]; then
                systemctl --user enable onedrive
                systemctl --user start onedrive
                [ ! -d ~/.config/onedrive ] && mkdir -p ~/.config/onedrive
                [ ! -f ~/.config/onedrive/config ] && cp ./config ~/.config/onedrive/config
                touch ~/.config/onedrive/sync_list
                # sync_dir: directory where the files will be synced
                # skip_file: any files or directories that match this pattern will be skipped during sync
        else
                echo -e "You need to install onedrive-git \nAborting."
                return 1
        fi
} # }}}

setup-dropbox() { # {{{
        if [ ! -x /usr/bin/dropbox ] || [ ! -x /usr/bin/dropbox-cli ]; then
                echo -e "You need to install dropbox and dropbox-cli.\nAborting."
                return 1
        else
                dropbox-cli autostart y
        fi
} # }}}

mackup-restore() { # {{{
        if [ -x /usr/bin/dropbox ] && [ -d ~/Dropbox ] && [ -x /usr/bin/mackup ]; then
                mackup restore
        else
                echo -e "You need to install and set up MACKUP.\nAborting."
                return 1
        fi
} # }}}

setup-systemd() { # {{{
        sudo systemctl enable thermald
        sudo systemctl enable cronie
} # }}}

setup-gdrive() { # {{{
        if [ ! -x /usr/bin/gdrive ]; then
                echo -e "You need to install gdrive.\nAborting."
                return 1
        else
                [ ! -d ~/GDrive ] && mkdir -p ~/GDrive
                [ ! -f ~/GDrive/.gdriveignore ] && touch ~/GDrive/.gdriveignore
        fi
} # }}}

install-pacman-packages() { # {{{

        [ ! -x /usr/bin/pacman ] && echo -e "This script is preconfigured ONLY for Arch Linux.\nYou don't appear to have pacman.\nAborting." && return 1

        local NEED_TO_BE_INSTALLED=(\ # list of pacman packages
                "aria2c" "cronie" "fdupes" "ddupes"
                "aspell" "bluej" "ctags" "bashmount" "bmenu"
                "aspell-en" "gdrive" "ca-certificates"
                "crontab" "psysh" "emacs" "cmake"
                "csslint" "thinkfinger" "the_silver_searcher"
                "curl" "dos2unix" "pdftotext" "make"
                "freetype2" "fontconfig" "pkg-config"
                "ghc-mod" "cabal" "node" "gawk" "i3"
                "git" "expac" "onedrive-git" "git-imerge-git" "git-extras" "thinkfan"
                "google-chrome" "coreutils" "hub" "htop"
                "intellij-idea-community-edition" "jdk-8"
                "lshw" "less" "nvim" "spotify" "astyle"
                "python" "tig" "apacman" "yaourt" "tmux"
                "rofi" "stylish-haskell" "tidy"
                "sed" "pandoc" "openssh" "openvpn" "p7zip"
                "thermald" "dropbox" "dropbox-cli" "python-pip" "alsa-utils"
                "upower" "npm" "ruby" "gem" "timeshift"
                "wget" "curl" "wordnet" "xclip" "xclip"
                "xf86-input-keyboard" "xf86-input-libinput"
                "xf86-input-mouse" "xf86-input-synaptics"
                "xf86-input-void" "xf86-video-intel"
                "xmonad" "autojump" "php" "sncli" "bashlint"
                "xmonad-contrib" "shfmt" "xmonad-utils" "acpid"
                "perl" "shellcheck" "zsh")

        for i in ${NEED_TO_BE_INSTALLED[*]}; do # quite mode # won't give feedback # won't install if already present and up-to-date
                echo -e "${MAGENTA}installing ${i} ${DEFCOLOR}" # what is to be installed
                [ ! -x "/usr/bin/$i" ] && sudo pacman -S --quiet --noconfirm --needed "$i"
        done

} # }}}

# TODO restore-system {{{
# FUNCTION
# The aim of the script is to do nothing when the system is OK
# and restore the whole system when it's just been reinstalled.
# to be run on own machine with administrative privilidges

restore-system() {
        install-pacman-packages
        #setup-dropbox
        download-dotfiles
        install-vim-plug
        install-tmux-plugs
        # mackup-restore # needs authentication so won't work # TODO find a way to get Dropbox to work from a script
        setup-onedrive # needs authentication so won't work
        setup-gdrive   # needs authentication so won't work
        install-alacritty
        setup-systemd

        # at this point variables will need to be reset
        echo "RESOURCING BASHRC"
        source "~/.bashrc"

} # }}}

# show-ip {{{
# FUNCTION
# print in a JSON format a dict with your IP
# and other details about the network you are on
show-ip() {
        if grep -P "(([1-9]\d{0,2})\.){3}(?2)" <<<"$1"; then
                curl ipinfo.io/"$1"
        else
                ipawk=($(hostname "$1" | awk '/address/ { print $NF }'))
                curl ipinfo.io/${ipawk[1]}
        fi
}
# ============================== }}}

# show-colors {{{
# FUNCTION  :: prints the 256 colors with their corresponding numbers
show-colors() {
        (
                x=$(tput op) y=$(printf %76s)
                for i in {0..256}; do
                        o=00"$i"
                        echo -e ${o:${#o}-3:3} $(
                                tput setaf "$i"
                                tput setab "$i"
                        )${y// /=}"$x"
                done
        )
}
# ============================== }}}

# ex {{{
# FUNCTION  :: general purpose archive extracting
# USAGE: ex <file>
ex() {
        if [ -f "$1" ] && [ $# == 1 ]; then
                case $1 in
                        *.tar.bz2) tar xjf "$1" ;;
                        *.tar.gz) tar xzf "$1" ;;
                        *.bz2) bunzip2 "$1" ;;
                        *.rar) unrar x "$1" ;;
                        *.gz) gunzip "$1" ;;
                        *.tar) tar xf "$1" ;;
                        *.tbz2) tar xjf "$1" ;;
                        *.tgz) tar xzf "$1" ;;
                        *.zip) unzip "$1" ;;
                        *.Z) uncompress "$1" ;;
                        *.7z) 7z x "$1" ;;
                        *) echo "'$1' cannot be extracted via ex()" ;;
                esac
        else
                echo "'$1' is not a valid file or you supplied to many args"
        fi
}
# ============================== }}}

set-shopts() { # {{{
        # stty -ixon              # enable inc search <C-s> which is often disabled by terminal emulators
        complete -cf sudo
        complete -d cd
        [ -r /usr/share/bash-completion/bash_completion ] && . /usr/share/bash-completion/bash_completion
        shopt -s autocd
        shopt -s cdspell      # correct minor spelling errors
        shopt -s checkwinsize # update the value of LINES and COLUMNS after each command if altered
        shopt -s direxpand    # replaces directory names with expansion when <tab>
        shopt -s dirspell     # correct minor spelling errors
        shopt -s dotglob      # Include dotfiles in pathname expansion
        shopt -s checkjobs    # Include dotfiles in pathname expansion
        shopt -s extglob      # Enable extended pattern-matching features
        shopt -s nullglob
        shopt -s globstar   # ** becomes a recursive wildstar
        shopt -s histappend # Append each session's history to $HISTFILE
        shopt -s histverify # Edit a recalled history line before executing
}

# make sure zsh isn't able to source it
[ ! -n "${ZSH+2}" ] && set-shopts # }}}
