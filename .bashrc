# Darwin, Debian, Cygwin, and MSYS have differing behavior:
#   - Darwin (and MacVim) runs a login shell every time (sources .bash_profile)
#   - MSYSGIT sources .bashrc _then_ .bash_profile (wtf?)
#   - Cygwin runs a login shell every time (sources .bash_profile)
#   - Debian/Ubuntu sources .bash_profile on login; thereafter only .bashrc

# Environment variables (non-bash-specific)
# =============================================================================

if [[ "$MSYSTEM" != MINGW32 && "$TERM" != cygwin && $OSTYPE != 'msys' ]] ; then
  umask 0077

  sudo() {
    local old=$(umask)
    umask 0022
    command sudo $@
    umask $old
  }
fi

command -v nvim 2>&1 > /dev/null && EDITOR=nvim

[ -d "$HOME/opt/gwt" ] && export GWT_HOME=$HOME/opt/gwt && PATH=$PATH:$GWT_HOME

# golang root
[ -d "/usr/local/go" ] && export GOROOT=/usr/local/go && PATH=$PATH:$GOROOT/bin
# golang workspace / packages
# https://code.google.com/p/go-wiki/wiki/GOPATH
# TODO: It is useful to have two GOPATH entries:
#   - first entry is for 3rd-party goinstalled packages (goinstall will use this as the default destination)
#   - second entry is for your own projects
[ -d "$HOME/dev/go" ] && export GOPATH=$HOME/dev/go
# add all $GOPATH/bin directories.
[ -z "$GOPATH" ] || PATH=$PATH:${GOPATH//://bin:}/bin

# Writing, building, installing, and testing Go code:
#   http://www.youtube.com/watch?v=XCsL89YtqCs
#
# create package source in $GOPATH/src/foo/bar/qux.go
#   $ go install qux
#   edit some other module, example.go: 
#       import "foo/bar"
#   $ go build example.go  
# 

# do not continue if we are not in a bash shell
[ -z "$BASH_VERSION" ] && return
# do not continue if we are not running interactively
[ -z "$PS1" ] && return

# =============================================================================
# Bash-specific commands
# =============================================================================

# HIST* are bash-only variables, not environmental variables, so do not 'export'
# ignoredups only ignores _consecutive_ duplicates.
HISTCONTROL=erasedups:ignoreboth
HISTSIZE=20000
HISTFILESIZE=20000
HISTIGNORE='cd:ls:bg:fg:history:f:fd'
HISTTIMEFORMAT='%F %T '
# append to the history file, don't overwrite it
shopt -s histappend
# write history file after each command
PROMPT_COMMAND='history -a'

# update $LINES and $COLUMNS after each command.
shopt -s checkwinsize

# (bash 4+) enable recursive glob for grep, rsync, ls, ...
shopt -s globstar &> /dev/null

#disable ctrl-s (scroll-lock) and ctrl-q
command -v stty > /dev/null 2>&1 && stty -ixon -ixoff

SSHAGENT=/usr/bin/ssh-agent
SSHAGENTARGS="-s"
if [ -z "$SSH_AUTH_SOCK" -a -x "$SSHAGENT" ]; then
  eval `$SSHAGENT $SSHAGENTARGS`
  trap "kill $SSH_AGENT_PID" 0
fi

# Set PATH so it includes user's private bin if it exists                       
[ -d "${HOME}/bin" ] && PATH=${HOME}/bin:${PATH}

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# msysgit cygwin
# if [[ $TERM == 'cygwin' && $OSTYPE == 'msys' ]] ; then
if [ "$MSYSTEM" == MINGW32 ]; then
    export LS_COLORS='di=01;36'
    alias pt='pt --nocolor'
fi

#try to find git-prompt.sh if __git_ps1 was not automatically provided.
if ! type -t __git_ps1 &> /dev/null ; then
    #cygwin (non-msysgit): try to find git-prompt.sh
    gitprompt_home="`which git`/../../etc/git-prompt.sh" 
    [ -f "$gitprompt_home" ] && source "$gitprompt_home"
fi

#bash completion; this also provides __git_ps1 on some systems
if command -v brew > /dev/null 2>&1 && [ -f $(brew --prefix)/etc/bash_completion ]; then
  . $(brew --prefix)/etc/bash_completion
elif [ -f /etc/bash_completion ] && ! shopt -oq posix; then
  . /etc/bash_completion
fi

PS1='\[\033[1;32m\]\u@\h \[\033[36m\]\w\[\033[0m\]'

# set git prompt iff function exists.
if type -t __git_ps1 &> /dev/null ; then
    PS1=$PS1'\[\033[1;33m$(__git_ps1)\033[0m\]'
    GIT_PS1_STATESEPARATOR=""
    GIT_PS1_SHOWUPSTREAM="verbose"
    GIT_PS1_SHOWCOLORHINTS=1
fi

PS1=$PS1'
$ '

[[ "$SSH_TTY" != "" ]] && PS1='\[\033[0;30m\]\[\033[47m\]SSH\[\033[0m\] '$PS1

if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
fi

alias ls='ls -C --color=auto'
alias l='ls -lrt'
alias gitk='gitk --all'

# change to parent directory matching partial string, eg:
# in directory /home/foo/bar/baz, 'bd f' changes to /home/foo
function bd () {
  local old_dir=`pwd`
  local new_dir=`echo $old_dir | sed 's|\(.*/'$1'[^/]*/\).*|\1|'`
  index=`echo $new_dir | awk '{ print index($1,"/'$1'"); }'`
  if [ $index -eq 0 ] ; then
    echo "No such occurrence."
  else
    echo $new_dir
    cd "$new_dir"
  fi
}

# provide 'watch' for systems that do not have it.
#   passive tail approach: http://stackoverflow.com/a/9574526/152142
if ! command -v watch > /dev/null 2>&1 ; then
  function watch() {
    while sleep 1; do
      # clear screen if possible
      command -v clear > /dev/null 2>&1 && clear
      $*
    done
  }
fi

#msysgit cygwin sets this, even over ssh; full cygwin sets this to 'xterm'.
if [[ $TERM != 'cygwin' ]]; then
    alias tmux='tmux -2'
fi

#some old systems (msysgit) do not support grep --color.
if grep --color "a" <<< "a" &> /dev/null ; then
    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

#MacOS
# http://stackoverflow.com/q/394230/152142
#   also: $OSTYPE
if [[ `uname` == 'Darwin' ]]; then
    export LSCOLORS=GxFxCxDxBxegedabagaced

    #BSD-style aliases 
    alias ls='ls -GC'
    alias su='echo "***REMINDER: verify umask" && su -l'

    #MacVim: ensure Core Text renderer (improves performance)
    defaults write org.vim.MacVim MMRenderer 2

    if [[ 0 == `defaults read com.apple.finder DisableAllAnimations` ]] ; then
      # Display ASCII control characters using caret notation in standard text views
      # Try e.g. `cd /tmp; unidecode "\x{0000}" > cc.txt; open -e cc.txt`
      defaults write -g NSTextShowsControlCharacters -bool true

      defaults write com.apple.finder DisableAllAnimations -bool true

      # Display full POSIX path as Finder window title
      defaults write com.apple.finder _FXShowPosixPathInTitle -bool true

      # Avoid creating .DS_Store files on network volumes
      defaults write com.apple.desktopservices DSDontWriteNetworkStores -bool true

      # Crank key-repeat up to 11. Improves perceived Vim responsiveness...
      defaults write -g KeyRepeat -int 0
      defaults write -g InitialKeyRepeat -int 15
    fi

    function __setterminalcsikeys() {
      # From https://github.com/boochtek/mac_config/blob/d7a873089fba2291e09841da19be54df9b60c98c/key_codes.sh#L67
      #     Apple documentation:
      #         https://developer.apple.com/library/mac/documentation/cocoa/conceptual/eventoverview/TextDefaultsBindings/TextDefaultsBindings.html.
      #     Better documentation:
      #         http://heisencoder.net/2008/04/fixing-up-mac-key-bindings-for-windows.html
      #     Also see:
      #         http://xahlee.info/kbd/osx_keybinding_key_syntax.html
      #         /Applications/Utilities/Terminal.app/Contents/Resources/English.lproj/modifierDescriptions.strings
      #     NOTE: mappings in Terminal.app don't support command (and probably not numpad).
      #     NOTE: the order below is the order that must be used within a shortcut definition.
      #     MODIFIERS=(
      #       'command=@'
      #       'apple=@'
      #       'option=~'
      #       'alt=~'
      #       'control=^'
      #       'shift=$'
      #       'numpad=#'
      #       'num=#'
      #       'keypad=#'
      #       )

      profile=Basic
      # clear existing values
      defaults+ delete com.apple.Terminal 'Window Settings.Basic.keyMapBoundKeys'

      # tab
      #defaults+ write com.apple.Terminal 'Window Settings.Basic.keyMapBoundKeys.0009' '?'
      # ctrl-tab (ASCII 9 0x9)
      defaults+ write com.apple.Terminal 'Window Settings.Basic.keyMapBoundKeys.^0009'  $(echo -e "\033[9;5u")
      # shift-tab
      defaults+ write com.apple.Terminal 'Window Settings.Basic.keyMapBoundKeys.$0009'  $(echo -e "\033[Z")
      # ctrl-shift-tab
      defaults+ write com.apple.Terminal 'Window Settings.Basic.keyMapBoundKeys.^$0009'  $(echo -e "\033[1;5Z")
      # ctrl-i (ASCII 105 0x69)
      defaults+ write com.apple.Terminal 'Window Settings.Basic.keyMapBoundKeys.^0069'  $(echo -e "\033[105;5u")
      # ctrl-s-I (ASCII 73 0x49)
      defaults+ write com.apple.Terminal 'Window Settings.Basic.keyMapBoundKeys.^$0049' $(echo -e "\033[73;5u")
      # ctrl-a (ASCII 97 0x61)
      defaults+ write com.apple.Terminal 'Window Settings.Basic.keyMapBoundKeys.^0061'  $(echo -e "\033[97;5u")
      # ctrl-s-A (ASCII 65 0x41)
      defaults+ write com.apple.Terminal 'Window Settings.Basic.keyMapBoundKeys.^$0041' $(echo -e "\033[65;5u")
    }
fi

# Add an "alert" alias for long running commands. eg:
#   sleep 10; alert
alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

# fzf (https://github.com/junegunn/fzf)
if [ -f ~/.fzf.bash ]; then
  export FZF_DEFAULT_OPTS='--multi --black -x --inline-info'
  source ~/.fzf.bash
  f() { # fzf / includes hidden directories (except .git)
    find . -name .git -prune -o $1 -print 2> /dev/null | sed s/..// | fzf
  }
  fd() { # fzf / change to directory
    local path="$(f '-type d')"
    [ -z "$path" ] || cd $path
  }
  fv() { # fzf / open file in Vim
    local path="$(f '-type f')"
    [ -z "$path" ] || $EDITOR $path
  }
fi

#TODO: https://github.com/fwalch/dotfiles/blob/4398b2e9ac321574f2a5d42848028d5c7bb60343/zshrc#L48
ghrebasepr() {
  GITHUB_PR=$1
  git fetch --all &&
    git checkout refs/pull/upstream/$GITHUB_PR &&
    git rebase upstream/master &&
    git checkout master &&
    git stash save autosave-$(date +%Y%m%d_%H%M%S) &&
    git reset --hard upstream/master &&
    git rebase upstream/master &&
    git merge --no-ff -
}

ghrebase1() {
  GITHUB_PR=$1
  #FOO=bar nvim -c 'au VimEnter * Gcommit --amend' -s <(echo 'Afoo')
  git fetch --all &&
    git checkout refs/pull/upstream/$GITHUB_PR &&
    git rebase upstream/master &&
    git commit --amend -m "$(git log -1 --pretty=format:"%B" \
      | sed -E "1 s/^(.*)\$/\\1 #$GITHUB_PR/g")" &&
    git l -n 5
}

[ -f ~/.bashrc.local ] && source ~/.bashrc.local

