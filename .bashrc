# Darwin, Debian, Cygwin, and MSYS have differing behavior:
#   - Darwin (and MacVim) runs a login shell every time (sources .bash_profile)
#   - MSYSGIT bash sources .bashrc _then_ .bash_profile (wtf?)
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
      "$@"
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
      defaults write NSGlobalDomain NSTextShowsControlCharacters -bool true

      defaults write com.apple.finder DisableAllAnimations -bool true

      # Display full POSIX path as Finder window title
      defaults write com.apple.finder _FXShowPosixPathInTitle -bool true

      # Avoid creating .DS_Store files on network volumes
      defaults write com.apple.desktopservices DSDontWriteNetworkStores -bool true

      # Crank key-repeat up to 11. Improves perceived Vim responsiveness...
      defaults write -g KeyRepeat -int 0
      defaults write -g InitialKeyRepeat -int 15
    fi
fi

# Add an "alert" alias for long running commands. eg:
#   sleep 10; alert
alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

# fzf (https://github.com/junegunn/fzf)
if [ -f ~/.fzf.bash ]; then
  export FZF_DEFAULT_OPTS='--black -x'
  source ~/.fzf.bash
  f() { # fzf / includes hidden directories
    find . -name .git -prune -o $1 -print 2> /dev/null | sed s/..// | fzf
  }
  fd() { # fzf / change to directory
    cd $(f "-type d")
  }
  fv() { # fzf / open file in Vim
    $EDITOR $(f)
  }
fi

[ -f ~/.bashrc.local ] && source ~/.bashrc.local

