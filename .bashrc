# Darwin, Debian, Cygwin, and MSYS have differing behavior:
#   - Darwin (and MacVim) runs a login shell every time (sources .bash_profile)
#   - MSYSGIT bash sources .bashrc _then_ .bash_profile (wtf?)
#   - Cygwin runs a login shell every time (sources .bash_profile)
#   - Debian/Ubuntu sources .bash_profile on login; thereafter only .bashrc

# Environment variables (non-bash-specific; bash-specific commands follow below)
# =============================================================================

if [[ "$MSYSTEM" != MINGW32 && "$TERM" != cygwin && $OSTYPE != 'msys' ]] ; then
    umask 0077
fi
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
HISTCONTROL=erasedups
HISTSIZE=10000
HISTFILESIZE=20000
# append to the history file, don't overwrite it
shopt -s histappend

# update $LINES and $COLUMNS after each command.
shopt -s checkwinsize

#disable ctrl-s scroll-lock
command -v stty > /dev/null 2>&1 && stty -ixon

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

# if [[ $TERM == 'cygwin' && $OSTYPE == 'msys' ]] ; then
if [ "$MSYSTEM" == MINGW32 ]; then
    # msysgit cygwin
    export LS_COLORS='di=01;36'
fi

#try to find git-prompt.sh if __git_ps1 was not automatically provided.
if ! type -t __git_ps1 &> /dev/null ; then
    #cygwin (non-msysgit): try to find git-prompt.sh
    gitprompt_home="`which git`/../../etc/git-prompt.sh" 
    [ -f "$gitprompt_home" ] && source "$gitprompt_home"
    [ -f ~/bin/git-prompt.sh ] && source ~/bin/git-prompt.sh
fi

if type -t __git_ps1 &> /dev/null ; then
    PS1='\[\033[1;32m\]\u@\h \[\033[36m\]\w\[\033[33m$(__git_ps1)\033[0m\]
$ '
    GIT_PS1_STATESEPARATOR=""
    GIT_PS1_SHOWUPSTREAM="verbose"
    GIT_PS1_SHOWCOLORHINTS=1
fi

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
    
    if [[ 0 == `defaults read com.apple.finder DisableAllAnimations` ]] ; then
      # Display ASCII control characters using caret notation in standard text views
      # Try e.g. `cd /tmp; unidecode "\x{0000}" > cc.txt; open -e cc.txt`
      defaults write NSGlobalDomain NSTextShowsControlCharacters -bool true

      defaults write com.apple.finder DisableAllAnimations -bool true

      # Display full POSIX path as Finder window title
      defaults write com.apple.finder _FXShowPosixPathInTitle -bool true

      # Avoid creating .DS_Store files on network volumes
      defaults write com.apple.desktopservices DSDontWriteNetworkStores -bool true
    fi
fi

# Add an "alert" alias for long running commands. eg:
#   sleep 10; alert
alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

if command -v brew > /dev/null 2>&1 && [ -f $(brew --prefix)/etc/bash_completion ]; then
  . $(brew --prefix)/etc/bash_completion
elif [ -f /etc/bash_completion ] && ! shopt -oq posix; then
  . /etc/bash_completion
fi

PATH=$PATH:$HOME/.rvm/bin # Add RVM to PATH for scripting
