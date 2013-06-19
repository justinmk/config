# ~/.bashrc: executed by bash(1) for non-login shells.

# If not running interactively, don't do anything
[ -z "$PS1" ] && return

# HIST* are bash-only variables, not environmental variables, so do not 'export'
HISTCONTROL=erasedups
HISTSIZE=10000
HISTFILESIZE=20000
# append to the history file, don't overwrite it
shopt -s histappend

# update $LINES and $COLUMNS after each command.
shopt -s checkwinsize

if [[ "$MSYSTEM" != MINGW32 && "$TERM" == cygwin && $OSTYPE == 'msys' ]] ; then
    umask 0077
fi

#disable ctrl-s scroll-lock
if [ -x stty ] ; then stty -ixon ; fi

SSHAGENT=/usr/bin/ssh-agent                                                     
SSHAGENTARGS="-s"                                                               
if [ -z "$SSH_AUTH_SOCK" -a -x "$SSHAGENT" ]; then                              
    eval `$SSHAGENT $SSHAGENTARGS`                                              
    trap "kill $SSH_AGENT_PID" 0                                                
fi                                 

# Set PATH so it includes user's private bin if it exists                       
if [ -d "${HOME}/bin" ] ; then                                                
  PATH=${HOME}/bin:${PATH}                                                    
fi   

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
fi

if type -t __git_ps1 &> /dev/null ; then
    PS1='\[\033[1;32m\]\u@\h \[\033[36m\]\w\[\033[33m$(__git_ps1)\033[0m\]
$ '
    GIT_PS1_STATESEPARATOR=""
    GIT_PS1_SHOWUPSTREAM="verbose"
    GIT_PS1_SHOWCOLORHINTS=1
fi

#GNU-style aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
fi

alias ls='ls -CF --color=auto'
alias gitk='gitk --all'

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
    alias ls='ls -GCF'

    alias su='echo "***REMINDER: verify umask" && su'
    
    # Display ASCII control characters using caret notation in standard text views
    # Try e.g. `cd /tmp; unidecode "\x{0000}" > cc.txt; open -e cc.txt`
    defaults write NSGlobalDomain NSTextShowsControlCharacters -bool true

    defaults write com.apple.finder DisableAllAnimations -bool true

    # Display full POSIX path as Finder window title
    defaults write com.apple.finder _FXShowPosixPathInTitle -bool true

    # Avoid creating .DS_Store files on network volumes
    defaults write com.apple.desktopservices DSDontWriteNetworkStores -bool true
fi

# Add an "alert" alias for long running commands.  Use like so:
#   sleep 10; alert
alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

# enable programmable completion 
if [ -f /etc/bash_completion ] && ! shopt -oq posix; then
    . /etc/bash_completion
fi

[[ -d "$HOME/opt/gwt" ]] && PATH=$PATH:$HOME/opt/gwt

#local settings
if [ -f "${HOME}/.bashrcx" ]; then
  . "${HOME}/.bashrcx"
fi

PATH=$PATH:$HOME/.rvm/bin # Add RVM to PATH for scripting
