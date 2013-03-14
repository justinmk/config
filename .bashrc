# ~/.bashrc: executed by bash(1) for non-login shells.

# If not running interactively, don't do anything
[ -z "$PS1" ] && return

export HISTCONTROL=erasedups
export HISTSIZE=10000
export HISTFILESIZE=100000
# append to the history file, don't overwrite it
shopt -s histappend

# update $LINES and $COLUMNS after each command.
shopt -s checkwinsize

if [[ $TERM != 'cygwin' && $OSTYPE != 'msys' ]] ; then
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

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "$debian_chroot" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

# set a fancy prompt
case "$TERM" in
    xterm-color) color_prompt=yes;;
esac

if [ -x /usr/bin/tput ] && tput setaf 1 >&/dev/null; then
	# We have color support; assume it's compliant with Ecma-48 (ISO/IEC-6429). 
    # (Lack of such support is extremely rare, and such a case would tend to support setf rather than setaf.)
    color_prompt=yes
else
    color_prompt=
fi

# allow msysgit to set $PS1
if [[ $TERM == 'cygwin' && $OSTYPE == 'msys' ]] ; then
    color_prompt=
fi

if [ "$color_prompt" = yes ]; then
    PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '
else
    PS1='${debian_chroot:+($debian_chroot)}\u@\h:\w\$ '
fi
unset color_prompt 

parse_git_branch() {
    git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/(\1)/'
}

# http://stackoverflow.com/q/394230/152142
#   also: $OSTYPE
if [[ `uname` == 'Darwin' ]]; then
    export PS1="\[\033[00m\]\u@\h\[\033[0;36m\] \W \[\033[32m\]\$(parse_git_branch 2> /dev/null) \[\033[00m\]$\[\033[00m\] "
    export LSCOLORS=GxFxCxDxBxegedabagaced
fi

#GNU-style aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
fi

alias ls='ls -CF --color=auto'
alias gitk='gitk --all'

#some old systems (msysgit) do not support grep --color.
if grep --color "a" <<< "a" &> /dev/null ; then
    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

#MacOS
if [[ `uname` == 'Darwin' ]]; then
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

#local settings
if [ -f "${HOME}/.bashrcx" ]; then
  . "${HOME}/.bashrcx"
fi


PATH=$PATH:$HOME/.rvm/bin # Add RVM to PATH for scripting
