if [ -f "${HOME}/.bashrc" ]; then
  . "${HOME}/.bashrc"
fi

umask 0077

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

parse_git_branch() {
    git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/(\1)/'
}

# http://stackoverflow.com/q/394230/152142
#   also: $OSTYPE
if [[ `uname` == 'Darwin' ]]; then
    export PS1="\[\033[00m\]\u@\h\[\033[0;36m\] \W \[\033[32m\]\$(parse_git_branch 2> /dev/null) \[\033[00m\]$\[\033[00m\] "
    export LSCOLORS=GxFxCxDxBxegedabagaced
fi


