# Darwin/Debian/Cygwin/MSYS have different behavior:
#   - Darwin (and MacVim) runs a login shell every time (sources .bash_profile)
#   - MSYSGIT sources .bashrc _then_ .bash_profile (wtf?)
#   - Cygwin runs a login shell every time (sources .bash_profile)
#   - Debian/Ubuntu sources .bash_profile on login; thereafter only .bashrc
#   - GFW/MSYS2: .bash_profile > .profile > .bashrc

# Environment variables (non-bash-specific)
# =============================================================================

# TODO: Nice for performance/explicitness, but doesn't play well with fugitive.
#export GIT_CEILING_DIRECTORIES=~

# do not continue if we are not in a bash shell
[ -z "$BASH_VERSION" ] && return
# do not continue if we are not running interactively
[ -z "$PS1" ] && return

GPG_TTY=$(tty)
export GPG_TTY

command -v nvim > /dev/null 2>&1 && export MANPAGER="nvim '+set ft=man' -"

# =============================================================================
# Bash-specific commands
# =============================================================================

# HIST* are bash-only variables, not environmental variables, so do not 'export'
# ignoredups only ignores _consecutive_ duplicates.
HISTCONTROL=erasedups:ignoreboth
HISTSIZE=20000
HISTFILESIZE=20000
HISTIGNORE='exit:cd:ls:bg:fg:history:f:fd'
HISTTIMEFORMAT='%F %T '
# append to the history file, don't overwrite it
shopt -s histappend
PROMPT_COMMAND='history -a' # append history file after each command
# truncate long paths to ".../foo/bar/baz"
PROMPT_DIRTRIM=4

# update $LINES and $COLUMNS after each command.
shopt -s checkwinsize
# (bash 4+) enable recursive glob for grep, rsync, ls, ...
shopt -s globstar &> /dev/null

#disable ctrl-s (scroll-lock) and ctrl-q
command -v stty > /dev/null 2>&1 && stty -ixon -ixoff

# Display matches for ambiguous patterns at first tab press
bind "set show-all-if-ambiguous on"

SSHAGENT=/usr/bin/ssh-agent
SSHAGENTARGS="-s"
if [ -z "$SSH_AUTH_SOCK" -a -x "$SSHAGENT" ]; then
  eval `$SSHAGENT $SSHAGENTARGS`
  trap "kill $SSH_AGENT_PID" 0
fi

# Set PATH so it includes user bin if it exists.
[ -d "${HOME}/bin" ] && PATH="${HOME}/bin:${PATH}"
[ -d "${HOME}/bin/ctags/bin" ] && PATH="${HOME}/bin/ctags/bin:${PATH}"

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# msysgit cygwin
# if [[ $TERM == 'cygwin' && $OSTYPE == 'msys' ]] ; then
if [ "$MSYSTEM" = MINGW32 ]; then
    export LS_COLORS='di=01;36'
    alias pt='pt --nocolor'
fi

#try to find git-prompt.sh if __git_ps1 was not automatically provided.
if ! type -t __git_ps1 > /dev/null 2>&1 ; then
    #cygwin (non-msysgit): try to find git-prompt.sh
    gitprompt_home="`which git`/../../etc/git-prompt.sh" 
    [ -f "$gitprompt_home" ] && source "$gitprompt_home"
fi

if [ -n "$TMUX" ]; then
# bash completion (also provides __git_ps1 on some systems)
# Slow as dirt.
if [ -f /usr/local/etc/bash_completion ]; then
  # `brew --prefix` is horribly expensive, do not use it!
  source /usr/local/etc/bash_completion
elif [ -f /etc/bash_completion ] && ! shopt -oq posix; then
  . /etc/bash_completion
fi
fi

PS1='\[\033[0;32m\]\u@\h \[\033[36m\]\w\[\033[0m\]'

# set git prompt iff function exists.
if type -t __git_ps1 &> /dev/null ; then
    PS1=$PS1'\[\033[0;33m$(__git_ps1)\033[0m\]'
    GIT_PS1_STATESEPARATOR=""
    GIT_PS1_SHOWUPSTREAM="verbose"
    GIT_PS1_SHOWCOLORHINTS=1
fi

PS1=$PS1'
$ '

[ -z $SSH_TTY ] || PS1='\[\033[0;30m\]\[\033[47m\]SSH\[\033[0m\] '$PS1

if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
fi

alias ls='ls -C --color=auto'

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
fi

#MacOS
# http://stackoverflow.com/q/394230/152142
#   also: $OSTYPE
if [ "$(uname)" = Darwin ] ; then
    export LSCOLORS=GxFxCxDxBxegedabagaced

    #BSD-style aliases
    alias ls='ls -GC'

    if ! [ 2 = "$(defaults read -g KeyRepeat)" ] ; then
      # Display ASCII control characters using caret notation in standard text views
      # Try e.g. `cd /tmp; unidecode "\x{0000}" > cc.txt; open -e cc.txt`
      defaults write -g NSTextShowsControlCharacters -bool true

      defaults write com.apple.finder DisableAllAnimations -bool true

      # Display full POSIX path as Finder window title
      defaults write com.apple.finder _FXShowPosixPathInTitle -bool true

      # Avoid creating .DS_Store files on network volumes
      defaults write com.apple.desktopservices DSDontWriteNetworkStores -bool true

      # Set key-repeat rate. But not too high: on macOS 10.13 KeyRepeat=0 is insane.
      defaults write NSGlobalDomain ApplePressAndHoldEnabled -bool false
      defaults write NSGlobalDomain KeyRepeat -int 2
      defaults write -g KeyRepeat -int 2
      defaults write -g InitialKeyRepeat -int 15
    fi
fi

# fzf (https://github.com/junegunn/fzf)
if [ -f ~/.fzf.bash ] || command -v peco >/dev/null 2>&1 ; then
  export FZF_DEFAULT_OPTS="--multi --black -x --inline-info --no-color"

  if command -v peco >/dev/null 2>&1 ; then
    _fzfprog=peco
  else
    source ~/.fzf.bash
    _fzfprog=fzf
  fi

  f() { # includes hidden directories (except .git)
    find . -name .git -prune -o $1 -print 2> /dev/null | sed s/^..// | $_fzfprog
  }
  d() { # change to directory
    local path="$(f '-type d')"
    [ -z "$path" ] || cd $path
  }
fi

ghpr() {
  local sed_cmd=$( [ "$(uname)" = Darwin ] && echo 'sed -E' || echo 'sed -r' )
  local PR=${1}
  local REPO_SLUG="$(git config --get remote.upstream.url \
    | sed 's/^.*github.com[\/:]\(.*\)\.git/\1/')"
  local req_url="https://api.github.com/repos/${REPO_SLUG}/pulls/${PR}"
  local PR_TITLE="$(curl -Ss "$req_url" \
    | grep '"title"' \
    | $sed_cmd 's/.*(\[(RFC|RDY)\]) *(.*)../\3/')"
  #                                         ^ Trailing ", in JSON response.

  [ -z "$PR_TITLE" ] && { printf "error. request: $req_url\n       response: $(curl -Ss $req_url)\n"; return 1; }

  git fetch --all --prune \
    && git checkout master \
    && git stash save autosave-$(date +%Y%m%d_%H%M%S) \
    && git reset --hard upstream/master \
    && git merge --no-commit --no-ff -m "Merge #${PR} '${PR_TITLE}'" refs/pull/upstream/${PR}
}

ghrebase1() {
  local PR=${1}
  local sed_cmd=$( [ "$(uname)" = Darwin ] && echo 'sed -E' || echo 'sed -r' )

  #FOO=bar nvim -c 'au VimEnter * Gcommit --amend' -s <(echo 'Afoo')
  git fetch --all --prune \
    && git checkout --quiet refs/pull/upstream/${PR} \
    && git rebase upstream/master \
    && git checkout master \
    && git stash save autosave-$(date +%Y%m%d_%H%M%S) \
    && git reset --hard upstream/master \
    && git merge --ff-only - \
    && git commit --amend -m "$(git log -1 --pretty=format:"%B" \
      | $sed_cmd "1 s/^(.*)\$/\\1 #${PR}/g")" \
    && git log --oneline --graph --decorate -n 5
}

upload-video() {
  [ -z "$1" ] && { echo "missing arg 1"; return 1; }
  ~/bin/upload_video.py --file="$HOME/Downloads/$1.mov" \
    --title="$(date +%Y-%m-%d-%H:%M)" --privacyStatus="unlisted"
}

p() {
  ps -e -o pidns,pid,command
}

[ -f ~/.bashrc.local ] && source ~/.bashrc.local

