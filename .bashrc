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

2>&1 command -v nvim > /dev/null \
  && [ 1 = $(2>&1 nvim -u NONE -i NONE --headless +'echo has("nvim-0.3.2")' +q) ] \
  && export MANPAGER="nvim +Man!"

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

# Display matches for ambiguous patterns at first tab press
bind "set show-all-if-ambiguous on"

SSHAGENT=/usr/bin/ssh-agent
SSHAGENTARGS="-s"
if [ -z "$SSH_AUTH_SOCK" -a -x "$SSHAGENT" ]; then
  eval `$SSHAGENT $SSHAGENTARGS`
  trap "kill $SSH_AGENT_PID" 0
fi

is_in_path() {
  echo "${PATH}:" | 2>&1 >/dev/null grep -E "${HOME}/bin"'/?:'
}

path_prepend() {
  [ -z "$1" ] && { echo 'path_prepend: missing/empty arg' ; exit 1 ; }
  if ! is_in_path "$1" && [ -d "$1" ] ; then
    PATH="${1}:${PATH}"
  fi
}

# Add these dirs to $PATH.
path_prepend "${HOME}/bin/ctags/bin"
path_prepend "${HOME}/dasht/bin"
path_prepend "${HOME}/bin"

# Add these dirs to $MANPATH.
[ -d "${HOME}/dasht/man" ] && MANPATH="${HOME}/dasht/man:$MANPATH"

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

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

PS1='$(date +%m%d.%H%M) \[\033[0;32m\]\u@\h \[\033[36m\]\w\[\033[0m\]'

PS1=$PS1'
$ '

[ -z $SSH_TTY ] || PS1='\[\033[0;30m\]\[\033[47m\]SSH\[\033[0m\] '$PS1

if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
fi

alias ls='ls -C --color=auto'

# change to parent directory matching partial string, eg:
# in directory /home/foo/bar/baz, 'bd f' changes to /home/foo
bd() {
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

#some old systems (msysgit) do not support grep --color.
if grep --color "a" <<< "a" &> /dev/null ; then
    alias grep='grep --color=auto'
fi

# Backup ~/.bash_history
if ! [ -f "$HOME/.bash_history.bk" ] \
   || [ $(wc -l "$HOME/.bash_history" | cut -d ' ' -f 1) -gt $(wc -l "$HOME/.bash_history.bk" | cut -d ' ' -f 1) ] ; then
  cp "$HOME/.bash_history" "$HOME/.bash_history.bk"
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
if [ -f ~/.fzf.bash ] ; then
  export FZF_DEFAULT_OPTS="--multi --black -x --inline-info --no-color"
  source ~/.fzf.bash

  f() { # includes hidden directories (except .git)
    find . -name .git -prune -o $1 -print 2> /dev/null | sed s/^..// | fzf
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

  git checkout upstream/master \
    && git merge --no-commit --no-ff -m "Merge #${PR} '${PR_TITLE}'" refs/pull/upstream/${PR}
}

ghco() {
  git checkout refs/pull/upstream/${1}
}

ghrebase1() {
  local PR=${1}
  local sed_cmd=$( [ "$(uname)" = Darwin ] && echo 'sed -E' || echo 'sed -r' )

  git checkout --quiet refs/pull/upstream/${PR} \
    && git rebase upstream/master \
    && git checkout upstream/master \
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
  if [ "$(uname)" = Darwin ] ; then
    ps -e -o pid,rss,vsz,command
  else
    ps -e -o pidns,pid,rss,vsz,command
  fi
}

[ -f ~/.bashrc.local ] && source ~/.bashrc.local

