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

# Non-default history file, to avoid accidental truncation.
[ -f "$HOME/.bash_history_x" ] || { [ -f "$HOME/.bash_history" ] && cp "$HOME/.bash_history" "$HOME/.bash_history_x" ; }
HISTFILE="$HOME/.bash_history_x"
HISTCONTROL=erasedups:ignoreboth
HISTSIZE=99999
HISTFILESIZE=99999
HISTIGNORE='exit:cd:ls:bg:fg:history:f:fd'
HISTTIMEFORMAT='%F %T '
# append to the history file, don't overwrite it
shopt -s histappend
# Before each command:
# - announce current directory (OSC 7)
# - mark start of prompt (OSC 133)
# - set term title to [SSH]$_MY_TITLE or [SSH]cwd
# - append to history file
function print_osc7() {
  printf '\033]7;file://%s%s\033\\' "$HOSTNAME" "$PWD"
}
PROMPT_COMMAND='print_osc7; printf "\033]0;${SSH_TTY:+SSH }${_MY_TITLE:-${PWD##*/}}\007" ; history -a'
# truncate long paths to ".../foo/bar/baz"
PROMPT_DIRTRIM=4

# update $LINES and $COLUMNS after each command.
shopt -s checkwinsize
# (bash 4+) enable recursive glob for grep, rsync, ls, ...
shopt -s globstar &> /dev/null

# Display matches for ambiguous patterns at first tab press
bind "set show-all-if-ambiguous on"

SSHAGENT=/usr/bin/ssh-agent
if [ -z "$SSH_AUTH_SOCK" -a -x "$SSHAGENT" ]; then
  eval $($SSHAGENT -s)
  trap "kill $SSH_AGENT_PID" 0
fi

path_prepend() {
  [ -z "$1" ] && { echo 'path_prepend: missing/empty arg' ; exit 1 ; }
  # Remove existing match, if any.
  local path=''$(echo $PATH | sed 's:\:'$1'$::' | sed 's:\:'$1'\::\::' | sed 's:^'$1'\:::')
  PATH="${1}:${path}"
}

# Add these dirs to $PATH.
path_prepend "${HOME}/bin"
# For uv/pip
path_prepend "${HOME}/.local/bin"
path_prepend "/opt/homebrew/bin/"
path_prepend "/opt/homebrew/opt/curl/bin/"
# path_prepend "${HOME}/dasht/bin"
# path_prepend "${HOME}/ctags/"

if [ -f /opt/homebrew/bin/brew ]; then
  eval "$(/opt/homebrew/bin/brew shellenv)"
elif [ -f /home/linuxbrew/.linuxbrew/bin/brew ]; then
  eval "$(/home/linuxbrew/.linuxbrew/bin/brew shellenv)"
fi

# Add these dirs to $MANPATH.
[ -d "${HOME}/dasht/man" ] && MANPATH="${HOME}/dasht/man:$MANPATH"

# bash completion (also provides __git_ps1 on some systems). Slow as dirt.
# `brew --prefix` is also slow, avoid it.
# brew install bash-completion@2
# https://github.com/homebrew/brew/blob/89b8619153ce7f523fcf4d1bb5fa4b3a375c22a4/docs/Shell-Completion.md
if [ -r '/opt/homebrew/etc/profile.d/bash_completion.sh' ]; then
  # homebrew uses /opt/homebrew for ARM packages. https://apple.stackexchange.com/a/410829/36305
  . '/opt/homebrew/etc/profile.d/bash_completion.sh'

  # Requires "brew install git":
  . /opt/homebrew/etc/bash_completion.d/git-completion.bash
  # Requires "brew install node":
  . /opt/homebrew/etc/bash_completion.d/npm
elif [ -r '/usr/local/etc/profile.d/bash_completion.sh' ]; then
  # homebrew uses /usr/local for Intel packages. https://apple.stackexchange.com/a/410829/36305
  . '/usr/local/etc/profile.d/bash_completion.sh'
elif [ -r '/home/linuxbrew/.linuxbrew/etc/profile.d/bash_completion.sh' ]; then
   . '/home/linuxbrew/.linuxbrew/etc/profile.d/bash_completion.sh'
elif [ -d '/usr/local/etc/bash_completion.d' ]; then
  for f in '/usr/local/etc/bash_completion.d/'*; do
    [ -r "$f" ] && source "$f"
  done
elif [ -f /etc/bash_completion ] && ! shopt -oq posix; then
  . /etc/bash_completion
fi

PS1='$([ "$?" = 0 ] || printf "\[\e[1;31m\]")$(date +%m%d.%H%M)\[\e[0m\] \u@\h \w\[\e[0m\]
\[\e]133;A\007\]\$ '
[ -z $SSH_TTY ] || PS1='\[\e[0;30m\]\[\e[47m\]SSH\[\e[0m\] '$PS1

if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
fi

alias ls='ls -C --color=auto'

# change to parent directory matching partial string, eg:
# in directory /home/foo/bar/baz, 'bd f' changes to /home/foo
bd_() {
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

# Some old systems (msysgit) do not support grep --color.
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

    if ! [ '0' = "$(defaults read -g ApplePressAndHoldEnabled)" ] ; then
      printf '\n%s\n\n' 'bashrc: initialize macOS defaults'
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

      defaults write com.apple.dock orientation -string left  # Set Dock position (left, bottom, right)
      defaults write com.apple.dock tilesize -int 48          # Set Dock size (range 16-128)
      defaults write com.apple.dock autohide -bool true       # Enable auto-hide
      killall Dock # Restart the Dock to apply changes
    fi
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
  git checkout refs/pull/upstream/${1} || git checkout refs/pull/origin/${1}
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

p() {
  (
    if [ "$(uname)" = Darwin ] ; then
      ps -e "$@" -o state,pid,rss,vsz,command
    else
      ps -e "$@" -o state,pidns,pid,rss,vsz,command
    fi
  ) | grep -v ' grep '
}

_Z_DATA="$HOME/.config/z/z"
. ~/bin/z.sh

[ -f ~/.bashrc.local ] && source ~/.bashrc.local

