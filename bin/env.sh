#!/bin/bash
# Load my environment, without actually installing my junk into $HOME.

# set -x
set -e
set -u
set -o pipefail

_SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
[ -z "$_SCRIPT_DIR" ] && { echo 'error: invalid $_SCRIPT_DIR'; exit 1; }

_REPO_DIR="$(cd "$_SCRIPT_DIR"/.. && pwd)"

if ! [ -d "$_REPO_DIR" ] || ! 2>&1 >/dev/null grep justinmk/config "$_REPO_DIR"/.git/config ; then
  echo 'expected justinmk/config repo'
  exit 1
fi

# Start bash+tmux with cleared environment.
env -i HOME="$_REPO_DIR" TERM="$TERM" bash -l -c tmux

