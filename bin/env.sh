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
#
# Invoke bash with explicit env vars because:
#   - If tmux is already running, new sessions inherit THAT environment!
#   - "tmux set update-environment …" doesn't work at runtime.
tmux source-file "${_REPO_DIR}/.tmux.conf" || true
env -i HOME="$_REPO_DIR" PATH="${PATH:-}" DISPLAY="${DISPLAY:-}" LC_ALL="${LC_ALL:-}" LC_CTYPE="${LC_CTYPE:-}" LANG="${LANG:-}" TERM="$TERM" \
  tmux new-session -E "HOME='$_REPO_DIR' bash -l"

