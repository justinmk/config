# .profile: for cases where .bash_profile is skipped

export EDITOR=vim

# msysgit bash runs .bashrc _and_ .bash_profile, so avoid redundant run.
if [[ "$MSYSTEM" != MINGW32 ]] ; then
  # always run .bashrc; it checks the actual environment and reacts appropriately.
  [ -f "$HOME/.bashrc" ] && . "$HOME/.bashrc"
fi

