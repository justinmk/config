# .profile: provided for cases where .bash_profile is skipped.

# msysgit bash runs .bashrc _and_ .bash_profile, so avoid redundant run.
if [[ "$MSYSTEM" != MINGW32 && $OSTYPE != msys ]] ; then
  # always run .bashrc; it checks the actual environment and reacts appropriately.
  [ -f "$HOME/.bashrc" ] && . "$HOME/.bashrc"
fi

