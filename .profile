# .profile: for cases where .bash_profile is skipped

if >/dev/null 2>&1 command -v nvim ; then
  export EDITOR=nvim
  export MANPAGER="nvim +Man!"
elif >/dev/null 2>&1 command -v vim ; then
  export EDITOR=vim
else
  export EDITOR=vi
fi

# msysgit bash runs .bashrc _and_ .bash_profile, so avoid redundant run.
if ! [ "$MSYSTEM" = MINGW32 ] ; then
  # always run .bashrc; it checks the actual environment and reacts appropriately.
  [ -f "$HOME/.bashrc" ] && . "$HOME/.bashrc"
fi

