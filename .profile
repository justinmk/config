# .profile: for cases where .bash_profile is skipped

if >/dev/null 2>&1 command -v nvim ; then
  export EDITOR=nvim
  nvim -es +'exe !has("nvim-0.3.2")."cq"' && export MANPAGER="nvim +Man!"
else
  export EDITOR=vim
fi

# msysgit bash runs .bashrc _and_ .bash_profile, so avoid redundant run.
if ! [ "$MSYSTEM" = MINGW32 ] ; then
  # always run .bashrc; it checks the actual environment and reacts appropriately.
  [ -f "$HOME/.bashrc" ] && . "$HOME/.bashrc"
fi

