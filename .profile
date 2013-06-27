if [[ "$MSYSTEM" != MINGW32 && "$TERM" != cygwin && $OSTYPE != 'msys' ]] ; then
    umask 0077
fi

