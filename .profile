if [[ "$MSYSTEM" != MINGW32 && "$TERM" != cygwin && $OSTYPE != 'msys' ]] ; then
    umask 0077
fi
[ -d "$HOME/opt/gwt" ] && export GWT_HOME=$HOME/opt/gwt && PATH=$PATH:$GWT_HOME

# golang root
[ -d "/usr/local/go" ] && export GOROOT=/usr/local/go && PATH=$PATH:$GOROOT/bin  
# golang workspace / package
[ -d "$HOME/dev/go" ] && export GOPATH=$HOME/dev/go && PATH=$PATH:$GOPATH/bin 
# Writing, building, installing, and testing Go code:
#   http://www.youtube.com/watch?v=XCsL89YtqCs
#
# create package source in $GOPATH/src/foo/bar/qux.go
#   $ go install qux
#   edit some other module, example.go: 
#       import "foo/bar"
#   $ go build example.go  
# 

[ -n "$BASH_VERSION" ] && [ -f "$HOME/.bashrc" ] && . "$HOME/.bashrc"

