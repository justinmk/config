# login shell bash looks for (in order):
#   .bash_profile
#   .bash_login
#   .profile 
# and executes _only_ the first one it finds
[[ -s "${HOME}/.profile" ]] && source "${HOME}/.profile"

[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" # Load RVM into a shell session *as a function*

