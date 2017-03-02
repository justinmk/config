" lua =========================================================================
"    https://github.com/xolox/vim-lua-inspect

" from https://github.com/tpope/tpope/blob/master/.vimrc
setlocal includeexpr=substitute(v:fname,'\\.','/','g').'.lua'

let b:printf_pattern = 'print(string.format("%s", %s))'
