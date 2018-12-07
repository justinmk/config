" LSP: https://github.com/Alloyed/lua-lsp

" from https://github.com/tpope/tpope/blob/master/.vimrc
setlocal includeexpr=substitute(v:fname,'\\.','/','g').'.lua'

setlocal omnifunc=lsp#complete

let b:printf_pattern = 'print(string.format("%s", %s))'
