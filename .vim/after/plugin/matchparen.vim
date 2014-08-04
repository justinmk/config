
" a massively simplified take on https://github.com/chreekat/vim-paren-crosshairs
augroup matchparen_cursorcolumn
  autocmd!
  autocmd CursorMoved * if get(w:, "paren_hl_on", 0) | set cursorcolumn | else | set nocursorcolumn | endif
  autocmd InsertEnter * set nocursorcolumn
augroup END
