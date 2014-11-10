setlocal commentstring=//\ %s

if exists(":YcmCompleter")
  nnoremap <buffer> gd    :<c-u>YcmCompleter GoToDefinition<cr>
endif
