setlocal commentstring=//\ %s

if exists(":YcmCompleter")
  nnoremap <buffer> gd    :<c-u>YcmCompleter GoToDefinition<cr>
endif

if exists(":Lattach") " vim-lldb plugin
  nnoremap <buffer> yda     :<c-u>Lattach<space>
  nnoremap <buffer> <up>    :<c-u>Lcontinue<cr>
  nnoremap <buffer> <right> :<c-u>Lnext<cr>
  nnoremap <buffer> <left>  :<c-u>Lnext<cr>
  nnoremap <buffer> <down>  :<c-u>Lstep<cr>
endif
