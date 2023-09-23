lua vim.treesitter.start()

" execute/evaluate
nnoremap <buffer>         yxal    :source %<cr>
nnoremap <silent><buffer> yxx     :keeppatterns          .g/^/exe getline('.')<CR>
xnoremap <silent><buffer> <enter> :<C-U>keeppatterns '<,'>g/^/exe getline('.')<CR>
