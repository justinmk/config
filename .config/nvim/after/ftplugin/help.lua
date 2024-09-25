-- execute/evaluate
vim.cmd[[
  nnoremap <buffer>         yxal    :Runtime<cr>
  nnoremap <silent><buffer> yxx     :keeppatterns          .g/^/exe getline('.')<CR>
  xnoremap <silent><buffer> <enter> :<C-U>keeppatterns '<,'>g/^/exe getline('.')<CR>

  " Justify a line, using cursor position as the midpoint.
  nnoremap <buffer> <leader><space> :let g:old_et=&l:et<bar>setlocal et<cr>
    \i<cr><c-c>:right<cr>k
    \:left<cr>0
    \:keeppatterns norm! dv/\ze\s*$<c-v><cr><cr>
    \jR<c-r>"<c-c>0k"_dd
    \:let &l:et=g:old_et<bar>unlet g:old_et<bar>.retab!<cr>
]]
