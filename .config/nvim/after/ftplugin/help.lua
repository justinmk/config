-- execute/evaluate
vim.cmd[[
  " Use default 'iskeyword' which is much nicer for "*", etc.
  " Since "K" (and 'keywordprg') uses ":help!" there is no need to screw up 'iskeyword'.
  set iskeyword&

  nnoremap <buffer>         yxal    :Runtime<cr>
  xnoremap <silent><buffer> <enter> :<C-U>keeppatterns '<,'>g/^/exe getline('.')<CR>

  " Justify a line, using cursor position as the midpoint.
  nnoremap <buffer> <leader><space> :let g:old_et=&l:et<bar>setlocal et<cr>
    \i<cr><c-c>:right<cr>k
    \:left<cr>0
    \:keeppatterns norm! dv/\ze\s*$<c-v><cr><cr>
    \jR<c-r>"<c-c>0k"_dd
    \:let &l:et=g:old_et<bar>unlet g:old_et<bar>.retab!<cr>
]]
