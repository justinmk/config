setlocal shiftwidth=4

" Nvim 0.10 URL tokens + treesitter!
nnoremap <buffer> <silent> z<Tab> <cmd>exe 'setlocal ' (&conceallevel ? 'conceallevel=0 concealcursor=' : 'conceallevel=2 concealcursor=nv')<cr>

" URL/footnote macro
nnoremap <buffer> <leader>fn "fyiWmfGo[f]: <Esc>`fyiwi[<Esc>ebEa]<Esc>b

" Github "collapse" markup.
nnoremap <buffer> <leader>det o<lt>details><cr><lt>summary>xxx<lt>/summary><cr><lt>/details><esc>

inoreabbrev <buffer> h1 <cr>================================================================================<up>
inoreabbrev <buffer> h2 <cr>--------------------------------------------------------------------------------<up>
