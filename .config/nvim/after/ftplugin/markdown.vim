setlocal shiftwidth=4

" Presentation-mode: Nvim 0.10 URL tokens + treesitter!
"nnoremap <buffer> <silent> <bs> <cmd>exe 'setlocal ' (&conceallevel ? 'conceallevel=0 concealcursor=' : 'conceallevel=2 concealcursor=nv')<cr>
nnoremap <buffer> <silent> <bs> :lua if not package.loaded['markview'] then vim.cmd'packadd markview.nvim'; end; vim.cmd'Markview toggle'<cr>

" URL/footnote macro
nnoremap <buffer> <leader>fn "fyiWmfGo[f]: <Esc>`fyiwi[<Esc>ebEa]<Esc>b

" Github "collapse" markup.
nnoremap <buffer> <leader>det o<lt>details><cr><lt>summary>xxx<lt>/summary><cr><lt>/details><esc>

inoreabbrev <buffer> h1 <cr>================================================================================<up>
inoreabbrev <buffer> h2 <cr>--------------------------------------------------------------------------------<up>
