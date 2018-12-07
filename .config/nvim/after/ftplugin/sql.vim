setlocal commentstring=--\ %s

nnoremap <buffer> yx.     :%DBExecRangeSQL<cr>
nnoremap <buffer> yxx     :.,.DBExecRangeSQL<cr>
xmap     <buffer> <enter> <Plug>DBExecVisualSQL

