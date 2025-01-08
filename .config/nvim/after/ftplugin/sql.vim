setlocal commentstring=--\ %s

nnoremap <buffer> yxal    :%DBExecRangeSQL<cr>
nnoremap <buffer> yxx     :.,.DBExecRangeSQL<cr>
xmap     <buffer> <enter> <Plug>DBExecVisualSQL

