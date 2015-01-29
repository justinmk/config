nnoremap <buffer> yxx     :.,.Eval<cr>
nnoremap <buffer> yx.     :%Eval<cr>
xnoremap <buffer> <enter> :Eval<cr>
" eval-last-sexp
inoremap <buffer> <c-j>   <esc>"9y%:Eval! <c-r>9<cr>%a

