setlocal copyindent
setlocal formatprg=python3\ -m\ yapf

let b:printf_pattern = "logging.info('%s'.format(%s))"
command! -buffer InsertPrintf norm! i<c-r>=strftime('%Y/%m/%d %H:%M:%S')<cr>

" setlocal omnifunc=lsp#complete
" setlocal keywordprg=:LspHover
" nnoremap <buffer> <C-]> :LspDefinition<CR>
