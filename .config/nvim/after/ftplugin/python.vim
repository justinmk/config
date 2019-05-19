setlocal copyindent
setlocal formatprg=python3\ -m\ yapf

let b:printf_pattern = "logging.info('%s'.format(%s))"

" setlocal omnifunc=lsp#complete
" setlocal keywordprg=:LspHover
" nnoremap <buffer> <C-]> :LspDefinition<CR>
