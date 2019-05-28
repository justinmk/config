setlocal copyindent
setlocal formatprg=python3\ -m\ black\ --quiet\ --skip-string-normalization\ -

if 0 != 0+search('import logging', 'nw', 0, 100)
  let b:printf_pattern = "logging.info('%{}'.format(%s))"
else
  let b:printf_pattern = "print('%{}'.format(%s))"
endif

" setlocal omnifunc=lsp#complete
" setlocal keywordprg=:LspHover
" nnoremap <buffer> <C-]> :LspDefinition<CR>
