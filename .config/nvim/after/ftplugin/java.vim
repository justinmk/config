autocmd FileType java setlocal tabstop=4 shiftwidth=4 copyindent nolist

if fnamemodify(@%, ':p') =~# 'hw-dashboard'
  nnoremap <silent><buffer> <leader>log oLog.info("XXX: ");<esc>
endif

let b:printf_pattern = 'System.out.println(String.format("%s", %s));'
