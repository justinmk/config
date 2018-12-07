autocmd FileType java setlocal tabstop=4 shiftwidth=4 copyindent nolist

if isdirectory(expand("~/.vim/eclim", 1))
  autocmd FileType java nnoremap <buffer> gd :<c-u>JavaSearchContext<cr>
        \| nnoremap <buffer> <silent> gI :<c-u>JavaSearch -x implementors -s workspace<cr>
        \| nnoremap <buffer> <c-t> :<c-u>JavaHierarchy<cr>
        \| nnoremap <buffer> cri   :<c-u>JavaImportOrganize<cr>
        \| nnoremap <buffer> K     :<c-u>JavaDocPreview<cr>
        \| nnoremap <buffer> <bs>  :<c-u>JavaCorrect<cr>
endif

let b:printf_pattern = 'System.out.println(String.format("%s", %s));'
