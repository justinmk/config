setlocal textwidth=100
setlocal commentstring=//\ %s
setlocal comments-=:// comments+=:///,://

nnoremap <buffer> [[    [[3<c-y>

" indent after parens, etc.
setlocal cinoptions=>s,e0,n0,f0,{0,}0,^0,:s,=s,l1,b0  " Control structures
setlocal cinoptions+=ps,t0                            " function declarations
setlocal cinoptions+=c3,C1,/0                         " Comments
setlocal cinoptions+=+s                               " Continuation lines
setlocal cinoptions+=(0,u0,U1,w1,W0,m0,M0             " Parens and arguments
setlocal cinoptions+=)20,*30                          " Search range

if '' ==# findfile('.clang-format', ';')
  setlocal formatprg=clang-format\ -style=LLVM
else
  setlocal formatprg=clang-format\ -style=file
endif

if fnamemodify(@%, ':p') =~# 'neovim'
  let b:printf_pattern = 'ILOG("%d", %s);'
  nnoremap <silent><buffer> <leader>log oELOG("");<esc>
endif
