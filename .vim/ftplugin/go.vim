inoremap <buffer> <leader>err if err != nil {<C-j>log.Fatal(err)<C-j>}<C-j>

for s:gopath in split($GOPATH, ':')
  "set up Golint    https://github.com/golang/lint
  if isdirectory(s:gopath."/src/github.com/golang/lint/misc/vim")
    exe 'set runtimepath+='.s:gopath.'/src/github.com/golang/lint/misc/vim'
    autocmd BufWritePost,FileWritePost *.go execute 'Lint' | cwindow
    break
  endif
endfor
