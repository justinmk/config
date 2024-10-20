-- TODO: use this instead?: https://github.com/sotte/presenting.nvim
-- TODO: https://github.com/3rd/image.nvim (hack it to use imagemagick cli instead of ffi)

vim.cmd[[
func! s:slides_hl() abort
  if hlID('SlidesHide') == 0
    " use folds instead of highlight
  else
    exe 'match SlidesHide /\%>'.(line('w0')-1).'l\n=\{20}=*\n\_.\{-}\zs\n.*\n=\{20}=\_.*/'
  endif
endfunc
func! Slides(...) abort
  let editing = !!a:0
  if editing
    setlocal colorcolumn=54,67 textwidth=53
    exe printf('hi ColorColumn gui%sg=#555555 cterm%sg=235', 'bf'[&bg], 'bf'[&bg])
  else
    setlocal cmdheight=1 nowrapscan nohlsearch scrolloff=0
  endif

  hi markdownError ctermbg=NONE ctermfg=NONE guifg=NONE guibg=NONE

  " For cterm.
  let bg = synIDattr(synIDtrans(hlID('Normal')), 'bg', 'cterm')
  let bg = !empty(bg) ? bg : (&bg==#'dark'?'black':'white')
  " Hide slides before/after the current one.
  exe 'hi SlidesHide ctermbg='..bg..' ctermfg='..bg..' cterm=nocombine guibg=bg guifg=bg gui=nocombine'

  nnoremap <buffer><silent> <Right> :keeppatterns /^======<CR>zt<C-Y>:call <SID>slides_hl()<CR>
  nnoremap <buffer><silent> <Left>   :keeppatterns ?^======<CR>zt<C-Y>:call <SID>slides_hl()<CR>
endf
func! SlidesEnd() abort
  call clearmatches()
  mapclear <buffer>
endf
]]
