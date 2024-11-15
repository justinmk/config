-- TODO: https://sli.dev/
-- TODO: use this instead?: https://github.com/sotte/presenting.nvim
-- TODO: timg + https://github.com/neovim/neovim/issues/30889

vim.cmd[[
func! s:slides_hl() abort
  if hlID('SlidesHide') == 0
    " use folds instead of highlight
  else
    exe 'match SlidesHide /\%>'.(line('w0')-1).'l\n\(#[^#]\|==\)[^\n]*\n\_.\{-}\zs\n.*\n\(#[^#]\|==\)\+\_.*/'
  endif
endfunc
func! Slides(...) abort
  call SlidesEnd()

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

  nnoremap <buffer><silent> <Right> :keeppatterns /\v^(#[^#]<bar>\=\=+)<CR>zt<C-Y>:call <SID>slides_hl()<CR>
  nnoremap <buffer><silent> <Left>   :keeppatterns ?\v^(#[^#]<bar>\=\=+)<CR>zt<C-Y>:call <SID>slides_hl()<CR>
  let old_ctrl_l = substitute(maparg('<c-l>', 'n', 0), '|', '<bar>', 'g')
  exe "nnoremap <buffer><silent><expr> <C-L> ':call clearmatches()<cr>'.."..old_ctrl_l
endf
func! SlidesEnd() abort
  call clearmatches()
  mapclear <buffer>
endf

command! -nargs=? Slides call Slides()
]]
