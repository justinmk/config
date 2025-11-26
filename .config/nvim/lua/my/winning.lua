vim.cmd[[
" go to the previous window (or any other window if there is no 'previous' window).
func! s:switch_to_alt_win() abort
  let currwin = winnr()
  wincmd p
  if winnr() == currwin "window didn't change; no previous window.
    wincmd w
  endif
endf

" manage windows
"       [count]<c-w>s and [count]<c-w>v create a [count]-sized split
"       [count]<c-w>| and [count]<c-w>_ resize the current window
" user recommendation:
"       <c-w>eip
" available:
"       <c-w><space>{motion}
"       <c-_> (<c-->)
nnoremap <silent><M-h> <C-\><C-N><C-w><C-h>
nnoremap <silent><M-j> <C-\><C-N><C-w><C-j>
nnoremap <silent><M-k> <C-\><C-N><C-w><C-k>
nnoremap <silent><M-l> <C-\><C-N><C-w><C-l>
inoremap <silent><M-h> <C-\><C-N><C-w><C-h>
inoremap <silent><M-j> <C-\><C-N><C-w><C-j>
inoremap <silent><M-k> <C-\><C-N><C-w><C-k>
inoremap <silent><M-l> <C-\><C-N><C-w><C-l>
tnoremap <silent><M-h> <C-\><C-N><C-w><C-h>
tnoremap <silent><M-j> <C-\><C-N><C-w><C-j>
tnoremap <silent><M-k> <C-\><C-N><C-w><C-k>
tnoremap <silent><M-l> <C-\><C-N><C-w><C-l>
nnoremap <c-i> <c-i>
nnoremap <silent><expr> <tab> (v:count > 0 ? '<C-w>w' : ':call <SID>switch_to_alt_win()<CR>')
nnoremap <silent>      <s-tab>  <C-^>
" inoremap <c-r><c-w> <esc>:call <sid>switch_to_alt_win()<bar>let g:prev_win_buf=@%<cr><c-w><c-p>gi<c-r>=g:prev_win_buf<cr>

" Fit current window (vertically) to the buffer text.
nnoremap <silent> <m-=> <cmd>exe min([winheight('%'),line('$')]).'wincmd _'<bar>setlocal winfixheight<cr>
" Fit current window (vertically) to the selected text.
xnoremap <silent> <m-=> <esc><cmd>exe (line("'>") - line("'<") + 1).'wincmd _'<bar>setlocal winfixheight<cr>

" manage tabs
nnoremap <silent> <M-t>    :tab split<cr>
nnoremap <silent> <M-]>    gt
nnoremap <silent> <M-[>    gT
nnoremap <silent> ZT       :tabclose<cr>
" move tab to Nth position
nnoremap <expr> <M-L> ':<C-u>tabmove '.(v:count ? (v:count - 1) : '+1').'<CR>'
nnoremap <expr> <M-H> ':<C-u>tabmove '.(v:count ? (v:count - 1) : '-1').'<CR>'

" quickfix window (in quickfix: toggles between qf & loc list)
nnoremap <silent><expr> <M-q> '@_:'.(&bt!=#'quickfix'<bar><bar>!empty(getloclist(0))?'lclose<bar>botright copen':'cclose<bar>botright lopen')
      \.(v:count ? '<bar>wincmd L' : '').'<CR>'

nnoremap <expr> zt (v:count > 0 ? '@_zt'.v:count.'<c-y>' : 'zt')
nnoremap <expr> zb (v:count > 0 ? '@_zb'.v:count.'<c-e>' : 'zb')

nnoremap <expr> <c-w><c-q>  (v:count ? ':<c-u>confirm qa<cr>' : '<c-w><c-q>')
nnoremap <expr> <c-w>q      (v:count ? ':<c-u>confirm qa<cr>' : '<c-w><c-q>')
nnoremap <expr> ZZ          (v:count ? ':<c-u>xa!<cr>' : '@_ZZ')
nnoremap <expr> ZQ          (v:count ? ':<c-u>qa!<cr>' : '@_ZQ')
nnoremap <expr> <c-w>=      (v:count ? ':<c-u>windo setlocal nowinfixheight nowinfixwidth<cr><c-w>=' : '@_<c-w>=')

func! s:zoom_toggle(cnt) abort
  if a:cnt  " Fallback to default '|' behavior if count was provided.
    exe 'norm! '.v:count.'|'
  endif

  if 1 == winnr('$')
    return
  endif
  let restore_cmd = winrestcmd()
  wincmd |
  wincmd _
  if exists('t:zoom_restore')
    exe t:zoom_restore
    unlet t:zoom_restore
  elseif winrestcmd() !=# restore_cmd
    let t:zoom_restore = restore_cmd
  endif
endfunc
nnoremap +     :<C-U>call <SID>zoom_toggle(v:count)<CR>
nnoremap <Bar> <cmd>ZenMode<cr>

augroup config_winning
  autocmd!

  autocmd CmdwinEnter * nnoremap <nowait><silent><buffer> q <C-W>c

  " Closes the current quickfix list and returns to the alternate window.
  func! s:close_qflist()
    let altwin = winnr('#')
    wincmd c
    exe altwin.'wincmd w'
  endf
  autocmd FileType qf nnoremap <buffer> <c-p> <up>
        \|nnoremap <buffer> <c-n> <down>
        \|nnoremap <silent><buffer><nowait> q :call <sid>close_qflist()<cr>
        \|setlocal nolist
augroup END

"
" Highlight current window
"
func! s:focusable_wins() abort
  return filter(nvim_tabpage_list_wins(0), {k,v-> !!nvim_win_get_config(v).focusable})
endf
augroup config_curwin_border
  autocmd!
  " highlight Normal guibg=NvimDarkGrey1
  " highlight NormalNC guibg=NvimDarkGrey2
  highlight CursorLineNC cterm=underdashed gui=underdashed ctermfg=gray guisp=NvimLightGrey4 ctermbg=NONE guibg=NONE
  highlight link WinBorder Statusline

  " Dim non-current cursorline.
  autocmd VimEnter,WinEnter,TabEnter,BufEnter * setlocal winhighlight-=CursorLine:CursorLineNC

  " Highlight curwin WinSeparator/SignColumn for "border" effect.
  "let s:winborder_hl = 'WinSeparator:WinBorder,SignColumn:WinBorder'
  autocmd WinLeave * exe 'setlocal winhighlight+=CursorLine:CursorLineNC'
  "autocmd WinEnter * if len(s:focusable_wins()) > 1 | exe 'setlocal winhighlight+='..s:winborder_hl | endif
  " Disable effect if there is only 1 window.
  "autocmd WinResized * if 1 == len(s:focusable_wins()) | exe 'setlocal winhighlight-='..s:winborder_hl | endif
augroup END
]]
