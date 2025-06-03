vim.cmd[[
func! s:compare_numbers(n1, n2) abort
  return a:n1 == a:n2 ? 0 : a:n1 > a:n2 ? 1 : -1
endfunc

func! s:compare_bufs(b1, b2) abort
  let b1_visible = index(tabpagebuflist(), a:b1) >= 0
  let b2_visible = index(tabpagebuflist(), a:b2) >= 0
  " - sort by buffer number (descending)
  " - prefer loaded, NON-visible buffers
  if bufloaded(a:b1)
    if bufloaded(a:b2)
      if b1_visible == b2_visible
        return s:compare_numbers(a:b1, a:b2)
      endif
      return s:compare_numbers(b2_visible, b1_visible)
    endif
    return 1
  endif
  return !bufloaded(a:b2) ? s:compare_numbers(a:b1, a:b2) : 1
endf

func! s:buf_find_displayed_bufs() abort " find all buffers displayed in any window, any tab.
  let bufs = []
  for i in range(1, tabpagenr('$'))
    call extend(bufs, tabpagebuflist(i))
  endfor
  return bufs
endf

func! s:buf_is_valid(bnr) abort
  " Exclude:
  "   - current
  "   - unlisted
  "   - buffers marked as 'readonly' AND 'modified' (netrw brain-damage)
  " Include: normal buffers; 'help' buffers
  return buflisted(a:bnr)
    \  && ("" ==# getbufvar(a:bnr, "&buftype") || "help" ==# getbufvar(a:bnr, "&buftype"))
    \  && a:bnr != bufnr("%")
    \  && -1 == index(tabpagebuflist(), a:bnr)
    \  && !(getbufvar(a:bnr, "&modified") && getbufvar(a:bnr, "&readonly"))
endfunc

" Gets the first empty, unchanged buffer not in the current tabpage.
func! s:buf_find_unused() abort
  for i in range(1, bufnr('$'))
    if bufexists(i)
          \&& -1 == index(tabpagebuflist(), i)
          \&& nvim_buf_get_changedtick(i) <= 2
          \&& buflisted(i)
          \&& bufname(i) ==# ''
          \&& getbufvar(i, '&buftype') ==# ''
      return i
    endif
  endfor
  return 0
endf

" Switches to a new (empty, unchanged) buffer or creates a new one.
func! s:buf_new() abort
  let newbuf = s:buf_find_unused()
  if newbuf == 0
    enew
  else
    exe 'buffer' newbuf
  endif
endf

func! s:buf_find_valid_next_bufs() abort
  let validbufs = filter(range(1, bufnr('$')), '<SID>buf_is_valid(v:val)')
  call sort(validbufs, '<SID>compare_bufs')
  return validbufs
endf

func! s:buf_switch_to_altbuff() abort
  " change to alternate buffer if it is not the current buffer (yes, that can happen)
  if buflisted(bufnr("#")) && bufnr("#") != bufnr("%")
    buffer #
    return 1
  endif

  " change to newest valid buffer
  let lastbnr = bufnr('$')
  if s:buf_is_valid(lastbnr)
    exe 'buffer '.lastbnr
    return 1
  endif

  " change to any valid buffer
  let validbufs = s:buf_find_valid_next_bufs()
  if len(validbufs) > 0
    exe 'buffer '.validbufs[0]
    return 1
  endif

  return 0
endf

" close the current buffer with a vengeance
" BDSN: Buffer DiScipliNe
func! s:buf_kill(mercy) abort
  let origbuf = bufnr("%")
  let origbufname = bufname(origbuf)
  if a:mercy && &modified
    echom 'buffer has unsaved changes (use "[count]ZB" to discard changes)'
    return
  endif

  if !s:buf_switch_to_altbuff()
    " No alternate buffer, create an empty buffer.
    " :bdelete still closes other windows displaying the buffer...
    call s:buf_new()
  endif

  " remove the buffer filename (if any) from the args list, else it might come back in the next session.
  if !empty(origbufname)
    silent! exe 'argdelete '.origbufname
  endif
  " Unload the buffer state and remove it from the buffer list.
  if buflisted(origbuf) || bufloaded(origbuf)
    exe 'bdelete! '.origbuf
  endif
endf

" manage buffers
nnoremap <expr><silent> ZB  ':<c-u>call <SID>buf_kill('. !v:count .')<cr>'
nnoremap <M-n> :call <SID>buf_new()<CR>
]]
