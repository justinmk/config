" Vim news: https://www.arp242.net/vimlog/
" Nvim news: https://neovim.io/doc/user/news.html
"==============================================================================

let g:did_install_default_menus = 1  " avoid stupid menu.vim (saves ~100ms)
let g:loaded_netrwPlugin = 0  " Disable netrw. 🚮

fun! InstallPlug() " Bootstrap plugin manager on new systems.
lua << EOF
  print(vim.fn.system({'git', 'clone', 'https://github.com/savq/paq-nvim.git',
    vim.fn.stdpath('data')..'/site/pack/paqs/start/paq-nvim'}))
EOF
endfun

" Use <C-L> to:
"   - redraw
"   - clear 'hlsearch'
"   - update the current diff (if any)
" Use {count}<C-L> to:
"   - reload (:edit) the current buffer (TODO: ":lockmarks edit" doesn't preserve '[ '] marks)
nnoremap <silent><expr> <C-L> (v:count ? ':<C-U>:edit<CR>' : '')
      \ . ':nohlsearch'.(has('diff')?'\|diffupdate':'')
      \ . '<CR><C-L>'

command! Session if filereadable(stdpath('config').'/session.vim') | exe 'source '.stdpath('config').'/session.vim'
      \ | else | exe 'Obsession '.stdpath('config').'/session.vim' | endif
set sessionoptions-=blank

"==============================================================================
" general settings / options
"==============================================================================
set exrc
set scrolloff=4
set fillchars+=msgsep:‾,eob:·
set inccommand=split

" theme/colorscheme
if !&termguicolors
  highlight SpellBad guibg=Red guifg=White
  highlight CursorLine ctermbg=235
  highlight Comment ctermfg=gray
endif

" https://github.com/neovim/neovim/issues/3463#issuecomment-148757691
" autocmd CursorHold,FocusGained,FocusLost * silent! rshada|silent! wshada
" :checktime is SLOW
" autocmd CursorHold,FocusGained * silent! checktime

set shada^=r/tmp/,r/private/,rfugitive:,rzipfile:,rterm:,rhealth:
set jumpoptions+=view
set tabclose=uselast
set cpoptions-=_
set guicursor+=n:blinkon175
au UIEnter * set guifont=Menlo:h20

" Don't mess with 'tabstop', with 'expandtab' it isn't used.
" Set softtabstop=-1 to mirror 'shiftwidth'.
set expandtab shiftwidth=2 softtabstop=-1
autocmd FileType * autocmd CursorMoved * ++once if !&expandtab | setlocal listchars+=tab:\ \  | endif
set list

let g:mapleader = "z,"

set undofile
set fileformats=unix,dos

" [i, [d
set path+=/usr/lib/gcc/**/include
" neovim
set path+=build/src/nvim/auto/**,.deps/build/src/**/,src,src/nvim
" DWIM 'includeexpr': make gf work on filenames like "a/…" (in diffs, etc.).
set includeexpr=substitute(v:fname,'^[^\/]*/','','')

let g:sh_noisk = 1
" set lazyredraw  " no redraws in macros. Disabled for: https://github.com/neovim/neovim/issues/22674
set cmdheight=2
set ignorecase " case-insensitive searching
set smartcase  " but become case-sensitive if you type uppercase characters

set foldopen-=search
set timeoutlen=3000
set noshowmode " Hide the mode text (e.g. -- INSERT --)
set foldlevelstart=99 "open all folds by default
if has('patch-7.4.314') | set shortmess+=cC | endif

nnoremap <silent> yoz :<c-u>if &foldenable && 2==&foldnestmax && 0==&foldlevel\|set nofoldenable\|
      \ else\|setl foldmethod=indent foldnestmax=2 foldlevel=0 foldenable\|set foldmethod=manual\|endif<cr>

nnoremap yoT :<c-u>setlocal textwidth=<C-R>=(!v:count && &textwidth != 0) ? 0 : (v:count ? v:count : 80)<CR><CR>

set nostartofline
set cursorline
set diffopt+=hiddenoff,linematch:60,foldcolumn:0

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
  let s:winborder_hl = 'WinSeparator:WinBorder,SignColumn:WinBorder'
  autocmd WinLeave * exe 'setlocal winhighlight+=CursorLine:CursorLineNC winhighlight-='..s:winborder_hl
  autocmd WinEnter * exe 'setlocal winhighlight+='..s:winborder_hl
  " Disable effect if there is only 1 window.
  autocmd WinResized * if 1 == len(s:focusable_wins()) | exe 'setlocal winhighlight-='..s:winborder_hl | endif
augroup END

"==============================================================================
" text, tab and indent

set formatoptions+=rno1l/
" don't syntax-highlight long lines
set synmaxcol=200

set linebreak
set nowrap

" key mappings/bindings =================================================== {{{
"
" available mappings:
"   visual: c-\ <space> m R c-r c-n c-g c-a c-x c-h,<bs><tab>
"   insert: c-\ c-g
"   normal: vd gy c-f c-t c-b c-j c-k + _ c-\ g= zu z/ m<enter> zy zi zp m<tab> q<special> y<special> q<special>
"           c<space>
"           !@       --> async run

nnoremap \q q
"tnoremap <expr> <C-R> '<C-\><C-N>"'.nr2char(getchar()).'pi'

" copy selection to gui-clipboard
xnoremap Y "+y
" copy entire file contents (to gui-clipboard if available)
nnoremap yY :let b:winview=winsaveview()<bar>exe 'keepjumps keepmarks norm ggVG"+y'<bar>call winrestview(b:winview)<cr>
" copy current (relative) filename (to gui-clipboard if available)
nnoremap "%y <cmd>let @+=fnamemodify(@%, ':.')<cr>

" Put filename tail.
cnoremap <m-%> <c-r>=fnamemodify(@%, ':t')<cr>
cmap     <m-s-5> <m-%>
" current-file directory
noremap! <m-/> <c-r>=expand('%:p:h', 1)<cr>
noremap! <c-r>? <c-r>=substitute(getreg('/'), '[<>\\]', '', 'g')<cr>
" /<BS>: Inverse search (line NOT containing pattern).
cnoremap <expr> <BS> (getcmdtype() =~ '[/?]' && getcmdline() == '') ? '\v^(()@!.)*$<Left><Left><Left><Left><Left><Left><Left>' : '<BS>'
" Hit space to match multiline whitespace.
cnoremap <expr> <Space> getcmdtype() =~ '[/?]' ? '\_s\+' : ' '
" //: "Search within visual selection".
cnoremap <expr> / (mode() =~# "[vV\<C-v>]" && getcmdtype() =~ '[/?]' && getcmdline() == '') ? "\<C-c>\<Esc>/\\%V" : '/'

nnoremap g: :lua =
nnoremap z= <cmd>setlocal spell<CR>z=
nnoremap ' `
inoremap <C-space> <C-x><C-o>

" niceblock
xnoremap <expr> I (mode()=~#'[vV]'?'<C-v>^o^I':'I')
xnoremap <expr> A (mode()=~#'[vV]'?'<C-v>0o$A':'A')


nnoremap g> :set nomore<bar>echo repeat("\n",&cmdheight)<bar>40messages<bar>set more<CR>

" word-wise i_CTRL-Y
inoremap <expr> <c-y> pumvisible() ? "\<c-y>" : matchstr(getline(line('.')-1), '\%' . virtcol('.') . 'v\%(\k\+\\|.\)')

" mark position before search
nnoremap / ms/

nnoremap <expr> n 'Nn'[v:searchforward]
nnoremap <expr> N 'nN'[v:searchforward]

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
nnoremap <M-n> :call <SID>buf_new()<CR>
nnoremap <c-i> <c-i>
nnoremap <silent><expr> <tab> (v:count > 0 ? '<C-w>w' : ':call <SID>switch_to_alt_win()<CR>')
nnoremap <silent>      <s-tab>  <C-^>
" inoremap <c-r><c-w> <esc>:call <sid>switch_to_alt_win()<bar>let g:prev_win_buf=@%<cr><c-w><c-p>gi<c-r>=g:prev_win_buf<cr>

" Fit current window (vertically) to the buffer text.
nnoremap <silent> <m-=> <cmd>exe min([winheight('%'),line('$')]).'wincmd _'<bar>setlocal winfixheight<cr>
" Fit current window (vertically) to the selected text.
xnoremap <silent> <m-=> <esc><cmd>exe (line("'>") - line("'<") + 1).'wincmd _'<bar>setlocal winfixheight<cr>

" go to the previous window (or any other window if there is no 'previous' window).
func! s:switch_to_alt_win() abort
  let currwin = winnr()
  wincmd p
  if winnr() == currwin "window didn't change; no previous window.
    wincmd w
  endif
endf

func! s:get_alt_winnr() abort
  call s:switch_to_alt_win()
  let n = winnr()
  call s:switch_to_alt_win()
  return n
endf

" manage tabs
nnoremap <silent> <M-t>    :tab split<cr>
nnoremap <silent> ZT       :tabclose<cr>
" move tab to Nth position
nnoremap <expr> <M-L> ':<C-u>tabmove '.(v:count ? (v:count - 1) : '+1').'<CR>'
nnoremap <expr> <M-H> ':<C-u>tabmove '.(v:count ? (v:count - 1) : '-1').'<CR>'

" quickfix window (in quickfix: toggles between qf & loc list)
nnoremap <silent><expr> <M-q> '@_:'.(&bt!=#'quickfix'<bar><bar>!empty(getloclist(0))?'lclose<bar>botright copen':'cclose<bar>botright lopen')
      \.(v:count ? '<bar>wincmd L' : '').'<CR>'

nnoremap <expr> zt (v:count > 0 ? '@_zt'.v:count.'<c-y>' : 'zt')
nnoremap <expr> zb (v:count > 0 ? '@_zb'.v:count.'<c-e>' : 'zb')
nnoremap <up> <c-u>
nnoremap <down> <c-d>


" :help :DiffOrig
command! DiffOrig leftabove vnew | set bt=nofile | r ++edit # | 0d_ | diffthis | wincmd p | diffthis

nnoremap yo<space> :set <C-R>=(&diffopt =~# 'iwhiteall') ? 'diffopt-=iwhiteall' : 'diffopt+=iwhiteall'<CR><CR>

" Format filters
" ideas: https://github.com/sbdchd/neoformat
nnoremap gqax    :%!tidy -q -i -xml -utf8<cr>
nnoremap gqah    :%!tidy -q -i -ashtml -utf8<cr>
nnoremap gqaj    :%!python3 -m json.tool<cr>
nnoremap gqav    :call append('$', json_encode(eval(join(getline(1,'$')))))<cr>'[k"_dVgg:%!python -m json.tool<cr>

" un-join (split) the current line at the cursor position
nnoremap gj i<c-j><esc>k$
xnoremap x  "_d

nnoremap vK <C-\><C-N>:help <C-R><C-W><CR>

" text-object: entire buffer
" Elegant text-object pattern hacked out of jdaddy.vim.
function! s:line_outer_movement(count) abort
  if empty(getline(1)) && 1 == line('$')
    return "\<Esc>"
  endif
  let [lopen, copen, lclose, cclose] = [1, 1, line('$'), 1]
  call setpos("'[", [0, lopen, copen, 0])
  call setpos("']", [0, lclose, cclose, 0])
  return "'[o']"
endfunction
xnoremap <expr>   al <SID>line_outer_movement(v:count1)
onoremap <silent> al :normal Val<CR>

" text-object: line
" Elegant text-object pattern hacked out of jdaddy.vim.
function! s:line_inner_movement(count) abort
  "TODO: handle count
  if empty(getline('.'))
    return "\<Esc>"
  endif
  let [lopen, copen, lclose, cclose] = [line('.'), 1, line('.'), col('$')-1]
  call setpos("'[", [0, lopen, copen, 0])
  call setpos("']", [0, lclose, cclose, 0])
  return "`[o`]"
endfunction
xnoremap <expr>   il <SID>line_inner_movement(v:count1)
onoremap <silent> il :normal vil<CR>

" Insert formatted datetime (from @tpope vimrc).
inoremap <silent> <C-G><C-T> <C-R>=repeat(complete(col('.'),map(["%Y-%m-%d %H:%M:%S","%a, %d %b %Y %H:%M:%S %z","%Y %b %d","%d-%b-%y","%a %b %d %T %Z %Y","%Y%m%d"],'strftime(v:val)')+[localtime()]),0)<CR>
" Print unix time at cursor as human-readable datetime. 1677604904 => '2023-02-28 09:21:45'
nnoremap gA :echo strftime('%Y-%m-%d %H:%M:%S', '<c-r><c-w>')<cr>

" Preserve '[ '] on :write.
nnoremap <silent> z. :silent lockmarks update ++p<cr>

" Select last inserted text.
nnoremap gV `[v`]

" Repeat last command for each line of a visual selection.
xnoremap . :normal .<CR>
" Repeat the last edit on the next [count] matches.
nnoremap <silent> gn :normal n.<CR>
nnoremap <C-v>q q

" Record a macro, or set the last-recorded macro to v:register (example: "aq).
nnoremap <expr> q (v:register==#'"')?'q':(':let @'.(empty(reg_recorded())?'q':reg_recorded())." = '<C-R>=substitute(@".v:register.",\"'\",\"''\",\"g\")<CR>'<C-F>010l")

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
nnoremap <Bar> :<C-U>call <SID>zoom_toggle(v:count)<CR>

command! -nargs=+ -bang -complete=command R if !<bang>0 | wincmd n | endif
    \ | call execute(printf("put=execute('%s')", substitute(escape(<q-args>, '"'), "'", "''", 'g')))
inoremap <c-r>R <c-o>:<up><home>R! <cr>

" }}} mappings

augroup config_autocmd
  autocmd!
  autocmd BufReadCmd *.vsix call zip#Browse(expand("<amatch>"))
  autocmd BufReadPost *.i setlocal filetype=c
  autocmd InsertLeave * if &buftype=='' && filereadable(expand('%:p')) | silent lockmarks update ++p | endif

  " Closes the current quickfix list and returns to the alternate window.
  func! s:close_qflist()
    let altwin = s:get_alt_winnr()
    wincmd c
    exe altwin.'wincmd w'
  endf
  autocmd FileType qf nnoremap <buffer> <c-p> <up>
        \|nnoremap <buffer> <c-n> <down>
        \|nnoremap <silent><buffer><nowait> q :call <sid>close_qflist()<cr>
        \|setlocal nolist

  autocmd CmdwinEnter * nnoremap <nowait><silent><buffer> q <C-W>c

  " :help restore-cursor
  autocmd BufReadPre * autocmd FileType <buffer> ++once
    \ if &ft !~# 'commit\|rebase' && line("'\"") > 1 && line("'\"") <= line("$") | exe 'normal! g`"' | endif

  autocmd BufNewFile,BufRead *.txt,README,INSTALL,NEWS,TODO if expand('<afile>:t') !=# 'CMakeLists.txt' | setf text | endif

  autocmd FileType css set omnifunc=csscomplete#CompleteCSS

  autocmd TextYankPost * silent! lua vim.highlight.on_yank {higroup='Visual', timeout=300}

  " autocmd VimEnter * if !empty($NVIM)
  "       \ |let g:r=jobstart(['nc', '-U', $NVIM],{'rpc':v:true})
  "       \ |let g:f=fnameescape(expand('%:p'))
  "       \ |noau bwipe
  "       \ |call rpcrequest(g:r, "nvim_command", "tabedit ".g:f)|qa|endif

  " if exists('##TextYankPost')
  "   autocmd TextYankPost * let g:yankring=get(g:,'yankring',[])
  "     \|call add(g:yankring, join(v:event.regcontents[:999], "\n"))|if len(g:yankring)>10|call remove(g:yankring, 0, 1)|endif
  " endif
augroup END

set wildcharm=<C-Z>
set wildoptions+=fuzzy
nnoremap <expr> <C-b> v:count ? ':<c-u>'.v:count.'buffer<cr>' : ':set nomore<bar>ls<bar>set more<cr>:buffer<space>'
" _opt-in_ to sloppy-search https://github.com/neovim/neovim/issues/3209#issuecomment-133183790
nnoremap <C-f> :edit **/
nnoremap \t    :tag<space>
" See `man fnmatch`.
nnoremap \g  mS:Ggrep! -q <C-R>=(system(['git','grep','-P'])=~#'no pattern')?'-P':'-E'<CR> <C-R>=shellescape(fnameescape(expand('<cword>')))<CR> -- ':/' ':/!*.mpack' ':/!*.pbf' ':/!*.pdf' ':/!*.po' ':(top,exclude,icase)notebooks/' ':/!data/' ':/!work/' ':/!qgis/' ':/!graphhopper_data/'
      \<Home><C-Right><C-Right><C-Right><C-Right><left>
nnoremap 9\g  :Ggrep<m-up><Home><C-Right><C-Right><C-Right><C-Right><left>
nnoremap \v  mS:<c-u>noau vimgrep /\C/j **<left><left><left><left><left>
" search all file buffers (clear qf first).
nnoremap \b  mS:<c-u>cexpr []<bar>exe 'bufdo silent! noau vimgrepadd/\C/j %'<bar>botright copen<s-left><s-left><left><left><left>
" search current buffer and open results in loclist
nnoremap \c   ms:<c-u>lvimgrep // % <bar>lw<s-left><left><left><left><left>

" =============================================================================
" autocomplete / omnicomplete / tags
" =============================================================================
set dictionary+=/usr/share/dict/words
set completeopt-=preview
set complete+=f,kspell
set wildignore+=tags,gwt-unitCache/*,*/__pycache__/*,build/*,build.?/*,*/node_modules/*
" Files with these suffixes get a lower priority when matching a wildcard
set suffixes+=.aux,.log,.dvi,.bbl,.blg,.brf,.cb,.ind,.idx,.ilg,.inx,.out,.toc
      \,.o,.obj,.dll,.class,.pyc,.ipynb,.so,.swp,.zip,.exe,.jar,.gz
" Better `gf`
set suffixesadd=.java,.cs

function! s:fzf_open_file_at_line(e) abort
  "Get the <path>:<line> tuple; fetch.vim plugin will handle the rest.
  execute 'edit' fnameescape(matchstr(a:e, '\v([^:]{-}:\d+)'))
endfunction
function! s:fzf_files() abort
  call fzf#run({
    \ 'source':'find . -type d \( -name build -o -name .git -o -name venv -o -name .vim-src -o -name buildd -o -name buildr -o -name .deps -o -name .vscode-test -o -name node_modules -o -name .coverage \) -prune -false -o -name "*"',
    \ 'sink':{f -> execute('edit '..f)}})
endfunction
function! s:fzf_search_fulltext() abort
  call fzf#run({
    \ 'source':'git grep --line-number --color=never -v "^[[:space:]]*$"',
    \ 'sink':function('<sid>fzf_open_file_at_line')})
endfunction

" Search current-working-directory _or_ current-file-directory
nnoremap <silent><expr> <M-/> v:count ? ':<C-U>call <SID>fzf_search_fulltext()<CR>' : ':<C-U>call <SID>fzf_files()<CR>'
" Search MRU files
nnoremap <silent>       <M-\> :FzHistory<cr>
nnoremap <silent><expr> <C-\> v:count ? 'mS:<C-U>FzLines<CR>' : ':<C-U>FzBuffers<CR>'

nnoremap <silent> gO    :call fzf#vim#buffer_tags('')<cr>
nnoremap <silent> z/    :call fzf#vim#tags('')<cr>


set title
set titlestring=%{getpid().':'.getcwd()}

" special-purpose mappings/commands ===========================================
nnoremap <leader>vv   :exe 'e' fnameescape(resolve($MYVIMRC))<cr>
nnoremap <leader>vp   :exe 'e' stdpath('config')..'/plugin/'<cr>
nnoremap <leader>vr   :Vimref<cr>

" scriptease
" TODO:
"   - g={motion} / g!{motion}
"   - :PP (pretty print expression)
nnoremap <leader>vs   :caddexpr join(map(split(execute('scriptnames'), '\n'), {k,v->matchstr(v,':\s*\zs.*')..':1: '}),"\n")<bar>copen<cr>
nnoremap zS <cmd>Inspect<cr>
command! -addr=other -range=-1 -nargs=? -complete=command Time exe TimeCommand(<q-args>, <count>)
function! TimeCommand(cmd, count) abort
  let time = reltime()
  try
    if a:count > 1
      let i = 0
      while i < a:count
        execute a:cmd
        let i += 1
      endwhile
    else
      execute a:cmd
    endif
  finally
    let elapsed = reltime(time)
    redraw
    echomsg matchstr(reltimestr(elapsed), '.*\..\{,3\}') . ' seconds to run :'.a:cmd
  endtry
  return ''
endfunction

nnoremap <leader>== <cmd>set paste<cr>o<cr><c-r>=repeat('=',80)<cr><cr><c-r>=strftime('%Y%m%d')<cr><cr>.<cr><c-r>+<cr>tags: <esc><cmd>set nopaste<cr>

command! CdVim          exe 'e '.finddir(".vim-src", expand("~")."/neovim/**,".expand("~")."/dev/neovim/**")<bar>lcd %
command! NvimTestScreenshot put =\"local Screen = require('test.functional.ui.screen')\nlocal screen = Screen.new()\nscreen:attach()\nscreen:snapshot_util({},true)\"

func! GetVimref(...) abort
  let tagpat = '\v(\d+\.){2}(\d)+'
  let ref = (a:0 is 0 || empty(a:1)) ? matchstr(expand('<cWORD>'), tagpat) : matchstr(a:1, tagpat)
  if empty(ref) && (a:0 is 0 || empty(a:1))
    return expand('<cword>')
  endif
  return empty(ref) ? a:1 : 'v'..ref
endfunc
func! EditVimref(...) abort
  let ref = GetVimref(a:0 ? a:1 : '')
  let vimdir = luaeval('vim.fs.normalize(vim.fn.expand("~/dev/neovim/.vim-src/"))')
  if winnr('#') != 0
    wincmd p
    let cwd = luaeval('vim.fs.normalize(vim.fn.expand(vim.fn.getcwd()))')
    if cwd !=# vimdir
      " Switch back
      wincmd p
      split ~/dev/neovim/.vim-src/
      lcd %:h
    endif
  else
    split ~/dev/neovim/.vim-src/
    lcd %:h
  endif
  exe 'Gedit '..ref
endfunc
command! -nargs=? Vimref call EditVimref('<args>')
command! Tags !ctags -R -I EXTERN -I INIT --exclude='build*/**' --exclude='**/build*/**' --exclude='cdk.out/**' --exclude='**/cdk.out/**' --exclude='.vim-src/**' --exclude='**/dist/**' --exclude='node_modules/**' --exclude='**/node_modules/**' --exclude='venv/**' --exclude='**/site-packages/**' --exclude='data/**' --exclude='dist/**' --exclude='notebooks/**' --exclude='Notebooks/**' --exclude='*graphhopper_data/*.json' --exclude='*graphhopper/*.json' --exclude='*.json' --exclude='qgis/**'
  \ --exclude=.git --exclude=.svn --exclude=.hg --exclude="*.cache.html" --exclude="*.nocache.js" --exclude="*.min.*" --exclude="*.map" --exclude="*.swp" --exclude="*.bak" --exclude="*.pyc" --exclude="*.class" --exclude="*.sln" --exclude="*.Master" --exclude="*.csproj" --exclude="*.csproj.user" --exclude="*.cache" --exclude="*.dll" --exclude="*.pdb" --exclude=tags --exclude="cscope.*" --exclude="*.tar.*"
  \ *

" Neovim/Vim development
autocmd BufEnter */.vim-src/* setlocal nolist

function! Cxn_py() abort
  vsplit
  terminal
  let py = isdirectory('venv') ? './venv/bin/python3' : 'python3'
  call chansend(&channel, py .. "\nimport pynvim\n")
  call chansend(&channel, "n = pynvim.attach('socket', path='".g:cxn."')\n")
endfunction
function! Cxn(addr) abort
  silent! unlet g:cxn
  tabnew
  if !empty(a:addr)  " Only start the client.
    let g:cxn = a:addr
    call Cxn_py()
    return
  endif

  terminal
  let nvim_path = v:progpath " executable('build/bin/nvim') ? 'build/bin/nvim' : 'nvim'
  call chansend(&channel, nvim_path." -u NORC\n")
  call chansend(&channel, ":let j=jobstart('nc -U ".v:servername."',{'rpc':v:true})\n")
  call chansend(&channel, ":call rpcrequest(j, 'nvim_set_var', 'cxn', v:servername)\n")
  call chansend(&channel, ":call rpcrequest(j, 'nvim_command', 'call Cxn_py()')\n")
endfunction
command! -nargs=* NvimCxn call Cxn(<q-args>)

function! s:init_lynx() abort
  nnoremap <nowait><buffer> <C-F> i<PageDown><C-\><C-N>
  tnoremap <nowait><buffer> <C-F> <PageDown>

  nnoremap <nowait><buffer> <C-B> i<PageUp><C-\><C-N>
  tnoremap <nowait><buffer> <C-B> <PageUp>

  nnoremap <nowait><buffer> <C-D> i:DOWN_HALF<CR><C-\><C-N>
  tnoremap <nowait><buffer> <C-D> :DOWN_HALF<CR>

  nnoremap <nowait><buffer> <C-U> i:UP_HALF<CR><C-\><C-N>
  tnoremap <nowait><buffer> <C-U> :UP_HALF<CR>

  nnoremap <nowait><buffer> <C-N> i<Delete><C-\><C-N>
  tnoremap <nowait><buffer> <C-N> <Delete>

  nnoremap <nowait><buffer> <C-P> i<Insert><C-\><C-N>
  tnoremap <nowait><buffer> <C-P> <Insert>

  nnoremap <nowait><buffer> u     i<Left><C-\><C-N>
  nnoremap <nowait><buffer> <C-R> i<C-U><C-\><C-N>
  nnoremap <nowait><buffer> <CR>  i<CR><C-\><C-N>
  nnoremap <nowait><buffer> gg    i:HOME<CR><C-\><C-N>
  nnoremap <nowait><buffer> G     i:END<CR><C-\><C-N>
  nnoremap <nowait><buffer> zl    i:SHIFT_LEFT<CR><C-\><C-N>
  nnoremap <nowait><buffer> zL    i:SHIFT_LEFT<CR><C-\><C-N>
  nnoremap <nowait><buffer> zr    i:SHIFT_RIGHT<CR><C-\><C-N>
  nnoremap <nowait><buffer> zR    i:SHIFT_RIGHT<CR><C-\><C-N>
  nnoremap <nowait><buffer> gh    i:HELP<CR><C-\><C-N>
  nnoremap <nowait><buffer> cow   i:LINEWRAP_TOGGLE<CR><C-\><C-N>

  tnoremap <buffer> <C-C> <C-G><C-\><C-N>
  nnoremap <buffer> <C-C> i<C-G><C-\><C-N>
endfunction
command! -nargs=1 Web       vnew|call termopen('lynx -use_mouse '.shellescape(<q-args>))|call <SID>init_lynx()
command! -nargs=1 Websearch vnew|call termopen('lynx -use_mouse https://duckduckgo.com/?q='.shellescape(substitute(<q-args>,'#','%23','g')))|call <SID>init_lynx()

silent! source ~/.vimrc.local

