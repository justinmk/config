vim.cmd[==[
" key mappings/bindings =================================================== {{{
"
" available mappings:
"   visual: c-\ <space> m R c-r c-n c-g c-a c-x c-h,<bs><tab>
"   insert: c-\ c-g
"   normal: vd gy c-f c-t c-b c-j c-k + _ c-\ g= zu z/ m<enter> zy zi zp m<tab> q<special> y<special> q<special>
"           c<space>
"           !@       --> async run

"tnoremap <expr> <C-R> '<C-\><C-N>"'.nr2char(getchar()).'pi'

nnoremap j gj
nnoremap k gk
xnoremap j gj
xnoremap k gk

:xnoremap y zy
:nnoremap p zp
:nnoremap P zP
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
noremap! <m-/> <c-r>=expand('%:.:h', 1)<cr>
noremap! <c-r>? <c-r>=substitute(getreg('/'), '[<>\\]', '', 'g')<cr>
" /<BS>: Inverse search (line NOT containing pattern).
cnoremap <expr> <BS> (getcmdtype() =~ '[/?]' && getcmdline() == '') ? '\v^(()@!.)*$<Left><Left><Left><Left><Left><Left><Left>' : '<BS>'
" Hit space to match multiline whitespace.
cnoremap <expr> <Space> getcmdtype() =~ '[/?]' ? '\_s\+' : ' '
" //: "Search within visual selection".
cnoremap <expr> / (getcmdtype() =~ '[/?]' && getcmdline() == '') ? "\<C-c>\<Esc>/\\%V" : '/'

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

" Select last inserted/edited text.
nnoremap gV `[v`]

" Repeat last command for each line of a visual selection.
xnoremap . :normal .<CR>
" Repeat the last edit on the next [count] matches.
nnoremap <silent> gn :normal n.<CR>

" Record a macro, or set the last-recorded macro to v:register (example: "aq).
nnoremap <expr> q (v:register==#'"')?'q':(':let @'.(empty(reg_recorded())?'q':reg_recorded())." = '<C-R>=substitute(@".v:register.",\"'\",\"''\",\"g\")<CR>'<C-F>010l")
nnoremap <C-v>q q
nnoremap \q q

command! -nargs=+ -bang -complete=command R if !<bang>0 | wincmd n | endif
    \ | call execute(printf("put=execute('%s')", substitute(escape(<q-args>, '"'), "'", "''", 'g')))
inoremap <c-r>R <c-o>:<up><home>R! <cr>

" special-purpose mappings/commands ===========================================
nnoremap <expr> <leader>r '<cmd>update<bar>confirm restart edit '..fnameescape(fnamemodify(expand('%'),':p'))..'<cr>'
nnoremap <leader>vv   :exe 'edit' fnameescape(resolve($MYVIMRC))<cr>
nnoremap <leader>vp   :exe 'edit' fnameescape(fnamemodify(resolve($MYVIMRC),':p:h'))..'/lua/my/'<cr>M
nnoremap <leader>vr   :Vimref<cr>

" Make "*" stay on the first match.
nnoremap <silent> *  ms:<c-u>let @/='\V\<'.escape(expand('<cword>'), '/\').'\>'<bar>call histadd('/',@/)<bar>set hlsearch<cr>
nnoremap <silent> g* ms:<c-u>let @/='\V' . escape(expand('<cword>'), '/\')     <bar>call histadd('/',@/)<bar>set hlsearch<cr>

" Configure https://github.com/inkarkat/vim-mark until it enrages me enough to rewrite it.
let g:mw_no_mappings = 1
"nnoremap <silent> m.. :exe 'Mark /\%'..line('.')..'l./'<cr>
nnoremap <silent> m.. :exe 'Mark /\V'..escape(getline('.'), '/\')..'/'<cr>
nnoremap <silent> m*  :exe 'Mark /\V'..escape(substitute('<c-r><c-w>', "'", "''", 'g'), '/\')..'/'<cr>
nmap     <silent> m?  <Plug>MarkToggle
nnoremap <silent> m<bs> :MarkClear<cr>
nmap     <silent> ]m <Plug>MarkSearchAnyOrDefaultNext
nmap     <silent> [m <Plug>MarkSearchAnyOrDefaultPrev
xmap              m*  <Plug>MarkIWhiteSet

nmap <C-j> m'<Plug>(edgemotion-j)
nmap <C-k> m'<Plug>(edgemotion-k)
xmap <C-j> m'<Plug>(edgemotion-j)
xmap <C-k> m'<Plug>(edgemotion-k)
omap <C-j> <Plug>(edgemotion-j)
omap <C-k> <Plug>(edgemotion-k)

inoremap [[ [[ ]]<Left><Left><Left>
inoremap [= [=[ ]=]<Left><Left><Left><Left>
inoremap {<CR> {<CR>}<Esc>O
inoremap {; {<CR>};<Esc>O
inoremap {, {<CR>},<Esc>O
inoremap [<CR> [<CR>]<Esc>O
inoremap [; [<CR>];<Esc>O
inoremap [, [<CR>],<Esc>O

" Use <C-L> to:
"   - redraw
"   - clear 'hlsearch'
"   - update the current diff (if any)
" Use {count}<C-L> to also:
"   - clear all extmark namespaces
nnoremap <silent><expr> <C-L> (v:count ? '<cmd>call nvim_buf_clear_namespace(0,-1,0,-1)<cr>' : '')
      \ .. ':nohlsearch'.(has('diff')?'\|diffupdate':'')
      \ .. '<CR><C-L>'

nnoremap <silent> yoz :<c-u>if &foldenable && 2==&foldnestmax && 0==&foldlevel\|set nofoldenable\|
      \ else\|setl foldmethod=indent foldnestmax=2 foldlevel=0 foldenable\|set foldmethod=manual\|endif<cr>

nnoremap yoT :<c-u>setlocal textwidth=<C-R>=(!v:count && &textwidth != 0) ? 0 : (v:count ? v:count : 80)<CR><CR>

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

nnoremap <leader>== <cmd>set paste<cr>o<cr><c-r>=repeat('=',80)<cr><cr><c-r>=strftime('%Y%m%d')<cr><cr><c-r>+<cr>tags: <esc><cmd>set nopaste<cr>

]==]

-- TODO:
-- run closest zig test case: https://github.com/mfussenegger/dotfiles/commit/8e827b72e2b72e7fb240e8a270d786cffc38a2a5#diff-7d18f76b784e0cb761b7fc0a995680cf2a27b6f77031b60b854248478aed8b6fR5
-- run closest neovim lua test case via make: https://github.com/mfussenegger/dotfiles/commit/a32190b76b678517849d6da84d56836d44a22f2d#diff-f81a3d06561894224d8353f9babc6a7fa9b4962a40c191eb5c23c9cdcc6004c0R158
vim.cmd([[nnoremap mT mT:FocusDispatch VIMRUNTIME= TEST_COLORS=0 TEST_FILE=<c-r>% TEST_FILTER= TEST_TAG= make functionaltest<S-Left><S-Left><S-Left><Left>]])
-- nnoremap <silent> yr  :<c-u>set opfunc=<sid>tmux_run_operator<cr>g@

-- g?: Web search
vim.keymap.set('n', 'g??', function()
  vim.ui.open(('https://google.com/search?q=%s'):format(vim.fn.expand('<cword>')))
end)
vim.keymap.set('x', 'g??', function()
  vim.ui.open(('https://google.com/search?q=%s'):format(vim.trim(table.concat(
    vim.fn.getregion(vim.fn.getpos('.'), vim.fn.getpos('v'), { type=vim.fn.mode() }), ' '))))
  vim.api.nvim_input('<esc>')
end)
