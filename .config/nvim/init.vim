" Windows Registry Editor Version 5.00
"
" [HKEY_CLASSES_ROOT\*\shell\Edit with Vim]
"
" [HKEY_CLASSES_ROOT\*\shell\Edit with Vim\command]
" @="nvim-qt.exe \"%L\""
"==============================================================================

let g:loaded_rrhelper = 1
let g:did_install_default_menus = 1  " avoid stupid menu.vim (saves ~100ms)

let s:plugins = filereadable(expand("~/.config/nvim/autoload/plug.vim", 1))
let s:plugins_extra = s:plugins

if !s:plugins "{{{
  fun! InstallPlug() "bootstrap plug.vim on new systems
    silent call mkdir(expand("~/.config/nvim/autoload", 1), 'p')
    exe '!curl -fLo '.expand("~/.config/nvim/autoload/plug.vim", 1)
      \ .' https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'
  endfun
else

call plug#begin('~/.local/share/nvim/bundle')

if v:false  " Bring the circus to town!
  " Plug 'terryma/vim-multiple-cursors'
  Plug 'vim-airline/vim-airline'
  " Plug 'lfv89/vim-interestingwords'
  Plug 'majutsushi/tagbar'
endif

Plug 'justinmk/molokai'
Plug 'mptre/vim-printf'

" git clone https://github.com/sunaku/dasht
" dasht-docsets-install python
Plug 'sunaku/vim-dasht'
nnoremap <silent> gK :call Dasht([expand('<cword>'), expand('<cWORD>')])<CR>

Plug 'sbdchd/neoformat'
if has('nvim')
  Plug 'justinmk/vim-highlightedyank'
endif

if v:version > 703 && !has('win32') && !has('win32unix')
Plug 'ludovicchabant/vim-gutentags'
let g:gutentags_ctags_exclude = [
      \'.vim-src/**',
      \'venv/**',
      \'**/site-packages/**',
      \'data/**',
      \'dist/**',
      \'notebooks/**',
      \'Notebooks/**',
      \'*graphhopper_data/*.json',
      \'*graphhopper/*.json',
      \'*.json',
      \'qgis/**'
      \]
endif

Plug 'tommcdo/vim-exchange'

Plug 'https://github.com/justinmk/vim-ipmotion.git'
Plug 'https://github.com/justinmk/vim-gtfo.git'
Plug 'https://github.com/justinmk/vim-dirvish.git'
" Disable netrw, but autoload it for `gx`.
let g:loaded_netrwPlugin = 0
nmap gx <Plug>NetrwBrowseX
nnoremap <silent> <Plug>NetrwBrowseX :call netrw#BrowseX(expand((exists("g:netrw_gx")? g:netrw_gx : '<cfile>')),netrw#CheckIfRemote())<CR>

Plug 'https://github.com/justinmk/vim-sneak.git'
let g:sneak#label = 1
let g:sneak#use_ic_scs = 1
let g:sneak#absolute_dir = 1
map f <Plug>Sneak_f
map F <Plug>Sneak_F
map t <Plug>Sneak_t
map T <Plug>Sneak_T
map <M-;> <Plug>Sneak_,

if executable("tmux")
Plug 'wellle/tmux-complete.vim'
endif

Plug 'tpope/vim-characterize'
" Plug 'tpope/vim-sleuth'
Plug 'tpope/vim-scriptease'
Plug 'tpope/vim-apathy'

Plug 'zhaocai/DirDiff.vim', { 'on': ['DirDiff'] }
Plug 'tpope/vim-fugitive'
Plug 'junegunn/gv.vim'
Plug 'tpope/vim-rhubarb'

Plug 'tpope/vim-surround'
let g:surround_indent = 0
let g:surround_no_insert_mappings = 1

Plug 'tpope/vim-dispatch'
nnoremap mT mT:FocusDispatch NVIM_LISTEN_ADDRESS= VIMRUNTIME= TEST_FILE=<c-r>% TEST_FILTER= TEST_TAG= make functionaltest<S-Left><S-Left><S-Left><Left>
" nnoremap <silent> yr  :<c-u>set opfunc=<sid>tmux_run_operator<cr>g@
" xnoremap <silent> R   :<c-u>call <sid>tmux_run_operator(visualmode(), 1)<CR>

Plug 'tpope/vim-repeat'
Plug 'tpope/vim-eunuch'
Plug 'tpope/vim-rsi'

Plug 'tpope/vim-unimpaired'
nmap co yo

Plug 'tpope/vim-endwise'
inoremap (<CR> (<CR>)<Esc>O
inoremap {<CR> {<CR>}<Esc>O
inoremap {; {<CR>};<Esc>O
inoremap {, {<CR>},<Esc>O
inoremap [<CR> [<CR>]<Esc>O
inoremap ([[ ([[<CR>]])<Esc>O
inoremap ([=[ ([=[<CR>]=])<Esc>O
inoremap [; [<CR>];<Esc>O
inoremap [, [<CR>],<Esc>O

Plug 'tpope/vim-obsession'
let g:obsession_no_bufenter = 1  " https://github.com/tpope/vim-obsession/issues/40

let g:markdown_syntax_conceal = 0

Plug 'AndrewRadev/linediff.vim'
let g:linediff_buffer_type = 'scratch'
Plug 'mbbill/undotree', { 'on': ['UndotreeToggle'] }

Plug 'tpope/vim-commentary'

if s:plugins_extra
  Plug 'guns/vim-sexp'
  Plug 'guns/vim-clojure-highlight'
  let g:clojure_fold = 1
  let g:sexp_filetypes = ''

  Plug 'tpope/vim-salve'
  let g:salve_auto_start_repl = 1
  Plug 'tpope/vim-fireplace'

  Plug 'justinmk/nvim-repl'
    nmap yx       <Plug>(ReplSend)
    nmap yxx      <Plug>(ReplSendLine)
    xmap <Enter>  <Plug>(ReplSend)
    nnoremap <c-q> :Repl<CR>

  Plug 'PProvost/vim-ps1'
  Plug 'chrisbra/Colorizer', { 'on': ['ColorHighlight'] }

  Plug 'tommcdo/vim-lion'

  Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': 'yes n \| ./install' }
  Plug 'junegunn/fzf.vim'
  let g:fzf_command_prefix = 'Fz'

  Plug 'tpope/vim-dadbod'

  Plug 'tpope/vim-projectionist'
  " look at derekwyatt/vim-fswitch for more C combos.
if has("nvim") && exists('*luaeval')
lua << EOF
  vim.api.nvim_set_var('projectionist_heuristics', {
      ['*.sln'] = {
        ['*.cs'] = {['alternate'] = {'{}.designer.cs'}},
        ['*.designer.cs'] = {['alternate'] = {'{}.cs'}},
      },
      ['/*.c|src/*.c'] = {
        ['*.c'] = {['alternate'] = {'../include/{}.h', '{}.h'}},
        ['*.h'] = {['alternate'] = '{}.c'},
      },
      ['Makefile'] = {
        ['*Makefile'] = {['alternate'] = '{dirname}CMakeLists.txt'},
        ['*CMakeLists.txt'] = {['alternate'] = '{dirname}Makefile'},
      },
    })
EOF
endif

  " Plug 'Valloric/MatchTagAlways', { 'for': 'xml' }
endif

call plug#end()

" Eager-load these plugins so we can override their settings. {{{
runtime! plugin/rsi.vim
runtime! plugin/commentary.vim
" }}}
endif "}}}

" Searches process tree for a process name. Limited breadth/depth.
fun! s:find_proc_in_tree(rootpid, name, accum) abort
  if a:accum > 9 || !exists('*nvim_get_proc')
    return v:false
  endif
  let p = nvim_get_proc(a:rootpid)
  if !empty(p) && p.name ==# a:name
    return v:true
  endif
  for c in nvim_get_proc_children(a:rootpid)[:9]
    if s:find_proc_in_tree(c, a:name, 1 + a:accum)
      return v:true
    endif
  endfor
  return v:false
endfun

if has("nvim")
  set inccommand=split
  "tnoremap <expr> <C-R> '<C-\><C-N>"'.nr2char(getchar()).'pi'
  tnoremap <silent><expr> <esc> <SID>find_proc_in_tree(b:terminal_job_pid, 'nvim', 0) ? '<esc>' : '<c-\><c-n>'
  augroup vimrc_nvim
    autocmd!
    " https://github.com/neovim/neovim/issues/3463#issuecomment-148757691
    " autocmd CursorHold,FocusGained,FocusLost * silent! rshada|silent! wshada
    " :checktime is SLOW
    " autocmd CursorHold,FocusGained * silent! checktime
    autocmd FocusGained * call <SID>halo()
  augroup END

  if has('nvim-0.3.1')
    set fillchars+=msgsep:‾
    hi MsgSeparator ctermbg=black ctermfg=white
  endif
endif

" Use <C-L> to:
"   - redraw
"   - clear 'hlsearch'
"   - update the current diff (if any)
" Use {count}<C-L> to:
"   - reload (:edit) the current buffer
nnoremap <silent><expr> <C-L> (v:count ? ':<C-U>:call <SID>save_change_marks()\|edit\|call <SID>restore_change_marks()<CR>' : '')
      \ . ':nohlsearch'.(has('diff')?'\|diffupdate':'')
      \ . '<CR><C-L>'

inoremap <C-U> <C-G>u<C-U>

function! s:ctrl_u() abort "{{{ rsi ctrl-u, ctrl-w
  if getcmdpos() > 1
    let @- = getcmdline()[:getcmdpos()-2]
  endif
  return "\<C-U>"
endfunction

function! s:ctrl_w_before() abort
  let s:cmdline = getcmdpos() > 1 ? getcmdline() : ""
  return "\<C-W>"
endfunction

function! s:ctrl_w_after() abort
  if strlen(s:cmdline) > 0
    let @- = s:cmdline[(getcmdpos()-1) : (getcmdpos()-2)+(strlen(s:cmdline)-strlen(getcmdline()))]
  endif
  return ""
endfunction

cnoremap <expr> <C-U> <SID>ctrl_u()
cnoremap <expr> <SID>(ctrl_w_before) <SID>ctrl_w_before()
cnoremap <expr> <SID>(ctrl_w_after) <SID>ctrl_w_after()
cmap   <script> <C-W> <SID>(ctrl_w_before)<SID>(ctrl_w_after)
cnoremap        <C-Y> <C-R>-

"}}}


command! Session if filereadable(stdpath('config').'/session.vim') | exe 'source '.stdpath('config').'/session.vim'
      \ | else | exe 'Obsession '.stdpath('config').'/session.vim' | endif
set sessionoptions-=blank

"==============================================================================
" general settings / options
"==============================================================================
if has('nvim-0.2')
  set cpoptions-=_
  set guicursor+=n:blinkon175
endif

" Don't mess with 'tabstop', with 'expandtab' it isn't used.
" Instead set softtabstop=-1, then 'shiftwidth' is used.
set expandtab shiftwidth=2 softtabstop=-1

" vim-vertical-move replacement
" credit: cherryberryterry: https://www.reddit.com/r/vim/comments/4j4duz/a/d33s213
function! s:vjump(dir) abort
  let c = '%'.virtcol('.').'v'
  let flags = a:dir ? 'bnW' : 'nW'
  let bot = search('\v'.c.'.*\n^(.*'.c.'.)@!.*$', flags)
  let top = search('\v^(.*'.c.'.)@!.*$\n.*\zs'.c, flags)

  " norm! m`
  return a:dir ? (line('.') - (bot > top ? bot : top)).'k'
    \        : ((bot < top ? bot : top) - line('.')).'j'
endfunction
nnoremap <expr> <C-j> <SID>vjump(0)
nnoremap <expr> <C-k> <SID>vjump(1)
xnoremap <expr> <C-j> <SID>vjump(0)
xnoremap <expr> <C-k> <SID>vjump(1)
onoremap <expr> <C-j> <SID>vjump(0)
onoremap <expr> <C-k> <SID>vjump(1)

let g:mapleader = "z,"

set undofile
set list
set fileformats=unix,dos

" [i, [d
set path+=/usr/lib/gcc/**/include
" neovim
set path+=build/src/nvim/auto/**,.deps/build/src/**/,src,src/nvim
" DWIM 'includeexpr': make gf work on filenames like "a/…" (in diffs, etc.).
set includeexpr=substitute(v:fname,'^[^\/]*/','','')

let g:sh_noisk = 1
set hidden      " Allow buffer switching even if unsaved 
set lazyredraw  " no redraws in macros
set cmdheight=2
set ignorecase " case-insensitive searching
set smartcase  " but become case-sensitive if you type uppercase characters

set foldopen-=search
set timeoutlen=3000
set noshowmode " Hide the mode text (e.g. -- INSERT --)
set foldlevelstart=99 "open all folds by default
set splitright
if has('patch-7.4.314') | set shortmess+=c | endif

nnoremap <silent> coz :<c-u>if &foldenable\|set nofoldenable\|
      \ else\|setl foldmethod=indent foldnestmax=2 foldlevel=0 foldenable\|set foldmethod=manual\|endif<cr>

nnoremap cot :setlocal textwidth<C-R>=(&textwidth == 80) ? '<' : '=80'<CR><CR>

set nojoinspaces
set nostartofline
set cursorline
set mouse=nvi

"colorscheme {{{
    " Clear `Normal` cterm values, so terminal emulators won't treat negative
    " space as extra whitespace (makes mouse-copy nicer).
    hi Normal cterm=NONE ctermfg=NONE ctermbg=NONE guifg=white guibg=black
    set background=dark
    hi Cursor gui=NONE cterm=NONE guibg=#F92672 guifg=white ctermbg=197 ctermfg=white
    hi SpecialKey ctermfg=241
    hi! link NonText Comment
    hi Whitespace ctermfg=darkgrey
    hi Comment guifg=#7E8E91 ctermfg=244
    hi! link Title Comment
    hi! link Constant Normal

    hi QuickFixLine guifg=black guibg=cyan ctermfg=black ctermbg=cyan
    " Special should be (at least) slightly distinct from SpecialKey.
    hi Special ctermfg=lightgrey guifg=lightgrey
    " hi Special guifg=#F92672 gui=bold ctermfg=197 cterm=NONE

    " cyan
    hi Identifier ctermfg=cyan guifg=cyan
    hi! link Statement Identifier
    hi! link Exception Identifier
    " affects:
    "   - NONE string in 'hi Normal ctermfg=NONE …'
    "   - helpHeader
    hi! link PreProc Identifier

    " hi Type ctermfg=NONE
    hi! link Type Identifier
    " hi String guifg=#FFE792 guibg=NONE gui=NONE ctermfg=222 ctermbg=NONE cterm=NONE
    hi MoreMsg guifg=LightGreen guibg=NONE gui=NONE ctermfg=LightGreen ctermbg=NONE cterm=NONE
    hi! link String MoreMsg
    hi! link Question MoreMsg

    hi Todo guifg=black guibg=lightgreen ctermfg=black ctermbg=lightgreen
    " hi! link WildMenu Todo
    hi WildMenu ctermbg=cyan ctermfg=black

    " completion/popup menu
    hi Pmenu guifg=#FFFFFF guibg=#585858 gui=NONE ctermfg=255 ctermbg=240 cterm=NONE
    hi! link PmenuSel Todo
    hi PmenuSbar guibg=darkgray ctermbg=darkgray
    hi PmenuThumb ctermbg=lightgreen ctermfg=lightgreen

    " tabline
    " hi default link TabLineFill TabLine
    hi! link TabLineFill TabLine

    " diff (unified)
    hi diffAdded       guifg=#00ff5f gui=NONE      ctermfg=47  cterm=NONE
    hi diffRemoved     guifg=#ff5f5f gui=NONE      ctermfg=203 cterm=NONE
    hi link diffSubname Normal

    " diff (side-by-side)
    hi DiffAdd         guifg=#000000 guibg=#00ff5f ctermfg=0   ctermbg=47  gui=NONE cterm=NONE
    hi DiffChange      guifg=#FFFFFF guibg=#4C4745 ctermfg=255 ctermbg=239 gui=NONE cterm=NONE
    hi DiffDelete      guifg=#ff5f5f guibg=NONE    ctermfg=203 ctermbg=NONE gui=NONE cterm=NONE
    hi DiffText        guifg=black   guibg=cyan    ctermfg=16  ctermbg=cyan gui=NONE cterm=NONE

    "If 242 is too dark, keep incrementing...
    hi FoldColumn      guifg=#465457 guibg=#000000 ctermfg=242 ctermbg=16
    hi Folded          guifg=#465457 guibg=NONE    ctermfg=242 ctermbg=NONE

    hi Error           guifg=#FFFFFF   guibg=Red   ctermfg=15 ctermbg=9
    hi ErrorMsg        ctermfg=203 ctermbg=NONE guifg=#ff5f5f guibg=#161821

    hi Search guifg=#000000 guibg=#FFE792 ctermfg=0 ctermbg=222 cterm=NONE
    hi! link IncSearch Todo
    hi! link Substitute Visual

    hi Visual gui=NONE cterm=NONE guifg=black guibg=white ctermfg=black ctermbg=white
    hi StatusLine cterm=bold,reverse gui=bold,reverse
    hi StatusLineNC guifg=bg guibg=darkgrey ctermfg=232 ctermbg=242 cterm=NONE gui=NONE
    hi VertSplit guifg=#808080 guibg=#080808 gui=bold ctermfg=244 ctermbg=232 cterm=bold

    hi! link Directory Identifier
    hi CursorLine guibg=#303030 ctermbg=236 cterm=NONE
    hi! link LineNr CursorLine
    hi! link SignColumn StatusLineNC
    hi! link CursorLineNr Normal

    hi SpellBad ctermbg=red ctermfg=255 cterm=undercurl gui=undercurl guisp=Red
    hi SpellCap ctermbg=lightgrey ctermfg=red cterm=undercurl gui=undercurl guisp=Blue
    hi! link SpellRare SpellCap

    hi Underlined ctermfg=NONE cterm=underline gui=underline guifg=NONE

    " other
    hi helpHyperTextJump cterm=underline ctermfg=cyan
    hi MatchParen ctermfg=black ctermbg=white guifg=black guibg=white
"}}}

"==============================================================================
" text, tab and indent 

set formatoptions+=rno1l
" don't syntax-highlight long lines
set synmaxcol=200

set linebreak
set nowrap

" =============================================================================
" normal mode

nnoremap & n:&&<CR>
xnoremap & n:&&<CR>

" Make Y consistent with C and D.
nnoremap Y y$
" copy selection to gui-clipboard
xnoremap Y "+y
" copy entire file contents (to gui-clipboard if available)
nnoremap yY :let b:winview=winsaveview()<bar>exe 'keepjumps keepmarks norm ggVG'.(has('clipboard')?'"+y':'y')<bar>call winrestview(b:winview)<cr>
inoremap <insert> <C-r>+

" delete the 'head' of a path on the command line
cnoremap <silent> <c-x> <C-\>e<sid>delete_until()<cr>

func! s:delete_until() abort
  let c = nr2char(getchar())
  return substitute(getcmdline(), '\(.*['.escape(c, '\').']\).*', '\1', '')
endfunc

" key mappings/bindings =================================================== {{{
nnoremap z= :setlocal spell<CR>z=
nnoremap ' `
inoremap <C-space> <C-x><C-o>

" niceblock
xnoremap <expr> I (mode()=~#'[vV]'?'<C-v>^o^I':'I')
xnoremap <expr> A (mode()=~#'[vV]'?'<C-v>0o$A':'A')


nnoremap g> :set nomore<bar>40messages<bar>set more<CR>

xnoremap g/ <Esc>/\%V
" inverse-search: line NOT containing pattern
nnoremap g? /\v^(()@!.)*$<Left><Left><Left><Left><Left><Left><Left>

" word-wise i_CTRL-Y
inoremap <expr> <c-y> pumvisible() ? "\<c-y>" : matchstr(getline(line('.')-1), '\%' . virtcol('.') . 'v\%(\k\+\\|.\)')

" current-file directory
noremap! <silent> <c-r><c-\> <c-r>=expand('%:p:h', 1)<cr>

noremap! <c-r>? <c-r>=substitute(getreg('/'), '[<>\\]', '', 'g')<cr>

" mark position before search
nnoremap / ms/

func! s:maybe_zz(cmd,halo) abort
  let topline = line('w0')
  try
    exe a:cmd
  catch /E486:/
    echohl ErrorMsg | echom matchstr(v:exception, 'E486:.*') | echohl None
  endtry
  if topline != line('w0')
    normal! zz
    if a:halo
      call s:halo()
    endif
  endif
endf
nnoremap <silent> n :<C-U>call <SID>maybe_zz('norm! '.v:count1.'Nn'[v:searchforward],1)<CR>
nnoremap <silent> N :<C-U>call <SID>maybe_zz('norm! '.v:count1.'nN'[v:searchforward],1)<CR>

" manage windows
"       [count]<c-w>s and [count]<c-w>v create a [count]-sized split
"       [count]<c-w>| and [count]<c-w>_ resize the current window
" user recommendation:
"       <c-w>eip
" available:
"       <c-w><space>{motion}
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
nnoremap Zh     :leftabove vsplit<CR>
nnoremap Zj     :belowright split<CR>
nnoremap Zk     :aboveleft split<CR>
nnoremap Zl     :rightbelow vsplit<CR>
nmap     ZH     Zh
nmap     ZJ     Zj
nmap     ZK     Zk
nmap     ZL     Zl
nnoremap <M-n> :call <SID>buf_new()<CR>
nnoremap <silent><expr> <tab> (v:count > 0 ? '<C-w>w' : ':call <SID>switch_to_alt_win()<CR>')
nnoremap <silent>      <s-tab>  <C-^>
nnoremap <m-i> <c-i>
" inoremap <c-r><c-w> <esc>:call <sid>switch_to_alt_win()<bar>let g:prev_win_buf=@%<cr><c-w><c-p>gi<c-r>=g:prev_win_buf<cr>
" nnoremap y@%   :<c-u>let @"=@%<cr>

func! s:win_motion_resize(type) abort
  let sel_save = &selection
  let &selection = "inclusive"

  if a:type ==# 'line' || line("']") > line("'[")
    exe (line("']") - line("'[") + 1) 'wincmd _'
    norm! `[zt
  endif
  if a:type !=# 'line'
    "TODO: this assumes sign column is visible.
    exe ( col("']") -  col("'[") + 3) 'wincmd |'
  endif

  let &selection = sel_save
endf

" fit the current window height to the selected text
xnoremap <silent> <c-w><c-w>  :<C-u>set winfixwidth winfixheight opfunc=<sid>win_motion_resize<CR>gvg@

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

" manage buffers
nnoremap <expr><silent> ZB  ':<c-u>call <SID>buf_kill('. !v:count .')<cr>'

" quickfix window (in quickfix: toggles between qf & loc list)
nnoremap <silent><expr> <M-q> '@_:'.(&bt!=#'quickfix'<bar><bar>!empty(getloclist(0))?'lclose<bar>botright copen':'cclose<bar>botright lopen').'<CR>'

nnoremap <expr> zt (v:count > 0 ? '@_zt'.v:count.'<c-y>' : 'zt')
nnoremap <expr> zb (v:count > 0 ? '@_zb'.v:count.'<c-e>' : 'zb')

" set working directory to the current buffer's directory
nnoremap cd :lcd %:p:h<bar>pwd<cr>
nnoremap cu :lcd ..<bar>pwd<cr>

if findfile('plugin/fugitive.vim', &rtp) !=# ''
  func! s:ctrl_g(cnt) abort
    redraw
    redir => msg | silent exe "norm! 1\<c-g>" | redir END
    " Show git branch.
    echo fugitive#head(7) msg[2:] (a:cnt?strftime('%Y-%m-%d %H:%M',getftime(expand('%:p'))):'')
    " Show current directory.
    echo 'dir:' fnamemodify(getcwd(), ':~')
    " Show current session.
    echo 'ses:' (strlen(v:this_session) ? fnamemodify(v:this_session, ':~') : '<none>')
    " Show current context.
    " https://git.savannah.gnu.org/cgit/diffutils.git/tree/src/diff.c?id=eaa2a24#n464
    echohl ModeMsg
    echo getline(search('\v^[[:alpha:]$_]', 'bn', 1, 100))
    echohl None
  endf
  nnoremap <C-g> :<c-u>call <sid>ctrl_g(v:count)<cr>
endif

nnoremap <silent> <C-n> :<C-U>call <SID>maybe_zz(&diff ? 'norm ]c]n' : 'norm ]n',1)<CR>
nnoremap <silent> <C-p> :<C-U>call <SID>maybe_zz(&diff ? 'norm [c[n' : 'norm [n',1)<CR>

" version control
xnoremap <expr> D (mode() ==# "V" ? ':Linediff<cr>' : 'D')
nnoremap <silent> Ub             :Gblame<cr>
nnoremap <silent> Ud :<C-U>if &diff<bar>diffupdate<bar>elseif !v:count && empty(<SID>git_do('diff -- '.shellescape(fugitive#buffer().path())))<bar>echo 'no changes'<bar>else<bar>exe 'Gvdiff'.(v:count ? ' HEAD'.repeat('^', v:count) : '')<bar>call feedkeys('<c-v><c-l>')<bar>endif<cr>
                    "\:call feedkeys("\<lt>C-w>\<lt>C-w>gg]c")<CR>
nnoremap <silent> Ue             :exe 'Gedit\|'.line('.')<cr>zz
nnoremap          Uf             :Gcommit --fixup=
nnoremap <silent> Ug             :Gedit <C-R><C-W><cr>
nnoremap <silent> Ul :GV!<cr>
nnoremap          Um :GV -L :<C-r><C-w>:<C-r>%
nmap     <silent> Up :<c-u>call <sid>git_blame_line('<C-R><C-G>', line('.'))<CR>
"                                        ^ Get repo-relative path via fugitive
nnoremap <silent> Ur             :Gread<cr>
nnoremap <silent> Us             :Gstatus<cr>
nnoremap <silent> Uw :if !exists(":Gwrite")<bar>call fugitive#detect(expand('%:p'))<bar>endif<bar>Gwrite<cr>

nmap UB Ub
nmap UD Ud
nmap UE Ue
nmap UF Uf
nmap UG Ug
nmap UL Ul
nmap UM Um
nmap UP Up
nmap UR Ur
nmap US Us
nmap UW Uw

"linewise partial staging in visual-mode.
xnoremap <c-p> :diffput<cr>
xnoremap <c-o> :diffget<cr>
nnoremap <expr> dp &diff ? 'dp' : ':Printf<cr>'

" Executes git cmd in the context of b:git_dir.
function! s:git_do(cmd) abort
  " git 1.8.5: -C is a (more reliable) alternative to --git-dir/--work-tree.
  return systemlist('git -C '.shellescape(fnamemodify(b:git_dir, ':p:h:h'))
        \ . ' ' . a:cmd)
endfunction

" Gets the git commit hash associated with the given file line.
function! s:git_sha(filepath, line) abort
  if '' ==# s:trimws_ml(a:filepath)
    throw 'invalid (empty) filepath'
  elseif !exists("b:git_dir")
    throw 'Missing b:git_dir'
  elseif a:line <= 0
    throw 'Invalid a:line: '.a:line
  endif

  let cmd_out = join(s:git_do('blame --root -l -L'.a:line.','.a:line.' -- '.a:filepath))
  if cmd_out =~# '\v^0{40,}'
    return ''
  endif

  return matchstr(cmd_out, '\w\+\ze\W\?', 0, 1)
endfunction

function! s:git_blame_line(filepath, line) abort
  if '' ==# s:trimws_ml(a:filepath)
    echo 'cannot blame'
    return
  endif

  if 'blob' ==# get(b:,'fugitive_type','')
    " Use fugitive blame in fugitive buffers, instead of git directly.
    let commit_id = substitute(execute('.Gblame'), '\v\_s*(\S+)\s+.*', '\1', '')
  else
    " Use git directly.
    let commit_id = s:git_sha(a:filepath, a:line)
  endif

  if commit_id ==# ''
    echo 'not committed'
    return
  elseif commit_id ==# 'fatal'
    echo 'cannot blame'
    return
  endif

  " Find commit(s) with 0 parents.
  " Note: `git blame` prepends a caret ^ to root commits unless --root is
  "       passed. But it doesn't always mark the 'root' commits we are
  "       interested in, so collect them explicitly with `git rev-list`.
  let b:git_root_commits = get(b:, 'git_root_commits', s:git_do('rev-list --max-parents=0 HEAD'))

  if -1 != index(b:git_root_commits, commit_id)
    echo 'root commit'
    return
  endif

  exe 'Gpedit '.commit_id
endfunction

" :help :DiffOrig
command! DiffOrig leftabove vnew | set bt=nofile | r ++edit # | 0d_ | diffthis | wincmd p | diffthis

nnoremap co<space> :set <C-R>=(&diffopt =~# 'iwhite') ? 'diffopt-=iwhite' : 'diffopt+=iwhite'<CR><CR>

" filter
nnoremap gqax    :%!tidy -q -i -xml -utf8<cr>
nnoremap gqah    :%!tidy -q -i -ashtml -utf8<cr>
nnoremap gqaj    :%!python -m json.tool<cr>

" available mappings:
"   visual: c-\ <space> m R c-r c-n c-g c-a c-x c-h,<bs><tab>
"   insert: c-\ c-g
"   normal: gy c-f c-t c-b c-j c-k + _ c-\ g= zu z/ m<enter> zy zi zp m<tab> q<special> y<special> q<special>
"           c<space>
"           !@       --> async run

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
  if -1 != bufnr("#") && bufnr("#") != bufnr("%")
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
  " obliterate the buffer and all of its related state (marks, local options, ...), 
  if bufexists(origbuf) "some other mechanism may have deleted the buffer already.
    exe 'bdelete! '.origbuf
  endif
endf

nnoremap <m-cr> -
xnoremap <m-cr> -
onoremap <m-cr> -

" un-join (split) the current line at the cursor position
nnoremap gj i<c-j><esc>k$
" vaporize delete without overwriting the default register
nnoremap vd "_d
xnoremap x  "_d
nnoremap vD "_D
xnoremap P  "0p

nnoremap vK <C-\><C-N>:help <C-R><C-W><CR>

func! s:trimws_ml(s) abort "trim whitespace across multiple lines
  return substitute(a:s, '^\_s*\(.\{-}\)\_s*$', '\1', '')
endf
"why?
" - repeatable
" - faster/more convenient than visual-replace
" - does not modify ' mark
" - DWIM behavior for linewise => characterwise
let s:rr_reg = '"'
func! s:set_reg(reg_name) abort
  let s:rr_reg = a:reg_name
endf
func! s:replace_without_yank(type) abort
  let rr_orig = getreg(s:rr_reg, 1) "save registers and types to restore later.
  let rr_type = getregtype(s:rr_reg)
  let ur_orig = getreg('"', 1)
  let ur_type = getregtype('"')
  let sel_save = &selection
  let &selection = "inclusive"
  let replace_curlin = (1==col("'[") && (col('$')==1 || col('$')==(col("']")+1)) && line("'[")==line("']"))

  if a:type ==? 'line' || replace_curlin
    exe "keepjumps normal! '[V']\"".s:rr_reg."p"
  elseif a:type ==? 'block'
    exe "keepjumps normal! `[\<C-V>`]\"".s:rr_reg."p"
  else
    "DWIM: if pasting linewise contents in a _characterwise_ motion, trim
    "      surrounding whitespace from the content to be pasted.
    if rr_type ==# "V"
      call setreg(s:rr_reg, s:trimws_ml(rr_orig), "v")
    endif
    exe "keepjumps normal! `[v`]\"".s:rr_reg."p"
  endif

  let &selection = sel_save
  call setreg('"',      ur_orig, ur_type)
  call setreg(s:rr_reg, rr_orig, rr_type)
endf

nnoremap <silent> dr  :<C-u>call <sid>set_reg(v:register)<bar>set opfunc=<sid>replace_without_yank<CR>g@
nnoremap <silent> drr :<C-u>call <sid>set_reg(v:register)<cr>0:<C-u>set opfunc=<sid>replace_without_yank<CR>g@$

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

" from tpope vimrc
inoremap <M-o> <C-O>o
inoremap <M-O> <C-O>O
inoremap <silent> <C-G><C-T> <C-R>=repeat(complete(col('.'),map(["%Y-%m-%d %H:%M:%S","%a, %d %b %Y %H:%M:%S %z","%Y %b %d","%d-%b-%y","%a %b %d %T %Z %Y"],'strftime(v:val)')+[localtime()]),0)<CR>

"do not clobber '[ '] on :write
function! s:save_change_marks() abort
  let s:change_marks = [getpos("'["), getpos("']")]
endfunction
function! s:restore_change_marks() abort
  call setpos("'[", s:change_marks[0])
  call setpos("']", s:change_marks[1])
endfunction
nnoremap z. :call <SID>save_change_marks()<Bar>w<Bar>call <SID>restore_change_marks()<cr>

" select last inserted text
nnoremap gV `[v`]

" repeat last command for each line of a visual selection
xnoremap . :normal .<CR>
" XXX: fix this
" repeat the last edit on the next [count] matches.
nnoremap <silent> gn :normal n.<CR>:<C-U>call repeat#set("n.")<CR>
nnoremap <C-v>q q

" Replay @q or set it from v:register.
nnoremap <expr> <Space> (v:register==#'"')?'@q':(":let @q = '<C-R><C-R>".v:register."'<C-F>010l")
" Replay @q for each line of the visual selection.
xnoremap <Space> @q

nnoremap <expr> <c-w><c-q>  (v:count ? ':<c-u>confirm qa<cr>' : '<c-w><c-q>')
nnoremap <expr> <c-w>q      (v:count ? ':<c-u>confirm qa<cr>' : '<c-w><c-q>')
nnoremap <expr> ZZ          (v:count ? ':<c-u>xa!<cr>' : '@_ZZ')
nnoremap <expr> ZQ          (v:count ? ':<c-u>qa!<cr>' : '@_ZQ')
nnoremap <expr> <c-w>=      (v:count ? ':<c-u>windo setlocal nowinfixheight nowinfixwidth<cr><c-w>=' : '@_<c-w>=')

func! s:zoom_toggle() abort
  if 1 == winnr('$')
    return
  endif
  let restore_cmd = winrestcmd()
  wincmd |
  wincmd _
  if exists('t:zoom_restore')
    exe t:zoom_restore
    unlet t:zoom_restore
  else
    let t:zoom_restore = restore_cmd
  endif
  return '<Nop>'
endfunc
func! s:zoom_or_goto_column(cnt) abort
  if a:cnt
    exe 'norm! '.v:count.'|'
  else
    call s:zoom_toggle()
  endif
endfunc
nnoremap +     :<C-U>call <SID>zoom_or_goto_column(v:count)<CR>
nnoremap <Bar> :<C-U>call <SID>zoom_or_goto_column(v:count)<CR>

func! ReadExCommandOutput(newbuf, cmd) abort
  redir => l:message
  silent! execute a:cmd
  redir END
  if a:newbuf | wincmd n | endif
  silent put=l:message
endf
command! -nargs=+ -bang -complete=command R call ReadExCommandOutput(<bang>0, <q-args>)
inoremap <c-r>R <c-o>:<up><home>R! <cr>

func! s:get_visual_selection_list() abort
  let [lnum1, col1] = getpos("'<")[1:2]
  let [lnum2, col2] = getpos("'>")[1:2]
  let lines = getline(lnum1, lnum2)
  let lines[-1] = lines[-1][: col2 - (&selection ==? 'inclusive' ? 1 : 2)]
  let lines[0] = lines[0][col1 - 1:]
  return lines
endf

func! s:get_visual_selection_searchpattern() abort
  let lines = s:get_visual_selection_list()
  let lines = map(lines, 'escape(v:val, ''/\'')')
  " Join with a _literal_ \n to make a valid search pattern.
  return join(lines, '\n')
endf

"read last visual-selection into command line
cnoremap <c-r><c-v> <c-r>=join(<sid>get_visual_selection_list(), " ")<cr>
inoremap <c-r><c-v> <c-r>=join(<sid>get_visual_selection_list(), " ")<cr>

xnoremap * <esc>ms/\V<c-r>=<sid>get_visual_selection_searchpattern()<cr><cr>
nnoremap <silent> *  ms:<c-u>let @/='\V\<'.escape(expand('<cword>'), '/\').'\>'<bar>set hlsearch<cr>
nnoremap <silent> g* ms:<c-u>let @/='\V' . escape(expand('<cword>'), '/\')     <bar>set hlsearch<cr>

hi MarkLine guibg=darkred guifg=gray ctermbg=9 ctermfg=15
func! s:markline() abort
  let b:vimrc_markedlines = get(b:, "vimrc_markedlines", {})
  "TODO: This will get stale if the line moves.
  "      :sign is a solution, but need to create a way to get un-used sign {id}s.
  let b:vimrc_markedlines[line('.')] = matchaddpos("MarkLine", [line('.')])
endf
nnoremap <silent> m.  :call <sid>markline()<cr>
nnoremap <silent> m<bs> :call matchdelete(b:vimrc_markedlines[line('.')])<cr>

" }}} mappings

highlight Halo  guifg=black guibg=white   ctermfg=black ctermbg=white
highlight Halo2 guifg=white guibg=#F92672 ctermfg=white ctermbg=197
let g:halo = {}
function! s:halo_clear(id) abort
  silent! call matchdelete(g:halo['hl_id'])
endfunction
function! s:halo() abort
  call s:halo_show('Halo', -1)
  call timer_start(200, function('s:halo_show', ['Halo2']))
  call timer_start(500, function('s:halo_clear'))
endfunction
function! s:halo_show(hl, id) abort
  call s:halo_clear(-1)
  let lcol = col('.') > 10 ? col('.') - 10 : 1
  let g:halo['hl_id'] = matchaddpos(a:hl,
        \[[line('.'),   lcol, 20],
        \ [line('.')-1, lcol, 20],
        \ [line('.')-2, lcol, 20],
        \ [line('.')+1, lcol, 20],
        \ [line('.')+2, lcol, 20]]
        \)
endfunction
augroup halo_plugin
  autocmd!
  autocmd WinLeave * call <SID>halo_clear(-1)
augroup END

" A massively simplified take on https://github.com/chreekat/vim-paren-crosshairs
func! s:matchparen_cursorcolumn_setup() abort
  augroup matchparen_cursorcolumn
    autocmd!
    autocmd CursorMoved * if get(w:, "paren_hl_on", 0) | set cursorcolumn | else | set nocursorcolumn | endif
    autocmd InsertEnter * set nocursorcolumn
  augroup END
endf
if 0 && !&cursorcolumn
  augroup matchparen_cursorcolumn_setup
    autocmd!
    " - Add the event _only_ if matchparen is enabled.
    " - Event must be added _after_ matchparen loaded (so we can react to w:paren_hl_on).
    autocmd CursorMoved * if exists("#matchparen#CursorMoved") | call <sid>matchparen_cursorcolumn_setup() | endif
          \ | autocmd! matchparen_cursorcolumn_setup
  augroup END
endif

augroup vimrc_savecommitmsg
  autocmd!
  " Remember last git commit message
  func! s:store_commit_msg()
    " save
    let [w,r]=[winsaveview(),getreg('"', 1)]
    let [_reg_c,_cmark1,_cmark2] = [@c,getpos("'["),getpos("']")]

    let @c=''
    silent! keepmarks keepjumps keeppatterns 1;/^#/g/\v(^$)|^([^#].*$)/y C
    keepmarks keepjumps let g:removed_whitespace =
           \ substitute(@c, '\_[[:space:]]*', '', 'g')
    let @c = len(g:removed_whitespace) < 10
           \ ? _reg_c : @c[1:]  " remove first (empty) line

    " restore
    call winrestview(w)
    call setreg('"', r)
    call setpos("'[", _cmark1)
    call setpos("']", _cmark2)
  endf
  autocmd BufEnter COMMIT_EDITMSG
        \ autocmd! vimrc_savecommitmsg TextChanged,TextChangedI <buffer> silent call <SID>store_commit_msg()
  autocmd BufWritePost COMMIT_EDITMSG let g:LAST_COMMIT_MSG = @c
augroup END

augroup vimrc_autocmd
  autocmd!

  autocmd TabLeave * let g:lasttab=tabpagenr()
  nnoremap <silent><expr> <c-tab> g:lasttab.'gt'

  autocmd FileType text setlocal textwidth=80
  autocmd BufReadPost *.i setlocal filetype=c

  " Closes the current quickfix list and returns to the alternate window.
  func! s:close_qflist()
    let altwin = s:get_alt_winnr()
    wincmd c
    exe altwin.'wincmd w'
    " let [win_cnt, last_win] = [winnr('$'), winnr() == winnr('$')]
    " if last_win "If quickfix window is open, it is _always_ the last window.
    "   cclose
    "   if win_cnt == winnr('$')
    "     lclose " :cclose didn't change win count; that means we were in loclist.
    "   end
    " else
    "   lclose
    " endif
  endf
  autocmd FileType qf nnoremap <buffer> <c-p> <up>
        \|nnoremap <buffer> <c-n> <down>
        \|nnoremap <silent><buffer> q :call <sid>close_qflist()<cr>

  autocmd CmdwinEnter * nnoremap <silent><buffer> q <C-W>c

  " Jump to the last position when reopening a file (except Git commit)
  autocmd BufReadPost * if @% !~# '\.git[\/\\]COMMIT_EDITMSG$' && line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g`\"" | endif

  autocmd BufNewFile,BufRead *.txt,README,INSTALL,NEWS,TODO setf text
  autocmd BufNewFile,BufRead *.proj set ft=xml "force filetype for msbuild
  autocmd FileType gitconfig setlocal commentstring=#\ %s
  function! s:setup_gitstatus() abort
    nmap <buffer> <M-n> <c-n>dvgg:call feedkeys('<c-v><c-w>h<c-v><c-n><c-v><c-w>P<c-v><c-l>')<cr>
    nmap <buffer> <M-p> <c-p>dvgg:call feedkeys('<c-v><c-w>h<c-v><c-n><c-v><c-w>P<c-v><c-l>')<cr>
    nmap <buffer><nowait> <M-u> U<Esc>
  endfunction
  autocmd FileType gitcommit call <SID>setup_gitstatus()
  autocmd BufWinEnter * if exists("*fugitive#detect") && empty(expand('<afile>'))|call fugitive#detect(getcwd())|endif

  autocmd FileType css set omnifunc=csscomplete#CompleteCSS

  au BufWritePre,FileWritePre * call mkdir(expand('<afile>:p:h'), 'p')

  "when Vim starts in diff-mode (vim -d, git mergetool):
  "  - do/dp should not auto-fold
  autocmd VimEnter * if &diff | exe 'windo set foldmethod=manual' | endif

  " autocmd VimEnter * if !empty($NVIM_LISTEN_ADDRESS) && $NVIM_LISTEN_ADDRESS !=# v:servername
  "       \ |let g:r=jobstart(['nc', '-U', $NVIM_LISTEN_ADDRESS],{'rpc':v:true})
  "       \ |let g:f=fnameescape(expand('%:p'))
  "       \ |noau bwipe
  "       \ |call rpcrequest(g:r, "nvim_command", "tabedit ".g:f)|qa|endif

  autocmd BufRead,BufNewFile *.{ascx,aspx} setlocal shiftwidth=2 copyindent

  if exists('##TextYankPost')
    autocmd TextYankPost * let g:yankring=get(g:,'yankring',[])
      \|call add(g:yankring, join(v:event.regcontents[:999], "\n"))|if len(g:yankring)>10|call remove(g:yankring, 0, 1)|endif
  endif
augroup END

" :shell
" Creates a global default :shell with maximum 'scrollback'.
func! s:ctrl_s(cnt, new, here) abort
  let init = { 'termbuf': -1,  'prevwid': win_getid() }
  let g:term_shell = a:new ? init : get(g:, 'term_shell', init)
  let d = g:term_shell
  let b = d.termbuf

  if a:here && bufexists(b)  " Edit the :shell buffer in this window.
    exe 'buffer' b
    return
  endif

  " Return to previous window.
  if bufnr('%') == b
    let term_prevwid = win_getid()
    if !win_gotoid(d.prevwid)
      wincmd p
    endif
    if bufnr('%') == b
      " Edge-case: :shell buffer showing in multiple windows in curtab.
      " Find a non-:shell window in curtab.
      let bufs = filter(tabpagebuflist(), 'v:val != '.b)
      if len(bufs) > 0
        exe bufwinnr(bufs[0]).'wincmd w'
      else
        " Last resort: WTF, just go to previous tab.
        tabprevious
      endif
    endif
    let d.prevwid = term_prevwid
    return
  endif

  " Go to existing :shell or create new.
  let curwinid = win_getid()
  if a:cnt == 0 && bufexists(b) && winbufnr(d.prevwid) == b
    call win_gotoid(d.prevwid)
  elseif bufexists(b)
    let w = bufwinid(b)
    if a:cnt == 0 && w > 0  " buffer exists in current tabpage
      call win_gotoid(w)
    else  " not in current tabpage; go to first found
      let ws = win_findbuf(b)
      if a:cnt == 0 && !empty(ws)
        call win_gotoid(ws[0])
      else
        exe ((a:cnt == 0) ? 'tab split' : a:cnt.'split')
        exe 'buffer' b
        " augroup nvim_shell
        "   autocmd!
        "   autocmd TabLeave <buffer> if winnr('$') == 1 && bufnr('%') == g:term_shell.termbuf | tabclose | endif
        " augroup END
      endif
    endif
  else
    let origbuf = bufnr('%')
    exe ((a:cnt == 0) ? 'tab split' : a:cnt.'split')
    terminal
    " augroup nvim_shell
    "   autocmd!
    "   autocmd TabLeave <buffer> if winnr('$') == 1 && bufnr('%') == g:term_shell.termbuf | tabclose | endif
    " augroup END
    exe 'file :shell '.jobpid(&channel)
    " XXX: original term:// buffer hangs around after :file ...
    bwipeout! #
    " Set alternate buffer to something intuitive.
    let @# = origbuf
    tnoremap <buffer> <C-s> <C-\><C-n>:call <SID>ctrl_s(0, v:false, v:true)<CR>
    let d.termbuf = bufnr('%')
  endif
  " if a:enter
  "   startinsert  " enter terminal-mode
  " endif
  let d.prevwid = curwinid
endfunc
nnoremap <C-s> :<C-u>call <SID>ctrl_s(v:count, v:false, v:false)<CR>
nnoremap g<C-s> :<C-u>call <SID>ctrl_s(v:count, v:false, v:true)<CR>

set wildcharm=<C-Z>
nnoremap <C-b> :set nomore<bar>ls<bar>set more<cr>:buffer<space>
" _opt-in_ to sloppy-search https://github.com/neovim/neovim/issues/3209#issuecomment-133183790
nnoremap <C-f> :edit **/
nnoremap >t    :tag<space>
" See `man fnmatch`.
nnoremap >g  mS:Ggrep! -E <C-R>=shellescape(fnameescape(expand('<cword>')))<CR> -- ':/' ':/\!*.md' ':/\!*.mpack' ':/\!*.pbf' ':/\!*.pdf' ':/\!*.po' ':(top,exclude,icase)notebooks/' ':/\!data/' ':/\!work/' ':/\!qgis/' ':/\!graphhopper_data/'
      \<Home><C-Right><C-Right><C-Right><left>
nnoremap >v  mS:<c-u>noau vimgrep /\C/j **<left><left><left><left><left>
" search all file buffers (clear qf first).
nnoremap >b  mS:<c-u>cexpr []<bar>exe 'bufdo silent! noau vimgrepadd/\C/j %'<bar>botright copen<s-left><s-left><left><left><left>
" search current buffer and open results in loclist
nnoremap >.   ms:<c-u>lvimgrep // % <bar>lw<s-left><left><left><left><left>
" search-replace
nnoremap gsal mr:%s/
xnoremap gs   mr:s/\%V

" =============================================================================
" autocomplete / omnicomplete / tags
" =============================================================================
set completeopt-=preview
set complete+=kspell
set wildignore+=tags,*/gwt-unitCache/*
" Files with these suffixes get a lower priority when matching a wildcard
set suffixes+=.aux,.log,.dvi,.bbl,.blg,.brf,.cb,.ind,.idx,.ilg,.inx,.out,.toc
      \,.o,.obj,.dll,.class,.pyc,.ipynb,.so,.swp,.zip,.exe,.jar,.gz
" Better `gf`
set suffixesadd=.java,.cs

function! s:fzf_open_file_at_line(e) abort
  "Get the <path>:<line> tuple; fetch.vim plugin will handle the rest.
  execute 'edit' fnameescape(matchstr(a:e, '\v([^:]{-}:\d+)'))
endfunction
function! s:fzf_insert_at_point(s) abort
  execute "put ='".a:s."'"
endfunction
function! s:fzf_search_fulltext() abort
  call fzf#run({'source':'git grep --line-number --color=never -v "^[[:space:]]*$"',
        \ 'sink':function('<sid>fzf_open_file_at_line')})
endfunction

" Search current-working-directory _or_ current-file-directory
nnoremap <silent><expr> <M-/> v:count ? ':<C-U>call <SID>fzf_search_fulltext()<CR>' : ':<C-U>FzFiles<CR>'
" Search MRU files
nnoremap <silent>       <M-\> :FzHistory<cr>
nnoremap <silent><expr> <C-\> v:count ? 'mS:<C-U>FzLines<CR>' : ':<C-U>FzBuffers<CR>'
nmap                    g/    <M-/>
if !empty(findfile('plugin/tmuxcomplete.vim', &rtp))
  inoremap <expr> <C-l> fzf#complete(tmuxcomplete#list('lines', 0))
  inoremap <expr> <M-w> fzf#complete(tmuxcomplete#list('words', 0))
  nnoremap <silent> <M-w> :call fzf#run({'source':tmuxcomplete#list('words', 0),
        \                              'sink':function('<SID>fzf_insert_at_point')})<CR>
endif

nnoremap <silent> gO    :call fzf#vim#buffer_tags('')<cr>
nnoremap <silent> z/    :call fzf#vim#tags('')<cr>


" Slides plugin {{{
func! s:slides_hl() abort
  if hlID('SlidesHide') == 0
    " use folds instead of highlight
  else
    exe 'match SlidesHide /\%>'.(line('w0')-1).'l\n=\{20}=*\n\_.\{-}\zs\n.*\n=\{20}=\_.*/'
  endif
endfunc
func! Slides() abort
  set cmdheight=1
  set nowrapscan
  set nohlsearch

  try
    hi SlidesHide ctermbg=bg ctermfg=bg guibg=bg guifg=bg
  catch /E420/
  endtry
  hi markdownError ctermbg=NONE ctermfg=NONE guifg=NONE guibg=NONE

  nnoremap <silent> <Down> :keeppatterns /^======<CR>zt<C-Y>:call <SID>slides_hl()<CR>
  nnoremap <silent> <Up>   :keeppatterns ?^======<CR>zt<C-Y>:call <SID>slides_hl()<CR>
  setlocal colorcolumn=54,67 textwidth=53
  hi ColorColumn guibg=#555555 guifg=#ffffff
  " hi SlidesSign guibg=white guifg=black ctermbg=black ctermfg=white gui=NONE cterm=NONE
  " sign define limit  text== texthl=SlidesSign
  " sign unplace
endf
func! SlidesEnd() abort
  call clearmatches()
endf
" }}}

set title
set titlestring=%{getpid().':'.getcwd()}
set titleold=?

" special-purpose mappings/commands ===========================================
nnoremap <leader>vft  :e ~/.config/nvim/ftplugin<cr>
nnoremap <leader>vv   :exe 'e' fnameescape(resolve($MYVIMRC))<cr>
nnoremap <silent> <leader>vs :Scriptnames<cr>
inoremap <silent> <leader>log ELOG("");<Left><Left><Left>
xnoremap <leader>{ <esc>'<A {`>o}==`<

command! InsertDate           norm! i<c-r>=strftime('%Y/%m/%d %H:%M:%S')<cr>
command! InsertDateYYYYMMdd   norm! i<c-r>=strftime('%Y%m%d')<cr>
command! InsertCBreak         norm! i#include <signal.h>raise(SIGINT);
command! CdNotes        exe 'e '.finddir("notes", expand('~').'/Desktop/github,'.expand('~').'/dev')<bar>lcd %
command! CdLibUV        exe 'e '.finddir(".deps/build/src/libuv", expand("~")."/neovim/**,".expand("~")."/dev/neovim/**")<bar>lcd %
command! CdNvimDeps     exe 'e '.finddir(".deps", expand("~")."/neovim/**,".expand("~")."/dev/neovim/**")<bar>lcd %
command! CdNvimLuaClient exe 'e '.finddir("nvim", expand("~")."/neovim/.deps/usr/share/lua/**,".expand("~")."/neovim/.deps/usr/share/lua/**")<bar>lcd %
command! CdVim          exe 'e '.finddir(".vim-src", expand("~")."/neovim/**,".expand("~")."/dev/neovim/**")<bar>lcd %
command! ProfileVim     exe 'Start '.v:progpath.' --startuptime "'.expand("~/vimprofile.txt").'" -c "e ~/vimprofile.txt"'
command! NvimTestScreenshot put =\"local Screen = require('test.functional.ui.screen')\nlocal screen = Screen.new()\nscreen:attach()\nscreen:snapshot_util({},true)\"
command! ConvertBlockComment keeppatterns .,/\*\//s/\v^((\s*\/\*)|(\s*\*\/)|(\s*\*))(.*)/\/\/\/\5/
command! Vimsrc tabedit ~/neovim/.vim-src/|exe 'lcd %:h'|edit src/eval.c
command! -nargs=1 Vimref tabedit ~/neovim/.vim-src/|exe 'lcd %:h'|exe 'Gedit '.(-1 == match('<args>', '\v(\d+\.){2}') ? '' : 'v').'<args>'
command! -nargs=1 Vimtag exe 'noswapfile edit '.finddir(".vim-src", expand("~")."/neovim/**,".expand("~")."/dev/neovim/**").'/src/version.c'
                              \.'|tag <args>'
command! -nargs=1 Ghpr GV refs/pull/upstream/<args>

function! Cxn_py() abort
  vsplit
  terminal
  call chansend(&channel, "python3\nimport neovim\n")
  call chansend(&channel, "n = neovim.attach('socket', path='".g:cxn."')\n")
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
  let nvim_path = executable('build/bin/nvim') ? 'build/bin/nvim' : 'nvim'
  call chansend(&channel, "NVIM_LISTEN_ADDRESS= ".nvim_path." -u NORC\n")
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
nnoremap gow :Start "C:\Program Files (x86)\Google\Chrome\Application\chrome.exe" --no-proxy-server "%:p"<cr>

silent! source ~/.vimrc.local

