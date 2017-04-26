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
let g:plug_window = 'enew'

call plug#begin('~/.local/share/nvim/bundle')

Plug 'justinmk/molokai'
Plug 'mptre/vim-printf'
nnoremap crp :Printf<CR>
Plug 'sbdchd/neoformat'
Plug 'majutsushi/tagbar'
Plug 'https://gitlab.com/HiPhish/info.vim.git'
Plug 'machakann/vim-highlightedyank'

if v:version > 703 && !has('win32') && !has('win32unix')
Plug 'ludovicchabant/vim-gutentags'
let g:gutentags_ctags_exclude = ['.vim-src', 'build']
endif

Plug 'tommcdo/vim-exchange'
Plug 'kopischke/vim-fetch'

Plug 'https://github.com/justinmk/vim-ipmotion.git'
Plug 'https://github.com/justinmk/vim-dirvish.git'
Plug 'https://github.com/justinmk/vim-gtfo.git'

Plug 'https://github.com/justinmk/vim-sneak.git'
let g:sneak#label = 1
let g:sneak#use_ic_scs = 1
let g:sneak#absolute_dir = 1
nmap f <Plug>Sneak_f
nmap F <Plug>Sneak_F
xmap f <Plug>Sneak_f
xmap F <Plug>Sneak_F
omap f <Plug>Sneak_f
omap F <Plug>Sneak_F
nmap t <Plug>Sneak_t
nmap T <Plug>Sneak_T
xmap t <Plug>Sneak_t
xmap T <Plug>Sneak_T
omap t <Plug>Sneak_t
omap T <Plug>Sneak_T
map <M-;> <Plug>Sneak_,
let g:sneak#target_labels = ";sftunq/SFGHLTUNRMQZ?0"

" Plug 'https://github.com/justinmk/vim-matchparenalways.git'

if executable("tmux")
Plug 'tpope/vim-tbone'
Plug 'wellle/tmux-complete.vim'
endif

Plug 'tpope/vim-characterize'
" Plug 'tpope/vim-sleuth'
Plug 'tpope/vim-scriptease'

Plug 'zhaocai/DirDiff.vim', { 'on': ['DirDiff'] }
Plug 'tpope/vim-fugitive'
Plug 'junegunn/gv.vim'
Plug 'tpope/vim-rhubarb'
Plug 'shumphrey/fugitive-gitlab.vim'
let g:fugitive_gitlab_domains = ['http://cwsrc2']

Plug 'tpope/vim-surround'
let g:surround_indent = 1
let g:surround_no_insert_mappings = 1

Plug 'tpope/vim-dispatch'
nnoremap !m :<c-u>Make<cr>
" ways to run external commands in vim: https://gist.github.com/sjl/b9e3d9f821e57c9f96b3
nnoremap !t :<c-u>Trun TEST_FILE=<c-r>% make functionaltest<cr>
nnoremap !T :<c-u>Make unittest<cr>

nnoremap <silent> yr  :<c-u>set opfunc=<sid>tmux_run_operator<cr>g@
xnoremap <silent> R   :<c-u>call <sid>tmux_run_operator(visualmode(), 1)<CR>
nnoremap <silent> yrr V:<c-u>call <sid>tmux_run_operator(visualmode(), 1)<CR>
func! s:tmux_run_operator(type, ...) abort
  let sel_save = &selection
  let &selection = "inclusive"
  let isvisual = a:0

  let lines = isvisual ? getline("'<", "'>") : getline("'[", "']")
  if a:type !=# 'line' && a:type !=# 'V'
    let startcol  = isvisual ? col("'<") : col("'[")
    let endcol    = isvisual ? col("'>")-2 : col("']")
    let lines[0]  = lines[0][startcol-1 : ]
    let lines[-1] = lines[-1][ : endcol-1]
  endif

  call s:tmux_run(join(lines))

  let &selection = sel_save
endf
"Sends `cmd` to the bottom-right pane and optionally runs it.
func! s:tmux_run(creatnew, run, cmd) abort
  "Create a new pane if demanded or if we are _in_ the target pane.
  if a:creatnew || tbone#pane_id(".") == tbone#pane_id("bottom-right")
    Tmux split-window -d -p 33
  endif
  call tbone#send_keys("bottom-right",
        \"\<c-e>\<c-u>".a:cmd.(a:run ? "\<cr>" : ""))
endf
command! -nargs=? -bang Trun call s:tmux_run(<bang>0, 1, <q-args>)

Plug 'tpope/vim-repeat'
Plug 'tpope/vim-eunuch'
Plug 'tpope/vim-rsi'
Plug 'tpope/vim-unimpaired'

Plug 'tpope/vim-endwise'
inoremap (<CR> (<CR>)<Esc>O
inoremap {<CR> {<CR>}<Esc>O
inoremap {; {<CR>};<Esc>O
inoremap {, {<CR>},<Esc>O
inoremap [<CR> [<CR>]<Esc>O
inoremap ([[ ([[<CR>]])<Esc>O
inoremap ([=[ ([=[<CR>]])<Esc>O
inoremap [; [<CR>];<Esc>O
inoremap [, [<CR>],<Esc>O

Plug 'tpope/vim-obsession'

let g:markdown_syntax_conceal = 0

Plug 'AndrewRadev/linediff.vim'
let g:linediff_buffer_type = 'scratch'
Plug 'mbbill/undotree', { 'on': ['UndotreeToggle'] }
Plug 'kana/vim-niceblock'

Plug 'tpope/vim-commentary'

Plug 'mhinz/vim-signify'
let g:signify_vcs_list = [ 'git' ]
let g:signify_realtime = 1
let g:signify_cursorhold_normal = 0
let g:signify_cursorhold_insert = 0

if s:plugins_extra
  Plug 'guns/vim-sexp'
  Plug 'guns/vim-clojure-highlight'
  let g:clojure_fold = 1
  let g:sexp_filetypes = ''

  Plug 'tpope/vim-salve'
  let g:salve_auto_start_repl = 1
  Plug 'tpope/vim-fireplace'

  Plug 'jalvesaq/vimcmdline'

  Plug 'PProvost/vim-ps1'
  Plug 'chrisbra/Colorizer', { 'on': ['ColorHighlight'] }

  Plug 'inside/vim-search-pulse'
  let g:vim_search_pulse_mode = 'pattern'
  let g:vim_search_pulse_disable_auto_mappings = 1
  let g:vim_search_pulse_color_list = ["red", "white"]
  let g:vim_search_pulse_duration = 200

  Plug 'ryanss/vim-hackernews', { 'on': ['HackerNews'] }
  Plug 'junegunn/vim-github-dashboard', { 'on': ['GHDashboard'] }
  Plug 'mattn/webapi-vim'
  Plug 'mattn/gist-vim', { 'on': ['Gist'] }
  Plug 'gcavallanti/vim-noscrollbar'

  Plug 'tommcdo/vim-lion'

  Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': 'yes n \| ./install' }
  Plug 'junegunn/fzf.vim'
  let g:fzf_command_prefix = 'Fz'

  Plug 'tpope/vim-projectionist'
  " look at derekwyatt/vim-fswitch for more C combos.
  let g:projectionist_heuristics = {
        \  '*.sln': {
        \    '*.cs': {'alternate': ['{}.designer.cs']},
        \    '*.designer.cs': {'alternate': ['{}.cs']},
        \  },
        \  '/*.c|src/*.c': {
        \    '*.c': {'alternate': ['../include/{}.h', '{}.h']},
        \    '*.h': {'alternate': '{}.c'},
        \  },
        \  'Makefile': {
        \    '*Makefile': {'alternate': '{dirname}CMakeLists.txt'},
        \    '*CMakeLists.txt': {'alternate': '{dirname}Makefile'},
        \  },
        \}

  " Plug 'Valloric/MatchTagAlways', { 'for': 'xml' }
endif

call plug#end()

" Eager-load these plugins so we can override their settings. {{{
runtime! plugin/rsi.vim
runtime! plugin/commentary.vim
" }}}
endif "}}}

if has("nvim")
  tnoremap <esc> <c-\><c-n>
  augroup nvimrc_aucmd
    autocmd!
    " https://github.com/neovim/neovim/issues/3463#issuecomment-148757691
    autocmd CursorHold,FocusGained,FocusLost * rshada|wshada
    autocmd FocusGained * call <SID>halo()
  augroup END
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


command! LoadSession if filereadable(expand("~/.vim/session.vim", 1)) | source ~/.vim/session.vim
      \ | else | Obsession ~/.vim/session.vim | endif
set sessionoptions-=blank

"==============================================================================
" general settings / options
"==============================================================================
if has('nvim-0.2')
  set cpoptions-=_
endif
set updatetime=2000
set expandtab shiftwidth=0 softtabstop=2 tabstop=2

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

let mapleader = "z,"
let g:mapleader = "z,"

try | lang en_US | catch | endtry

set undofile
set list
set fileformats=unix,dos

set cursorline

set path+=/usr/lib/gcc/**/include
" neovim
set path+=.deps/build/src/*/include,src

let g:sh_noisk = 1
set hidden      " Allow buffer switching even if unsaved 
set lazyredraw  " no redraws in macros
set cmdheight=2
set ignorecase " case-insensitive searching
set smartcase  " but become case-sensitive if you type uppercase characters

set timeoutlen=3000
set noshowmode " Hide the mode text (e.g. -- INSERT --)
set foldlevelstart=99 "open all folds by default
set splitright
set inccommand=split
if has('patch-7.4.314') | set shortmess+=c | endif

nnoremap <silent> coz :<c-u>if &foldenable\|set nofoldenable\|
      \ else\|setl foldmethod=indent foldnestmax=2 foldlevel=0 foldenable\|set foldmethod=manual\|endif<cr>

nnoremap cot :setlocal textwidth<C-R>=(&textwidth == 80) ? '<' : '=80'<CR><CR>

set nojoinspaces

set nostartofline
" restore cursor position upon returning to a buffer, _without_ permanently 
" setting 'nostartofline' (which affects many other behaviors).
if &startofline
  augroup vimrc_stayput
    autocmd!
      " 1. disable 'startofline' temporarily while switching buffers, 
      " 2. then re-enable it on CursorMoved, 
      " 3. then clear the CursorMoved autocmd to avoid spam
      autocmd BufLeave * set nostartofline |
            \ autocmd vimrc_stayput CursorMoved,CursorMovedI * set startofline |
            \ autocmd! vimrc_stayput CursorMoved,CursorMovedI
  augroup END
endif

"colorscheme {{{
  if (!empty(&t_Co) && &t_Co <= 88) || findfile('colors/molokai.vim', &rtp) ==# ''
    silent! colorscheme ron
  else
    let s:color_override_dark = '
          \ if &background == "dark"
          \ | hi StatusLine    guifg=#000000 guibg=#ffffff gui=NONE  ctermfg=16 ctermbg=15     cterm=NONE
          \ | hi WildMenu      gui=NONE cterm=NONE guifg=#f8f6f2 guibg=#0a9dff ctermfg=255 ctermbg=39
          \ | hi Visual        gui=NONE cterm=NONE guifg=black   guibg=white   ctermfg=0   ctermbg=255
          \ | endif
          \'

    if has('vim_starting') "only on startup
      exe 'autocmd ColorScheme * '.s:color_override_dark
      " expects &runtimepath/colors/{name}.vim.
      silent! colorscheme molokai
    endif
  endif

func! s:set_CursorLine() abort
  hi clear CursorLine
  if &diff
    hi CursorLine gui=underline cterm=underline
  else
    hi CursorLine guibg=#293739 ctermbg=236
  endif
endf
"}}}

"==============================================================================
" text, tab and indent 

set formatoptions+=rno1l
" don't syntax-highlight long lines
set synmaxcol=1000

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
nnoremap g> :set nomore<bar>40messages<bar>set more<CR>

xnoremap / <Esc>/\%V

" word-wise i_CTRL-Y
inoremap <expr> <c-y> pumvisible() ? "\<c-y>" : matchstr(getline(line('.')-1), '\%' . virtcol('.') . 'v\%(\k\+\\|.\)')

" current-file directory
noremap! <silent> <c-r><c-\> <c-r>=expand('%:p:h', 1)<cr>

noremap! <c-r>? <c-r>=substitute(getreg('/'), '[<>\\]', '', 'g')<cr>

" mark position before search
nnoremap / ms/

func! s:maybe_zz(cmd) abort
  let topline = line('w0')
  try
    exe a:cmd
  catch /E486:/
    echohl ErrorMsg | echom matchstr(v:exception, 'E486:.*') | echohl None
  endtry
  if topline != line('w0')
    normal! zz
    call feedkeys("\<Plug>Pulse", 'm')
  endif
endf
nnoremap <silent> n :<C-U>call <SID>maybe_zz('norm! '.'Nn'[v:searchforward])<CR>
nnoremap <silent> N :<C-U>call <SID>maybe_zz('norm! '.'nN'[v:searchforward])<CR>

" manage windows
"       [count]<c-w>s and [count]<c-w>v create a [count]-sized split
"       [count]<c-w>| and [count]<c-w>_ resize the current window
" user recommendation:
"       <c-w>eip
" available:
"       <c-w><space>{motion}

" vim+tmux window navigation (credit: mhinz)
function! s:vim_tmux_nav(direction) abort
  if empty($TMUX)
    execute 'wincmd' a:direction
  else
    let oldwin = winnr()
    execute 'wincmd' a:direction
    if winnr() == oldwin
      let sock = split($TMUX, ',')[0]
      let direction = tr(a:direction, 'hjkl', 'LDUR')
      silent execute printf('!tmux -S %s select-pane -%s', sock, direction)
    endif
  endif
endfunction
nnoremap <silent><M-h> :<c-u>call <SID>vim_tmux_nav('h')<cr>
nnoremap <silent><M-j> :<c-u>call <SID>vim_tmux_nav('j')<cr>
nnoremap <silent><M-k> :<c-u>call <SID>vim_tmux_nav('k')<cr>
nnoremap <silent><M-l> :<c-u>call <SID>vim_tmux_nav('l')<cr>
imap     <silent><M-h> <C-\><C-N><M-h>
imap     <silent><M-j> <C-\><C-N><M-j>
imap     <silent><M-k> <C-\><C-N><M-k>
imap     <silent><M-l> <C-\><C-N><M-l>
if has('nvim')
  tnoremap <silent><M-h> <C-\><C-n>:call <SID>vim_tmux_nav('h')<cr>
  tnoremap <silent><M-j> <C-\><C-n>:call <SID>vim_tmux_nav('j')<cr>
  tnoremap <silent><M-k> <C-\><C-n>:call <SID>vim_tmux_nav('k')<cr>
  tnoremap <silent><M-l> <C-\><C-n>:call <SID>vim_tmux_nav('l')<cr>
  tnoremap <silent><C-^> <C-\><C-n><C-^>
endif

nnoremap Zh     :leftabove vsplit<CR>
nnoremap Zj     :belowright split<CR>
nnoremap Zk     :aboveleft split<CR>
nnoremap Zl     :rightbelow vsplit<CR>
nmap     ZH     Zh
nmap     ZJ     Zj
nmap     ZK     Zk
nmap     ZL     Zl

nnoremap <C-w>gt :tab sp<CR>
nnoremap <M-H> :aboveleft vsplit<CR>
nnoremap <M-J> :belowright split<CR>
nnoremap <M-K> <C-W>s
nnoremap <M-L> <C-W>v
nnoremap <M-n> :enew<CR>
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
nnoremap <expr> ]gt ':<C-u>tabmove '.(v:count ? (v:count - 1) : '+1').'<CR>'
nnoremap <expr> [gt ':<C-u>tabmove '.(v:count ? (v:count - 1) : '-1').'<CR>'

" manage buffers
nnoremap <expr><silent> ZB  ':<c-u>call <SID>buf_kill('. !v:count .')<cr>'

" quickfix window (in quickfix: toggles between qf & loc list)
nnoremap <silent><expr>   Q '@_:botright '.(&bt!=#'quickfix'<bar><bar>!empty(getloclist(0))?'lclose<bar>copen':'cclose<bar>lopen').'<CR>'

nnoremap <expr> zt (v:count > 0 ? '@_zt'.v:count.'<c-y>' : 'zt')
nnoremap <expr> zb (v:count > 0 ? '@_zb'.v:count.'<c-e>' : 'zb')

" set working directory to the current buffer's directory
nnoremap cd :lcd %:p:h<bar>pwd<cr>
nnoremap cu :lcd ..<bar>pwd<cr>

if findfile('plugin/fugitive.vim', &rtp) !=# ''
  " show git branch with ctrl-g info
  func! s:ctrl_g(cnt) abort
    redir => msg | silent exe "norm! 1\<c-g>" | redir END
    echo fugitive#head(7) msg[2:] (a:cnt?strftime('%Y-%m-%d %H:%M',getftime(expand('%:p'))):'')
  endf
  nnoremap <C-g> :<c-u>call <sid>ctrl_g(v:count)<cr>
endif

" show the working directory and session
nnoremap <M-g> :<C-u>echo fnamemodify(getcwd(), ":~")
      \ (strlen(v:this_session) ? fnamemodify(v:this_session, ":~") : "[No session]")<cr>

nnoremap <silent> <C-n> :<C-U>call <SID>maybe_zz('norm ]c]n')<CR>
nnoremap <silent> <C-p> :<C-U>call <SID>maybe_zz('norm [c[n')<CR>

" version control
xnoremap <expr> D (mode() ==# "V" ? ':Linediff<cr>' : 'D')
nnoremap <silent> Ub             :Gblame<cr>
nnoremap <silent> Ud :<C-U>if &diff<bar>diffupdate<bar>else<bar>exe 'Gdiff'.(v:count ? ' HEAD'.repeat('^', v:count) : '')<bar>endif<cr>
nnoremap <silent> Ue             :exe 'Gedit\|'.line('.')<cr>zz
nnoremap          Uf             :Gcommit --fixup=
nnoremap <silent> Ugf            :Gedit <C-R><C-W><cr>
nnoremap <silent> Ul :GV! -100<cr>
nnoremap <silent> Ur             :Gread<cr>
nnoremap <silent> Us             :Gstatus<cr>
nnoremap <silent> Uw :if !exists(":Gwrite")<bar>call fugitive#detect(expand('%:p'))<bar>endif<bar>Gwrite<bar>SignifyRefresh<cr>

nmap UB Ub
nmap UD Ud
nmap UE Ue
nmap UF Uf
nmap UL Ul
nmap UR Ur
nmap US Us
nmap UW Uw

"linewise partial staging in visual-mode.
xnoremap <c-p> :diffput<cr>
xnoremap <c-o> :diffget<cr>
nnoremap <expr> dp &diff ? 'dp' : ':pclose<cr>'

" Executes git cmd in the context of b:git_dir.
function! s:git_do(cmd) abort
  " git 1.8.5: -C is a (more reliable) alternative to --git-dir/--work-tree.
  return systemlist('git -C '.shellescape(fnamemodify(b:git_dir, ':p:h:h'))
        \ . ' ' . a:cmd)
endfunction

" Gets the git commit hash associated with the given file line.
function! s:git_sha(filepath, line) abort
  if "" ==# s:trimws_ml(a:filepath)
    echoerr "invalid (empty) filepath"
  elseif !exists("b:git_dir")
    echoerr "Missing b:git_dir"
  elseif a:line <= 0
    echoerr "Invalid a:line: ".a:line
  endif

  let cmd_out = join(s:git_do('blame --root -l -L'.a:line.','.a:line.' -- '.a:filepath))
  if cmd_out =~# '\v^0{40,}'
    return ""
  endif

  return matchstr(cmd_out, '\w\+\ze\W\?', 0, 1)
endfunction

function! s:git_blame_line(filepath, line) abort
  let commit_id = s:git_sha(a:filepath, a:line)
  if commit_id ==# ""
    echo 'not committed'
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

nmap UP Up
nmap Up :<c-u>call <sid>git_blame_line('<c-r><c-g>', line('.'))<cr>
"                                        ^ Get repo-relative path via fugitive

" :help :DiffOrig
command! DiffOrig leftabove vnew | set bt=nofile | r ++edit # | 0d_ | diffthis | wincmd p | diffthis

nnoremap co<space> :set <C-R>=(&diffopt =~# 'iwhite') ? 'diffopt-=iwhite' : 'diffopt+=iwhite'<CR><CR>

" execute/evaluate
nnoremap yxal             :keeppatterns           g/^/exe getline('.')<CR>
nnoremap <silent> yxx     :keeppatterns          .g/^/exe getline('.')<CR>
xnoremap <silent> <enter> :<C-U>keeppatterns '<,'>g/^/exe getline('.')<CR>

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
    "No alternate buffer found; create a new, blank buffer.
    "Note: :bd will still close any window displaying the buffer. To prevent
    "      that the windows would each need to be switched to another buffer.
    enew
  endif

  " remove the buffer filename (if any) from the args list, else it might come back in the next session.
  if !empty(origbufname)
    silent! exe 'argdelete '.origbufname
  endif
  " obliterate the buffer and all of its related state (marks, local options, ...), 
  if bufexists(origbuf) "some other mechanism may have deleted the buffer already.
    exe 'bwipeout! '.origbuf
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

" map m-] to be the inverse of c-]
nnoremap <m-]> <c-t>

" select last inserted text
nnoremap gV `[v`]

" replay macro for each line of a visual selection
nnoremap , @q
xnoremap , :normal @q<CR>
" repeat last command for each line of a visual selection
xnoremap . :normal .<CR>
" XXX: fix this
" repeat the last edit on the next [count] matches.
nnoremap <silent> gn :normal n.<CR>:<C-U>call repeat#set("n.")<CR>
nnoremap <M-q> qq
nnoremap cqq :let @q='<C-R><C-R>q'

nnoremap c<Space> :nnoremap <lt>Space> 
nnoremap c<Space> :nnoremap <lt>Space> 

" disable F1 help key
noremap! <F1> <nop>
noremap <F1> <nop>

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
  " If the layout did not change, it's a toggle (un-zoom).
  if restore_cmd ==# winrestcmd()
    exe t:zoom_restore
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

"read the current line into command line
cnoremap <c-r><c-l> <c-r>=getline('.')<cr>

xmap * <esc>/\V<c-r>=<sid>get_visual_selection_searchpattern()<cr><cr><Plug>Pulse
nmap <silent> *  :<c-u>let @/='\V\<'.escape(expand('<cword>'), '/\').'\>'<bar>set hlsearch<cr>
nmap <silent> g* :<c-u>let @/='\V' . escape(expand('<cword>'), '/\')     <bar>set hlsearch<cr>

hi MarkLine guibg=darkred guifg=gray ctermbg=9 ctermfg=15
func! s:markline() abort
  let b:vimrc_markedlines = get(b:, "vimrc_markedlines", {})
  "TODO: This will get stale if the line moves.
  "      :sign is a solution, but need to create a way to get un-used sign {id}s.
  let b:vimrc_markedlines[line('.')] = matchaddpos("MarkLine", [line('.')])
endf
nnoremap <silent> m.  :call <sid>markline()<cr>
nnoremap <silent> m<space> :call matchdelete(b:vimrc_markedlines[line('.')])<cr>

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
nnoremap <silent> <Esc> :<C-U>call <SID>halo()<CR>
"nnoremap <silent> <C-c> :<C-U>call <SID>halo()<CR><C-c>

" }}} mappings

augroup vimrc_groovy
  autocmd!
  autocmd FileType groovy setlocal commentstring=//%s
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
    silent! keepmarks keepjumps keeppatterns g/\v(^$)|^([^#].*$)/y C
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

  autocmd FileType text setlocal textwidth=80

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
    nnoremap <buffer> <silent> cF :<C-U>Gcommit --fixup=HEAD<CR>
    nmap <buffer> <M-n> <c-n>dvgg<c-n>:call feedkeys("\<lt>c-w>P")<cr>
    nmap <buffer> <M-p> <c-p>dvgg<c-n>:call feedkeys("\<lt>c-w>P")<cr>
    nmap <buffer><nowait> <M-u> U<Esc>
  endfunction
  autocmd FileType gitcommit call <SID>setup_gitstatus()
  autocmd FileType dirvish call fugitive#detect(@%)
  autocmd BufWinEnter * if empty(expand('<afile>'))|call fugitive#detect(getcwd())|endif

  autocmd FileType css set omnifunc=csscomplete#CompleteCSS

  " syntaxcomplete provides basic completion for filetypes that lack a custom one.
  "   :h ft-syntax-omni
  if has("autocmd") && exists("+omnifunc")
    autocmd Filetype * if &omnifunc == "" | setlocal omnifunc=syntaxcomplete#Complete | endif
  endif

  if exists("*mkdir") "auto-create directories for new files
    au BufWritePre,FileWritePre * silent! call mkdir(expand('<afile>:p:h'), 'p')
  endif

  "when Vim starts in diff-mode (vim -d, git mergetool):
  "  - do/dp should not auto-fold
  "  - tweak CursorLine to be less broken
  autocmd VimEnter * if &diff | exe 'windo set foldmethod=manual' | call <sid>set_CursorLine() | endif
  autocmd BufWinEnter,WinEnter * call <sid>set_CursorLine()

  " autocmd VimEnter * if !empty($NVIM_LISTEN_ADDRESS) && $NVIM_LISTEN_ADDRESS !=# v:servername
  "       \ |let g:r=jobstart(['nc', '-U', $NVIM_LISTEN_ADDRESS],{'rpc':v:true})
  "       \ |let g:f=fnameescape(expand('%:p'))
  "       \ |noau bwipe
  "       \ |call rpcrequest(g:r, "nvim_command", "tabedit ".g:f)|qa|endif

  autocmd BufRead,BufNewFile *.{ascx,aspx} setlocal tabstop=4 copyindent

  if exists('##TextYankPost')
    autocmd TextYankPost * let g:yankring=get(g:,'yankring',[])
      \|call add(g:yankring, deepcopy(v:event))|if len(g:yankring)>50|call remove(g:yankring, 0, 1)|endif
  endif
augroup END

nnoremap <C-b> :set nomore<bar>ls<bar>set more<cr>:buffer<space>
" _opt-in_ to sloppy-search https://github.com/neovim/neovim/issues/3209#issuecomment-133183790
nnoremap <C-f> :edit **/
nnoremap >t    :tag<space>
nnoremap >g  mS:Ggrep! -E <C-R>=shellescape(fnameescape(expand('<cword>')))<CR><Left>
nnoremap >v  mS:<c-u>noau vimgrep /\C/j **<left><left><left><left><left>
" search all file buffers (clear qf first).
nnoremap >b  mS:<c-u>cexpr []<bar>exe 'bufdo silent! noau vimgrepadd/\C/j %'<bar>botright copen<s-left><s-left><left><left><left>
" search current buffer and open results in loclist
nnoremap >%   ms:<c-u>lvimgrep // % <bar>lw<s-left><left><left><left><left>
" search-replace
nnoremap gsal ms:<c-u>%s/
xnoremap gs   ms:s/\%V

" =============================================================================
" autocomplete / omnicomplete / tags
" =============================================================================
set completeopt-=preview
set wildignore+=tags,*/gwt-unitCache/*
" Files with these suffixes get a lower priority when matching a wildcard
set suffixes+=.aux,.log,.dvi,.bbl,.blg,.brf,.cb,.ind,.idx,.ilg,.inx,.out,.toc
      \,.o,.obj,.dll,.class,.pyc,.so,.swp,.zip,.exe,.jar,.gz
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

nnoremap <silent> <m-o> :call fzf#vim#buffer_tags('', g:fzf#vim#default_layout)<cr>
nnoremap <silent> z/    :call fzf#vim#tags('', g:fzf#vim#default_layout)<cr>


" statusline  ░▒▓█ ============================================================
if s:plugins_extra
  hi NoScrollBar  guibg=black guifg=darkgrey ctermbg=0 ctermfg=darkgrey gui=NONE cterm=NONE
  set statusline=%{winnr('$')>2?winnr():''}\ %<%f\ %h%m%*%r\ %=%#NoScrollBar2#%P%*%#NoScrollBar#%{noscrollbar#statusline(20,'\ ','▒',['▐'],['▌'])}%*\ %{strlen(&fenc)?&fenc:&enc}\ %{(&ff==#'unix')?'':(&ff==#'dos')?'CRLF':&ff}\ %y\ %-10.(%l,%c%V%)
else
  set statusline=
endif

" Slides plugin {{{
func! SlidesStatusline() abort
  let st = get(b:, 'slides',
        \ {'topline':0,'nr_tot':0,'nr_cur':0,'title':''})
  if winsaveview().topline == st.topline
    return (st.nr_cur) . '/' . (st.nr_tot)
  endif
  let st.topline = winsaveview().topline
  let st.nr_tot  = len(filter(getline(1, '$'), "v:val =~# '^=============='"))
  let st.nr_cur  = len(filter(getline(1, '.'), "v:val =~# '^=============='"))
  let b:slides   = st
  return (st.nr_cur).'/'.(st.nr_tot)
endf
func! StartSlides() abort
  silent! autocmd! slides_plugin_edit
  silent! autocmd! BufReadPost

  let g:oldstatusline = get(g:, 'oldstatusline', &statusline)
  let g:oldguifont = get(g:, 'oldguifont', &guifont)
  set statusline=%{SlidesStatusline()}
  set guifont=Consolas:h32 cmdheight=1 nocursorline background=light

  hi markdownError ctermbg=NONE ctermfg=NONE guifg=NONE guibg=NONE
  hi Normal ctermbg=white ctermfg=black guifg=black guibg=white
  hi StatusLine guibg=#000000 guifg=#ffffff

  augroup slides_plugin_show
    autocmd!
    autocmd BufEnter,WinEnter <buffer> setlocal nocursorline
          \ nocursorcolumn colorcolumn=
    autocmd BufEnter,WinEnter * silent! call sy#stop(bufnr('%'))
  augroup END

  nnoremap <Down> /^======<CR>:nohls<CR>zt<C-Y>
  nnoremap <Up>   ?^======<CR>:nohls<CR>zt<C-Y>
  sign unplace *

  "faster ]f [f
  set noshelltemp
endf
func! EditSlides() abort
  silent! autocmd! slides_plugin_show

  silent! let &statusline = g:oldstatusline
  silent! let &guifont = g:oldguifont
  hi SlidesSign guibg=white guifg=black ctermbg=black ctermfg=white gui=NONE cterm=NONE
  sign define limit  text== texthl=SlidesSign

  augroup slides_plugin_edit
    autocmd!
    autocmd BufEnter,WinEnter * set colorcolumn=54,67 textwidth=53
    autocmd FileType dirvish map <buffer> <down> jpp<c-w>p|map <buffer> <up> kpp<c-w>p
  augroup END

  hi ColorColumn guibg=#555555 guifg=#ffffff
  set cmdheight=2

  "faster ]f [f
  set noshelltemp
endf
" }}}

set title
set titlestring=%{getpid().':'.getcwd()}
set titleold=?

" special-purpose mappings/commands ===========================================
nnoremap <leader>vft  :e ~/.config/nvim/ftplugin<cr>
nnoremap <leader>vv   :exe 'e' fnameescape(resolve($MYVIMRC))<cr>
nnoremap <silent> <leader>vs :Scriptnames<cr>
command! InsertDate           norm! i<c-r>=strftime('%Y/%m/%d %H:%M:%S')<cr>
command! InsertDateYYYYMMdd   norm! i<c-r>=strftime('%Y%m%d')<cr>
command! CdNotes        exe 'e '.finddir("notes", expand('~').'/Desktop/github,'.expand('~').'/dev')<bar>lcd %
command! CdLibUV        exe 'e '.finddir(".deps/build/src/libuv", expand("~")."/neovim/**,".expand("~")."/dev/neovim/**")<bar>lcd %
command! CdNvimDeps     exe 'e '.finddir(".deps", expand("~")."/neovim/**,".expand("~")."/dev/neovim/**")<bar>lcd %
command! CdNvimLuaClient exe 'e '.finddir("nvim", expand("~")."/neovim/.deps/usr/share/lua/**,".expand("~")."/neovim/.deps/usr/share/lua/**")<bar>lcd %
command! CdVim          exe 'e '.finddir(".vim-src", expand("~")."/neovim/**,".expand("~")."/dev/neovim/**")<bar>lcd %
command! ProfileVim     exe 'Start '.v:progpath.' --startuptime "'.expand("~/vimprofile.txt").'" -c "e ~/vimprofile.txt"'
command! NvimGDB      call s:tmux_run(1, 1, 'sudo gdb -q -tui $(which '.v:progpath.') '.getpid())
command! NvimTestScreenshot put =\"local Screen = require('test.functional.ui.screen')\nlocal screen = Screen.new()\nscreen:attach()\nscreen:snapshot_util({},true)\"
command! ConvertBlockComment .,/\*\//s/\v^((\s*\/\*)|(\s*\*\/)|(\s*\*))(.*)/\/\/\/\5/

function! Cxn_py() abort
  vsplit
  terminal
  call jobsend(b:terminal_job_id, "python3\nimport neovim\n")
  call jobsend(b:terminal_job_id, "n = neovim.attach('socket', path='".g:cxn."')\n")
endfunction
function! Cxn() abort
  silent! unlet g:cxn
  tabnew
  terminal
  call jobsend(b:terminal_job_id, "NVIM_LISTEN_ADDRESS= ./build/bin/nvim -u NORC\n")
  call jobsend(b:terminal_job_id, ":let j=jobstart('nc -U ".v:servername."',{'rpc':v:true})\n")
  call jobsend(b:terminal_job_id, ":call rpcrequest(j, 'nvim_set_var', 'cxn', v:servername)\n")
  call jobsend(b:terminal_job_id, ":call rpcrequest(j, 'nvim_command', 'call Cxn_py()')\n")
endfunction
command! NvimCxn call Cxn()

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

xnoremap <leader>{ <esc>'<A {`>o}==`<

silent! source ~/.vimrc.local

