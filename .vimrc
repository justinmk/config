" windows builds: http://tuxproject.de/projects/vim/
"                 http://files.kaoriya.net/vim/
"                 64-bit: http://solar-blogg.blogspot.ca/p/vim-build.html
" MacVim with homebrew:
"   brew install macvim --with-cscope --with-lua --with-luajit --HEAD --override-system-vim
"   brew linkapps --system
"
" If this .vimrc is not in $HOME, add these lines to $HOME/.vimrc :
"    set runtimepath+=/path/to/.vim
"    source /path/to/.vimrc
"==============================================================================

let s:starting = has('vim_starting')
if s:starting
  " ensure that we always start with vim defaults (as opposed to those set by the current system)
  set all&
  " caution: this resets many settings, eg 'history'
  set nocompatible
endif

" removing this breaks alt/meta mappings (on win32 gvim at least).
set encoding=utf-8

if exists('&guioptions')
    "no toolbar, no menu bar, no left scroll bar, no gui tabs
    set guioptions-=T guioptions-=m guioptions-=L guioptions-=l guioptions-=e
    "don't source &runtimepath/menu.vim. (must be done before 'filetype on' / 'syntax on')
    set guioptions-=M
    "use console dialogs instead of popup dialogs for simple choices.
    set guioptions+=rc
    " cursor behavior:
    "   - no blinking in normal/visual mode
    "   - manic blinking in insert-mode
    set guicursor+=n-v-c:blinkon0,sm:hor30-Cursor,i-ci:ver25-Cursor/lCursor-blinkwait30-blinkoff100-blinkon100
endif

let mapleader = ","
let g:mapleader = ","

let s:is_cygwin = has('win32unix') || has('win64unix')
let s:is_windows = has('win32') || has('win64')
let s:is_mac = has('gui_macvim') || has('mac')
let s:is_unix = has('unix')
let s:is_msysgit = (has('win32') || has('win64')) && $TERM ==? 'cygwin'
let s:is_tmux = !empty($TMUX)
let s:is_ssh = !empty($SSH_TTY)
let s:lua_patch885 = has('lua') && (v:version > 703 || (v:version == 703 && has('patch885')))
let s:has_eclim = isdirectory(expand("~/.vim/eclim"))
let s:plugins=isdirectory(expand("~/.vim/bundle/vundle"))

if s:starting && s:is_windows && !s:is_cygwin && !s:is_msysgit
  set runtimepath+=~/.vim/
endif

" 'is GUI' means vim is _not_ running within the terminal.
" sample values:
"   &term  = win32 //vimrc running in msysgit terminal
"   $TERM  = xterm-color , cygwin
"   &term  = builtin_gui //*after* vimrc but *before* gvimrc
"   &shell = C:\Windows\system32\cmd.exe , /bin/bash
let s:is_gui = has('gui_running') || strlen(&term) == 0 || &term ==? 'builtin_gui'

"==============================================================================
" vundle   https://github.com/gmarik/vundle/

"boostrap vundle on new systems
fun! InstallVundle()
    echo "Installing Vundle..."
    silent call mkdir(expand("~/.vim/bundle"), 'p')
    silent !git clone https://github.com/gmarik/vundle.git ~/.vim/bundle/vundle
endfun

if s:plugins "{{{

filetype off " required!

if s:starting
  set runtimepath+=~/.vim/bundle/vundle/
endif

call vundle#rc() 

" let Vundle manage Vundle (required!)
Bundle 'gmarik/vundle'

Bundle 'tomasr/molokai'
Bundle 'junegunn/seoul256.vim'
let g:seoul256_background=234
if !s:is_windows
Bundle 'benmills/vimux'
Bundle 'tpope/vim-tbone'
endif
Bundle 'sjl/clam.vim'
Bundle 'dbext.vim'
" dbext profile example:
"   let g:dbext_default_profile = 'default'
"   let g:dbext_default_profile_default = 'type=SQLSRV:integratedlogin=1:dbname=foo:host=localhost:srvname=localhost\sqlexpress:bin_path=C:\Program Files\Microsoft SQL Server\110\Tools\Binn'
Bundle 'thinca/vim-quickrun'
" Bundle 'xuhdev/SingleCompile'
Bundle 'tpope/vim-sensible'
Bundle 'tpope/vim-fugitive'
Bundle 'tpope/vim-surround'
Bundle 'tpope/vim-dispatch'
Bundle 'tpope/vim-repeat'
Bundle 'tpope/vim-eunuch'
Bundle 'tpope/vim-rsi'
Bundle 'tpope/vim-unimpaired'
Bundle 'tpope/vim-obsession'
Bundle 'tpope/vim-markdown'
Bundle 'tpope/vim-speeddating'
Bundle 'tpope/vim-vinegar'
Bundle 'kshenoy/vim-signature'
Bundle 'jiangmiao/auto-pairs'
Bundle 'zhaocai/DirDiff.vim'
Bundle 'AndrewRadev/linediff.vim'
Bundle 'mbbill/undotree'
" Bundle 'justinmk/TextObjectify'
Bundle 'kana/vim-textobj-user'
Bundle 'kana/vim-textobj-indent'
Bundle 'guns/vim-sexp'
Bundle 'guns/vim-clojure-static'
Bundle 'tpope/vim-fireplace'
Bundle 'gaving/vim-textobj-argument'
if !s:is_cygwin && has('python')
" delimiter highlighting? https://github.com/mhinz/vim-blockify/blob/master/plugin/blockify.vim
Bundle 'Valloric/MatchTagAlways'
endif
if !s:is_cygwin && (has('python') || has('python3'))
Bundle 'davidhalter/jedi-vim'
endif
Bundle 'bling/vim-airline'
Bundle 'PProvost/vim-ps1'
Bundle 'pangloss/vim-javascript'
Bundle 'tomtom/tcomment_vim'
Bundle 'chrisbra/color_highlight'
Bundle 'osyo-manga/vim-over'
Bundle 'mhinz/vim-signify'
if exists("$GOPATH")
Bundle 'Blackrush/vim-gocode'
endif
Bundle 'bruno-/vim-vertical-move'
Bundle 'justinmk/vim-ipmotion'
Bundle 'justinmk/vim-gtfo'
Bundle 'justinmk/vim-sneak'
" https://github.com/vim-scripts/surrparen
Bundle 'xolox/vim-misc'
Bundle 'tsukkee/unite-tag'
Bundle 'Shougo/unite.vim'
Bundle 'Shougo/unite-outline'
Bundle 'junegunn/vader.vim'
if s:lua_patch885
Bundle 'Shougo/neocomplete.vim'
endif

endif "}}}

filetype plugin indent on     " required!


fun! EnsureDir(path)
    let l:path = expand(a:path)
    if !isdirectory(l:path)
      call mkdir(l:path, 'p')
    endif
    return isdirectory(l:path)
endfun

func! EnsureFile(path)
  let l:path = expand(a:path)
  if !filereadable(l:path) && -1 == writefile([''], l:path)
    echoerr "failed to create file: ".l:path
  endif
endf

"==============================================================================
" general settings / options
"==============================================================================
let g:sneak#streak = 1

let g:vertical_move_default_mapping = 0
nmap <silent> + <Plug>(vertical_move_down)
nmap <silent> _ <Plug>(vertical_move_up)
xmap <silent> + <Plug>(vertical_move_down)
xmap <silent> _ <Plug>(vertical_move_up)
omap <silent> + <Plug>(vertical_move_down)
omap <silent> _ <Plug>(vertical_move_up)

let g:signify_vcs_list = [ 'git' ]

let g:dbext_default_history_file = expand('~/.dbext_sql_history')
let g:dbext_default_history_size = 1000
let g:dbext_default_history_max_entry = 10*1024

let g:EclimBufferTabTracking = 0 "legacy version
let g:EclimBuffersTabTracking = 0

let g:SignatureEnableDefaultMappings = 2

let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#buffer_nr_show = 1
let g:airline#extensions#tabline#fnamemod = ':t'
let g:airline#extensions#tabline#buffer_min_count = 2
let g:airline#extensions#tabline#tab_nr_type = 1
let g:airline#extensions#whitespace#enabled = 0
let g:airline_left_sep = ''
let g:airline_left_alt_sep = ''
let g:airline_right_sep = ''
let g:airline_right_alt_sep = ''
if !s:is_gui
  let g:airline_theme='simple'
endif

" To map a 'meta' escape sequence in a terminal, you must map the literal control character.
" insert-mode, type ctrl-v, then press alt+<key>. Must be done in a terminal, not gvim/macvim.
" http://vim.wikia.com/wiki/Mapping_fast_keycodes_in_terminal_Vim
" http://stackoverflow.com/a/10633069/152142
if !s:is_msysgit && !s:is_gui
    "avoid: m-b m-d m-f
    set <m-g>=g <m-h>=h <m-j>=j <m-k>=k <m-l>=l
          \ <m-o>=o <m-p>=p <m-q>=q <m-r>=r <m-s>=s
          \ <m-t>=t <m-x>=x <m-y>=y <m-z>=z
          \ <m-]>=]
endif

try | lang en_US | catch | endtry

if s:is_msysgit
  set listchars=tab:>\ ,trail:.,extends:>,precedes:<,nbsp:+
elseif s:is_windows || s:is_cygwin || s:is_ssh
  set listchars=tab:▸\ ,trail:▫,extends:>,precedes:<,nbsp:+
else
  set showbreak=↪\  " precedes line wrap
endif
set list

set hidden      " Allow buffer switching even if unsaved 
set mouse=a     " Enable mouse usage (all modes)
set lazyredraw  " no redraws in macros
if !s:is_ssh
set ttyfast
endif
set cmdheight=2 "The commandbar height
set backspace=eol,start,indent
set ignorecase " case-insensitive searching
set smartcase  " but become case-sensitive if you type uppercase characters
set hlsearch   " highlight search matches
set matchtime=3

"audible bell persists unless visualbell is enabled.
set noerrorbells novisualbell t_vb= visualbell

set timeoutlen=3000
if s:plugins
set noshowmode " Hide the default mode text (e.g. -- INSERT -- below the statusline)
else
set showtabline=1
endif
set foldmethod=marker
set sidescroll=2
set sidescrolloff=2

set nojoinspaces

" restore cursor position upon returning to a buffer, _without_ permanently 
" setting 'nostartofline' (which affects many other behaviors).
augroup vimrc_stayput
  autocmd!
  if &startofline
    " 1. disable 'startofline' temporarily while switching buffers, 
    " 2. then re-enable it on CursorMoved, 
    " 3. then clear the CursorMoved autocommand to avoid spam
    autocmd BufLeave * set nostartofline |
          \ autocmd vimrc_stayput CursorMoved,CursorMovedI * set startofline |
          \ autocmd! vimrc_stayput CursorMoved,CursorMovedI
  endif
augroup END

" platform-specific settings
if s:is_windows
    set winaltkeys=no
    set guifont=Consolas:h11
elseif s:is_mac
    " Use option (alt) as meta key.
    set macmeta

    " macvim options  :view $VIM/gvimrc
    let macvim_skip_colorscheme=1
    let macvim_skip_cmd_opt_movement=1

    if s:is_gui
        "set guifont=Monaco:h16
        set guifont=Menlo:h14
    endif
elseif s:is_gui "linux or other
    set guifont=Monospace\ 10
endif

"colorscheme {{{
  if s:plugins && &t_Co != 88 && &t_Co < 256 && (s:is_tmux || &term =~? 'xterm')
    " force colors
    set t_Co=256
  endif

  let s:color_force_high_contrast = ' 
        \ if &background == "dark"
        \ | hi Normal  guibg=black guifg=white ctermfg=255 ctermbg=0
        \ | hi NonText guibg=black guifg=white ctermfg=255 ctermbg=0
        \ | endif
        \'

  if !s:is_mac
    exe s:color_force_high_contrast
    if s:starting
      exe 'autocmd ColorScheme * '.s:color_force_high_contrast
    endif
  endif

  if s:is_msysgit
    hi CursorLine  term=NONE cterm=NONE ctermfg=NONE ctermbg=darkblue guifg=NONE guibg=NONE
    hi ColorColumn term=NONE cterm=NONE ctermfg=NONE ctermbg=darkblue guifg=NONE guibg=NONE
  elseif !s:is_gui && &t_Co <= 88
    hi CursorLine cterm=underline
  else
    let s:color_override = '
          \   hi Visual        ctermbg=23 guibg=#007475
          \ | hi VisualNOS     term=bold,underline cterm=bold,underline ctermbg=23 gui=bold,underline guibg=#007475
          \'

    let s:color_override_dark = '
          \ if &background == "dark"
          \ | hi Comment       guifg=#afafaf               gui=NONE  ctermfg=102               cterm=NONE
          \ | hi Cursor        guibg=#0a9dff guifg=white   gui=NONE
          \ | hi CursorLine    guibg=#293739 ctermbg=236
          \ | hi PmenuSel      guibg=#0a9dff guifg=white   gui=NONE  ctermbg=39 ctermfg=white  cterm=NONE
          \ | hi PmenuSbar     guibg=#857f78
          \ | hi PmenuThumb    guifg=#242321
          \ | hi WildMenu      gui=NONE cterm=NONE guifg=#f8f6f2 guibg=#0a9dff ctermfg=255 ctermbg=39
          \ | hi IncSearch     guifg=white   guibg=LimeGreen         ctermfg=NONE ctermbg=154 gui=bold cterm=NONE
          \ | hi Search        guifg=white   guibg=#FF870A gui=NONE  ctermfg=255  ctermbg=208  cterm=NONE
          \ | hi DiffAdd       guifg=#ffffff guibg=#006600 gui=NONE  ctermfg=231  ctermbg=22   cterm=NONE 
          \ | hi DiffChange    guifg=#ffffff guibg=#007878 gui=NONE  ctermfg=231  ctermbg=30   cterm=NONE 
          \ | hi DiffDelete    guifg=#ff0101 guibg=#9a0000 gui=NONE  ctermfg=196  ctermbg=88   cterm=NONE 
          \ | hi DiffText      guifg=#000000 guibg=#ffb733 gui=NONE  ctermfg=000  ctermbg=214  cterm=NONE 
          \ | hi TODO          guibg=#00acbc guifg=#ffff87 gui=bold,underline
          \ | endif
          \'

    if s:starting "only on startup
      exe 'autocmd ColorScheme * '.s:color_override
      exe 'autocmd ColorScheme * '.s:color_override_dark
      " expects &runtimepath/colors/{name}.vim.
      colorscheme molokai
      if !(s:is_gui || s:is_mac || s:is_cygwin)
        exe s:color_override
      endif
    endif
  endif
"}}}

"==============================================================================
" text, tab and indent 

set formatoptions+=rno1l
if v:version > 703 || v:version == 703 && has("patch541")
  " Delete comment character when joining commented lines
  set formatoptions+=j
endif
" don't syntax-highlight long lines
set synmaxcol=1000

set expandtab
set tabstop=2
set shiftwidth=2
set smarttab " Use 'shiftwidth' when using <Tab> in front of a line. By default it's used only for shift commands ("<", ">").

set linebreak
set nowrap

set autoindent " Autoindent when starting new line, or using 'o' or 'O'.
set smartindent

" =============================================================================
" util functions

" :help :DiffOrig
command! DiffOrig leftabove vnew | set bt=nofile | r ++edit # | 0d_ | diffthis | wincmd p | diffthis

func! TrimTrailingWhitespace()
  let _s=@/
  let _w=winsaveview()
  %s/\s\+$//ge
  call winrestview(_w)
  call histdel("/", histnr("/"))
  let @/=_s
endfunc

"return the syntax highlight group under the cursor
function! GetSyntaxName()
    let l:name = synIDattr(synID(line('.'),col('.'),1),'name')
    return l:name == '' ? '' : '[' . l:name . ']'
endfunction

func! AppendToFile(file, lines)
  let l:file = expand(a:file)
  call EnsureFile(l:file)
  "credit ZyX: http://stackoverflow.com/a/8976314/152142
  call writefile(readfile(l:file)+a:lines, l:file)
endf

"TODO:
" http://vim.wikia.com/wiki/Have_Vim_check_automatically_if_the_file_has_changed_externally
" http://www.vim.org/scripts/script.php?script_id=1714
" FileChangedShell & :checktime
let s:orig_updtime = &updatetime
func! Blah()
  let w = winsaveview()
  e!
  call winrestview(w)
endf
func! Tail(filepath)
  exec 'split '.a:filepath
  "TODO: disable undo for buffer

  "initial cursor activity to kick CursorHold
  let &updatetime=2000
  call feedkeys("f\e")
  let b:tail_follow = 0

  let aubuff = '<buffer='.bufnr("%").'>'
  let updtime = s:orig_updtime
  augroup TailSmurf
    au!
    exec 'autocmd BufEnter,WinEnter '.aubuff.' let &updatetime=2000'
    if s:is_windows
      "force cursor activity to trigger next CursorHold
      exec 'autocmd CursorHold '.aubuff.' call Blah() | call feedkeys("f\e") | if b:tail_follow | exec "norm! G" | endif'
    else
      "force cursor activity to trigger next CursorHold
      " call AppendToFile("~/foo2.txt", [string(localtime())])
      exec 'autocmd CursorHold '.aubuff.' checktime | call feedkeys("f\e")'
      exec 'autocmd FileChangedShellPost '.aubuff.' if b:tail_follow | exec "norm! G" | endif'
      exec 'autocmd FileChangedShell '.aubuff.' call Blah() '
    endif
    "one-time event to kick the updatetime initially
    " exec 'autocmd CursorMoved '.aubuff.' let &updatetime=2000 | au! CursorMoved TailSmurf '.aubuff
    exec 'autocmd BufHidden,WinLeave '.aubuff.' let &updatetime='.updtime
  augroup END
endf

" =============================================================================
" normal mode

nnoremap & :&&<CR>
xnoremap & :&&<CR>
" Make Y consistent with C and D.
nnoremap Y y$
" copy selection to gui-clipboard
xnoremap Y "+y
" copy entire file contents (to gui-clipboard if available)
nnoremap yY :let b:winview=winsaveview()<bar>exe 'norm ggVG'.(has('clipboard')?'"+y':'y')<bar>call winrestview(b:winview)<cr>

" delete the 'head' of a path on the command line
cno <c-o>/ <C-\>e<sid>deleteTillSlash()<cr>

func! s:deleteTillSlash()
  return s:is_windows ? substitute(getcmdline(), '\(.*[/\\]\).*', '\1', '') : substitute(getcmdline(), '\(.*[/]\).*', '\1', '')
endfunc


" abbreviations ===============================================================

iab date- <c-r>=strftime("%d/%m/%Y %H:%M:%S")<cr>

" paste current dir to command line
cabbrev ]c <c-r>=expand("%:p:h")<cr>

"==============================================================================
" key mappings/bindings

" manage windows
nnoremap gw <c-w>
nnoremap gw<c-w> :setlocal winfixwidth!<bar>echo 'winfixwidth='.&winfixwidth<cr>
nnoremap gw<c-h> :setlocal winfixheight!<bar>echo 'winfixheight='.&winfixheight<cr>
nnoremap gwV :vnew<cr>
nnoremap <silent> gww :<C-u>call <sid>switch_to_alt_win()<cr>
" fit the current window height to the text height
nnoremap <expr> gw<bs> 'ggz'.line('$')."\<cr>"

" go to the previous window (or any other window if there is no 'previous' window).
func! s:switch_to_alt_win()
  let currwin = winnr()
  wincmd p
  if winnr() == currwin "window didn't change, so there was no 'previous' window.
    wincmd W
  endif
endf

" manage tabs
nnoremap gwN :tabnew<cr>
nnoremap gwC :tabclose<cr>
nnoremap gwT :wincmd T<cr>

" manage buffers
nnoremap <leader>bd :<c-u>call <SID>buf_kill(1)<cr>
nnoremap <leader>b! :<c-u>call <SID>buf_kill(0)<cr>
nnoremap <leader>bn  :<c-u>enew<cr>
nnoremap gb :<c-u>exec (v:count ? 'b '.v:count : 'bnext')<cr>
nnoremap gB :<c-u>exec (v:count ? 'b '.v:count : 'bprevious')<cr>

" quickfix window
nnoremap <C-q> :botright copen<cr>

" working with projects/directories
" nnoremap ^ :exec get(w:, "netrw_winnr", 0) ? 'Rexplore' : 'Vexplore'<cr>
nmap ^ <Plug>VinegarUp
" set working directory to the current buffer's directory
nnoremap <leader>cw :cd %:p:h<bar>pwd<cr>
" show the current working directory
nnoremap <M-g> :<C-u>pwd<cr>
" insert the current file path
nnoremap <leader>fn i<c-r>=expand('%:p')<cr> 
" insert the current file directory
nnoremap <leader>fd i<c-r>=expand('%:p:h').'/'<cr>

" version control
xnoremap <leader>v  :Linediff<cr>
nnoremap <leader>vd :Gdiff<cr>

" execute/evaluate
nmap gX      <Plug>(quickrun)
xmap <enter> <Plug>(quickrun)

" filter
" nnoremap c<cr>jj    :%!python -m json.tool<cr>
" nnoremap z<cr>jj    :%!python -m json.tool<cr>
nnoremap c<space>jj :%!python -m json.tool<cr>

" align
" nnoremap c<space>       :easyalign...
" xnoremap <space><space> :easyalign...

func! BufDeath_Comparebuf(b1, b2)
  "prefer loaded buffers before unloaded buffers
  if bufloaded(a:b1)
    return bufloaded(a:b2) ? 0 : -1
  endif
  return !bufloaded(a:b2) ? 0 : 1
endf

func! s:buf_find_displayed_bufs()
  " all buffers displayed in any window, any tab.
  let s:displayedbufs = []
  for i in range(1, tabpagenr('$'))
    call extend(s:displayedbufs, tabpagebuflist(i))
  endfor
  return s:displayedbufs
endf

func! s:buf_find_valid_next_bufs()
  "valid 'next' buffers 
  "   EXCLUDE: 
  "     - current, unlisted, Unite
  "     - directory buffers marked as 'readonly' and 'modified' (netrw often leaves a _listed_ buffer in this weird state)
  "   INCLUDE: normal buffers; 'help' buffers
  let l:valid_buffers = filter(range(1, bufnr('$')), 
              \ 'buflisted(v:val) 
              \  && ("" ==# getbufvar(v:val, "&buftype") || "help" ==# getbufvar(v:val, "&buftype")) 
              \  && v:val != bufnr("%") 
              \  && -1 == index(tabpagebuflist(), v:val) 
              \  && !(isdirectory(bufname(v:val)) && getbufvar(v:val, "&modified") && getbufvar(v:val, "&readonly"))
              \ ')
  call sort(l:valid_buffers, 'BufDeath_Comparebuf')
  return l:valid_buffers
endf

func! s:buf_switch_to_altbuff()
  " change to the 'alternate' buffer if:
  "   - it exists, and 
  "   - it is not the current buffer (yes, this really happens, eg with netrw)
  if -1 != bufnr("#") && bufnr("#") != bufnr("%")
    buffer #
  else " change to first 'valid' buffer
    let l:valid_buffers = s:buf_find_valid_next_bufs()
    if len(l:valid_buffers) > 0
      exe 'buffer '.l:valid_buffers[0]
    endif
  endif
endf

" close the current buffer with a vengeance
" BDSN: Buffer DiScipliNe
func! s:buf_kill(mercy)
  let l:origbuf = bufnr("%")
  let l:origbufname = bufname(l:origbuf)
  if a:mercy && &modified
    echom 'buffer has unsaved changes (use '.g:mapleader.'d! to override)'
    return
  endif

  silent call <sid>buf_switch_to_altbuff()

  " remove the buffer filename (if any) from the args list.
  if !empty(l:origbufname)
    silent! exe 'argdelete '.l:origbufname
  endif
  " obliterate the buffer and all of its related state (marks, local options, ...), 
  if bufexists(l:origbuf) "some other mechanism may have deleted the buffer already.
    exe 'bwipeout! '.l:origbuf
  endif
endf

nnoremap <c-^> :call <sid>buf_switch_to_altbuff()<cr>

"move to first non-blank character
nnoremap 0 ^
xnoremap 0 ^
"move to last character 
nnoremap - $
xnoremap - $

" un-join (split) the current line at the cursor position
nnoremap <c-j> i<c-j><esc>k$
" delete without overwriting the default register
nnoremap <leader>d "_d
xnoremap <leader>d "_d
nnoremap <leader>D "_D

func! s:replace_without_yank(type)
  let sel_save = &selection
  let l:col = col('.')
  let &selection = "inclusive"

  if a:type == 'line'
    silent normal! '[V']"_d
  elseif a:type == 'block'
    silent normal! `[`]"_d
  else
    silent normal! `[v`]"_d
  endif

  if col('.') == l:col "paste to the left.
    silent normal! P
  else "if the operation deleted the last column, then the cursor
       "gets bumped left (because its original position no longer exists),
       "so we need to paste to the right instead of the left.
    silent normal! p
  endif

  let &selection = sel_save
endf
nnoremap <silent> rr :<C-u>set opfunc=<sid>replace_without_yank<CR>g@

inoremap jk <esc>
inoremap kj <esc>
nnoremap ' `
xnoremap ' `

nnoremap <left>  4zh
nnoremap <right> 4zl
xnoremap <left>  4zh
xnoremap <right> 4zl
nnoremap <c-left>  :<c-u>exec 'norm! '.(winwidth(0)/2).'zh'<cr>
nnoremap <c-right> :<c-u>exec 'norm! '.(winwidth(0)/2).'zl'<cr>

nnoremap <c-d> <PageDown>
nnoremap <c-u> <PageUp>
nnoremap <space> :
nnoremap <leader>w :w<cr>

" map m-] to be the inverse of c-]
nnoremap <m-]> <c-t>

" search within visual block
xnoremap / <esc>/\%V

" select last inserted text
nnoremap gV `[v`]

" replay @q macro for each line of a visual selection
xnoremap @q :normal @q<CR>
" repeat last command for each line of a visual selection
xnoremap . :normal .<CR>

"j,k move by screen line instead of file line
nnoremap j gj
nnoremap k gk
inoremap <Down> <C-o>gj
inoremap <Up>   <C-o>gk

"allow cursor to move anywhere in all modes
nnoremap cov :set <C-R>=empty(&virtualedit) ? 'virtualedit=all' : 'virtualedit='<CR><CR>

" disable F1 help key
noremap! <F1> <nop>
noremap <F1> <nop>

" disable linewise undo
nnoremap U <nop>

" disable Ex mode key and map it to something awesome
nnoremap Q @@
xnoremap Q @@

nnoremap ZZ :<c-u>qa<cr>
nnoremap ZQ :<c-u>qa!<cr>

func! s:do_in_place(keyseq, line_offset, col_offset) "perform an edit without moving the cursor
  let pos = [line(".") + a:line_offset, col(".") + a:col_offset]
  exe "norm ".a:keyseq
  call cursor(pos)
endf

inoremap ,o <c-o>:<c-u>call <sid>do_in_place("o", 0, 0)<cr>
inoremap ,O <c-o>:<c-u>call <sid>do_in_place("O", 1, 0)<cr>


func! ReadExCommandOutput(cmd)
  redir => l:message
  silent execute a:cmd
  redir END
  "tabnew
  silent put=l:message
  "set nomodified
endf
command! -nargs=+ -complete=command R call ReadExCommandOutput(<q-args>)

" python ======================================================================
augroup vimrc_python
  autocmd!
  autocmd FileType python syn keyword pythonDecorator True None False self | setlocal nosmartindent
  let g:jedi#force_py_version = 3
  let g:jedi#auto_vim_configuration = 0
  let g:jedi#goto_assignments_command = 'gd'
  let g:jedi#goto_definitions_command = 'gD'
  let g:jedi#rename_command = 'cR'
  let g:jedi#usages_command = 'gr'
augroup END

" java ========================================================================
" gradle makeprg/quickfix settings:
"   https://github.com/luciano-fiandesio/dotfiles/blob/master/.vim/compiler/gradle.vim
augroup vimrc_java
  autocmd!
  autocmd FileType java setlocal tabstop=4 shiftwidth=4 noexpandtab copyindent softtabstop=0 nolist
  if s:has_eclim
    autocmd FileType java nnoremap <buffer> gd :<c-u>JavaSearchContext<cr>
          \ | nnoremap <buffer> gzt   :<c-u>JavaHierarchy<cr>
          \ | nnoremap <buffer> cri   :<c-u>JavaImportOrganize<cr>
          \ | nnoremap <buffer> K     :<c-u>JavaDocPreview<cr>
          \ | nnoremap <buffer> <bs>  :<c-u>JavaCorrect<cr>
  endif
augroup END


" golang ======================================================================
" code search:
"    https://sourcegraph.com/code.google.com/p/go/tree
" code navigation/inspection(!!!):
"   https://github.com/AndrewRadev/go-oracle.vim
" possible godoc solution    https://gist.github.com/mattn/569652
"    Bundle 'thinca/vim-ref'
"    let g:ref_use_vimproc = 1
"    let g:ref_open = 'vsplit'
"    let g:ref_cache_dir = expand('~/.vim/tmp/ref_cache/')
"    nnoremap g/k :<C-u>Unite ref/godoc -buffer-name=godoc -start-insert -horizontal<CR>
augroup vimrc_golang
  autocmd!
  autocmd FileType go iabbrev <buffer> err- if err != nil {<C-j>log.Fatal(err)<C-j>}<C-j>
  autocmd FileType go setlocal tabstop=4 shiftwidth=4 noexpandtab copyindent softtabstop=0 nolist

  if exists("$GOPATH")
    let s:gopaths = split($GOPATH, ':')
    for s:gopath in s:gopaths
      "set up Golint    https://github.com/golang/lint
      if isdirectory(s:gopath."/src/github.com/golang/lint/misc/vim")
        exe 'set runtimepath+='.s:gopath.'/src/github.com/golang/lint/misc/vim'
        autocmd BufWritePost,FileWritePost *.go execute 'Lint' | cwindow
        break
      endif
    endfor
  endif
augroup END

" clojure/lisp ================================================================

"transpose words, preserving punctuation
nnoremap <silent> gst :s,\v(\w+)(\W*%#\W*)(\w+),\3\2\1,<bar>nohl<CR>
"transpose WORDs, preserving whitespace
nnoremap <silent> gsT :s,\v(\S+)(\s*\S*%#\S*\s*)(\S+),\3\2\1,<bar>nohl<CR>

"cf. emacs 'sp-down-sexp'
nmap <M-j> l<Plug>(sexp_move_to_next_element_head)
"cf. emacs 'sp-up-sexp'
nmap <M-k> h<Plug>(sexp_move_to_prev_element_head)

let g:sexp_enable_insert_mode_mappings = 0
let g:sexp_mappings = {
      \ 'sexp_outer_list':                'af',
      \ 'sexp_inner_list':                'if',
      \ 'sexp_outer_top_list':            'aF',
      \ 'sexp_inner_top_list':            'iF',
      \ 'sexp_outer_element':             'ae',
      \ 'sexp_inner_element':             'ie',
      \ 'sexp_move_to_prev_element_head': '(',
      \ 'sexp_move_to_next_element_head': ')',
      \ 'sexp_round_head_wrap_list':      'gS(',
      \ 'sexp_round_tail_wrap_list':      'gS)',
      \ 'sexp_square_head_wrap_list':     'gS[',
      \ 'sexp_square_tail_wrap_list':     'gS]',
      \ 'sexp_curly_head_wrap_list':      'gS{',
      \ 'sexp_curly_tail_wrap_list':      'gS}',
      \ 'sexp_round_head_wrap_element':   'gs(',
      \ 'sexp_round_tail_wrap_element':   'gs)',
      \ 'sexp_square_head_wrap_element':  'gs[',
      \ 'sexp_square_tail_wrap_element':  'gs]',
      \ 'sexp_curly_head_wrap_element':   'gs{',
      \ 'sexp_curly_tail_wrap_element':   'gs}',
      \ 'sexp_insert_at_list_head':       '<I',
      \ 'sexp_insert_at_list_tail':       '>I',
      \ 'sexp_splice_list':               'dsf',
      \ 'sexp_raise_list':                'gsO',
      \ 'sexp_raise_element':             'gso',
      \ 'sexp_swap_list_backward':        '<f',
      \ 'sexp_swap_list_forward':         '>f',
      \ 'sexp_swap_element_backward':     '<e',
      \ 'sexp_swap_element_forward':      '>e',
      \ 'sexp_emit_head_element':         'gsB',
      \ 'sexp_emit_tail_element':         'gsb',
      \ 'sexp_capture_prev_element':      'gsS',
      \ 'sexp_capture_next_element':      'gss',
      \ 'sexp_move_to_prev_bracket':      '',
      \ 'sexp_move_to_next_bracket':      '',
      \ 'sexp_outer_string':              '',
      \ 'sexp_inner_string':              '',
      \ 'sexp_move_to_prev_element_tail': '',
      \ 'sexp_move_to_next_element_tail': '',
      \ }

" lua =========================================================================
"    https://github.com/xolox/vim-lua-inspect

" csharp ======================================================================
" $COMSPEC /k "C:/Program Files (x86)/Microsoft Visual Studio 11.0/Common7/Tools/vsvars32.bat"



augroup BufferDeath
  autocmd!
  " on BufLeave:
  "   1. remove existing autocommand, if any
  "   2. set up CursorHold autocommand
  " on CursorHold:
  "   1. call function
  "   2. remove the autocommand to avoid spam
  autocmd BufLeave * exec 'autocmd! BufferDeath CursorHold' |
        \ autocmd BufferDeath CursorHold * silent call <sid>clear_empty_buffers() |
        \ autocmd! BufferDeath CursorHold
augroup END

augroup vimrc_autocmd
  autocmd!
  autocmd BufReadPost quickfix map <buffer> <c-p> <up>|map <buffer> <c-n> <down>

  " Highlight VCS conflict markers
  " autocmd BufEnter fugitive\:* match ErrorMsg '^\(<\|=\|>\)\{7\}\([^=].\+\)\?$'

  autocmd FileType unite call s:unite_settings()
  " obliterate unite buffers (marks especially).
  autocmd BufLeave \[unite\]* if "nofile" ==# &buftype | setlocal bufhidden=wipe | endif

  " Jump to the last position when reopening a file
  autocmd BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g`\"" | endif

  " force windows to be sized equally after viewport resize
  autocmd VimResized * wincmd =

  autocmd FileType text setlocal tabstop=4 shiftwidth=4

  autocmd FileType css set omnifunc=csscomplete#CompleteCSS

  autocmd BufWritePre *.py :call TrimTrailingWhitespace()

  autocmd BufEnter,WinEnter * setlocal cursorline | silent! setlocal colorcolumn=80
  autocmd WinLeave * setlocal nocursorline | silent! setlocal colorcolumn=

  if exists("*mkdir") "auto-create directories for new files
    au BufWritePre,FileWritePre * call EnsureDir('<afile>:p:h')
  endif

  if s:is_windows
    " always maximize initial GUI window size
    autocmd GUIEnter * simalt ~x
  endif
augroup END

" https://github.com/thinca/vim-visualstar/blob/master/plugin/visualstar.vim
" makes * and # work on visual mode too.
function! s:visual_search(cmdtype)
  let l:temp = @s
  exe "normal! \<esc>gv\"sy"
  let l:foo = substitute(escape(@s, a:cmdtype.'\'), '\n', '\\n', 'g')
  let @s = l:temp
  return l:foo
endfunction

" :noau speeds up vimgrep
nnoremap g// :<c-u>noau vimgrep // **<left><left><left><left>
" search current buffer and open results in quickfix window
nnoremap g/% :<c-u>vimgrep  % <bar> cw<left><left><left><left><left><left><left>
" search and replace
nnoremap g/r :<c-u>OverCommandLine<cr>%s/
xnoremap g/r :<c-u>OverCommandLine<cr>%s/\%V

" in visual mode, press * or # to search for the current selection
xnoremap * /\V<C-R>=<SID>visual_search('/')<cr><cr>
xnoremap # ?\V<C-R>=<SID>visual_search('?')<cr><cr>

" recursively grep for word under cursor
nnoremap g/* :<c-u>noau vimgrep /\V<c-r><c-w>/ **<CR>
xnoremap g/* :<c-u>noau vimgrep /<c-r>=<SID>visual_search('/')<cr>/ **<CR>

" show :ilist or ]I results in the quickfix window
function! s:ilist_qf(start_at_cursor)
  redir => output
    silent! exec 'normal! '.(a:start_at_cursor ? ']I' : '[I')
  redir END
  let lines = split(output, '\n')
  if lines[0] =~ '^Error detected'
    echomsg "Could not find the word in file"
    return
  endif
  let [filename, line_info] = [lines[0], lines[1:-1]]
  "turn the :ilist output into a quickfix dictionary
  let qf_entries = map(line_info, "{
        \ 'filename': filename,
        \ 'lnum': split(v:val)[1],
        \ 'text': getline(split(v:val)[1])
        \ }")
  call setqflist(qf_entries)
  cwindow
endfunction
nnoremap <silent> [I :call <sid>ilist_qf(0)<CR>
nnoremap <silent> ]I :call <sid>ilist_qf(1)<CR>

" =============================================================================
" autocomplete / omnicomplete / tags
" =============================================================================
" Don't scan includes; tags file is more performant.
set complete-=i
set completeopt-=preview
set completeopt+=longest

inoremap <C-Space> <C-x><C-o>
if !s:is_gui
  inoremap <C-@> <C-x><C-o>
endif

" syntaxcomplete provides basic completion for filetypes that lack a custom one.
"   :h ft-syntax-omni
if has("autocmd") && exists("+omnifunc")
  autocmd Filetype *
        \ if &omnifunc == "" |
        \   setlocal omnifunc=syntaxcomplete#Complete |
        \ endif
endif

func! s:init_neocomplete()
  nnoremap <leader>neo :NeoCompleteEnable<cr>
  let g:neocomplete#enable_smart_case = 1
  " let force = get(g:, 'neocomplete#force_omni_input_patterns', {})
  let omni = get(g:, 'neocomplete#sources#omni#input_patterns', {})
  let g:neocomplete#sources#omni#input_patterns = omni
  let omni.go  = '[^.[:digit:] *\t]\.\w*'
  let omni.sql = '[^.[:digit:] *\t]\%(\.\)\%(\h\w*\)\?'
  let omni.python = '\%([^. \t]\.\|^\s*@\|^\s*from\s.\+import \|^\s*from \|^\s*import \)\w*'
endf

if s:lua_patch885
  call s:init_neocomplete()
endif

set wildmode=full
set wildignore+=tags,*.o,*.obj,*.dll,*.class,.git,.hg,.svn,*.pyc,*/tmp/*,*/grimoire-remote/*,*.so,*.swp,*.zip,*.exe,*.jar,*/opt/*,*/gwt-unitCache/*,*.cache.html,*.pdf,*.wav,*.mp3,*.ogg

" Files with these suffixes get a lower priority when matching a wildcard
set suffixes=.aux,.log,.dvi,.bbl,.blg,.brf,.cb,.ind,.idx,.ilg,.inx,.out,.toc

if s:is_windows
  set wildignore+=*\\Debug\\*,*\\Release\\*,*\\Windows\\*,*\\Program\ Files*\\*,*\\AppData\\*,*.pch,*.ipch,*.pdb,*.sdf,*.opensdf,*.idb,*.suo,*.ntuser,*.blf,*.dat,*.regtrans-ms
  let g:neocomplete#ctags_command = '~/bin/ctags.exe'
endif

" important!: semicolon means 'walk up until found'
set tags^=./tags;,tags;,~/.vimtags

if s:plugins "unite.vim =============================================== {{{
call unite#custom#profile('files', 'filters', 'sorter_rank')

"let g:unite_source_grep_command=expand($ProgramFiles.'\Git\bin\grep.exe')
let g:unite_source_history_yank_enable = 1
let g:unite_force_overwrite_statusline = 0
let g:unite_source_file_mru_time_format = "(%Y/%m/%d %H:%M) "
let g:unite_source_buffer_time_format = "(%Y/%m/%d %H:%M) "
let g:unite_enable_start_insert = 1

call unite#filters#matcher_default#use(['matcher_fuzzy'])
call unite#filters#sorter_default#use(['sorter_rank'])
" TODO: https://github.com/bling/dotvim/blob/master/vimrc#L535
"       https://github.com/Shougo/unite.vim/issues/347
let s:unite_sources = 'file_rec/async,file_rec,file_mru,directory_rec/async,directory_rec,directory_mru'
let g:unite_source_rec_max_cache_files = 5000
call unite#custom#source(s:unite_sources, 'max_candidates', 5000)
call unite#custom#source(s:unite_sources,
            \ 'converters',
            \ ['converter_relative_abbr', 'converter_file_directory'])

" extend default ignore pattern for file_rec source (same as directory_rec)
let s:file_rec_ignore = unite#get_all_sources('file_rec')['ignore_pattern'] .
    \ '\|\.\%(jar\|jpg\|gif\|png\)$' .
    \ '\|opt\|Downloads\|eclipse_workspace\|gwt-unitCache\|grimoire-remote'
if s:is_windows
    let s:file_rec_ignore .= '\|AppData'
elseif s:is_mac
    let s:file_rec_ignore .= '\|Library'
endif
call unite#custom#source('file_rec,directory_rec', 'ignore_pattern', s:file_rec_ignore)

" search hidden directories:
nnoremap <c-p> :<C-u>Unite -no-split -buffer-name=files file_mru file_rec <cr>
nnoremap g/f :<C-u>Unite -no-split -buffer-name=functions function<cr>
nnoremap gl  :<C-u>Unite -no-split -buffer-name=buffer buffer<cr>
" auto-generates an outline of the current buffer
nnoremap <m-o> :<C-u>Unite -no-split -buffer-name=outline outline<cr>
" TODO: https://github.com/ivalkeen/vim-ctrlp-tjump
nnoremap <m-t> :<C-u>Unite -no-split -buffer-name=tag tag/include<cr>
nmap g/t <m-t>
nnoremap <m-y> :<C-u>Unite -no-split -buffer-name=yank history/yank<cr>
imap     <m-y> <C-o><m-y>
nnoremap <leader>cd :<C-u>Unite -no-split directory_mru directory_rec:. -buffer-name=cd -default-action=cd<CR>
nnoremap g/ps :<C-u>Unite process -buffer-name=processes<CR>

" Custom mappings for the unite buffer
function! s:unite_settings()
  setlocal nopaste
  nmap <buffer> <nowait> <esc> <Plug>(unite_exit)
  silent! unmap! <c-g>
  imap <buffer> <c-a> <Plug>(unite_choose_action)
  imap <buffer> <nowait> <C-g> <Plug>(unite_exit)
  " change directories in unite
  nmap <buffer> <nowait> <leader>cd <Plug>(unite_restart)
endfunction

" delete empty, non-visible, non-special buffers having no significant undo stack.
" TODO: exclude buffers that have an undo stack
function! s:clear_empty_buffers()
  let displayedbufs = <sid>buf_find_displayed_bufs()

  " if the buffer is loaded, just check to see if its content is empty:
  "     [""] == getbufline(v:val, 1, 2)
  let empty_bufs = filter(range(1, bufnr('$')),
        \ 'bufloaded(v:val) 
        \  && 0 == getbufvar(v:val, "&modified") 
        \  && "" ==# getbufvar(v:val, "&buftype") 
        \  && [""] == getbufline(v:val, 1, 2) 
        \  && -1 == index(displayedbufs, v:val) 
        \  && -1 == bufwinnr(v:val) 
        \ ')

  " if the buffer is _not_ loaded, we do _not_ want to load every buffer just to check if it is empty.
  "     - get its file path and check getfsize().
  "     - if getfsize() fails, then the filepath must be non-existent; and 
  "       an unloaded buffer with an invalid a filepath must be empty.
  let nonexistent = filter(range(1, bufnr('$')),
        \ 'bufexists(v:val) && !bufloaded(v:val) 
        \  && -1 == getfsize(expand("#".v:val.":p")) 
        \ ')

  " echom "========================================================"
  " echom "empty_bufs=" len(empty_bufs)
  " echom "nonexistent=" len(nonexistent)
  " for i in empty_bufs
  "   echom "empty /" i "/" bufname(i)
  " endfor
  " for i in nonexistent
  "   echom "nonexistent /" i "/" bufname(i)
  " endfor

  if !empty(empty_bufs + nonexistent)
    exe 'bwipeout! '.join(empty_bufs + nonexistent, ' ')
  endif
endfunction

endif "}}}

" session  ====================================================================
set sessionoptions-=globals
set sessionoptions-=blank
let s:sessiondir  = expand("~/.vim/sessions")
let s:sessionfile = expand(s:sessiondir . "/session.vim")
let s:sessionlock = expand(s:sessiondir . "/session.lock")

function! LoadSession()
  if !isdirectory(s:sessiondir) && !EnsureDir(s:sessiondir)
    echoerr "failed to create session dir: " . s:sessiondir 
  endif

  if -1 == writefile([''], s:sessionlock)
    echoerr "failed to create session lock: " . s:sessionlock
    return
  endif

  if filereadable(s:sessionfile)
    exe 'source ' s:sessionfile
  endif

  exe 'Obsession '.s:sessionfile
  "unlock the session
  autocmd VimLeave * call delete(s:sessionlock)
endfunction

if s:is_gui
  if !filereadable(s:sessionlock)
    "use a separate viminfo to avoid losing command history by other vim instances
    set viminfo+=n~/.viminfo_session

    " set viminfo+=% "remember buffer list
    autocmd VimEnter * :call LoadSession()
  endif
endif

if s:is_cygwin
  " use separate viminfo to avoid weird permissions issues
  set viminfo+=n~/.viminfo_cygwin
endif

if s:is_cygwin
  " Mode-dependent cursor   https://code.google.com/p/mintty/wiki/Tips
  let &t_ti.="\e[1 q"
  let &t_SI.="\e[5 q"
  let &t_EI.="\e[1 q"
  let &t_te.="\e[0 q"
endif

" tree view / disable until issue is fixed: https://code.google.com/p/vim/issues/detail?id=140
" let g:netrw_liststyle = 3
let g:netrw_list_hide = '\~$,^tags$,\(^\|\s\s\)\zs\.\.\S\+'

"ensure transient dirs
let s:dir = has('win32') ? '$APPDATA/Vim' : s:is_mac ? '~/Library/Vim' : empty($XDG_DATA_HOME) ? '~/.local/share/vim' : '$XDG_DATA_HOME/vim'
call EnsureDir(s:dir)

if isdirectory(expand(s:dir))
  call EnsureDir(s:dir . '/swap/')
  call EnsureDir(s:dir . '/backup/')
  call EnsureDir(s:dir . '/undo/')

  if &directory =~# '^\.,'
    let &directory = expand(s:dir) . '/swap//,' . &directory
  endif
  if &backupdir =~# '^\.,'
    let &backupdir = expand(s:dir) . '/backup//,' . &backupdir
  endif
  if has("persistent_undo") && &undodir =~# '^\.\%(,\|$\)'
    let &undodir = expand(s:dir) . '/undo//,' . &undodir
    set undofile
  endif
endif

silent! source ~/.vimrc.local

" r!wget -qO - https://raw.github.com/tpope/vim-sensible/master/plugin/sensible.vim
