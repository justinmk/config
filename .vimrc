" windows builds: http://tuxproject.de/projects/vim/
"                 http://files.kaoriya.net/vim/
"                 64-bit: http://solar-blogg.blogspot.ca/p/vim-build.html
" MacVim with homebrew:
"   brew install macvim --with-cscope --with-lua --HEAD --override-system-vim
"   brew linkapps --system
"
" If this .vimrc is not in $HOME, add these lines to $HOME/.vimrc :
"    set runtimepath+=/path/to/.vim
"    source /path/to/.vimrc
"==============================================================================

if exists('&guioptions')
    "no toolbar, no menu bar, no left scroll bar
    set guioptions-=T guioptions-=m guioptions-=L guioptions-=l
    "don't source &runtimepath/menu.vim. (must be done before 'filetype on' / 'syntax on')
    set guioptions-=M
    "use console dialogs instead of popup dialogs for simple choices.
    set guioptions+=rc
endif

let mapleader = ","
let g:mapleader = ","

"exists() tests whether an option exists
"has() tests whether a feature was compiled in
let s:is_cygwin = has('win32unix')
let s:is_windows = has('win32') || has('win64')
let s:is_mac = has('gui_macvim') || has('mac')
let s:is_unix = has('unix')
let s:is_msysgit = (has('win32') || has('win64')) && $TERM ==? 'cygwin'
let s:is_tmux = !(empty($TMUX))
let s:is_vimRecentBuildWithLua = has('lua') && (v:version > 703 || (v:version == 703 && has('patch885')))

" 'is GUI' means vim is _not_ running within the terminal.
" sample values:
"   &term  = win32 //vimrc running in msysgit terminal
"   $TERM  = xterm-color , cygwin
"   &term  = builtin_gui //*after* vimrc but *before* gvimrc
"   &shell = C:\Windows\system32\cmd.exe , /bin/bash
let s:is_gui = has('gui_running') || strlen(&term) == 0 || &term ==? 'builtin_gui'

"==============================================================================
" vundle   https://github.com/gmarik/vundle/
"==============================================================================

"boostrap vundle on new systems
fun! InstallVundle()
    echo "Installing Vundle..."
    silent !mkdir -p ~/.vim/bundle
    silent !git clone https://github.com/gmarik/vundle.git ~/.vim/bundle/vundle
endfun

if &compatible
    " caution: this resets many settings, eg 'history'
    set nocompatible " be iMproved
endif
filetype off " required!

set runtimepath+=~/.vim/bundle/vundle/
call vundle#rc()

" let Vundle manage Vundle (required!)
Bundle 'gmarik/vundle'

Bundle 'tomasr/molokai'
" Bundle 'nanotech/jellybeans.vim'
if !s:is_windows
Bundle 'benmills/vimux'
Bundle 'tpope/vim-tbone'
endif
Bundle 'sjl/clam.vim'
Bundle 'thinca/vim-quickrun'
Bundle 'tpope/vim-sensible'
Bundle 'tpope/vim-fugitive'
Bundle 'tpope/vim-surround'
Bundle 'tpope/vim-dispatch'
Bundle 'tpope/vim-abolish'
Bundle 'tpope/vim-repeat'
Bundle 'tpope/vim-eunuch'
Bundle 'tpope/vim-rsi'
Bundle 'tpope/vim-unimpaired'
Bundle 'tpope/vim-obsession'
Bundle 'tpope/vim-markdown'
" Bundle 'tpope/vim-endwise'
Bundle 'tpope/vim-speeddating'
Bundle 'kshenoy/vim-signature'
Bundle 'jiangmiao/auto-pairs'
Bundle 'zhaocai/DirDiff.vim'
Bundle 'paradigm/TextObjectify'
Bundle 'kana/vim-textobj-user'
Bundle 'kana/vim-textobj-indent'
Bundle 'gaving/vim-textobj-argument'
Bundle 'Valloric/MatchTagAlways'
Bundle 'goldfeld/vim-seek'
" Bundle 'Lokaltog/vim-powerline'
Bundle 'bling/vim-airline'
Bundle 'PProvost/vim-ps1'
Bundle 'tomtom/tcomment_vim'
Bundle 'ap/vim-css-color'
Bundle 'airblade/vim-gitgutter'
Bundle 'derekwyatt/vim-scala'
Bundle 'Blackrush/vim-gocode'
Bundle 'justinmk/vim-ipmotion'
Bundle 'justinmk/vim-gtfo'
Bundle 'xolox/vim-misc'
Bundle 'xolox/vim-easytags'
Bundle 'tsukkee/unite-tag'
Bundle 'Shougo/unite.vim'
Bundle 'Shougo/unite-outline'
if s:is_vimRecentBuildWithLua
Bundle 'Shougo/neocomplete.vim'
else
Bundle 'Shougo/neocomplcache'
endif

filetype plugin indent on     " required!


fun! EnsureDir(path)
    let l:path = expand(a:path)
    if (filewritable(l:path) != 2)
        if s:is_windows
            exe 'silent !mkdir "' . l:path . '"'
        else
            exe 'silent !mkdir -p "' . l:path . '"'
        endif
    endif
    return isdirectory(a:path) 
endfun


"==============================================================================
" general settings / options
"==============================================================================
let g:gitgutter_escape_grep = 1
let g:gitgutter_eager = 0
if s:is_windows
  let g:gitgutter_realtime = 0
endif

" To map a 'meta' escape sequence in a terminal, you must map the literal control character.
" insert-mode, type ctrl-v, then press alt+<key>. Must be done in a terminal, not gvim/macvim.
" http://vim.wikia.com/wiki/Mapping_fast_keycodes_in_terminal_Vim
" http://stackoverflow.com/a/10633069/152142
if !s:is_msysgit && !s:is_gui
    set <m-p>=p <m-b>=b <m-o>=o <m-y>=y <m-j>=j <m-k>=k <m-r>=r
          \ <m-t>=t <m-l>=l <m-h>=h
endif

" removing this breaks alt/meta mappings (on win32 gvim at least).
set encoding=utf-8

try | lang en_US | catch | endtry

if s:is_windows || !s:is_gui || (&termencoding !=# 'utf-8' && &encoding !=# 'utf-8')
  set listchars=tab:>\ ,trail:.,extends:>,precedes:<,nbsp:+
endif
set list

set report=0
set hidden      " Allow buffer switching even if unsaved 
set mouse=a     " Enable mouse usage (all modes)
set lazyredraw  " no redraws in macros
"set ttyfast
set cmdheight=2 "The commandbar height
set backspace=eol,start,indent
set whichwrap+=<,>,h,l
set ignorecase " case-insensitive searching
set smartcase  " but become case-sensitive if you type uppercase characters
set hlsearch   " highlight search matches
set magic " change the way backslashes are used in search patterns
set mat=5     " showmatch time (tenths of a second)
set noerrorbells
set novb t_vb=
set tm=3000
set nonumber
set background=dark
set showtabline=1
set noshowmode " Hide the default mode text (e.g. -- INSERT -- below the statusline)
set foldmethod=marker

set virtualedit=all "allow cursor to move anywhere in all modes

if &startofline
" don't reset the cursor upon returning to a buffer:
augroup StayPut
  au!
  " 1. disable 'startofline' temporarily while switching buffers, 
  " 2. then re-enable it on CursorMoved, 
  " 3. then clear the CursorMoved autocommand to avoid spam
  autocmd BufLeave * set nostartofline | 
      \ autocmd StayPut CursorMoved,CursorMovedI * set startofline | autocmd! StayPut CursorMoved,CursorMovedI
augroup END
endif

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

        let g:Powerline_symbols = 'unicode'
    endif
elseif s:is_gui "linux or other
    set guifont=Monospace\ 10
endif

"colorscheme {{{
  "highlight line in the current window only
  augroup CursorLine
    au!
    au VimEnter,WinEnter,BufWinEnter * setlocal cursorline
    au WinLeave * setlocal nocursorline
  augroup END

  if &t_Co != 88 && &t_Co < 256 && (s:is_tmux || &term =~? 'xterm')
    " force colors
    set t_Co=256
  endif

  if !s:is_mac
    autocmd ColorScheme * highlight Normal ctermbg=black guibg=black
          \ | highlight NonText ctermbg=black guibg=black
  endif

  if !s:is_gui && &t_Co <= 88
    highlight CursorLine cterm=underline
  else
    "see :h 'highlight'
    "https://github.com/Pychimp/vim-luna
    let s:color_override = ' 
          \   hi Visual        guifg=#262626 guibg=#ffff4d gui=NONE  ctermfg=235  ctermbg=227  cterm=NONE
          \ | hi VisualNOS     guifg=#262626 guibg=#ffff4d gui=NONE  ctermfg=235  ctermbg=227  cterm=NONE
          \ | hi Cursor        guibg=#0a9dff guifg=white   gui=NONE  ctermfg=black
          \ | hi CursorLine    guibg=#293739 ctermbg=236
          \ | hi PmenuSel      guibg=#0a9dff guifg=white   gui=NONE  ctermbg=39 ctermfg=white  cterm=NONE
          \ | hi PmenuSbar     guibg=#857f78
          \ | hi PmenuThumb    guifg=#242321
          \ | hi WildMenu      gui=none cterm=none guifg=#f8f6f2 guibg=#0a9dff ctermfg=black ctermbg=39
          \ | hi IncSearch     guifg=white guibg=LimeGreen ctermfg=black ctermbg=154 gui=bold cterm=NONE
          \ | hi Search        guifg=black guibg=LightGoldenrod1 ctermfg=black ctermbg=227 gui=none cterm=NONE
          \ | hi DiffAdd       guifg=#ffffff guibg=#006600 gui=NONE  ctermfg=231  ctermbg=22   cterm=NONE 
          \ | hi DiffChange    guifg=#ffffff guibg=#007878 gui=NONE  ctermfg=231  ctermbg=30   cterm=NONE 
          \ | hi DiffDelete    guifg=#ff0101 guibg=#9a0000 gui=NONE  ctermfg=196  ctermbg=88   cterm=NONE 
          \ | hi DiffText      guifg=#000000 guibg=#ffb733 gui=NONE  ctermfg=000  ctermbg=214  cterm=NONE 
          \'

    " expects &runtimepath/colors/{name}.vim.
    colorscheme molokai

    if s:is_gui || s:is_mac
      exe 'autocmd ColorScheme *' s:color_override
    else
      exe s:color_override
    endif
  endif
"}}}

let g:Powerline_stl_path_style = 'short'

"==============================================================================
" text, tab and indent 
"==============================================================================
set formatoptions+=rn1
" don't syntax-highlight long lines
set synmaxcol=300

set expandtab
set tabstop=2
set shiftwidth=2
set smarttab " Use 'shiftwidth' when using <Tab> in front of a line. By default it's used only for shift commands ("<", ">").

autocmd FileType text setlocal tabstop=4 shiftwidth=4

set linebreak "wrap long lines at 'breakat' character
set textwidth=500

set autoindent " Autoindent when starting new line, or using 'o' or 'O'.
set smartindent
set nowrap 

if exists('&colorcolumn')
    set colorcolumn=80 "highlight the specified column
endif

" =============================================================================
" util functions
" =============================================================================

func! TrimTrailingWhitespace()
  normal mz
  %s/\s\+$//ge
  normal `zmz
endfunc

"return the syntax highlight group under the cursor ''
function! CurrentWordSyntaxName()
    let l:name = synIDattr(synID(line('.'),col('.'),1),'name')
    return l:name == '' ? '' : '[' . l:name . ']'
endfunction

" HiCursorWords "{{{
exec ((s:is_gui || &t_Co > 16) ? 'autocmd ColorScheme * ' : '') . 'highlight WordUnderTheCursor guifg=black guibg=white ctermfg=black ctermbg=white' 

function! s:HiCursorWords__getHiName(linenum, colnum)
  let hiname = synIDattr(synID(a:linenum, a:colnum, 0), "name")
  let hiname = s:HiCursorWords__resolveHiName(hiname)
  return hiname
endfunction

function! s:HiCursorWords__resolveHiName(hiname)
  redir => resolved
  silent execute 'highlight ' . a:hiname
  redir END

  if stridx(resolved, 'links to') == -1
    return a:hiname
  endif

  return substitute(resolved, '\v(.*) links to ([^ ]+).*$', '\2', '')
endfunction

function! s:HiCursorWords__getWordUnderTheCursor(linestr, linenum, colnum)
  "let word = substitute(a:linestr, '.*\(\<\k\{-}\%' . a:colnum . 'c\k\{-}\>\).*', '\1', '') "expand('<word>')
  let word = matchstr(a:linestr, '\k*\%' . a:colnum . 'c\k\+')
  if word == ''
    return ''
  endif
  return '\V\<' . word . '\>'
endfunction

let s:HiCursorWords_hiGroupRegexp = ''
function! s:HiCursorWords__execute()
  if exists("w:HiCursorWords__matchId")
    call matchdelete(w:HiCursorWords__matchId)
    unlet w:HiCursorWords__matchId
  endif

  let linestr = getline('.')
  let linenum = line('.')
  let colnum = col('.')

  "debug
  "echo s:HiCursorWords__getHiName(linenum, colnum)

  let word = s:HiCursorWords__getWordUnderTheCursor(linestr, linenum, colnum)
  if strlen(word) != 0
    if match(s:HiCursorWords__getHiName(linenum, colnum), s:HiCursorWords_hiGroupRegexp) == -1
      return
    endif
    let w:HiCursorWords__matchId = matchadd('WordUnderTheCursor', word, 0)
  endif
endfunction " }}}

" generate random number at end of current line 
" credit: http://mo.morsi.org/blog/node/299
" usage:
"   :Rand <CR>
"   :Rand 100<CR>
function! s:Rand(max)
y a         
redir @b    
ruby << EOF
  rmax = VIM::evaluate("a:max")
  rmax = nil if rmax == ""
  printf rand(rmax).to_s
EOF
redir END 
    let @a = strpart(@a, 0, strlen(@a) - 1)
    let @b = strpart(@b, 1, strlen(@b) - 1)
    let @c = @a . @b
    .s/.*/\=@c/g
endfunction
command! -nargs=? Rand :call <SID>Rand(<q-args>)

function! BreakBefore(s)
    execute ':%s/' . a:s . '/\r' . a:s . '/g'
endfunction
function! BreakAfter(s)
    "break after text like: 221= 
    "%s/\(\d\{3}=\)/\r\1/g
    execute ':%s/' . a:s . '/' . a:s . '\r/g'
endfunction

" =============================================================================
" normal mode
" =============================================================================
" Remove the Windows ^M - when the encodings gets messed up
" nnoremap <Leader>m mmHmt:%s/<C-V><cr>//ge<cr>'tzt'm

nnoremap & :&&<CR>
xnoremap & :&&<CR>
" Make Y consistent with C and D.
nnoremap Y y$
" copy selection to gui-clipboard
xnoremap Y "+y
" copy entire file contents (to gui-clipboard if available)
nnoremap yY :let b:winview=winsaveview()<bar>exe 'norm ggVG'.(has('clipboard')?'"+y':'y')<bar>call winrestview(b:winview)<cr>

set pastetoggle=<leader>pp

" paste current dir to command line
cabbrev ]c <c-r>=expand("%:p:h")<cr>

" delete the 'head' of a path on the command line
cno <c-o>dts <C-\>e<sid>deleteTillSlash()<cr>

func! s:deleteTillSlash()
  return s:is_windows ? substitute(getcmdline(), '\(.*[/\\]\).*', '\1', '') : substitute(getcmdline(), '\(.*[/]\).*', '\1', '')
endfunc


" abbreviations ===============================================================

iab xdate <c-r>=strftime("%d/%m/%Y %H:%M:%S")<cr>

"==============================================================================
" key mappings/bindings
"==============================================================================
" manage windows
nnoremap gw <c-w>
nnoremap gwW :setlocal winfixwidth<cr>
nnoremap gwF :setlocal winfixheight<cr>

" manage tabs
nnoremap gwe :tabnew<cr>
nnoremap gwT :wincmd T<cr>

nnoremap <leader>bdd :call <SID>buf_kill(1)<cr>
nnoremap <leader>bd! :call <SID>buf_kill(0)<cr>
nnoremap <leader>be :enew<cr>

" switch to the directory of the open buffer
nnoremap gcd :cd %:p:h<bar>pwd<cr>

func! BufDeath_Comparebuf(b1, b2)
  "prefer loaded buffers before unloaded buffers
  if bufloaded(a:b1)
    return bufloaded(a:b2) ? 0 : -1
  endif
  return !bufloaded(a:b2) ? 0 : 1
endf

func! s:buf_find_displayed_bufs()
  " all buffers displayed in windows in any tab.
  let s:displayedbufs = []
  for i in range(1, tabpagenr('$'))
    call extend(s:displayedbufs, tabpagebuflist(i))
  endfor
  return s:displayedbufs
endf

func! s:buf_findvalid()
  "valid 'next' buffers 
  "   EXCLUDE: current, unlisted, Unite, Vundle
  "   INCLUDE: normal buffers; 'help' buffers
  let l:valid_buffers = filter(range(1, bufnr('$')), 
              \ 'buflisted(v:val) '.
              \ '&& ("" ==# getbufvar(v:val, "&buftype") || "help" ==# getbufvar(v:val, "&buftype")) '.
              \ '&& v:val != bufnr("%") '.
              \ '&& bufname(v:val) !~# ''\*unite\*\|\[\(unite\|Vundle\)\]''')
              " \ '&& (a:valid_even_if_open_in_other_win || -1 == index(tabpagebuflist(), v:val)) '.
  call sort(l:valid_buffers, 'BufDeath_Comparebuf')
  return l:valid_buffers
endf

func! s:buf_switch_to_altbuff()
  let l:valid_buffers = s:buf_findvalid()

  if len(l:valid_buffers) > 0
    " change to the 'alternate' buffer iff it is a 'valid' buffer.
    if -1 < index(l:valid_buffers, bufnr("#"))
      buffer #
    else
      " just pick the first 'valid' buffer.
      exe 'buffer '.l:valid_buffers[0]
    endif
  endif
endf

" close the current buffer with a vengeance
func! s:buf_kill(mercy)
  let l:origbuf = bufnr("%")
  if a:mercy && &modified
    echom 'buffer has unsaved changes'
    return
  endif

  silent call <sid>buf_switch_to_altbuff()

  " obliterate the buffer and all of its related state (marks, local options, ...), 
  " _iff_ it is not displaying in a _different_ window in the _current_ tab.
  if bufexists(l:origbuf) && (-1 == bufwinnr(l:origbuf) || bufwinnr(l:origbuf) == bufwinnr(bufnr("%")))
    exe 'bwipeout! '.l:origbuf
  endif
endf

nnoremap <c-^> :call <sid>buf_switch_to_altbuff()<cr>

"move to first non-blank character
noremap 0 ^
"move to last character 
noremap - $

" un-join (split) the current line at the cursor position
nnoremap K i<cr><esc>k$
" delete without overwriting yank register
noremap <leader>d "_d
nnoremap <leader>D "_D

inoremap jj <esc>
inoremap kk <esc>l
nnoremap <left> zh
nnoremap <right> zl
nnoremap <c-d> <PageDown>
nnoremap <c-u> <PageUp>
nnoremap <space> :
xnoremap <space> :
nnoremap <leader>w :w!<cr>

"toggle/untoggle spell checking
nnoremap <leader>ss :setlocal spell!<cr>

"text bubbling: move text up/down with meta-[jk] 
nnoremap <M-j> m`:m+<cr>``
nnoremap <M-k> m`:m-2<cr>``
xnoremap <M-j> :m'>+<cr>gv
xnoremap <M-k> :m'<-2<cr>gv

" replay @q macro for each line of a visual selection
xnoremap @q :normal @q<CR>
" repeat last command for each line of a visual selection
xnoremap . :normal .<CR>

"j,k move by screen line instead of file line
nnoremap j gj
nnoremap k gk

" disable F1 help key
noremap! <F1> <nop>
noremap <F1> <nop>

" disable linewise undo
nnoremap U <nop>

" disable Ex mode shortcut
nnoremap Q <nop>

" turn off search highlighting
nnoremap <silent> <leader>hs :nohlsearch<cr>
" highlight current word
nnoremap <silent> <leader>hw :call <sid>HiCursorWords__execute()<cr>

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
autocmd BufWrite *.py :call TrimTrailingWhitespace()

au FileType python syn keyword pythonDecorator True None False self

" javascript ==================================================================

" golang ======================================================================
" possible godoc solution    https://gist.github.com/mattn/569652
"    Bundle 'thinca/vim-ref'
"    let g:ref_use_vimproc = 1
"    let g:ref_open = 'vsplit'
"    let g:ref_cache_dir = expand('~/.vim/tmp/ref_cache/')
"    nno <leader>K :<C-u>Unite ref/godoc -buffer-name=godoc -start-insert -horizontal<CR>

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

autocmd BufWrite *.go if exists("$GOPATH") | exe "keepjumps Fmt" | else | call TrimTrailingWhitespace() | endif
autocmd FileType go setlocal tabstop=4 shiftwidth=4 noexpandtab copyindent softtabstop=0 nolist

" abbreviations
au FileType go iab <buffer> ife if err != nil {<cr>log.Fatal(err)}<cr>
au FileType go iab <buffer> ,,e if err != nil {<cr>log.Fatal(err)}<cr>
au FileType go iab <buffer> er- if err != nil {<cr>log.Fatal(err)}<cr>

" clojure =====================================================================
"    https://github.com/tpope/vim-fireplace
"    https://github.com/guns/vim-clojure-static
"    https://github.com/guns/vim-sexp
"    https://bitbucket.org/kovisoft/slimv
"    http://kovisoft.bitbucket.org/tutorial.html


"==============================================================================
" vim grep/search/replace
"==============================================================================
nnoremap <leader>cc :botright copen<cr>
"toggle quickfix window
autocmd BufReadPost quickfix map <buffer> <leader>cc :cclose<cr>
autocmd BufReadPost quickfix map <buffer> <c-p> <up>
autocmd BufReadPost quickfix map <buffer> <c-n> <down>

" :noau speeds up vimgrep
noremap <leader>grep :<c-u>noau vimgrep // **<left><left><left><left>
" search and replace word under cursor
nnoremap <leader>sr :<c-u>%s/\<<c-r><c-w>\>//gc<left><left><left>
xnoremap <leader>sr :<c-u>%s/<c-r>=<SID>VSetSearch('/')<cr>//gc<left><left><left>

" https://github.com/thinca/vim-visualstar/blob/master/plugin/visualstar.vim
" makes * and # work on visual mode too.
function! s:VSetSearch(cmdtype)
  let l:temp = @s
  exe "normal! \<esc>gv\"sy"
  let l:foo = substitute(escape(@s, a:cmdtype.'\'), '\n', '\\n', 'g')
  let @s = l:temp
  return l:foo
endfunction

" in visual mode, press * or # to search for the current selection
xnoremap * /\V<C-R>=<SID>VSetSearch('/')<cr><cr>
xnoremap # ?\V<C-R>=<SID>VSetSearch('?')<cr><cr>

" recursively vimgrep for word under cursor or selection if you hit leader-star
nnoremap <leader>* :<c-u>noau vimgrep /\V<c-r><c-w>/ **<CR>
xnoremap <leader>* :<c-u>noau vimgrep /<c-r>=<SID>VSetSearch('/')<cr>/ **<CR>

" =============================================================================
" autocomplete / omnicomplete / tags
" =============================================================================
autocmd FileType css set omnifunc=csscomplete#CompleteCSS

" Don't scan included files. Tags file is more performant.
set complete-=i
set completeopt-=preview
set completeopt+=longest

if s:is_vimRecentBuildWithLua
    nnoremap <leader>neo :NeoCompleteEnable<cr>
    let g:neocomplete#enable_smart_case = 1
    if !exists('g:neocomplete#force_omni_input_patterns')
        let g:neocomplete#force_omni_input_patterns = {}
    endif
    if !exists('g:neocomplete#sources#omni#input_patterns')
        let g:neocomplete#sources#omni#input_patterns = {}
    endif
    let g:neocomplete#sources#omni#input_patterns.go = '[^.[:digit:] *\t]\.\w*'
    let g:neocomplete#force_omni_input_patterns.go = '[^.[:digit:] *\t]\.\w*'
else
    nnoremap <leader>neo :NeoComplCacheEnable<cr>
endif

set wildmode=full
set wildignore+=tags,*.o,*.obj,*.class,.git,.hg,.svn,*.pyc,*/tmp/*,*.so,*.swp,*.zip,*.exe,*.jar

if s:is_windows
    set wildignore+=Windows\\*,Program\ Files\\*,Program\ Files\ \(x86\)\\* 
    " TODO: https://github.com/ivalkeen/vim-ctrlp-tjump
    " let g:ctrlp_buftag_ctags_bin = '~/bin/ctags.exe'
endif

" let g:ctrlp_custom_ignore = {
"   \ 'dir':  '\v[\/]\.(git|hg|svn|cache)$|AppData|eclipse_workspace|grimoire-remote',
"   \ 'file': '\v\~$|\.(exe|so|dll|pdf|ntuser|blf|dat|regtrans-ms|o|swp|pyc|wav|mp3|ogg|blend)$' }

" important!: semicolon means 'walk up until tags/ is found'
set tags=./tags;,tags;
let g:easytags_auto_highlight = 0
let g:easytags_dynamic_files = 1

" Unite ======================================================================= {{{
try
  call unite#custom#profile('files', 'filters', 'sorter_rank')
catch E117
endtry
if exists('*unite#custom#profile')
let g:unite_source_history_yank_enable = 1
let g:unite_force_overwrite_statusline = 0

" see unite/custom.vim
call unite#custom#source(
            \ 'buffer,file_rec/async,file_rec,file_mru,directory_rec,outline', 
            \ 'sorters',
            \ ['sorter_ftime', 'sorter_rank'])
call unite#custom#source(
            \ 'buffer,file_rec/async,file_rec,file_mru,directory_rec,outline', 
            \ 'matchers',
            \ ['matcher_fuzzy'])
call unite#custom#source(
            \ 'file_rec/async,file_rec,file_mru', 
            \ 'converters',
            \ ['converter_relative_abbr', 'converter_file_directory'])

" extend default ignore pattern for file_rec source (same as directory_rec)
let s:file_rec_ignore = unite#get_all_sources('file_rec')['ignore_pattern'] .
    \ '\|\.\%(jar\|jpg\|gif\|png\)$' .
    \ '\|Downloads\|eclipse_workspace\|gwt-unitCache'
if s:is_windows
    let s:file_rec_ignore .= '\|AppData'
elseif s:is_mac
    let s:file_rec_ignore .= '\|Library'
endif
call unite#custom#source('file_rec,directory_rec', 'ignore_pattern', s:file_rec_ignore)

" search hidden directories:
" nnoremap <c-p>   :Unite -no-split -buffer-name=files  -start-insert file_rec:. directory_rec:. <cr>
nnoremap <c-p> :<C-u>Unite -no-split -buffer-name=files -start-insert file_mru file_rec <cr>
nnoremap <c-n> :<C-u>UniteWithBufferDir -no-split -buffer-name=filescurrbuff -start-insert file_rec<cr>
nnoremap <m-l> :<C-u>Unite -no-split -buffer-name=buffer -start-insert buffer<cr>
" auto-generates an outline of the current buffer
nnoremap <m-o> :<C-u>Unite -no-split -buffer-name=outline -start-insert outline<cr>
" tags are auto-generated by easytags.vim
nnoremap <m-t> :<C-u>Unite -no-split -buffer-name=tag -start-insert tag<cr>
nnoremap <m-y> :<C-u>Unite -no-split -buffer-name=yank history/yank<cr>
nnoremap <leader>cd :<C-u>Unite -no-split directory_mru directory_rec:. -start-insert -buffer-name=cd -default-action=cd<CR>
nnoremap <leader>ps :<C-u>Unite process -buffer-name=processes -start-insert<CR>

" Custom mappings for the unite buffer
autocmd FileType unite call s:unite_settings()
function! s:unite_settings()
  setlocal nolist nopaste
  nmap <buffer> <nowait> <esc> <Plug>(unite_exit)
  " refresh the cache
  nmap <buffer> <nowait> <F5>  <Plug>(unite_redraw)
  imap <buffer> <nowait> <F5>  <Plug>(unite_redraw)
  " change directories in unite
  nmap <buffer> <nowait> <leader>cd <Plug>(unite_restart)
endfunction

"TODO: :keepalt
function! s:clearUniteBuffers()
  "find [unite] or *unite* buffers to be wiped-out
  "TODO: unite-outline buffers are listed (bug in unite-outline?), so we can't test !buflisted(v:val)
  let displayedbufs = <sid>buf_find_displayed_bufs()
  let unitebuffs = filter(range(1, bufnr('$')), 
        \ '"nofile" ==# getbufvar(v:val, "&buftype") 
        \  && -1 == index(displayedbufs, v:val) 
        \  && bufname(v:val) =~# ''*unite*\|\[unite\]''')

  " obliterate the buffers and their related state (marks especially).
  if !empty(unitebuffs)
    exe 'bwipeout! '.join(unitebuffs, ' ')
  endif
endfunction

" delete empty, non-alternate, non-visible, non-special buffers
" ensure there is always a useful 'alternate' buffer
" ensure that the next switched-to buffer is not already displayed in some other window in the current tab
function! s:clear_empty_buffers()
  " test for empty buffer:
  "   expand('%') == '' && !&modified && line('$') <= 1 && getline(1) == ''
  " :bufdo if line2byte(line("$")+1)<=2 | call add(empty_bufs, bufnr("%")) | endif
  " if the buffer is loaded: we can just check its contents via getbufline() or ZyX's method.
  let displayedbufs = <sid>buf_find_displayed_bufs()
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
augroup clearuselessbuffers
  au!
autocmd BufEnter * silent call <sid>clearUniteBuffers()
autocmd BufEnter * silent call <sid>clear_empty_buffers()
augroup END

endif "}}}

" session  ====================================================================
set sessionoptions-=globals
set sessionoptions-=blank
let s:sessiondir  = expand("~/.vim/sessions")
let s:sessionfile = expand(s:sessiondir . "/session.vim")
let s:sessionlock = expand(s:sessiondir . "/session.lock")
function! LoadSession()
  if filereadable(s:sessionlock) " session already open in another vim
    return 
  elseif !isdirectory(s:sessiondir) && !EnsureDir(s:sessiondir)
      echoerr "failed to create session dir: " . s:sessiondir 
  endif

  "write the session 'lock' file
  silent exe "new | w " . s:sessionlock . " | bd"
  
  if filereadable(s:sessionfile)
    "open the session
    exe 'source ' s:sessionfile
  endif

  exe 'Obsession '.s:sessionfile
  "unlock the session
  autocmd VimLeave * silent exe s:is_windows ? '!del ' . s:sessionlock : '!rm -f ' . s:sessionlock 
endfunction


" auto commands ===============================================================
if has("autocmd")
    " Jump to the last position when reopening a file
    au BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g'\"" | endif

    " force windows to be sized equally after viewport resize
    au VimResized * wincmd =

    if s:is_gui
        " set viminfo+=% "remember buffer list
        autocmd VimEnter * :call LoadSession()
    endif

    if s:is_windows
        " use .viminfo instead of _viminfo
        set viminfo+=n~/.viminfo
        " always maximize initial GUI window size
        autocmd GUIEnter * simalt ~x 
    endif
endif

" tree view
let g:netrw_liststyle = 3
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
  if exists('+undodir') && &undodir =~# '^\.\%(,\|$\)'
    let &undodir = expand(s:dir) . '/undo//,' . &undodir
  endif

  if exists('+undofile')
    set undofile
  endif
endif

