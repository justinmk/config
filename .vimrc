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

let s:vimrc_first_run = !exists("mapleader")
if s:vimrc_first_run
  " ensure that we always start with vim defaults (as opposed to those set by the current system)
  set all&
  " caution: this resets many settings, eg 'history'
  set nocompatible
endif

" removing this breaks alt/meta mappings (on win32 gvim at least).
set encoding=utf-8

if exists('&guioptions')
    "no toolbar, no menu bar, no left scroll bar
    set guioptions-=T guioptions-=m guioptions-=L guioptions-=l
    "don't source &runtimepath/menu.vim. (must be done before 'filetype on' / 'syntax on')
    set guioptions-=M
    "use console dialogs instead of popup dialogs for simple choices.
    set guioptions+=rc
    " disable cursor blinking for all modes
    set guicursor+=a:blinkon0
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
let s:is_vimRecentBuildWithLua = has('lua') && (v:version > 703 || (v:version == 703 && has('patch885')))
let s:has_eclim = isdirectory(expand("~/.vim/eclim"))

if s:is_windows && !s:is_cygwin && !s:is_msysgit
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
"==============================================================================

"boostrap vundle on new systems
fun! InstallVundle()
    echo "Installing Vundle..."
    silent !mkdir -p ~/.vim/bundle
    silent !git clone https://github.com/gmarik/vundle.git ~/.vim/bundle/vundle
endfun

let s:has_plugins=isdirectory(expand("~/.vim/bundle/vundle"))
if s:has_plugins "{{{

filetype off " required!

set runtimepath+=~/.vim/bundle/vundle/
call vundle#rc() 

" let Vundle manage Vundle (required!)
Bundle 'gmarik/vundle'

Bundle 'tomasr/molokai'
if !s:is_windows
Bundle 'benmills/vimux'
Bundle 'tpope/vim-tbone'
endif
Bundle 'sjl/clam.vim'
Bundle 'vim-scripts/dbext.vim'
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
Bundle 'tpope/vim-speeddating'
Bundle 'tpope/vim-endwise'
Bundle 'kshenoy/vim-signature'
" Bundle 'jiangmiao/auto-pairs'
Bundle 'zhaocai/DirDiff.vim'
" Bundle 'justinmk/TextObjectify'
Bundle 'kana/vim-textobj-user'
Bundle 'kana/vim-textobj-indent'
Bundle 'gaving/vim-textobj-argument'
if !s:is_cygwin && has('python')
" delimiter highlighting? https://github.com/mhinz/vim-blockify/blob/master/plugin/blockify.vim
Bundle 'Valloric/MatchTagAlways'
endif
Bundle 'bling/vim-airline'
Bundle 'PProvost/vim-ps1'
Bundle 'tomtom/tcomment_vim'
Bundle 'chrisbra/color_highlight'
Bundle 'airblade/vim-gitgutter'
Bundle 'derekwyatt/vim-scala'
if exists("$GOPATH")
Bundle 'Blackrush/vim-gocode'
endif
Bundle 'justinmk/vim-ipmotion'
Bundle 'justinmk/vim-gtfo'
Bundle 'justinmk/vim-sneak'
Bundle 'xolox/vim-misc'
Bundle 'tsukkee/unite-tag'
Bundle 'Shougo/unite.vim'
Bundle 'Shougo/unite-outline'
if s:is_vimRecentBuildWithLua
Bundle 'Shougo/neocomplete.vim'
endif

endif "}}}

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

func! EnsureFile(path)
  let l:path = expand(a:path)
  if !filereadable(l:path) && -1 == writefile([''], l:path)
    echoerr "failed to create file: ".l:path
  endif
endf

"==============================================================================
" general settings / options
"==============================================================================
let g:gitgutter_escape_grep = 1
let g:gitgutter_eager = 0
if s:is_windows
  let g:gitgutter_realtime = 0
endif

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
    set <m-b>=b <m-h>=h <m-j>=j <m-k>=k <m-l>=l
          \ <m-o>=o <m-p>=p <m-r>=r
          \ <m-t>=t <m-y>=y
          \ <m-]>=]
    "duds?: m-d
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

set report=0
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
set magic " change the way backslashes are used in search patterns
set mat=5     " showmatch time (tenths of a second)
set noerrorbells
set novb t_vb=
set timeoutlen=3000
set nonumber
set background=dark
if s:has_plugins
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

        let g:Powerline_symbols = 'unicode'
    endif
elseif s:is_gui "linux or other
    set guifont=Monospace\ 10
endif

"colorscheme {{{
  if s:has_plugins && &t_Co != 88 && &t_Co < 256 && (s:is_tmux || &term =~? 'xterm')
    " force colors
    set t_Co=256
  endif

  let s:color_force_high_contrast = ' 
        \   hi Normal     ctermbg=black guibg=black ctermfg=white guifg=white
        \ | hi NonText    ctermbg=black guibg=black ctermfg=white guifg=white
        \'

  if !s:is_mac
    exe s:color_force_high_contrast
    if s:vimrc_first_run
      exe 'autocmd ColorScheme * '.s:color_force_high_contrast
    endif
  endif

  if s:is_msysgit
    hi CursorLine  term=NONE cterm=NONE ctermfg=NONE ctermbg=darkblue guifg=NONE guibg=NONE
    hi ColorColumn term=NONE cterm=NONE ctermfg=NONE ctermbg=darkblue guifg=NONE guibg=NONE
  elseif !s:is_gui && &t_Co <= 88
    hi CursorLine cterm=underline
  else
    "see :h 'highlight'
    "https://github.com/Pychimp/vim-luna
    "hi Comment ctermfg=Cyan guifg=#afafaf
    let s:color_override = ' 
          \   hi Comment       guifg=#afafaf               gui=NONE  ctermfg=102               cterm=NONE
          \ | hi Visual        guifg=#ffffff guibg=#ff5f00 gui=NONE  ctermfg=255  ctermbg=202  cterm=NONE
          \ | hi VisualNOS     guifg=#ffffff guibg=#ff5f00 gui=NONE  ctermfg=255  ctermbg=202  cterm=NONE
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

    if s:vimrc_first_run "avoid re-triggering 'ColorScheme' event
      " expects &runtimepath/colors/{name}.vim.
      colorscheme molokai
    endif

    if s:is_gui || s:is_mac || s:is_cygwin
      if s:vimrc_first_run
        exe 'autocmd ColorScheme * '.s:color_override
      endif
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
if v:version > 703 || v:version == 703 && has("patch541")
  " Delete comment character when joining commented lines
  set formatoptions+=j
endif
" don't syntax-highlight long lines
set synmaxcol=300

set expandtab
set tabstop=2
set shiftwidth=2
set smarttab " Use 'shiftwidth' when using <Tab> in front of a line. By default it's used only for shift commands ("<", ">").

set linebreak "wrap long lines at 'breakat' character
set textwidth=500

set autoindent " Autoindent when starting new line, or using 'o' or 'O'.
set smartindent
set nowrap 

" =============================================================================
" util functions
" =============================================================================

" :help :DiffOrig
command! DiffOrig leftabove vnew | set bt=nofile | r ++edit # | 0d_ | diffthis | wincmd p | diffthis

func! TrimTrailingWhitespace()
  let winview=winsaveview()
  %s/\s\+$//ge
  call winrestview(winview)
endfunc

"return the syntax highlight group under the cursor
function! GetSyntaxName()
    let l:name = synIDattr(synID(line('.'),col('.'),1),'name')
    return l:name == '' ? '' : '[' . l:name . ']'
endfunction

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

func! AppendToFile(file, lines)
  let l:file = expand(a:file)
  call EnsureFile(l:file)
  "credit ZyX: http://stackoverflow.com/a/8976314/152142
  call writefile(readfile(l:file)+a:lines, l:file)
endf

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

" delete the 'head' of a path on the command line
cno <c-o>dts <C-\>e<sid>deleteTillSlash()<cr>

func! s:deleteTillSlash()
  return s:is_windows ? substitute(getcmdline(), '\(.*[/\\]\).*', '\1', '') : substitute(getcmdline(), '\(.*[/]\).*', '\1', '')
endfunc


" abbreviations ===============================================================

iab date- <c-r>=strftime("%d/%m/%Y %H:%M:%S")<cr>

" paste current dir to command line
cabbrev ]c <c-r>=expand("%:p:h")<cr>

cabbrev h h <bar> wincmd H<left><left><left><left><left><left><left><left><left><left><left>

"==============================================================================
" key mappings/bindings
"==============================================================================
" manage windows
nnoremap gw <c-w>
nnoremap gwW :setlocal winfixwidth!<bar>echo 'winfixwidth='.&winfixwidth<cr>
nnoremap gwF :setlocal winfixheight!<bar>echo 'winfixheight='.&winfixheight<cr>
nnoremap gwV :vnew<cr>

" manage tabs
nnoremap gwN :tabnew<cr>
nnoremap gwC :tabclose<cr>
nnoremap gwT :wincmd T<cr>

" manage buffers
nnoremap <leader>bd :<c-u>call <SID>buf_kill(1)<cr>
nnoremap <leader>b! :<c-u>call <SID>buf_kill(0)<cr>
nnoremap <leader>bn  :<c-u>enew<cr>
nnoremap gb :<c-u>exec (v:count ? 'b '.v:count : 'bn')<cr>

" set working directory to the current buffer's directory
nnoremap <leader>cw :cd %:p:h<bar>pwd<cr>

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
noremap 0 ^
"move to last character 
noremap - $

" un-join (split) the current line at the cursor position
nnoremap K i<cr><esc>k$
" delete without overwriting the default register
nnoremap <leader>d "_d
xnoremap <leader>d "_d
nnoremap <leader>D "_D

inoremap jj <esc>
inoremap kk <esc>l
nnoremap ' `
xnoremap ' `
nnoremap <C-e> 2<C-e>
nnoremap <C-y> 2<C-y>
nnoremap <left> 4zh
nnoremap <right> 4zl
nnoremap <c-d> <PageDown>
nnoremap <c-u> <PageUp>
nnoremap <space> :
xnoremap <space> :
nnoremap <leader>w :w!<cr>

" map m-] to be the inverse of c-]
nnoremap <m-]> <c-t>

" always 'very magic'
noremap / /\v

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

func! ReadExCommandOutput(cmd)
  redir => l:message
  silent execute a:cmd
  redir END
  "tabnew
  silent put=l:message
  "set nomodified
endf
command! -nargs=+ -complete=command R call ReadExCommandOutput(<q-args>)

" java ========================================================================
augroup vimrc_java
  autocmd!
  autocmd FileType java setlocal tabstop=4 shiftwidth=4 noexpandtab copyindent softtabstop=0 nolist
  if s:has_eclim
    autocmd FileType java nnoremap <buffer> gd :<c-u>JavaSearchContext<cr>
          \ | nnoremap <buffer> <c-t> :<c-u>JavaHierarchy<cr>
          \ | nnoremap <buffer> gjoi  :<c-u>JavaImportOrganize<cr>
          \ | nnoremap <buffer> <F2>  :<c-u>JavaDocPreview<cr>
  endif
augroup END


" golang ======================================================================
" possible godoc solution    https://gist.github.com/mattn/569652
"    Bundle 'thinca/vim-ref'
"    let g:ref_use_vimproc = 1
"    let g:ref_open = 'vsplit'
"    let g:ref_cache_dir = expand('~/.vim/tmp/ref_cache/')
"    nno <leader>K :<C-u>Unite ref/godoc -buffer-name=godoc -start-insert -horizontal<CR>
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

" clojure =====================================================================
" When the 'lisp' option is on, the '-' character is considered a 'iskeyword' character.
"    https://github.com/tpope/vim-fireplace
"    https://github.com/guns/vim-clojure-static
"    https://github.com/guns/vim-sexp
"    https://bitbucket.org/kovisoft/slimv
"    http://kovisoft.bitbucket.org/tutorial.html

" csharp ======================================================================
" $COMSPEC /k "C:/Program Files (x86)/Microsoft Visual Studio 11.0/Common7/Tools/vsvars32.bat"

"==============================================================================
" vim grep/search/replace
"==============================================================================
nnoremap <leader>qq :botright copen<cr>
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
  autocmd BufReadPost quickfix map <buffer> map <buffer> <c-p> <up>|map <buffer> <c-n> <down>

  autocmd FileType unite call s:unite_settings()
  " obliterate unite buffers (marks especially).
  autocmd BufLeave \[unite\]* if "nofile" ==# &buftype | setlocal bufhidden=wipe | endif

  " Jump to the last position when reopening a file
  autocmd BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g`\"" | endif

  " force windows to be sized equally after viewport resize
  autocmd VimResized * wincmd =

  autocmd FileType text setlocal tabstop=4 shiftwidth=4

  autocmd FileType css set omnifunc=csscomplete#CompleteCSS

  autocmd BufWrite *.py :call TrimTrailingWhitespace()
  autocmd FileType python syn keyword pythonDecorator True None False self

  autocmd BufRead,BufNewFile *.vrapperrc setlocal ft=vim

  "highlight in the current window only
  autocmd VimEnter,WinEnter,BufWinEnter * setlocal cursorline cursorcolumn colorcolumn=80
  autocmd WinLeave * setlocal nocursorline nocursorcolumn colorcolumn=

  if s:is_windows
    " always maximize initial GUI window size
    autocmd GUIEnter * simalt ~x 
  endif
augroup END

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
" Don't scan includes; tags file is more performant.
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
endif

set wildmode=full
set wildignore+=tags,*.o,*.obj,*.class,.git,.hg,.svn,*.pyc,*/tmp/*,*.so,*.swp,*.zip,*.exe,*.jar,gwt-unitCache/*,*.cache.html

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

if s:has_plugins "unite.vim =============================================== {{{
call unite#custom#profile('files', 'filters', 'sorter_rank')

let g:unite_source_history_yank_enable = 1
let g:unite_force_overwrite_statusline = 0

" TODO: https://github.com/bling/dotvim/blob/master/vimrc#L535
"       https://github.com/Shougo/unite.vim/issues/347
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
nnoremap <m-l> :<C-u>Unite -no-split -buffer-name=buffer -start-insert buffer<cr>
" auto-generates an outline of the current buffer
nnoremap <m-o> :<C-u>Unite -no-split -buffer-name=outline -start-insert outline<cr>
nnoremap <m-t> :<C-u>Unite -no-split -buffer-name=tag -start-insert tag/include<cr>
nnoremap <m-y> :<C-u>Unite -no-split -buffer-name=yank -start-insert history/yank<cr>
nnoremap <leader>cd :<C-u>Unite -no-split directory_mru directory_rec:. -start-insert -buffer-name=cd -default-action=cd<CR>
nnoremap <leader>ps :<C-u>Unite process -buffer-name=processes -start-insert<CR>

" Custom mappings for the unite buffer
function! s:unite_settings()
  setlocal nopaste
  nmap <buffer> <nowait> <esc> <Plug>(unite_exit)
  " refresh the cache
  nmap <buffer> <nowait> <F5>  <Plug>(unite_redraw)
  imap <buffer> <nowait> <F5>  <Plug>(unite_redraw)
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

" r!wget -qO - https://raw.github.com/tpope/vim-sensible/master/plugin/sensible.vim
