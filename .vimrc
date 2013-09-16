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
endif

let mapleader = ","
let g:mapleader = ","
nnoremap <silent> \ :norm! ,<cr>

let s:is_cygwin = has('win32unix') || has('win64unix')
let s:is_windows = has('win32') || has('win64')
let s:is_mac = has('gui_macvim') || has('mac')
let s:is_unix = has('unix')
let s:is_msysgit = (has('win32') || has('win64')) && $TERM ==? 'cygwin'
let s:is_tmux = !empty($TMUX)
let s:is_ssh = !empty($SSH_TTY)
let s:is_cygwin_ssh = !empty($SSH_CYGWIN) && $SSH_CYGWIN
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
Bundle 'kshenoy/vim-signature'
Bundle 'jiangmiao/auto-pairs'
Bundle 'zhaocai/DirDiff.vim'
Bundle 'justinmk/TextObjectify'
Bundle 'kana/vim-textobj-user'
Bundle 'kana/vim-textobj-indent'
Bundle 'gaving/vim-textobj-argument'
if !s:is_cygwin
Bundle 'Valloric/MatchTagAlways'
endif
Bundle 'bling/vim-airline'
Bundle 'PProvost/vim-ps1'
Bundle 'tomtom/tcomment_vim'
Bundle 'ap/vim-css-color'
Bundle 'airblade/vim-gitgutter'
Bundle 'derekwyatt/vim-scala'
if exists("$GOPATH")
Bundle 'Blackrush/vim-gocode'
endif
Bundle 'justinmk/vim-ipmotion'
Bundle 'justinmk/vim-gtfo'
Bundle 'xolox/vim-misc'
Bundle 'xolox/vim-easytags'
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
" let g:airline#extensions#tabline#buffer_nr_show = 1
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
    set <m-p>=p <m-b>=b <m-o>=o <m-y>=y <m-j>=j <m-k>=k <m-r>=r
          \ <m-t>=t <m-l>=l <m-h>=h
endif

try | lang en_US | catch | endtry

if s:is_msysgit
  set listchars=tab:>\ ,trail:.,extends:>,precedes:<,nbsp:+
elseif s:is_windows || s:is_cygwin || s:is_ssh
  set listchars=tab:▸\ ,trail:▫,extends:>,precedes:<,nbsp:+
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
if !s:has_plugins
set showtabline=1
endif
if s:has_plugins
set noshowmode " Hide the default mode text (e.g. -- INSERT -- below the statusline)
endif
set foldmethod=marker
set splitright
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

function! BreakBefore(s)
    execute ':%s/' . a:s . '/\r' . a:s . '/g'
endfunction
function! BreakAfter(s)
    "break after text like: 221= 
    "%s/\(\d\{3}=\)/\r\1/g
    execute ':%s/' . a:s . '/' . a:s . '\r/g'
endfunction

" vim-sneak: minimalist vertical movement in Vim
"   - repeatable with ; or ,
"   - without breaking f t ; ,
"   - without replacing the / register
"   - without triggering search results highlighting
"   - only shows highlights in current window
"   - (todo!) without adding noise to the / history
"   - (todo?) doesn't add to your jumps (use ; or , instead)
"   - does not wrap (wrap makes no sense)
"   - (todo) highlights additional matches until a key other than ; or , is pressed
"   - range => restrict search column to +/- range size
" TODO: dot-repeat; visual mode; map something other than F10; multibyte chars?
" see also: easymotion, seek.vim, cleverf, https://github.com/svermeulen/vim-extended-ft
" g@ and vim-repeat example: https://github.com/tpope/vim-commentary/blob/master/plugin/commentary.vim
func! SneakToString(op, s, isrepeat, isreverse) abort "TODO: range
  " echom 'v:count/prev='.v:count.'/'.v:prevcount.' v:reg='.v:register.' v:op='.v:operator

  if empty(a:s) "user canceled
    redraw | echo '' | return
  endif

  " do not wrap
  let l:searchoptions = 'W'
  " search backwards
  if a:isreverse | let l:searchoptions .= 'b' | endif
  " save the jump on the initial invocation, _not_ repeats.
  if !a:isrepeat | let l:searchoptions .= 's' | endif

  if !empty(a:op)
    let l:histreg = @/
    " let [l:lin, l:col] = searchpos('\C\V'.a:s, l:searchoptions)
    try
      "until we can find a better way, operate on / and restore the history immediately after
      exec 'norm! '.a:op.(a:isreverse ? '?' : '/').a:s."\<cr>"
    catch E486
      redraw | echo 'not found: '.a:s | return
    finally
      call histdel("/", histnr("/"))
      let @/ = l:histreg
    endtry
  elseif !search('\C\V'.a:s, l:searchoptions) "jump to the first match, or exit
    redraw | echo 'not found: '.a:s | return
  endif

  silent! call matchdelete(w:sneak_hl_id)

  let l:startline_str = string(line('.') + (a:isreverse ? 1 : -1))
  let l:startcol_str  = string(col('.')  + (a:isreverse ? 1 : -1))

  "highlight matches at or below (above) the cursor position.
  "    example: highlight string "ab" after line 42, column 5 
  "             matchadd('foo', 'ab\%>42l\%5c', 1)
  let l:gt_lt = a:isreverse ? '<' : '>'
  let l:pattern = a:s.'\%'.l:gt_lt.l:startline_str.'l'

  if !a:isrepeat
    "this is a new search; set up the repeat mappings.
    exec printf('nnoremap <silent> ; :<c-u>call SneakToString("", "%s", 1, %d)'."\<cr>", escape(a:s, '"\'),  a:isreverse)
    exec printf('nnoremap <silent> \ :<c-u>call SneakToString("", "%s", 1, %d)'."\<cr>", escape(a:s, '"\'), !a:isreverse)

    "if f/F/t/T is invoked, unmap the temporary repeat mappings
    if empty(maparg("f", "n").maparg("F", "n").maparg("t", "n").maparg("T", "n"))
      nmap <silent> f <F10>f|nmap <silent> F <F10>F|nmap <silent> t <F10>t|nmap <silent> T <F10>T
    endif

    "on the initial invocation, only show matches after (before) the initial position.
    let l:pattern .= '\%'.l:gt_lt."''"

    augroup SneakPlugin
      autocmd!
      autocmd InsertEnter <buffer> silent! call matchdelete(w:sneak_hl_id)
    augroup END
  endif

  "perform the highlight...
  "  - scope to window because matchadd() highlight is per-window.
  "  - re-use w:sneak_hl_id if it exists (-1 lets matchadd() choose).
  let w:sneak_hl_id = matchadd('SneakPluginMatch', l:pattern, 1, get(w:, 'sneak_hl_id', -1))

endf
func! s:getInputChar()
  let l:c = getchar()
  return type(l:c) == type(0) ? nr2char(l:c) : l:c
endf
func! s:getNextNChars(n)
  let l:s = ''
  for i in range(1, a:n)
    let l:c = <sid>getInputChar()
    if -1 != index(["\<esc>", "\<c-c>", "\<backspace>", "\<del>"], l:c)
      return ""
    endif
    let l:s .= l:c
    redraw | echo l:s
  endfor
  return l:s
endf

augroup SneakPluginInit
  autocmd!
  if &background ==# 'dark'
    highlight SneakPluginMatch guifg=black guibg=white ctermfg=black ctermbg=white
    autocmd ColorScheme * highlight SneakPluginMatch guifg=black guibg=white ctermfg=black ctermbg=white
  else
    highlight SneakPluginMatch guifg=white guibg=black ctermfg=white ctermbg=black
    autocmd ColorScheme * highlight SneakPluginMatch guifg=white guibg=black ctermfg=white ctermbg=black
  endif
augroup END

nnoremap <F10> :<c-u>unmap f<bar>unmap F<bar>unmap t<bar>unmap T<bar>unmap ;<bar>silent! call matchdelete(w:sneak_hl_id)<cr>
nnoremap <silent> s :<c-u>call SneakToString('', <sid>getNextNChars(2), 0, 0)<cr>
nnoremap <silent> S :<c-u>call SneakToString('', <sid>getNextNChars(2), 0, 1)<cr>
"{op}v{motion} actually has a purpose in stock vim, but it is equivalent to v<motion><operator>
onoremap <silent> z :<c-u>call SneakToString(v:operator, <sid>getNextNChars(2), 0, 0)<cr>
onoremap <silent> Z :<c-u>call SneakToString(v:operator, <sid>getNextNChars(2), 0, 1)<cr>
" xnoremap <silent> <leader>s <esc>:<c-u>call SneakToString(visualmode(),...)<cr>
" xnoremap <silent> <leader>S <esc>:<c-u>call SneakToString(visualmode(),...)<cr>

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

" paste current dir to command line
cabbrev ]c <c-r>=expand("%:p:h")<cr>

" delete the 'head' of a path on the command line
cno <c-o>dts <C-\>e<sid>deleteTillSlash()<cr>

func! s:deleteTillSlash()
  return s:is_windows ? substitute(getcmdline(), '\(.*[/\\]\).*', '\1', '') : substitute(getcmdline(), '\(.*[/]\).*', '\1', '')
endfunc


" abbreviations ===============================================================

iab date- <c-r>=strftime("%d/%m/%Y %H:%M:%S")<cr>

"==============================================================================
" key mappings/bindings
"==============================================================================
" manage windows
nnoremap gw <c-w>
nnoremap gwW :setlocal winfixwidth<cr>
nnoremap gwF :setlocal winfixheight<cr>
nnoremap gwV :vnew<cr>

" manage tabs
nnoremap gwe :tabnew<cr>
nnoremap gwC :tabclose<cr>
nnoremap gwT :wincmd T<cr>

" manage buffers
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
  " all buffers displayed in any window, any tab.
  let s:displayedbufs = []
  for i in range(1, tabpagenr('$'))
    call extend(s:displayedbufs, tabpagebuflist(i))
  endfor
  return s:displayedbufs
endf

func! s:buf_find_valid_next_bufs()
  "valid 'next' buffers 
  "   EXCLUDE: current, unlisted, Unite
  "   INCLUDE: normal buffers; 'help' buffers
  let l:valid_buffers = filter(range(1, bufnr('$')), 
              \ 'buflisted(v:val) 
              \  && ("" ==# getbufvar(v:val, "&buftype") || "help" ==# getbufvar(v:val, "&buftype")) 
              \  && v:val != bufnr("%") 
              \  && -1 == index(tabpagebuflist(), v:val) 
              \ ')
  call sort(l:valid_buffers, 'BufDeath_Comparebuf')
  return l:valid_buffers
endf

func! s:buf_switch_to_altbuff()
  " change to the 'alternate' buffer if it exists
  if -1 != bufnr("#")
    buffer #
  else " change to first 'valid' buffer
    let l:valid_buffers = s:buf_find_valid_next_bufs()
    if len(l:valid_buffers) > 0
      exe 'buffer '.l:valid_buffers[0]
    endif
  endif
endf

" close the current buffer with a vengeance
func! s:buf_kill(mercy)
  let l:origbuf = bufnr("%")
  let l:origbufname = bufname(l:origbuf)
  if a:mercy && &modified
    echom 'buffer has unsaved changes (use '.mapleader.'d! to override)'
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

" disable Ex mode shortcut
nnoremap Q <nop>

func! ReadExCommandOutput(cmd)
  redir => l:message
  silent execute a:cmd
  redir END
  "tabnew
  silent put=l:message
  "set nomodified
endf
command! -nargs=+ -complete=command R call ReadExCommandOutput(<q-args>)

" golang ======================================================================
" possible godoc solution    https://gist.github.com/mattn/569652
"    Bundle 'thinca/vim-ref'
"    let g:ref_use_vimproc = 1
"    let g:ref_open = 'vsplit'
"    let g:ref_cache_dir = expand('~/.vim/tmp/ref_cache/')
"    nno <leader>K :<C-u>Unite ref/godoc -buffer-name=godoc -start-insert -horizontal<CR>
augroup vimrc_golang
  autocmd!
  autocmd FileType go iab <buffer> err- if err != nil {<cr>log.Fatal(err)}<cr>
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
" :h 'iskeyword' =>  "When the 'lisp' option is on the '-' character is always included."
"    https://github.com/tpope/vim-fireplace
"    https://github.com/guns/vim-clojure-static
"    https://github.com/guns/vim-sexp
"    https://bitbucket.org/kovisoft/slimv
"    http://kovisoft.bitbucket.org/tutorial.html


"==============================================================================
" vim grep/search/replace
"==============================================================================
nnoremap <leader>qq :botright copen<cr>
augroup BufferDeath
  autocmd!
  " on BufLeave:
  "   1. remove existing autocomand, if any
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
  "toggle quickfix window
  autocmd BufReadPost quickfix map <buffer> <leader>qq :cclose<cr>|map <buffer> <c-p> <up>|map <buffer> <c-n> <down>

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

  "highlight line in the current window only
  autocmd VimEnter,WinEnter,BufWinEnter * setlocal cursorline
  autocmd WinLeave * setlocal nocursorline

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
nnoremap <c-n> :<C-u>UniteWithBufferDir -no-split -buffer-name=filescurrbuff -start-insert file_rec<cr>
nnoremap <m-l> :<C-u>Unite -no-split -buffer-name=buffer -start-insert buffer<cr>
" auto-generates an outline of the current buffer
nnoremap <m-o> :<C-u>Unite -no-split -buffer-name=outline -start-insert outline<cr>
" tags are auto-generated by easytags.vim
nnoremap <m-t> :<C-u>Unite -no-split -buffer-name=tag -start-insert tag<cr>
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

if s:is_cygwin || s:is_cygwin_ssh
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
