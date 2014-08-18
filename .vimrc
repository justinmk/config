" r!wget -qO - https://raw.github.com/tpope/vim-sensible/master/plugin/sensible.vim
"
" windows builds: https://bitbucket.org/Haroogan/vim-for-windows/downloads
"                 http://tuxproject.de/projects/vim/
"                 http://files.kaoriya.net/vim/
"                 64-bit: http://solar-blogg.blogspot.ca/p/vim-build.html
" neovim:
"   msys:
"       http://sourceforge.net/projects/msys2/files/Alpha-versions/
"       Synchronize package databases
"         $ pacman -Sy
"       Install packages/groups:
"         $ pacman -S openssh
"         $ pacman -S base-devel
"         $ pacman -S msys2-devel
"   mingw:
"       http://mingw-w64.sourceforge.net/download.php#mingw-builds
"       http://nuwen.net/mingw.html
"   julia build moving to msys2:
"       https://github.com/JuliaLang/julia/issues/3640
"   julia msys2 build instructions:
"       https://github.com/JuliaLang/julia/pull/5982
"
" MacVim with homebrew:
"   brew install macvim --with-cscope --with-luajit --HEAD --override-system-vim
"   brew linkapps --system
"
" If this .vimrc is not in $HOME, add these lines to $HOME/.vimrc :
"    set runtimepath+=/path/to/.vim
"    source /path/to/.vimrc
"==============================================================================
" Windows Registry Editor Version 5.00
"
" [HKEY_CLASSES_ROOT\*\shell\Edit with Vim]
"
" [HKEY_CLASSES_ROOT\*\shell\Edit with Vim\command]
" @="C:\\opt\\vim\\gvim.exe \"%L\""
"==============================================================================

if has('vim_starting')
  " ensure that we always start with Vim defaults (as opposed to those set by the current system)
  set all&
endif

" required for alt/meta mappings  https://github.com/tpope/vim-sensible/issues/69
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

let s:is_cygwin = has('win32unix') || has('win64unix') "treat this as mintty
let s:is_windows = has('win32') || has('win64')
let s:is_mac = has('gui_macvim') || has('mac')
let s:is_msysgit = (has('win32') || has('win64')) && $TERM ==? 'cygwin'
let s:is_tmux = !empty($TMUX)
let s:is_ssh = !empty($SSH_TTY)
let s:lua_patch885 = has('lua') && (v:version > 703 || (v:version == 703 && has('patch885')))
let s:has_eclim = isdirectory(expand("~/.vim/eclim", 1))
let s:plugins=isdirectory(expand("~/.vim/bundle/vundle", 1))

if has('vim_starting') && s:is_windows && !s:is_cygwin && !s:is_msysgit
  set runtimepath+=~/.vim/
endif

" 'is GUI' means vim is _not_ running within the terminal.
" sample values:
"   &term  = win32 //vimrc running in msysgit terminal
"   $TERM  = xterm-color , cygwin
"   &term  = builtin_gui //*after* vimrc but *before* gvimrc
"   &shell = C:\Windows\system32\cmd.exe , /bin/bash
let s:is_gui = has('gui_running') || strlen(&term) == 0 || &term ==? 'builtin_gui'

if !s:plugins "{{{

fun! InstallVundle() "bootstrap vundle on new systems
    silent call mkdir(expand("~/.vim/bundle", 1), 'p')
    silent !git clone https://github.com/gmarik/vundle.git ~/.vim/bundle/vundle
endfun

else

filetype off " required!

if has('vim_starting')
  set runtimepath+=~/.vim/bundle/vundle/
endif

call vundle#rc() 

Plugin 'gmarik/vundle' " let Vundle manage Vundle (required!)

Plugin 'tomasr/molokai'
Plugin 'noahfrederick/vim-hemisu'
Plugin 'justinmk/vim-ipmotion'
Plugin 'justinmk/vim-gtfo'
Plugin 'justinmk/vim-sneak'
Plugin 'justinmk/vim-syntax-extra'
Plugin 'bruno-/vim-vertical-move'
if executable("tmux")
Plugin 'tpope/vim-tbone'
Plugin 'wellle/tmux-complete.vim'
let g:tmuxcomplete#trigger = ''
endif

Plugin 'dbext.vim'
" dbext profile example:
"   let g:dbext_default_profile = 'default'
"   let g:dbext_default_profile_default = 'type=SQLSRV:integratedlogin=1:dbname=foo:host=localhost:srvname=localhost\sqlexpress:bin_path=C:\Program Files\Microsoft SQL Server\110\Tools\Binn'
let g:dbext_default_history_file = expand('~/.dbext_sql_history', 1)
let g:dbext_default_history_size = 1000
let g:dbext_default_history_max_entry = 10*1024
let g:dbext_default_usermaps = 0

Plugin 'thinca/vim-quickrun'
" Plugin 'xuhdev/SingleCompile'
Plugin 'tpope/vim-sensible'
Plugin 'tpope/vim-fugitive'

Plugin 'tpope/vim-surround'
let g:surround_no_insert_mappings = 1

Plugin 'tpope/vim-dispatch'
nnoremap !m :<c-u>Make<cr>
nnoremap !t :<c-u>Start! ctags -R *<cr>
nnoremap !T :<c-u>Tmux send-keys -t bottom-left  C-m<left><left><left><left>
nnoremap zut :<c-u>Make unittest<cr>

Plugin 'tpope/vim-repeat'
Plugin 'tpope/vim-eunuch'
Plugin 'tpope/vim-rsi'
Plugin 'tpope/vim-unimpaired'
Plugin 'tpope/vim-obsession'
Plugin 'tpope/vim-markdown'
Plugin 'Raimondi/delimitMate'
" force delimitmate to leave <c-g> alone
imap <silent> <Plug>(blah) <Plug>delimitMateJumpMany
Plugin 'zhaocai/DirDiff.vim'
Plugin 'justinmk/diffchar.vim'
nmap dc <Plug>(DiffChar_ToggleCurrentLine)
Plugin 'AndrewRadev/linediff.vim'
let g:linediff_buffer_type = 'scratch'
" Plugin 'mbbill/undotree'
Plugin 'kana/vim-textobj-user'
Plugin 'gaving/vim-textobj-argument'
Plugin 'guns/vim-sexp'
Plugin 'guns/vim-clojure-static'
Plugin 'guns/vim-clojure-highlight'
Plugin 'tpope/vim-leiningen'
Plugin 'tpope/vim-fireplace'
Plugin 'tpope/vim-commentary'

if !s:is_cygwin && has('python')
" delimiter highlighting? https://github.com/mhinz/vim-blockify/blob/master/plugin/blockify.vim
Plugin 'Valloric/MatchTagAlways'
endif
if !s:is_cygwin && (has('python') || has('python3'))
Plugin 'davidhalter/jedi-vim'
endif
Plugin 'OrangeT/vim-csharp' "should come _before_ omnisharp for better syntax
" if s:is_windows && has('python') && !s:is_msysgit
" Plugin 'nosami/Omnisharp'
" endif

Plugin 'tpope/vim-projectionist'
" look at derekwyatt/vim-fswitch for more C combos.
let g:projectionist_heuristics = {
      \  '/*.c|src/*.c': {
      \    '*.c': {'alternate': ['../include/{}.h', '{}.h']},
      \    '*.h': {'alternate': '{}.c'},
      \  },
      \  'Makefile': {
      \    '*Makefile': {'alternate': '{dirname}CMakeLists.txt'},
      \    '*CMakeLists.txt': {'alternate': '{dirname}Makefile'},
      \  },
      \}

Plugin 'PProvost/vim-ps1'
Plugin 'pangloss/vim-javascript'
Plugin 'leafo/moonscript-vim'
Plugin 'chrisbra/Colorizer'
Plugin 'chrisbra/Recover.vim'
Plugin 'osyo-manga/vim-over'

Plugin 'inside/vim-search-pulse'
let g:vim_search_pulse_mode = 'pattern'
let g:vim_search_pulse_disable_auto_mappings = 1
let g:vim_search_pulse_color_list = ["red", "white"]
let g:vim_search_pulse_duration = 200
nmap n n<Plug>Pulse
nmap N N<Plug>Pulse

Plugin 'terryma/vim-expand-region'
Plugin 'mhinz/vim-signify'
let g:signify_vcs_list = [ 'git' ]

if exists("$GOPATH")
Plugin 'Blackrush/vim-gocode'
endif

" https://github.com/vim-scripts/surrparen
Plugin 'Keithbsmiley/investigate.vim'
Plugin 'tsukkee/unite-tag'
Plugin 'Shougo/unite.vim'
Plugin 'Shougo/unite-mru'
Plugin 'Shougo/unite-outline'
Plugin 'jeetsukumaran/vim-filebeagle'
let g:filebeagle_suppress_keymaps = 1
Plugin 'junegunn/vader.vim'
Plugin 'junegunn/vim-github-dashboard'
let g:github_dashboard = {}
let g:github_dashboard['position'] = 'right'
Plugin 'mattn/webapi-vim'
Plugin 'mattn/gist-vim'

if !s:is_windows && (has("python") || has("python3"))
  Plugin 'Valloric/YouCompleteMe'
  let g:ycm_enable_diagnostic_signs = 0
  let g:ycm_always_populate_location_list = 1
elseif s:lua_patch885
  Plugin 'Shougo/neocomplete.vim'

  let g:neocomplete#enable_at_startup = 1
  let g:neocomplete#enable_smart_case = 1
  inoremap <expr> <C-g> neocomplete#undo_completion()
  inoremap <expr> <C-l> neocomplete#complete_common_string()
  inoremap <expr> <cr>  pumvisible() && exists("*neocomplete#close_popup") ? neocomplete#close_popup() : "\<cr>"

  " let force = get(g:, 'neocomplete#force_omni_input_patterns', {})
  let omni = get(g:, 'neocomplete#sources#omni#input_patterns', {})
  let g:neocomplete#sources#omni#input_patterns = omni
  let omni.go  = '[^.[:digit:] *\t]\.\w*'
  let omni.sql = '[^.[:digit:] *\t]\%(\.\)\%(\h\w*\)\?'
  let omni.python = '\%([^. \t]\.\|^\s*@\|^\s*from\s.\+import \|^\s*from \|^\s*import \)\w*'
  let omni.cs = '.*[^=\);]'
endif

" eager-load these plugins so we can override their settings below
runtime plugin/sensible.vim
runtime plugin/rsi.vim

function! s:ctrl_u() "{{{ rsi ctrl-u, ctrl-w
  if getcmdpos() > 1
    let @- = getcmdline()[:getcmdpos()-2]
  endif
  return "\<C-U>"
endfunction

function! s:ctrl_w_before()
  let s:cmdline = getcmdpos() > 1 ? getcmdline() : ""
  return "\<C-W>"
endfunction

function! s:ctrl_w_after()
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

endif "}}}

filetype plugin indent on


func! EnsureDir(path)
  "trim
  let l:path = expand(substitute(a:path,  '\s\+', '', 'g'), 1)

  if empty(l:path)
    echom "EnsureDir(): invalid path: ".a:path
    return 0 "path is empty/blank.
  endif

  if !isdirectory(l:path)
    call mkdir(l:path, 'p')
  endif
  return isdirectory(l:path)
endfun

func! EnsureFile(path)
  let l:path = expand(a:path, 1)
  if !filereadable(l:path) && -1 == writefile([''], l:path)
    echoerr "failed to create file: ".l:path
  endif
endf

command! LoadSession if filereadable(expand("~/.vim/session.vim", 1)) | source ~/.vim/session.vim
      \ | else | Obsession ~/.vim/session.vim | endif
set sessionoptions-=blank

"==============================================================================
" general settings / options
"==============================================================================
let g:sneak#streak = 1
let g:sneak#use_ic_scs = 1
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
let g:sneak#target_labels = ";sftunq/SFGHLTUNRMQZ?0"

xmap m     <Plug>(expand_region_expand)
xmap <m-m> <Plug>(expand_region_shrink)

let g:vertical_move_default_mapping = 0
nmap <silent> + <Plug>(vertical_move_down)
nmap <silent> _ <Plug>(vertical_move_up)
xmap <silent> + <Plug>(vertical_move_down)
xmap <silent> _ <Plug>(vertical_move_up)
omap <silent> + <Plug>(vertical_move_down)
omap <silent> _ <Plug>(vertical_move_up)

let g:EclimBufferTabTracking = 0 "legacy version
let g:EclimBuffersTabTracking = 0

" To map a 'meta' escape sequence in a terminal, you must map the literal control character.
" insert-mode, type ctrl-v, then press alt+<key>. Must be done in a terminal, not gvim/macvim.
" http://vim.wikia.com/wiki/Mapping_fast_keycodes_in_terminal_Vim
" http://stackoverflow.com/a/10633069/152142
if !s:is_msysgit && !s:is_gui
    "avoid: m-b m-d m-f
    set <m-g>=g <m-h>=h <m-i>=i <m-j>=j <m-k>=k <m-l>=l <m-m>=m
          \ <m-o>=o <m-p>=p <m-q>=q <m-r>=r <m-s>=s
          \ <m-t>=t <m-w>=w <m-x>=x <m-y>=y <m-z>=z
          \ <m-]>=]
endif

try | lang en_US | catch | endtry

if !s:is_msysgit && (&termencoding ==# 'utf-8' || &encoding ==# 'utf-8')
  let &listchars = "tab:\u25b8 ,trail:\u25ab"

  if !(s:is_windows || s:is_cygwin || s:is_ssh)
    " may affect performance: https://github.com/tpope/vim-sensible/issues/57
    " let &listchars = "tab:\u21e5 ,trail:\u2423,extends:\u21c9,precedes:\u21c7,nbsp:\u00b7"
    " let &showbreak="\u21aa" " precedes line wrap
  endif
else
  set listchars+=trail:.
endif
set list

set cursorline

set hidden      " Allow buffer switching even if unsaved 
set mouse=a     " Enable mouse usage (all modes)
set lazyredraw  " no redraws in macros
set ttyfast
set cmdheight=2
set backspace=eol,start,indent
set ignorecase " case-insensitive searching
set smartcase  " but become case-sensitive if you type uppercase characters
set hlsearch   " highlight search matches

"audible bell persists unless visualbell is enabled.
set noerrorbells novisualbell t_vb= visualbell

set timeoutlen=3000
set noshowmode " Hide the mode text (e.g. -- INSERT --)
set foldmethod=marker
set foldlevelstart=99 "open all folds by default
nnoremap coz :<c-u>if &foldenable && &foldmethod==#'indent' <bar> set nofoldenable <bar> else <bar> set foldmethod=indent foldnestmax=3 foldlevel=0 foldenable <bar> endif<cr>
nnoremap zy  zt5<c-y>
set scrolloff=0
set sidescrolloff=0
set noequalalways
set splitright

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

" platform-specific settings
if s:is_windows
    set winaltkeys=no
    set guifont=Consolas:h11
elseif s:is_mac && s:is_gui
    set macmeta " Use option (alt) as meta key.

    " macvim options  :view $VIM/gvimrc
    let macvim_skip_colorscheme=1
    let macvim_skip_cmd_opt_movement=1

    set guifont=Menlo:h14 "Monaco:h16
elseif s:is_gui "linux or other
    set guifont=Monospace\ 10
endif

"colorscheme {{{
  let s:color_force_high_contrast = ' 
        \ if &background == "dark"
        \ | hi Normal  guibg=black guifg=white ctermfg=255 ctermbg=0
        \ | hi NonText guibg=black guifg=white ctermfg=255 ctermbg=0
        \ | endif
        \'

  if !s:is_mac
    exe s:color_force_high_contrast
    if has('vim_starting')
      exe 'autocmd ColorScheme * '.s:color_force_high_contrast
    endif
  endif

  if !s:is_gui && &t_Co <= 88
    hi CursorLine ctermfg=white
    hi Comment ctermfg=7
    hi PreProc ctermfg=10
  else
    let s:color_override = '
          \   hi Visual        guifg=#000000 guibg=#CBF8B0 gui=NONE ctermfg=000 ctermbg=193 cterm=none
          \ | hi VisualNOS     term=bold,underline cterm=bold,underline ctermbg=23 gui=bold,underline guibg=#007475
          \'

    " guibg=LimeGreen ctermbg=154
    let s:color_override_dark = '
          \ if &background == "dark"
          \ | hi Comment       guifg=#afafaf               gui=NONE  ctermfg=102               cterm=NONE
          \ | hi Cursor        guibg=#0a9dff guifg=white   gui=NONE
          \ | hi CursorLine    guibg=#293739 ctermbg=236
          \ | hi PmenuSel      guibg=#0a9dff guifg=white   gui=NONE  ctermbg=39 ctermfg=white  cterm=NONE
          \ | hi PmenuSbar     guibg=#857f78
          \ | hi PmenuThumb    guifg=#242321
          \ | hi WildMenu      gui=NONE cterm=NONE guifg=#f8f6f2 guibg=#0a9dff ctermfg=255 ctermbg=39
          \ | hi DiffAdd       guifg=#ffffff guibg=#006600 gui=NONE  ctermfg=231  ctermbg=22   cterm=NONE 
          \ | hi DiffChange    guifg=#ffffff guibg=#007878 gui=NONE  ctermfg=231  ctermbg=30   cterm=NONE 
          \ | hi DiffDelete    guifg=#ff0101 guibg=#9a0000 gui=NONE  ctermfg=196  ctermbg=88   cterm=NONE 
          \ | hi DiffText      guifg=#000000 guibg=#ffb733 gui=NONE  ctermfg=000  ctermbg=214  cterm=NONE 
          \ | hi TODO                        guifg=#ffff87 gui=bold,underline
          \ | hi Underlined    guifg=NONE
          \ | endif
          \'

    if has('vim_starting') "only on startup
      exe 'autocmd ColorScheme * '.s:color_override
      exe 'autocmd ColorScheme * '.s:color_override_dark
      " expects &runtimepath/colors/{name}.vim.
      silent! colorscheme molokai
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

set autoindent " NOTE: 'smartindent' is superseded by 'cindent' and 'indentexpr'. 

" =============================================================================
" util functions

func! s:trim_whitespace()
  let s=@/
  let w=winsaveview()
  s/\s\+$//ge
  call histdel("/", histnr("/"))
  call winrestview(w)
  let @/=s
endfunc
command! -range=% Trim <line1>,<line2>call s:trim_whitespace()

"return the syntax highlight group under the cursor
function! GetSyntaxName()
    let l:name = synIDattr(synID(line('.'),col('.'),1),'name')
    return l:name == '' ? '' : '[' . l:name . ']'
endfunction

func! AppendToFile(file, lines)
  let l:file = expand(a:file, 1)
  call EnsureFile(l:file)
  "credit ZyX: http://stackoverflow.com/a/8976314/152142
  call writefile(readfile(l:file)+a:lines, l:file)
endf

" http://www.vim.org/scripts/script.php?script_id=1714
" - uses FileChangedShell and :checktime to avoid re-loading an un-changed file.
" - only updates if the buffer is visible.
" - autocmds are torn down after buffer is closed or hidden.
" - let b:tail_follow=1 to auto-scroll
" - Vim:
"     - depends on CursorHold, so tailed buffers won't update while you are moving the cursor.
"     - set 'updatetime' lower if you want faster updates
"     - b:tail_follow only works if the tailed file is the current buffer
let s:tail_orig_updtime = &updatetime
func! Tail(filename) "TODO: disable undo for buffer?
  let filename = escape(dispatch#expand(a:filename), '#%')
  if strlen(filename) > 0
    exec 'split '.filename
  endif "else: tail the current buffer.

  let &updatetime=1000
  setlocal autoread

  let aubuff = '<buffer='.bufnr("%").'>'
  "buffer-local
  augroup TailSmurf
    exec 'au! * '.aubuff
    exec 'autocmd BufEnter,WinEnter '.aubuff.' let &updatetime=1000'
    exec 'autocmd BufLeave,BufHidden,WinLeave '.aubuff.' let &updatetime='.s:tail_orig_updtime
    "Follow only the current buffer (anything more sophisticated is a pain in the ass).
    exec 'autocmd FileChangedShellPost '.aubuff.' if get(b:, "tail_follow", 0) && bufnr("%")=='.bufnr("%").'|exec "norm! G"|endif'
  augroup END

  call feedkeys("f\e", "n") "force cursor activity to kick CursorHold

  "global
  au! TailSmurf CursorHold
  autocmd TailSmurf CursorHold * checktime | call feedkeys("f\e", "n")
endf
command! -bang -nargs=? -complete=file Tail call Tail(<q-args>)

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
inoremap <insert> <C-r>+

" delete the 'head' of a path on the command line
cnoremap <silent> <c-x> <C-\>e<sid>delete_until()<cr>

func! s:delete_until()
  let c = nr2char(getchar())
  return substitute(getcmdline(), '\(.*['.escape(c, '\').']\).*', '\1', '')
endfunc


" abbreviations ===============================================================

iabbrev date- <c-r>=strftime("%Y/%m/%d %H:%M:%S")<cr>
" current file path
iabbrev fn- <c-r>=expand('%:p', 1)<cr>
" current file directory
iabbrev fd- <c-r>=expand('%:p:h', 1)<cr>
cabbrev fd- <c-r>=expand("%:p:h", 1)<cr>

"==============================================================================
" key mappings/bindings

" mark position before search
nnoremap / ms/

" manage windows
nnoremap gw <c-w>
nnoremap gwV :vnew<cr>
nnoremap <silent> d<tab> <c-w>c
nnoremap <silent><expr> <tab> (v:count > 0 ? '<c-w>w' : ':<C-u>call <sid>switch_to_alt_win()<cr>')
xmap     <silent>       <tab> <esc><tab>
nnoremap <m-i> <c-i>
" fit the current window height to the selected text
xnoremap <expr> gw<bs> 'z'.(2*(&scrolloff)+1+abs(line('.')-line('v')))."\<cr>\<esc>".(min([line('.'),line('v')]))."ggzt"

" go to the previous window (or any other window if there is no 'previous' window).
func! s:switch_to_alt_win()
  let currwin = winnr()
  wincmd p
  if winnr() == currwin "window didn't change, so there was no 'previous' window.
    wincmd W
  endif
endf

" manage tabs
"        gwT (built-in) breaks out window into new Tab.
nnoremap gwN :tabnew<cr>
nnoremap gwC :tabclose<cr>
nnoremap ]gt :tabmove +1<cr>
nnoremap [gt :tabmove -1<cr>
" move tab to Nth position (this is slightly different than :tabmove)
nnoremap <expr> gT (v:count > 0 ? '<c-u>:tabmove '.(v:count - 1).'<cr>' : 'gT')

" manage buffers
nnoremap <silent> ZB :<c-u>call <SID>buf_kill(0)<cr>
nnoremap <silent> Zb :<c-u>call <SID>buf_kill(1)<cr>
set wildcharm=<C-z>
nnoremap gb :buffer <C-z><S-Tab>

" quickfix window
nnoremap <silent><c-q> :silent! botright copen<cr>
" location window
" nnoremap q] :botright lopen<cr>

nnoremap <silent> ^ :FileBeagleBufferDir<cr>
" set working directory to the current buffer's directory
nnoremap cd :cd %:p:h<bar>pwd<cr>
nnoremap cu :cd ..<bar>pwd<cr>
" show git branch with ctrl-g info
func! s:ctrl_g()
  redir => msg | silent exe "norm! 1\<c-g>" | redir END
  echo fugitive#head(7) msg[2:]
endf
nnoremap <C-g> :call <sid>ctrl_g()<cr>
" show the working directory and session
nnoremap <M-g> :<C-u>echo fnamemodify(getcwd(), ":~")
      \ (strlen(v:this_session) ? fnamemodify(v:this_session, ":~") : "[No session]")<cr>

" version control
xnoremap <expr> D (mode() ==# "V" ? ':Linediff<cr>' : 'D')
nnoremap UU :if &diff<bar>diffupdate<bar>else<bar>diffthis<bar>endif<cr>
nnoremap Ud :if &diff<bar>diffupdate<bar>else<bar>Gdiff<bar>endif<cr>
nnoremap Us :Gstatus<cr>
nnoremap Ul :Glog<cr>
nnoremap Ub :Gblame<cr>
nnoremap Uh :SignifyToggleHighlight<cr>
nnoremap UW :Gwrite<cr>
nnoremap <silent> UG :cd %:p:h<bar>silent exec '!git gui '.(has('win32')<bar><bar>has('win64') ? '' : '&')<bar>cd -<bar>if !has('gui_running')<bar>redraw!<bar>endif<cr>
nnoremap <silent> UL :cd %:p:h<bar>silent exec '!gitk --all '.(has('win32')<bar><bar>has('win64') ? '' : '&')<bar>cd -<bar>if !has('gui_running')<bar>redraw!<bar>endif<cr>
"linewise partial staging in visual-mode.
xnoremap <c-p> :diffput<cr>
xnoremap <c-o> :diffget<cr>

" :help :DiffOrig
command! DiffOrig leftabove vnew | set bt=nofile | r ++edit # | 0d_ | diffthis | wincmd p | diffthis

set diffopt+=vertical
nnoremap <silent> co<space> :set <C-R>=(&diffopt =~# 'iwhite') ? 'diffopt-=iwhite' : 'diffopt+=iwhite'<CR><CR>

" execute/evaluate
nmap yxx     <Plug>(quickrun)
xmap <enter> <Plug>(quickrun)

" filter
" nnoremap c<cr>jj    :%!python -m json.tool<cr>
" nnoremap z<cr>jj    :%!python -m json.tool<cr>
" nnoremap c<space>jj :%!python -m json.tool<cr>
" nnoremap g<bar>jj   :%!python -m json.tool<cr>
nnoremap <bar>jj :%!python -m json.tool<cr>
xnoremap <bar>jj :!python -m json.tool<cr>

" available mappings:
"   visual: <space> R c-r c-n c-g c-a c-x c-h,<bs>
"   insert: c-g
"   normal: m<enter> zi zp zy m<tab> q<special> y<special> <del> <pageup/down> q<special>
" nnoremap c<space>       :easyalign...

func! s:buf_compare(b1, b2)
  "prefer loaded buffers before unloaded buffers
  if bufloaded(a:b1)
    return bufloaded(a:b2) ? 0 : -1
  endif
  return !bufloaded(a:b2) ? 0 : 1
endf

func! s:buf_find_displayed_bufs() " find all buffers displayed in any window, any tab.
  let l:bufs = []
  for i in range(1, tabpagenr('$'))
    call extend(l:bufs, tabpagebuflist(i))
  endfor
  return l:bufs
endf

func! s:buf_find_valid_next_bufs()
  "valid 'next' buffers 
  "   EXCLUDE: 
  "     - current, unlisted
  "     - directory buffers marked as 'readonly' and 'modified' (netrw often leaves a _listed_ buffer in this weird state)
  "   INCLUDE: normal buffers; 'help' buffers
  let l:valid_buffers = filter(range(1, bufnr('$')), 
              \ 'buflisted(v:val) 
              \  && ("" ==# getbufvar(v:val, "&buftype") || "help" ==# getbufvar(v:val, "&buftype")) 
              \  && v:val != bufnr("%") 
              \  && -1 == index(tabpagebuflist(), v:val) 
              \  && !(isdirectory(bufname(v:val)) && getbufvar(v:val, "&modified") && getbufvar(v:val, "&readonly"))
              \ ')
  call sort(l:valid_buffers, '<sid>buf_compare')
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
    echom 'buffer has unsaved changes (use "ZB" to override)'
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

"move to last character 
nnoremap - $
xnoremap - $
onoremap - $

" un-join (split) the current line at the cursor position
nnoremap <c-j> i<c-j><esc>k$
" vaporize delete without overwriting the default register
nnoremap vd "_d
xnoremap x  "_d
nnoremap vD "_D
xnoremap P  "0p

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
nnoremap <silent> rrr 0:<C-u>set opfunc=<sid>replace_without_yank<CR>g@$

inoremap jk <esc>
inoremap kj <esc>
nnoremap ' `
xnoremap ' `

nnoremap <space> :
nnoremap <leader>w :w<cr>
nnoremap <leader>e :e<cr>

" map m-] to be the inverse of c-]
nnoremap <m-]> <c-t>

" search within visual block
xnoremap / <esc>/\%V

" select last inserted text
nnoremap gV `[v`]

nnoremap cg* *``cgn

" replay macro for each line of a visual selection
xnoremap @q :normal @q<CR>
xnoremap @@ :normal @@<CR>
" repeat last command for each line of a visual selection
xnoremap . :normal .<CR>
" disable Ex mode key and map it to something awesome
nnoremap Q @@
xnoremap Q :normal @@<CR>
" repeat the last edit on the next [count] matches.
nnoremap <C-n> @='n.'<cr>

"j,k move by screen line instead of file line
nnoremap j gj
nnoremap k gk
inoremap <Down> <C-o>gj
inoremap <Up>   <C-o>gk

" disable F1 help key
noremap! <F1> <nop>
noremap <F1> <nop>

nnoremap ZZ :wqa<cr>
nnoremap Zq :qa<cr>
nnoremap ZQ :qa!<cr>

func! s:do_in_place(keyseq, line_offset, col_offset) "perform an edit without moving the cursor
  let pos = [line(".") + a:line_offset, col(".") + a:col_offset]
  exe "norm ".a:keyseq
  call cursor(pos)
endf


func! ReadExCommandOutput(newbuf, cmd)
  redir => l:message
  silent! execute a:cmd
  redir END
  if a:newbuf | wincmd n | endif
  silent put=l:message
endf
command! -nargs=+ -bang -complete=command R call ReadExCommandOutput(<bang>1, <q-args>)
inoremap <c-r>R <c-o>:<up><home>R! <cr>

func! s:get_visual_selection()
  let [lnum1, col1] = getpos("'<")[1:2]
  let [lnum2, col2] = getpos("'>")[1:2]
  let lines = getline(lnum1, lnum2)
  let lines[-1] = lines[-1][: col2 - (&selection == 'inclusive' ? 1 : 2)]
  let lines[0] = lines[0][col1 - 1:]
  return join(lines, "\n")
endfunc
"read last visual-selection into command line
cnoremap <c-r><c-v> <c-r>=<sid>get_visual_selection()<cr>
inoremap <c-r><c-v> <c-r>=<sid>get_visual_selection()<cr>

xmap * <esc>/\V<c-r>=escape(<sid>get_visual_selection(), '/\')<cr><cr><Plug>Pulse
nmap * *<Plug>Pulse

" python ======================================================================
augroup vimrc_python
  autocmd!
  autocmd FileType python syn keyword pythonDecorator True None False self | setlocal nosmartindent
  let g:jedi#force_py_version = 3
  let g:jedi#auto_vim_configuration = 0
  let g:jedi#goto_assignments_command = 'gd'
  let g:jedi#goto_definitions_command = 'gD'
  let g:jedi#rename_command = 'crn'
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
          \ | nnoremap <buffer> <c-t> :<c-u>JavaHierarchy<cr>
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
"    Plugin 'thinca/vim-ref'
"    let g:ref_cache_dir = expand('~/.vim/tmp/ref_cache/', 1)
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
let g:clojure_fold = 1

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

augroup BufferDeath
  autocmd!
  " on BufLeave:
  "   1. remove pending CursorHold autocmd, if any
  "   2. set up CursorHold autocmd
  " on CursorHold:
  "   1. call function
  "   2. remove the autocmd to avoid spam
  autocmd BufLeave * exec 'autocmd! BufferDeath CursorHold' |
        \ autocmd BufferDeath CursorHold * call <sid>clear_empty_buffers() |
        \ autocmd! BufferDeath CursorHold
augroup END

" A massively simplified take on https://github.com/chreekat/vim-paren-crosshairs
func! s:matchparen_cursorcolumn_setup()
  augroup matchparen_cursorcolumn
    autocmd!
    autocmd CursorMoved * if get(w:, "paren_hl_on", 0) | set cursorcolumn | else | set nocursorcolumn | endif
    autocmd InsertEnter * set nocursorcolumn
  augroup END
endf
if !&cursorcolumn
  augroup matchparen_cursorcolumn_setup
    autocmd!
    " - Add the event _only_ if matchparen is enabled.
    " - Event must be added _after_ matchparen loaded (so we can react to w:paren_hl_on).
    autocmd CursorMoved * if exists("#matchparen#CursorMoved") | call <sid>matchparen_cursorcolumn_setup() | endif
          \ | autocmd! matchparen_cursorcolumn_setup
  augroup END
endif

augroup vimrc_autocmd
  autocmd!
  autocmd BufReadPost quickfix nnoremap <buffer> <c-p> <up>
        \ |nnoremap <buffer> <c-n> <down>
        \ |nnoremap <silent><buffer> q <c-w>c:call<sid>switch_to_alt_win()<cr>

  autocmd CmdwinEnter * nnoremap <silent><buffer> q <C-W>c

  " Jump to the last position when reopening a file (except Git commit)
  autocmd BufReadPost * if @% !~# '\.git[\/\\]COMMIT_EDITMSG$' && line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g`\"" | endif

  " force windows to be sized equally after viewport resize
  autocmd VimResized * wincmd =

  autocmd FileType text setlocal tabstop=4 shiftwidth=4

  autocmd FileType css set omnifunc=csscomplete#CompleteCSS

  autocmd FileType vim nnoremap <buffer> yxx :source %<cr> | xnoremap <buffer><silent> <enter> :<c-u>QuickRun -mode v -outputter message<cr>

  if exists("*mkdir") "auto-create directories for new files
    au BufWritePre,FileWritePre * call EnsureDir('<afile>:p:h')
  endif

  "when Vim is started in diff-mode (vim -d, git mergetool) do/dp should not auto-fold.
  autocmd VimEnter * if &diff | exe 'windo set foldmethod=manual' | endif

  autocmd BufRead,BufNewFile *.{ascx,aspx} setlocal tabstop=4 shiftwidth=4 copyindent

  if s:is_windows
    " always maximize initial GUI window size
    autocmd GUIEnter * simalt ~x
  endif
augroup END

nnoremap g// mS:<c-u>grep '' *<left><left><left>
" search all file buffers (clear quickfix first). g: get all matches. j: no jumping.
" :noau speeds up vimgrep
" search current buffer and open results in quickfix window
nnoremap g/% :<c-u>lvimgrep  % <bar>lw<left><left><left><left><left><left>
nnoremap g/b :<c-u>cex []<bar>exe 'bufdo silent! noau vimgrepadd//gj %'<bar>copen<left><left><left><left><left><left><left><left><left><left><left><left>
" search-replace
nnoremap g/r :<c-u>OverCommandLine<cr>%s/
xnoremap g/r :<c-u>OverCommandLine<cr>%s/\%V
" recursively search for word under cursor
nnoremap g/* :<c-u>grep '<c-r><c-w>' *<cr>
xnoremap g/* :<c-u>noau vimgrep /<c-r>=<SID>get_visual_selection()<cr>/ **<cr>
nnoremap g/g :<c-u>Ggrep<space>
if executable("pt")
set grepprg=pt\ --nocolor\ --nogroup\ $*
endif

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

set wildmode=full
"THIS AFFECTS expand() !!!!!!!!!!!!!!!!!!!!
set wildignore+=tags,*.o,*.obj,*.dll,*.class,.hg,.svn,*.pyc,*/tmp/*,*/grimoire-remote/*,*.so,*.swp,*.zip,*.exe,*.jar,*/opt/*,*/gwt-unitCache/*,*.cache.html,*.pdf,*.wav,*.mp3,*.ogg

" Files with these suffixes get a lower priority when matching a wildcard
set suffixes+=.aux,.log,.dvi,.bbl,.blg,.brf,.cb,.ind,.idx,.ilg,.inx,.out,.toc

if s:is_windows
  "THIS AFFECTS expand() !!!!!!!!!!!!!!!!!!!!
  set wildignore+=*\\Debug\\*,*\\Release\\*,*\\Windows\\*,*\\Program\ Files*\\*,*\\AppData\\*,*.pch,*.ipch,*.pdb,*.sdf,*.opensdf,*.idb,*.suo,*.ntuser,*.blf,*.dat,*.regtrans-ms
  let g:neocomplete#ctags_command = '~/bin/ctags.exe'
endif

if s:plugins "unite.vim =============================================== {{{
call unite#custom#profile('files', 'filters', 'sorter_rank')
call unite#custom#profile('default', 'context', {'no_split':1, 'resize':0})

"let g:unite_source_grep_command=expand($ProgramFiles.'\Git\bin\grep.exe', 1)
let g:unite_source_history_yank_enable = 1
let g:neomru#time_format = "(%Y/%m/%d %H:%M) "
let g:unite_source_buffer_time_format = "(%Y/%m/%d %H:%M) "
let g:unite_enable_start_insert = 1

call unite#filters#matcher_default#use(['matcher_fuzzy'])
call unite#filters#sorter_default#use(['sorter_rank'])
" TODO: https://github.com/Shougo/unite.vim/issues/347
let s:unite_sources = 'file_rec/async,file_rec,neomru/file,directory_rec/async,directory_rec,neomru/directory'
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
" don't track help files in MRU list
call unite#custom#source('neomru/file', 'ignore_pattern', '\v[/\\]doc[/\\]\w+\.txt')

nnoremap <silent> <c-p> :Unite -buffer-name=files file_rec <cr>
" search direcory of current file
nnoremap <silent> g/.   :exec ":Unite file_rec:".escape(expand("%:p:h"), ':\ ')<cr>
nnoremap <silent> g/f   :Unite function<cr>
nnoremap <silent> g/l   :Unite line<cr>
nnoremap <silent> g/v   :Unite runtimepath -default-action=rec<cr>
nnoremap <silent> gl    :Unite buffer neomru/file<cr>
" auto-generates an outline of the current buffer
nnoremap <silent> <m-o> :Unite outline<cr>
nnoremap <silent> g/t   :Unite tag tag/include tag/file <cr>
nnoremap <silent> <m-y> :Unite history/yank<cr>
imap     <silent> <m-y> <C-o><m-y>
nnoremap <silent> g/d   :Unite neomru/directory directory_rec:. -default-action=cd<CR>
nnoremap <silent> g/ps  :Unite process <CR>
nnoremap <silent> g/W   :Unite tmuxcomplete<CR>
imap     <silent> <m-w> <C-o>g/W
nnoremap <silent> g/L   :Unite tmuxcomplete/lines<CR>
imap     <silent> <m-l> <C-o>g/L
nnoremap <silent> <m-x> :Unite command<CR>

augroup vimrc_unite
  autocmd!
  " obliterate unite buffers (marks especially).
  autocmd BufLeave \[unite\]* if "nofile" ==# &buftype | setlocal bufhidden=wipe | endif
  autocmd FileType unite call s:unite_settings()
augroup END

function! s:unite_settings()
  setlocal nopaste
  let b:delimitMate_autoclose = 0
  unmap! <buffer> <c-d>
  unmap  <buffer> M
  nnoremap <silent><buffer> <C-n> j
  nnoremap <silent><buffer> <C-p> k
endfunction

" delete empty, non-visible, non-special, non-alternate buffers.
" TODO: exclude buffers that have an undo stack?
function! s:clear_empty_buffers()
  if '[Command Line]' ==# bufname("%") && 'nofile' ==# &buftype
    return 0 "avoid E11
  endif

  let displayedbufs = s:buf_find_displayed_bufs()

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

  " REMOVED: can't do this, some plugins use the buffer list as a queue with
  "          special buffer names, eg: fugitive:///foo/.git//<hash>:::<filename>
  "
  " if the buffer is _not_ loaded, we do _not_ want to load it just to check if it is empty.
  "     - get its file path and check getfsize().
  "     - if getfsize() fails, then the filepath must be non-existent; and 
  "       an unloaded buffer with an invalid a filepath must be empty.
  " let nonexistent = filter(range(1, bufnr('$')),
  "       \ 'bufexists(v:val) && !bufloaded(v:val) 
  "       \  && -1 == getfsize(expand("#".v:val.":p", 1)) 
  "       \ ')

  if !empty(empty_bufs)
    exe 'bwipeout! '.join(empty_bufs, ' ')
  endif
endfunction

endif "}}}

" statusline  =================================================================
" show winnr iff there are >2 windows
set statusline=%{winnr('$')>2?winnr():''}\ %<%f\ %h%#ErrorMsg#%m%*%r\ %=%{strlen(&fenc)?&fenc:&enc}\ %y\ %-10.(%l,%c%V%)\ %p%%
set title
set titlestring=%{getcwd()}
set titleold=?

" =============================================================================
if s:is_cygwin
  " use separate viminfo to avoid weird permissions issues
  set viminfo+=n~/.viminfo_cygwin

  " Mode-dependent cursor   https://code.google.com/p/mintty/wiki/Tips
  let &t_ti.="\e[1 q"
  let &t_SI.="\e[5 q"
  let &t_EI.="\e[1 q"
  let &t_te.="\e[0 q"

  " set escape key to an unambiguous keycode, to avoid escape timeout delay.
  let &t_ti.="\e[?7727h"
  let &t_te.="\e[?7727l"
  noremap  <Esc>O[ <Esc>
  noremap! <Esc>O[ <C-c>
endif

"ensure transient dirs
let s:dir = empty($XDG_DATA_HOME) ? '~/.local/share/vim' : $XDG_DATA_HOME.'/vim'
call EnsureDir(s:dir)

if isdirectory(expand(s:dir, 1))
  call EnsureDir(s:dir . '/swap/')
  call EnsureDir(s:dir . '/backup/')
  call EnsureDir(s:dir . '/undo/')

  if &directory =~# '^\.,'
    let &directory = expand(s:dir, 1) . '/swap//,' . &directory
  endif
  if &backupdir =~# '^\.,'
    let &backupdir = expand(s:dir, 1) . '/backup//,' . &backupdir
  endif
  if has("persistent_undo") && &undodir =~# '^\.\%(,\|$\)'
    let &undodir = expand(s:dir, 1) . '/undo//,' . &undodir
    set undofile
  endif
endif

silent! source ~/.vimrc.local
