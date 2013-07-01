
" windows builds: http://files.kaoriya.net/goto/vim73w32
" MacVim with homebrew:
"   brew install macvim --with-cscope --with-lua --override-system-vim
"

" If this .vimrc is not in $HOME, add these lines to $HOME/.vimrc :
"    set runtimepath+=/path/to/.vim
"    source /path/to/.vimrc
"==============================================================================

if exists('&guioptions')
    "no toolbar, no menu bar
    set guioptions-=T
    set guioptions-=m
    "don't source &runtimepath/menu.vim. (must be done before 'filetype on' / 'syntax on')
    set guioptions-=M
endif

let mapleader = ","
let g:mapleader = ","

fun! IsVimRecentBuildWithLua()
    return ! (!has('lua') || v:version < 703 || (v:version == 703 && !has('patch885')))
endfun

"==============================================================================
" vundle   https://github.com/gmarik/vundle/
"==============================================================================

"boostrap vundle on new systems
fun! InstallVundle()
    echo "Installing Vundle..."
    silent !mkdir -p ~/.vim/bundle
    silent !git clone https://github.com/gmarik/vundle.git ~/.vim/bundle/vundle
endfun

set nocompatible               " be iMproved
filetype off                   " required!

set runtimepath+=~/.vim/bundle/vundle/
call vundle#rc()

" let Vundle manage Vundle
" required! 
Bundle 'gmarik/vundle'

" repos on github
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
Bundle 'kshenoy/vim-signature'
Bundle 'kana/vim-smartinput'
Bundle 'Valloric/MatchTagAlways'
" Bundle 'Valloric/YouCompleteMe'
" Bundle 'Lokaltog/vim-easymotion'
Bundle 'Lokaltog/vim-powerline'
Bundle 'kien/ctrlp.vim'
Bundle 'PProvost/vim-ps1'
if IsVimRecentBuildWithLua()
Bundle 'Shougo/neocomplete.vim'
else
Bundle 'Shougo/neocomplcache'
endif
Bundle 'tomtom/tcomment_vim'
Bundle 'ap/vim-css-color'
Bundle 'airblade/vim-gitgutter'
Bundle 'derekwyatt/vim-scala'
Bundle 'Blackrush/vim-gocode'
Bundle 'Shougo/unite.vim'

filetype plugin indent on     " required!

"exists() tests whether an option exists
"has() tests whether a feature was compiled in
fun! IsWindows()
    return has('win32') || has('win64')
endf
fun! IsMac()
    return has('gui_macvim') || has('mac')
endf
fun! IsUnix()
    return has('unix')
endf
fun! IsMsysgit()
    return (has('win32') || has('win64')) && $TERM == 'cygwin'
endf
fun! IsTmux()
    return !(empty($TMUX))
endf

" 'is GUI' means vim is _not_ running within the terminal.
" sample values:
"   &term  = win32 //vimrc running in msysgit terminal
"   $TERM  = xterm-color , cygwin
"   &term  = builtin_gui //*after* vimrc but *before* gvimrc
"   &shell = C:\Windows\system32\cmd.exe , /bin/bash
fun! IsGui()
    return has('gui_running') || strlen(&term) == 0 || &term == 'builtin_gui'
endfun

" 'has GUI' means vim is "gui-capable"--but we may be using gvim/macvim from within the terminal.
fun! HasGui()
    return has('gui')
endfun

fun! EnsureDir(path)
    let l:path = expand(a:path)
    if (filewritable(l:path) != 2)
        if IsWindows()
            exe 'silent !mkdir "' . l:path . '"'
        else
            exe 'silent !mkdir -p "' . l:path . '"'
        endif
        redraw!
    endif
endfun

set hidden          " Allow buffer switching even if unsaved 
set mouse=a     	" Enable mouse usage (all modes)



"==============================================================================
" general
"==============================================================================
"set ttyfast
set lazyredraw  " no redraws in macros

set cmdheight=2 "The commandbar height

" Set backspace config
set backspace=eol,start,indent
set whichwrap+=<,>,h,l

set ignorecase " case-insensitive searching
set smartcase  " but become case-sensitive if you type uppercase characters
set hlsearch   " highlight search matches

set magic " change the way backslashes are used in search patterns

set showmatch " When inserting paren, jump briefly to matching one.
set mat=5     " showmatch time (tenths of a second)

" No sound on errors
set noerrorbells
set novb t_vb=
set tm=3000

"==============================================================================
set nonumber
set background=dark
set showtabline=1

" platform-specific settings
if !IsMsysgit()
    "highlight the current line
    set cursorline

    " expects &runtimepath/colors/{name}.vim.
    colorscheme molokai
endif
if IsWindows()
    set gfn=Consolas:h10
elseif IsMac()
    " Use option (alt) as meta key.
    set macmeta

    if IsGui()
        " macvim options  :view $VIM/gvimrc
        let macvim_skip_colorscheme=1
        let macvim_skip_cmd_opt_movement=1

        "set guifont=Monaco:h16
        set guifont=Menlo:h14

        let g:Powerline_symbols = 'unicode'
    endif
elseif IsGui() "linux or other
    set gfn=Monospace\ 10
endif

set fileformats=unix,dos,mac
set encoding=utf8
try
    lang en_US
catch
endtry


"==============================================================================
" text, tab and indent 
"==============================================================================
set formatoptions+=rn1

set expandtab
set tabstop=4
set shiftwidth=4
set smarttab " Use 'shiftwidth' when using <Tab> in front of a line. By default it's used only for shift commands ("<", ">").

set linebreak "wrap long lines at 'breakat' character
set textwidth=500

set autoindent " Autoindent when starting new line, or using 'o' or 'O'.
set smartindent
set nowrap 
"set textwidth=80
set nofoldenable " disable folding

if exists('+syntax') 
    syntax enable "Enable syntax highlighting
endif
if exists('&colorcolumn')
    set colorcolumn=80 "highlight the specified column
endif

" =============================================================================
" util functions
" =============================================================================

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
" command/ex mode
" =============================================================================
" Emacs-style movement (like Bash)
:cnoremap <M-b>  <S-Left>
:cnoremap <M-f>  <S-Right>

" =============================================================================
" normal mode
" =============================================================================
" Remove the Windows ^M - when the encodings gets messed up
nnoremap <Leader>m mmHmt:%s/<C-V><cr>//ge<cr>'tzt'm

" Toggle paste mode
nnoremap <leader>pp :setlocal paste! paste?<cr>

" paste current dir to command line
cno $c e <C-\>eCurrentFileDir("e")<cr>

" $q is super useful when browsing on the command line
cno $q <C-\>eDeleteTillSlash()<cr>


func! DeleteTillSlash()
  let g:cmd = getcmdline()
  if IsWindows()
    let g:cmd_edited = substitute(g:cmd, "\\(.*\[\\\\]\\).*", "\\1", "")
  else
    let g:cmd_edited = substitute(g:cmd, "\\(.*\[/\]\\).*", "\\1", "")
  endif
  if g:cmd == g:cmd_edited
    if IsWindows()
      let g:cmd_edited = substitute(g:cmd, "\\(.*\[\\\\\]\\).*\[\\\\\]", "\\1", "")
    else
      let g:cmd_edited = substitute(g:cmd, "\\(.*\[/\]\\).*/", "\\1", "")
    endif
  endif   
  return g:cmd_edited
endfunc

func! CurrentFileDir(cmd)
  return a:cmd . " " . expand("%:p:h") . "/"
endfunc


"==============================================================================
" tabs, buffers, windows
"==============================================================================

" move between windows
nnoremap <C-j> <C-W>j
nnoremap <C-k> <C-W>k
nnoremap <C-h> <C-W>h
nnoremap <C-l> <C-W>l

" Close the current buffer
nnoremap <leader>bd :bdelete!<cr>

" avoid annoying insert-mode ctrl+u behavior
inoremap <C-u> <Esc><C-u>i
inoremap <C-d> <Esc><C-d>i

" switch to the directory of the open buffer
nnoremap <leader>cd :cd %:p:h<cr>


"==============================================================================
" statusline
"==============================================================================
let g:Powerline_stl_path_style = 'short'


"return the syntax highlight group under the cursor ''
function! StatuslineCurrentHighlight()
    let name = synIDattr(synID(line('.'),col('.'),1),'name')
    if name == ''
        return ''
    else
        return '[' . name . ']'
    endif
endfunction

iab xdate <c-r>=strftime("%d/%m/%y %H:%M:%S")<cr>

"==============================================================================
" key mappings/bindings
"==============================================================================
"move to first non-whitespace char, instead of first column
nnoremap 0 ^

" un-join (split) the current line at the cursor position
nnoremap K i<cr><esc>k$

inoremap jj <esc>
nnoremap <space> :
nnoremap <leader>w :w!<cr>

"toggle/untoggle spell checking
nnoremap <leader>ss :setlocal spell!<cr>

"text bubbling: move text up/down with meta-[jk] 
nnoremap <M-j> mz:m+<cr>`z
nnoremap <M-k> mz:m-2<cr>`z
vnoremap <M-j> :m'>+<cr>`<my`>mzgv`yo`z
vnoremap <M-k> :m'<-2<cr>`>my`<mzgv`yo`z

"Delete trailing white space, useful for Python ;)
func! DeleteTrailingWS()
  exe "normal mz"
  %s/\s\+$//ge
  exe "normal `z"
endfunc
autocmd BufWrite *.py :call DeleteTrailingWS()

set guitablabel=%t

" replay @q macro for each line of a visual selection
vnoremap @q :normal @q<CR>
" repeat last command for each line of a visual selection
vnoremap . :normal .<CR>

nnoremap <leader>a :Ack

" navigate to the directory of the current file
if IsTmux()
    nnoremap <leader>gf :silent execute '!tmux split-window -h \; ' .  'send-keys "cd "' . substitute(expand("%:p:h")," ","\\\\ ","g") . ' C-m'<cr>
elseif IsWindows()
    nnoremap <leader>gf :silent !start explorer /select,%:p<cr>
elseif IsMac()
    nnoremap <leader>gf :silent execute '!open ' . substitute(expand("%:p:h")," ","\\\\ ","g")<cr>
endif


"==============================================================================
" cope/quickfix/errorlist
"==============================================================================
nnoremap <leader>cc :botright cope<cr>
nnoremap <leader>n :cnext<cr>
nnoremap <leader>p :cprevious<cr>


" python ======================================================================
let python_highlight_all = 1
au FileType python syn keyword pythonDecorator True None False self

au BufNewFile,BufRead *.jinja set syntax=htmljinja
au BufNewFile,BufRead *.mako set ft=mako

au FileType python inoremap <buffer> $r return 
au FileType python inoremap <buffer> $i import 
au FileType python inoremap <buffer> $p print 
au FileType python inoremap <buffer> $f #--- PH ----------------------------------------------<esc>FP2xi


" javascript ==================================================================
au FileType javascript setl nocindent
au FileType javascript inoremap <c-a> alert();<esc>hi
au FileType javascript inoremap <buffer> <leader>r return 
au FileType javascript inoremap <buffer> $f //--- PH ----------------------------------------------<esc>FP2xi


"==============================================================================
" vim grep
"==============================================================================
" :noau speeds up vimgrep
noremap <leader>grep :noau vimgrep // **<left><left><left><left>

" makes * and # work on visual mode too.
function! s:VSetSearch(cmdtype)
  let temp = @s
  norm! gv"sy
  let @/ = '\V' . substitute(escape(@s, a:cmdtype.'\'), '\n', '\\n', 'g')
  let @s = temp
endfunction

" in visual mode, press * or # to search for the current selection
xnoremap * :<C-u>call <SID>VSetSearch('/')<CR>/<C-R>=@/<CR><CR>
xnoremap # :<C-u>call <SID>VSetSearch('?')<CR>?<C-R>=@/<CR><CR>

" recursively vimgrep for word under cursor or selection if you hit leader-star
nmap <leader>* :execute 'noautocmd vimgrep /\V' . substitute(escape(expand("<cword>"), '\'), '\n', '\\n', 'g') . '/ **'<CR>
vmap <leader>* :<C-u>call <SID>VSetSearch()<CR>:execute 'noautocmd vimgrep /' . @/ . '/ **'<CR>

" =============================================================================
" omni complete
" =============================================================================
autocmd FileType css set omnifunc=csscomplete#CompleteCSS

" Don't scan included files. Tags file is more performant.
set complete-=i

if IsVimRecentBuildWithLua()
    nnoremap <leader>neo :NeoCompleteEnable<cr>
    let g:neocomplete#enable_smart_case = 1
    "disable neocomplcache for matching buffer names
    "let g:neocomplete#lock_buffer_name_pattern = '\*ku\*'
else
    nnoremap <leader>neo :NeoComplCacheEnable<cr>
    let g:neocomplcache_enable_smart_case = 1
endif

" =============================================================================
" ctrlp
" =============================================================================
set wildignore+=*.o,*.obj,*.class,.git,.hg,.svn,*.pyc,*/tmp/*,*.so,*.swp,*.zip,*.exe

if IsWindows()
    set wildignore+=Windows\\*,Program\ Files\\*,Program\ Files\ \(x86\)\\* 
    let g:ctrlp_buftag_ctags_bin = '~/bin/ctags.exe'
endif

let g:ctrlp_extensions = ['tag', 'buffertag', 'quickfix', 'dir', 'rtscript',
                        \ 'undo', 'line', 'changes', 'mixed', 'bookmarkdir']

" CtrlP auto-generates exuberant ctags for the current buffer!
nnoremap <c-t> :CtrlPBufTagAll<cr>

let g:ctrlp_clear_cache_on_exit = 0
let g:ctrlp_custom_ignore = {
  \ 'dir':  '\v[\/]\.(git|hg|svn|cache)$|AppData|eclipse_workspace|grimoire-remote',
  \ 'file': '\v\~$|\.(exe|so|dll|pdf|ntuser|blf|dat|regtrans-ms|o|swp|pyc|wav|mp3|ogg|blend)$',
  \ 'link': 'some_bad_symbolic_links',
  \ }

" Unite
let g:unite_source_history_yank_enable = 1
let g:unite_force_overwrite_statusline = 1
call unite#filters#matcher_default#use(['matcher_fuzzy'])
" nnoremap <c-p>    :Unite -no-split -buffer-name=files   -start-insert file_rec/async:!<cr>
" nnoremap <c-p>    :Unite -no-split -buffer-name=files   -start-insert file_rec:.:~/.:~ file_mru<cr>
nnoremap <c-b>      :Unite -no-split -buffer-name=buffer  -start-insert buffer<cr>
nnoremap <c-y>      :Unite -no-split -buffer-name=yank    history/yank<cr>

" Custom mappings for the unite buffer
autocmd FileType unite call s:unite_settings()
function! s:unite_settings()
  nmap <buffer> <esc> <Plug>(unite_exit)
  map  <buffer> <F5>  <Plug>(unite_redraw)
  imap <buffer> <C-j> <Plug>(unite_select_next_line)
  imap <buffer> <C-k> <Plug>(unite_select_previous_line)
endfunction

" =============================================================================
" session  http://vim.wikia.com/wiki/Go_away_and_come_back
" =============================================================================

" do *not* save global session variables/mappings/options
set sessionoptions-=options
set sessionoptions-=globals
set sessionoptions-=blank
set sessionoptions-=buffers

let s:sessiondir  = expand("~/.vim/sessions")
let s:sessionfile = expand(s:sessiondir . "/session.vim")
let s:sessionlock = expand(s:sessiondir . "/session.lock")

function! SaveSession(delete_lock)
  " use 'call' so that SaveSession() can also work in command-mode.
  call EnsureDir(s:sessiondir)

  if ! isdirectory(s:sessiondir)
    echoerr "Failed to create session dir: " . s:sessiondir
    return
  endif

  let b:sessionfile_bk = s:sessionfile . '.' . strftime("%Y-%m-%d") . '.bk'
  if IsWindows()
    if filereadable(s:sessionfile)
      exe 'silent !copy ' . s:sessionfile . ' ' . b:sessionfile_bk
    endif
    if a:delete_lock
      exe 'silent !del ' . s:sessionlock 
    endif
  else
    if filereadable(s:sessionfile)
      exe 'silent !cp ' . s:sessionfile . ' ' . b:sessionfile_bk
    endif
    if a:delete_lock
      exe 'silent !rm -f ' . s:sessionlock 
    endif
  endif

  exe "mksession! " . s:sessionfile
endfunction

function! LoadSession()
  if filereadable(s:sessionlock)
    echo "Session already open in another vim."
  elseif isdirectory(s:sessiondir)
    "write the session 'lock' file
    exe "new | w " . s:sessionlock . " | bd"
    
    if filereadable(s:sessionfile)
      "open the session
      exe 'source ' s:sessionfile
    else 
      echo "Session initialized."
    endif

    "set up trigger to save on exit
    autocmd VimLeave * :call SaveSession(1)
  else
    echoerr "Invalid session dir: " . s:sessiondir
  endif
endfunction


" Auto Commands
if has("autocmd")
    " Jump to the last position when reopening a file
    au BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g'\"" | endif

    " force windows to be sized equally after viewport resize
    au VimResized * wincmd =

    if IsGui()
        set viminfo+=% "remember buffer list
        autocmd VimEnter * nested :call LoadSession()
    endif

    if IsWindows()
        " use .viminfo instead of _viminfo
        set viminfo+=n~/.viminfo
        " always maximize initial GUI window size
        au GUIEnter * simalt ~x 
    endif
endif

" enforce black bg, etc.
highlight Normal ctermbg=black guibg=black 
highlight Normal ctermfg=white guifg=white 

"j,k move by screen line instead of file line
nnoremap j gj
nnoremap k gk

" disable F1 help key
inoremap <F1> <ESC>
nnoremap <F1> <ESC>
vnoremap <F1> <ESC>

" disable Ex mode shortcut
nnoremap Q <nop>

" turn off search highlighting
nnoremap <silent> <leader>hs :noh<cr>

if !exists('g:netrw_list_hide')
  let g:netrw_list_hide = '\~$,^tags$,\(^\|\s\s\)\zs\.\.\S\+'
endif

"ensure transient dirs
let s:dir = has('win32') ? '$APPDATA/Vim' : IsMac() > -1 ? '~/Library/Vim' : empty($XDG_DATA_HOME) ? '~/.local/share/vim' : '$XDG_DATA_HOME/vim'

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


