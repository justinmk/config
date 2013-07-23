
" windows builds: http://tuxproject.de/projects/vim/
"                 http://files.kaoriya.net/vim/
" MacVim with homebrew:
"   brew install macvim --with-cscope --with-lua --override-system-vim
"

" If this .vimrc is not in $HOME, add these lines to $HOME/.vimrc :
"    set runtimepath+=/path/to/.vim
"    source /path/to/.vimrc
"==============================================================================

if exists('&guioptions')
    set guitablabel=%t
    "no toolbar, no menu bar
    set guioptions-=T
    set guioptions-=m
    "don't source &runtimepath/menu.vim. (must be done before 'filetype on' / 'syntax on')
    set guioptions-=M
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
let s:is_vimRecentBuildWithLua = ! (!has('lua') || v:version < 703 || (v:version == 703 && !has('patch885')))

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

if s:is_windows
Bundle 'tomasr/molokai'
else
Bundle 'nanotech/jellybeans.vim'
Bundle 'benmills/vimux'
endif
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
Bundle 'tpope/vim-endwise'
Bundle 'tpope/vim-speeddating'
Bundle 'kshenoy/vim-signature'
Bundle 'kana/vim-smartinput'
Bundle 'kana/vim-textobj-user'
Bundle 'kana/vim-textobj-indent'
Bundle 'Valloric/MatchTagAlways'
" Bundle 'Valloric/YouCompleteMe'
" Bundle 'Lokaltog/vim-easymotion'
Bundle 'goldfeld/vim-seek'
Bundle 'Lokaltog/vim-powerline'
Bundle 'PProvost/vim-ps1'
Bundle 'tomtom/tcomment_vim'
Bundle 'ap/vim-css-color'
Bundle 'OrelSokolov/HiCursorWords'
Bundle 'airblade/vim-gitgutter'
Bundle 'derekwyatt/vim-scala'
Bundle 'Blackrush/vim-gocode'
Bundle 'justinmk/vim-ipmotion'
Bundle 'argtextobj.vim'
Bundle 'tsukkee/unite-tag'
Bundle 'Shougo/unite.vim'
Bundle 'Shougo/unite-outline'
if s:is_vimRecentBuildWithLua
Bundle 'Shougo/neocomplete.vim'
else
Bundle 'Shougo/neocomplcache'
endif

filetype plugin indent on     " required!


" 'is GUI' means vim is _not_ running within the terminal.
" sample values:
"   &term  = win32 //vimrc running in msysgit terminal
"   $TERM  = xterm-color , cygwin
"   &term  = builtin_gui //*after* vimrc but *before* gvimrc
"   &shell = C:\Windows\system32\cmd.exe , /bin/bash
fun! IsGui()
    return has('gui_running') || strlen(&term) == 0 || &term ==? 'builtin_gui'
endfun

" 'has GUI' means vim is "gui-capable"--but we may be using gvim/macvim from within the terminal.
fun! HasGui()
    return has('gui')
endfun

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
" general
"==============================================================================

" To map a 'meta' escape sequence in a terminal, you must map the literal control character.
" insert-mode, type ctrl-v, then press alt+<key>. This must be done in a terminal, not gvim/macvim.
" http://vim.wikia.com/wiki/Mapping_fast_keycodes_in_terminal_Vim
" http://stackoverflow.com/a/10633069/152142
if !s:is_msysgit && !IsGui()
    set <m-p>=p <m-b>=b <m-o>=o <m-y>=y <m-j>=j <m-k>=k <m-r>=r
    set <m-t>=t <m-l>=l
endif

set list
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

    if IsGui()
        "set guifont=Monaco:h16
        set guifont=Menlo:h14

        let g:Powerline_symbols = 'unicode'
    endif
elseif IsGui() "linux or other
    set guifont=Monospace\ 10
endif

if !s:is_msysgit
    "highlight the current line
    set cursorline

    if &t_Co != 88 && &t_Co < 256 && (s:is_tmux || &term =~? 'xterm')
        " force colors
        set t_Co=256
    endif

    if !s:is_mac
      autocmd ColorScheme * highlight Normal ctermbg=black guibg=black
      autocmd ColorScheme * highlight NonText ctermbg=black guibg=black
    endif

    if empty(&t_Co) || &t_Co > 16
        if s:is_windows
            " expects &runtimepath/colors/{name}.vim.
            colorscheme molokai
        else
            let g:jellybeans_use_lowcolor_black = 0
            colorscheme jellybeans
        endif

        autocmd ColorScheme * highlight Visual guibg=#35322d
        autocmd ColorScheme * highlight Cursor guibg=#0a9dff guifg=white gui=bold 
        autocmd ColorScheme * highlight Pmenu guifg=#f8f6f2 guibg=#1c1b1a 
        autocmd ColorScheme * highlight PmenuSel guibg=#0a9dff 
        autocmd ColorScheme * highlight PmenuSbar guibg=#857f78
        autocmd ColorScheme * highlight PmenuThumb guifg=#242321
        autocmd ColorScheme * highlight WildMenu gui=none guifg=#f8f6f2 guibg=#0a9dff
        autocmd ColorScheme * highlight StatusLine gui=reverse guifg=#455354 guibg=fg
        autocmd ColorScheme * highlight IncSearch guifg=white guibg=LimeGreen gui=bold
        autocmd ColorScheme * highlight Search guifg=black guibg=LightGoldenrod1
        "1c1b1a darkestgravel
        "141413 blackgravel
    endif
endif

set fileformats=unix,dos,mac
set encoding=utf8
try
    lang en_US
catch
endtry

let g:Powerline_stl_path_style = 'short'
let g:HiCursorWords_delay = 1000

"==============================================================================
" text, tab and indent 
"==============================================================================
set formatoptions+=rn1

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
"set textwidth=80
set nofoldenable " disable folding

if exists('&colorcolumn')
    set colorcolumn=80 "highlight the specified column
endif

" =============================================================================
" util functions
" =============================================================================

func! DeleteTrailingWhitespace()
  exe "normal mz"
  %s/\s\+$//ge
  exe "normal `z"
endfunc

"return the syntax highlight group under the cursor ''
function! CurrentWordSyntaxName()
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

" =============================================================================
" normal mode
" =============================================================================
" Remove the Windows ^M - when the encodings gets messed up
nnoremap <Leader>m mmHmt:%s/<C-V><cr>//ge<cr>'tzt'm

set pastetoggle=<leader>pp
" Paste Line: paste a word as a line
nnoremap <leader>pl o<esc>p==

" paste current dir to command line
cabbrev $c <c-r>=expand("%:p:h")<cr>

" $q is super useful when browsing on the command line
cno $q <C-\>eDeleteTillSlash()<cr>


func! DeleteTillSlash()
  let g:cmd = getcmdline()
  if s:is_windows
    let g:cmd_edited = substitute(g:cmd, "\\(.*\[\\\\]\\).*", "\\1", "")
  else
    let g:cmd_edited = substitute(g:cmd, "\\(.*\[/\]\\).*", "\\1", "")
  endif
  if g:cmd == g:cmd_edited
    if s:is_windows
      let g:cmd_edited = substitute(g:cmd, "\\(.*\[\\\\\]\\).*\[\\\\\]", "\\1", "")
    else
      let g:cmd_edited = substitute(g:cmd, "\\(.*\[/\]\\).*/", "\\1", "")
    endif
  endif   
  return g:cmd_edited
endfunc


" abbreviations ===============================================================

iab xdate <c-r>=strftime("%d/%m/%y %H:%M:%S")<cr>

"==============================================================================
" key mappings/bindings
"==============================================================================
" move between windows
nnoremap <silent> <C-j> :wincmd j<cr>
nnoremap <silent> <C-k> :wincmd k<cr>
nnoremap <silent> <C-h> :wincmd h<cr>
nnoremap <silent> <C-l> :wincmd l<cr>

" Close the current buffer
nnoremap <leader>bd :call <SID>BufKill()<cr>

function! <SID>BufKill()
  let l:bufnum = bufnr("%")
  "valid 'next' buffers 
  "   EXCLUDE: current, Unite, Vundle, and [buffers already open in another window in the current tab]
  "   INCLUDE: normal buffers; 'help' buffers
  let l:valid_buffers = filter(range(1, bufnr('$')), 
              \ '((buflisted(v:val) && "" ==# &buftype)|| "help" ==# &buftype) '.
              \ '&& v:val != l:bufnum '.
              \ '&& -1 == index(tabpagebuflist(), v:val) '.
              \ '&& bufname(v:val) !~# ''\\*unite\*\|[\(unite\|Vundle\)\]''')

  if len(l:valid_buffers) > 1
    if -1 != index(l:valid_buffers, bufnr("#"))
      buffer #
    else
      exe 'buffer '.l:valid_buffers[0]
    endif
  endif

  let l:is_in_another_win = l:bufnum != bufnr("%") && -1 < bufwinnr(l:bufnum)
  if !l:is_in_another_win && buflisted(l:bufnum)
    exe 'bdelete! '.l:bufnum
  endif
endfunction

"move to first non-whitespace char, instead of first column
noremap 1 ^
"move to last character 
noremap 0 $

" un-join (split) the current line at the cursor position
nnoremap K i<cr><esc>k$
" delete without overwriting yank register
noremap <leader>d "_d
nnoremap <leader>D "_D

inoremap jj <esc>
nnoremap <c-d> <PageDown>
nnoremap <c-u> <PageUp>
nnoremap <space> :
nnoremap <leader>w :w!<cr>

"toggle/untoggle spell checking
nnoremap <leader>ss :setlocal spell!<cr>

"text bubbling: move text up/down with meta-[jk] 
nnoremap <M-j> mz:m+<cr>`z
nnoremap <M-k> mz:m-2<cr>`z
xnoremap <M-j> :m'>+<cr>`<my`>mzgv`yo`z
xnoremap <M-k> :m'<-2<cr>`>my`<mzgv`yo`z

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

" disable Ex mode shortcut
nnoremap Q <nop>

" turn off search highlighting
nnoremap <silent> <leader>hs :nohlsearch<cr>

" navigate to the directory of the current file
if s:is_tmux
    nnoremap <leader>gf :silent execute '!tmux split-window -h \; ' .  'send-keys "cd "' . substitute(expand("%:p:h")," ","\\\\ ","g") . ' C-m'<cr>
elseif s:is_windows
    nnoremap <leader>gf :silent !start explorer /select,%:p<cr>
elseif s:is_mac
    nnoremap <leader>gf :silent execute '!open ' . substitute(expand("%:p:h")," ","\\\\ ","g")<cr>
endif


" python ======================================================================
autocmd BufWrite *.py :call DeleteTrailingWhitespace()

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


" golang ======================================================================
" possible godoc solution    https://gist.github.com/mattn/569652
"    Bundle 'thinca/vim-ref'
"    let g:ref_use_vimproc = 1
"    let g:ref_open = 'vsplit'
"    let g:ref_cache_dir = expand('~/.vim/tmp/ref_cache/')
"    nno <leader>K :<C-u>Unite ref/godoc -buffer-name=godoc -start-insert -horizontal<CR>

autocmd BufWrite *.go :call DeleteTrailingWhitespace()

" abbreviations
au FileType go iab <buffer> ife if err != nil {<cr>log.Fatal(err)}<cr>
au FileType go iab <buffer> ,,e if err != nil {<cr>log.Fatal(err)}<cr>
au FileType go iab <buffer> er- if err != nil {<cr>log.Fatal(err)}<cr>

"==============================================================================
" vim grep/search/replace
"==============================================================================
nnoremap <leader>cc :botright cope<cr>

" :noau speeds up vimgrep
noremap <leader>grep :noau vimgrep // **<left><left><left><left>
" search and replace word under cursor
nnoremap <leader>sr :<c-u>%s/<c-r><c-w>//gc<left><left><left>
xnoremap <leader>sr :<c-u>call <SID>VSetSearch('/')<cr>:%s/<c-r>=@/<cr>//gc<left><left><left>

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
nnoremap <leader>* :execute 'noautocmd vimgrep /\V' . substitute(escape(expand("<cword>"), '\'), '\n', '\\n', 'g') . '/ **'<CR>
vnoremap <leader>* :<C-u>call <SID>VSetSearch('/')<CR>:execute 'noautocmd vimgrep /' . @/ . '/ **'<CR>

" =============================================================================
" omni complete
" =============================================================================
autocmd FileType css set omnifunc=csscomplete#CompleteCSS

" Don't scan included files. Tags file is more performant.
set complete-=i
set completeopt-=preview
set completeopt+=longest

if s:is_vimRecentBuildWithLua
    nnoremap <leader>neo :NeoCompleteEnable<cr>
    let g:neocomplete#enable_smart_case = 1
    if !exists('g:neocomplete#sources#omni#input_patterns')
        let g:neocomplete#sources#omni#input_patterns = {}
    endif
    let g:neocomplete#sources#omni#input_patterns.go = '[^.[:digit:] *\t]\.\w*'
else
    nnoremap <leader>neo :NeoComplCacheEnable<cr>
    let g:neocomplcache_enable_smart_case = 1
    if !exists('g:neocomplcache_sources_omni_input_patterns')
        let g:neocomplcache_sources_omni_input_patterns = {}
    endif
    let g:neocomplcache_sources_omni_input_patterns.go = '[^.[:digit:] *\t]\.\w*'
endif

" =============================================================================
" autocomplete
" =============================================================================
set wildmode=full
set wildignore+=tags,*.o,*.obj,*.class,.git,.hg,.svn,*.pyc,*/tmp/*,*.so,*.swp,*.zip,*.exe

if s:is_windows
    set wildignore+=Windows\\*,Program\ Files\\*,Program\ Files\ \(x86\)\\* 
    " let g:ctrlp_buftag_ctags_bin = '~/bin/ctags.exe'
endif

" let g:ctrlp_custom_ignore = {
"   \ 'dir':  '\v[\/]\.(git|hg|svn|cache)$|AppData|eclipse_workspace|grimoire-remote',
"   \ 'file': '\v\~$|\.(exe|so|dll|pdf|ntuser|blf|dat|regtrans-ms|o|swp|pyc|wav|mp3|ogg|blend)$' }


" Unite =======================================================================
let g:unite_source_history_yank_enable = 1
let g:unite_force_overwrite_statusline = 0

call unite#custom#profile('files', 'filters', 'sorter_rank')
call unite#custom#profile('files_glob', 'matchers', ['matcher_glob'])
" see unite/custom.vim
call unite#custom#source(
            \ 'buffer,file_rec/async,file_rec,file_mru,directory_rec,outline', 
            \ 'sorters',
            \ ['sorter_rank'])
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
    \ '\|\.\%(jpg\|gif\|png\)$' .
    \ '\|Downloads\|eclipse_workspace'
if s:is_windows
    let s:file_rec_ignore .= '\|AppData'
elseif s:is_mac
    let s:file_rec_ignore .= '\|Library'
endif
call unite#custom#source('file_rec,directory_rec', 'ignore_pattern', s:file_rec_ignore)

" nnoremap <c-p> :Unite -no-split -buffer-name=files  -start-insert file_rec/async:!<cr>
" search hidden directories:
" nnoremap <c-p>   :Unite -no-split -buffer-name=files  -start-insert file_rec:. directory_rec:. <cr>
nnoremap <c-p> :Unite -no-split -buffer-name=files -start-insert file_mru file_rec <cr>
" nnoremap <m-p> :Unite -no-split -buffer-name=files_glob -start-insert file_rec <cr>
nnoremap <m-l> :Unite -no-split -buffer-name=buffer -start-insert buffer<cr>
" auto-generates exuberant ctags for the current buffer!
nnoremap <m-o> :Unite -no-split -buffer-name=outline -start-insert outline<cr>
nnoremap <m-t> :Unite -no-split -buffer-name=tag -start-insert tag<cr>
nnoremap <m-y> :Unite -no-split -buffer-name=yank history/yank<cr>
nnoremap <leader>cd :<C-u>Unite -no-split directory_mru directory_rec:. -start-insert -buffer-name=cd -default-action=cd<CR>
nnoremap <leader>ps :<C-u>Unite process -buffer-name=processes -start-insert<CR>

" Custom mappings for the unite buffer
autocmd FileType unite call s:unite_settings()
function! s:unite_settings()
  setlocal nolist
  nmap <buffer> <esc> <Plug>(unite_exit)
  " refresh the cache
  nmap <buffer> <F5>  <Plug>(unite_redraw)
  imap <buffer> <F5>  <Plug>(unite_redraw)
  " change directories in unite
  nmap <buffer> <leader>cd <Plug>(unite_restart) 
endfunction

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

    if IsGui()
        " set viminfo+=% "remember buffer list
        autocmd VimEnter * nested :call LoadSession()
        " autocmd GUIEnter * nested :call LoadSession()
    endif

    if s:is_windows
        " use .viminfo instead of _viminfo
        set viminfo+=n~/.viminfo
        " always maximize initial GUI window size
        autocmd GUIEnter * simalt ~x 
    endif
endif

if !exists('g:netrw_list_hide')
  let g:netrw_list_hide = '\~$,^tags$,\(^\|\s\s\)\zs\.\.\S\+'
endif

"ensure transient dirs
let s:dir = has('win32') ? '$APPDATA/Vim' : s:is_mac ? '~/Library/Vim' : empty($XDG_DATA_HOME) ? '~/.local/share/vim' : '$XDG_DATA_HOME/vim'

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


