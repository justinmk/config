" Needed when running as `vi`. {{{
set nocp
set hidden
set background=dark
set showcmd
" }}}

if has('vim_starting')
  " required for alt/meta mappings  https://github.com/tpope/vim-sensible/issues/69
  set encoding=utf-8
endif

" win32 gvim
if &rtp !~? '\v(('.escape(expand('~'), '/\').')|\~)[/\\]\.nvim'
  set runtimepath+=~/.config/nvim
endif

if 1
  let s:is_msysgit = (has('win32') || has('win64')) && $TERM ==? 'cygwin'
  let s:plugins = filereadable(expand("~/.config/nvim/autoload/plug.vim", 1))
  let s:is_gui = has('gui_running') || strlen(&term) == 0 || &term ==? 'builtin_gui'

  exe 'source '.expand('<sfile>:p:h').'/.config/nvim/init.vim'
endif


" To map a 'meta' escape sequence in a terminal, you must map the literal control character.
" insert-mode, type ctrl-v, then press alt+<key> (while in a terminal, not gvim).
" http://vim.wikia.com/wiki/Mapping_fast_keycodes_in_terminal_Vim
" http://stackoverflow.com/a/10633069/152142
if 1 && !s:is_msysgit && !s:is_gui
    "avoid: m-b m-d m-f
    set <m-g>=g <m-h>=h <m-i>=i <m-j>=j <m-k>=k <m-l>=l <m-m>=m <m-n>=n <m-o>=o <m-p>=p <m-q>=q <m-r>=r <m-s>=s <m-t>=t <m-w>=w <m-x>=x <m-y>=y <m-z>=z <m-]>=] <m-;>=;
endif

if has('win32')
  autocmd GUIEnter * simalt ~x  " always maximize initial GUI window

  set guifont=Consolas:h11
  if has("directx")
    set renderoptions=type:directx
  endif
endif

" sensible.vim {{{

" may affect performance: https://github.com/tpope/vim-sensible/issues/57
" let &listchars = "tab:\u21e5 ,trail:\u2423,extends:\u21c9,precedes:\u21c7,nbsp:\u00b7"
" let &showbreak="\u21aa" " precedes line wrap
set listchars=tab:>\ ,trail:-,nbsp:+

"transient dirs
if 1
  let s:dir = '~/.local/share/vim'
  let &directory = expand(s:dir, 1).'/swap//,'.&directory
  if has("persistent_undo")
    let &undodir = expand(s:dir, 1).'/undo//,'.&undodir
  endif
endif

set hidden
set ttimeout
set ttimeoutlen=100
set backspace=eol,start,indent
set wildmenu
set display+=lastline
set viminfo^=!
set sessionoptions-=options

if v:version > 703 || v:version == 703 && has("patch541")
  set formatoptions+=j " Delete comment character when joining commented lines
endif
setglobal tags-=./tags tags-=./tags; tags^=./tags;

set autoindent  " Note: 'smartindent' is deprecated by 'cindent' and 'indentexpr'.
set complete-=i " Don't scan includes (tags file is more performant).
set smarttab    " Use 'shiftwidth' when using <Tab> in front of a line.

set incsearch
set hlsearch    " highlight search matches

set autoread

" Load matchit.vim, but only if the user hasn't installed a newer version.
if 1 && !exists('g:loaded_matchit') && findfile('plugin/matchit.vim', &rtp) ==# ''
  runtime! macros/matchit.vim
endif

" Allow color schemes to do bright colors without forcing bold.
if &t_Co == 8 && $TERM !~# '^linux'
  set t_Co=16
endif

set nrformats-=octal
set laststatus=2
set history=10000

if !s:plugins
  if has('syntax') && !exists('g:syntax_on')
    syntax enable
  endif
  filetype plugin indent on
endif

" }}} sensible.vim

