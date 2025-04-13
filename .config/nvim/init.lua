vim.cmd[[

let g:did_install_default_menus = 1  " avoid stupid menu.vim (saves ~100ms)
let g:loaded_netrwPlugin = 0  " Disable netrw. 🚮

fun! InstallPlug() " Bootstrap plugin manager on new systems.
lua << EOF
  print(vim.fn.system({'git', 'clone', 'https://github.com/savq/paq-nvim.git',
    vim.fn.stdpath('data')..'/site/pack/paqs/start/paq-nvim'}))
EOF
endfun

" Use <C-L> to:
"   - redraw
"   - clear 'hlsearch'
"   - update the current diff (if any)
" Use {count}<C-L> to also:
"   - clear all extmark namespaces
nnoremap <silent><expr> <C-L> (v:count ? '<cmd>call nvim_buf_clear_namespace(0,-1,0,-1)<cr>' : '')
      \ .. ':nohlsearch'.(has('diff')?'\|diffupdate':'')
      \ .. '<CR><C-L>'

command! Session if filereadable(stdpath('config').'/session.vim') | exe 'source '.stdpath('config').'/session.vim'
      \ | else | exe 'Obsession '.stdpath('config').'/session.vim' | endif
set sessionoptions-=blank

"==============================================================================
" general settings / options
"==============================================================================
set exrc
set scrolloff=4
set fillchars+=msgsep:‾,eob:·
set inccommand=split
set winborder=rounded

" https://github.com/neovim/neovim/issues/3463#issuecomment-148757691
" autocmd CursorHold,FocusGained,FocusLost * silent! rshada|silent! wshada
" :checktime is SLOW
" autocmd CursorHold,FocusGained * silent! checktime

set shada^=r/tmp/,r/private/,rfugitive:,rzipfile:,rterm:,rhealth:
set jumpoptions+=view
set tabclose=uselast
set cpoptions-=_
set guicursor+=t:ver25
au UIEnter * set guifont=Menlo:h20

" Don't mess with 'tabstop', with 'expandtab' it isn't used.
" Set softtabstop=-1 to mirror 'shiftwidth'.
set expandtab shiftwidth=2 softtabstop=-1
autocmd FileType * autocmd CursorMoved * ++once if !&expandtab | setlocal listchars+=tab:\ \  | endif
set listchars+=trail:⣿
set list

let g:mapleader = "z,"

set undofile
set fileformats=unix,dos

" [i, [d
set path+=/usr/lib/gcc/**/include
" neovim
set path+=build/src/nvim/auto/**,.deps/build/src/**/,src,src/nvim
" DWIM 'includeexpr': make gf work on filenames like "a/…" (in diffs, etc.).
set includeexpr=substitute(v:fname,'^[^\/]*/','','')

let g:sh_noisk = 1
" set lazyredraw  " no redraws in macros. Disabled for: https://github.com/neovim/neovim/issues/22674
set cmdheight=2
set ignorecase " case-insensitive searching
set smartcase  " but become case-sensitive if you type uppercase characters

set foldopen-=search
set timeoutlen=3000
set noshowmode " Hide the mode text (e.g. -- INSERT --)
set foldlevelstart=99 "open all folds by default
if has('patch-7.4.314') | set shortmess+=cC | endif

nnoremap <silent> yoz :<c-u>if &foldenable && 2==&foldnestmax && 0==&foldlevel\|set nofoldenable\|
      \ else\|setl foldmethod=indent foldnestmax=2 foldlevel=0 foldenable\|set foldmethod=manual\|endif<cr>

nnoremap yoT :<c-u>setlocal textwidth=<C-R>=(!v:count && &textwidth != 0) ? 0 : (v:count ? v:count : 80)<CR><CR>

set nostartofline
set cursorline
set diffopt+=hiddenoff,linematch:60,foldcolumn:0

"==============================================================================
" text, tab and indent

set formatoptions+=rno1l/
" don't syntax-highlight long lines
set synmaxcol=200

set linebreak
set nowrap

" key mappings/bindings =================================================== {{{
"
" available mappings:
"   visual: c-\ <space> m R c-r c-n c-g c-a c-x c-h,<bs><tab>
"   insert: c-\ c-g
"   normal: vd gy c-f c-t c-b c-j c-k + _ c-\ g= zu z/ m<enter> zy zi zp m<tab> q<special> y<special> q<special>
"           c<space>
"           !@       --> async run

"tnoremap <expr> <C-R> '<C-\><C-N>"'.nr2char(getchar()).'pi'

:xnoremap y zy
:nnoremap p zp
:nnoremap P zP
" copy selection to gui-clipboard
xnoremap Y "+y
" copy entire file contents (to gui-clipboard if available)
nnoremap yY :let b:winview=winsaveview()<bar>exe 'keepjumps keepmarks norm ggVG"+y'<bar>call winrestview(b:winview)<cr>
" copy current (relative) filename (to gui-clipboard if available)
nnoremap "%y <cmd>let @+=fnamemodify(@%, ':.')<cr>

" Put filename tail.
cnoremap <m-%> <c-r>=fnamemodify(@%, ':t')<cr>
cmap     <m-s-5> <m-%>
" current-file directory
noremap! <m-/> <c-r>=expand('%:.:h', 1)<cr>
noremap! <c-r>? <c-r>=substitute(getreg('/'), '[<>\\]', '', 'g')<cr>
" /<BS>: Inverse search (line NOT containing pattern).
cnoremap <expr> <BS> (getcmdtype() =~ '[/?]' && getcmdline() == '') ? '\v^(()@!.)*$<Left><Left><Left><Left><Left><Left><Left>' : '<BS>'
" Hit space to match multiline whitespace.
cnoremap <expr> <Space> getcmdtype() =~ '[/?]' ? '\_s\+' : ' '
" //: "Search within visual selection".
cnoremap <expr> / (getcmdtype() =~ '[/?]' && getcmdline() == '') ? "\<C-c>\<Esc>/\\%V" : '/'

nnoremap g: :lua =
nnoremap z= <cmd>setlocal spell<CR>z=
nnoremap ' `
inoremap <C-space> <C-x><C-o>

" niceblock
xnoremap <expr> I (mode()=~#'[vV]'?'<C-v>^o^I':'I')
xnoremap <expr> A (mode()=~#'[vV]'?'<C-v>0o$A':'A')


nnoremap g> :set nomore<bar>echo repeat("\n",&cmdheight)<bar>40messages<bar>set more<CR>

" word-wise i_CTRL-Y
inoremap <expr> <c-y> pumvisible() ? "\<c-y>" : matchstr(getline(line('.')-1), '\%' . virtcol('.') . 'v\%(\k\+\\|.\)')

" mark position before search
nnoremap / ms/

nnoremap <expr> n 'Nn'[v:searchforward]
nnoremap <expr> N 'nN'[v:searchforward]

nnoremap <up> <c-u>
nnoremap <down> <c-d>


" :help :DiffOrig
command! DiffOrig leftabove vnew | set bt=nofile | r ++edit # | 0d_ | diffthis | wincmd p | diffthis

nnoremap yo<space> :set <C-R>=(&diffopt =~# 'iwhiteall') ? 'diffopt-=iwhiteall' : 'diffopt+=iwhiteall'<CR><CR>

" Format filters
" ideas: https://github.com/sbdchd/neoformat
nnoremap gqax    :%!tidy -q -i -xml -utf8<cr>
nnoremap gqah    :%!tidy -q -i -ashtml -utf8<cr>
nnoremap gqaj    :%!python3 -m json.tool<cr>
nnoremap gqav    :call append('$', json_encode(eval(join(getline(1,'$')))))<cr>'[k"_dVgg:%!python -m json.tool<cr>

" un-join (split) the current line at the cursor position
nnoremap gj i<c-j><esc>k$
xnoremap x  "_d

nnoremap vK <C-\><C-N>:help <C-R><C-W><CR>

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

" Insert formatted datetime (from @tpope vimrc).
inoremap <silent> <C-G><C-T> <C-R>=repeat(complete(col('.'),map(["%Y-%m-%d %H:%M:%S","%a, %d %b %Y %H:%M:%S %z","%Y %b %d","%d-%b-%y","%a %b %d %T %Z %Y","%Y%m%d"],'strftime(v:val)')+[localtime()]),0)<CR>
" Print unix time at cursor as human-readable datetime. 1677604904 => '2023-02-28 09:21:45'
nnoremap gA :echo strftime('%Y-%m-%d %H:%M:%S', '<c-r><c-w>')<cr>

" Preserve '[ '] on :write.
nnoremap <silent> z. :silent lockmarks update ++p<cr>

" Select last inserted text.
nnoremap gV `[v`]

" Repeat last command for each line of a visual selection.
xnoremap . :normal .<CR>
" Repeat the last edit on the next [count] matches.
nnoremap <silent> gn :normal n.<CR>

" Record a macro, or set the last-recorded macro to v:register (example: "aq).
nnoremap <expr> q (v:register==#'"')?'q':(':let @'.(empty(reg_recorded())?'q':reg_recorded())." = '<C-R>=substitute(@".v:register.",\"'\",\"''\",\"g\")<CR>'<C-F>010l")
nnoremap <C-v>q q
nnoremap \q q

command! -nargs=+ -bang -complete=command R if !<bang>0 | wincmd n | endif
    \ | call execute(printf("put=execute('%s')", substitute(escape(<q-args>, '"'), "'", "''", 'g')))
inoremap <c-r>R <c-o>:<up><home>R! <cr>

" }}} mappings

augroup config_autocmd
  autocmd!
  autocmd BufReadCmd *.vsix call zip#Browse(expand("<amatch>"))
  autocmd BufReadPost *.i setlocal filetype=c
  autocmd BufHidden,FocusLost * if &buftype=='' && filereadable(expand('%:p')) | silent lockmarks update ++p | endif

  " :help restore-cursor
  autocmd BufReadPre * autocmd FileType <buffer> ++once
    \ if !&diff && &ft !~# 'commit\|rebase' && line("'\"") > 1 && line("'\"") <= line("$") | exe 'normal! g`"' | endif

  autocmd BufNewFile,BufRead *.txt,README,INSTALL,NEWS,TODO if expand('<afile>:t') !=# 'CMakeLists.txt' | setf text | endif

  autocmd TextYankPost * silent! lua vim.hl.on_yank {higroup='Visual', timeout=300}

  " autocmd VimEnter * if !empty($NVIM)
  "       \ |let g:r=jobstart(['nc', '-U', $NVIM],{'rpc':v:true})
  "       \ |let g:f=fnameescape(expand('%:p'))
  "       \ |noau bwipe
  "       \ |call rpcrequest(g:r, "nvim_command", "tabedit ".g:f)|qa|endif

  " if exists('##TextYankPost')
  "   autocmd TextYankPost * let g:yankring=get(g:,'yankring',[])
  "     \|call add(g:yankring, join(v:event.regcontents[:999], "\n"))|if len(g:yankring)>10|call remove(g:yankring, 0, 1)|endif
  " endif
augroup END

set wildcharm=<C-Z>
set wildoptions+=fuzzy
nnoremap <expr> <C-b> v:count ? ':<c-u>'.v:count.'buffer<cr>' : ':set nomore<bar>ls<bar>set more<cr>:buffer<space>'
" _opt-in_ to sloppy-search https://github.com/neovim/neovim/issues/3209#issuecomment-133183790
nnoremap <C-f> :edit **/
nnoremap \t    :tag<space>
" See `man fnmatch`.
nnoremap \g  mS:Ggrep! -q <C-R>=(system(['git','grep','-P'])=~#'no pattern')?'-P':'-E'<CR> <C-R>=shellescape(fnameescape(expand('<cword>')))<CR> -- ':/' ':/!*.mpack' ':/!*.pbf' ':/!*.pdf' ':/!*.po' ':(top,exclude,icase)notebooks/' ':/!data/' ':/!work/' ':/!qgis/' ':/!graphhopper_data/'
      \<Home><C-Right><C-Right><C-Right><C-Right><left>
nnoremap 9\g  :Ggrep<m-up><Home><C-Right><C-Right><C-Right><C-Right><left>
nnoremap \v  mS:<c-u>noau vimgrep /\C/j **<left><left><left><left><left>
" search all file buffers (clear qf first).
nnoremap \b  mS:<c-u>cexpr []<bar>exe 'bufdo silent! noau vimgrepadd/\C/j %'<bar>botright copen<s-left><s-left><left><left><left>
" search current buffer and open results in loclist
nnoremap \c   ms:<c-u>lvimgrep // % <bar>lw<s-left><left><left><left><left>

" =============================================================================
" autocomplete / omnicomplete / tags
" =============================================================================
set dictionary+=/usr/share/dict/words
set completeopt=menuone,noselect,noinsert,fuzzy
set complete+=f,kspell
set wildignore+=tags,gwt-unitCache/*,*/__pycache__/*,build/*,build.?/*,*/node_modules/*
" Files with these suffixes get a lower priority when matching a wildcard
set suffixes+=.aux,.log,.dvi,.bbl,.blg,.brf,.cb,.ind,.idx,.ilg,.inx,.out,.toc
      \,.o,.obj,.dll,.class,.pyc,.ipynb,.so,.swp,.zip,.exe,.jar,.gz
" Better `gf`
set suffixesadd=.java,.cs

set title
set titlestring=%{fnamemodify(getcwd(),':~')}

" special-purpose mappings/commands ===========================================
nnoremap <leader>vv   :exe 'e' fnameescape(resolve($MYVIMRC))<cr>
nnoremap <leader>vp   :exe 'e' stdpath('config')..'/plugin/'<cr>
nnoremap <leader>vr   :Vimref<cr>

" scriptease
" TODO:
"   - g={motion} / g!{motion}
"   - :PP (pretty print expression)
nnoremap <leader>vs   :caddexpr join(map(split(execute('scriptnames'), '\n'), {k,v->matchstr(v,':\s*\zs.*')..':1: '}),"\n")<bar>copen<cr>
command! -addr=other -range=-1 -nargs=? -complete=command Time exe TimeCommand(<q-args>, <count>)
function! TimeCommand(cmd, count) abort
  let time = reltime()
  try
    if a:count > 1
      let i = 0
      while i < a:count
        execute a:cmd
        let i += 1
      endwhile
    else
      execute a:cmd
    endif
  finally
    let elapsed = reltime(time)
    redraw
    echomsg matchstr(reltimestr(elapsed), '.*\..\{,3\}') . ' seconds to run :'.a:cmd
  endtry
  return ''
endfunction

nnoremap <leader>== <cmd>set paste<cr>o<cr><c-r>=repeat('=',80)<cr><cr><c-r>=strftime('%Y%m%d')<cr><cr><c-r>+<cr>tags: <esc><cmd>set nopaste<cr>

command! CdVim          exe 'e '.finddir(".vim-src", expand("~")."/neovim/**,".expand("~")."/dev/neovim/**")<bar>lcd %
command! NvimTestScreenshot put =\"local Screen = require('test.functional.ui.screen')\nlocal screen = Screen.new()\nscreen:attach()\nscreen:snapshot_util({},true)\"

func! GetVimref(...) abort
  let tagpat = '\v(\d+\.){2}(\d)+'
  let ref = (a:0 is 0 || empty(a:1)) ? matchstr(expand('<cWORD>'), tagpat) : matchstr(a:1, tagpat)
  if empty(ref) && (a:0 is 0 || empty(a:1))
    return expand('<cword>')
  endif
  return empty(ref) ? a:1 : 'v'..ref
endfunc
func! EditVimref(...) abort
  let ref = GetVimref(a:0 ? a:1 : '')
  let vimdir = luaeval('vim.fs.normalize(vim.fn.expand("~/dev/neovim/.vim-src/"))')
  if winnr('#') != 0
    wincmd p
    let cwd = luaeval('vim.fs.normalize(vim.fn.expand(vim.fn.getcwd()))')
    if cwd !=# vimdir
      " Switch back
      wincmd p
      split ~/dev/neovim/.vim-src/
      lcd %:h
    endif
  else
    split ~/dev/neovim/.vim-src/
    lcd %:h
  endif
  exe 'Gedit '..ref
endfunc
command! -nargs=? Vimref call EditVimref('<args>')
command! Tags !ctags -R -I EXTERN -I INIT --exclude='build*/**' --exclude='**/build*/**' --exclude='cdk.out/**' --exclude='**/cdk.out/**' --exclude='.vim-src/**' --exclude='**/dist/**' --exclude='node_modules/**' --exclude='**/node_modules/**' --exclude='venv/**' --exclude='**/site-packages/**' --exclude='data/**' --exclude='dist/**' --exclude='notebooks/**' --exclude='Notebooks/**' --exclude='*graphhopper_data/*.json' --exclude='*graphhopper/*.json' --exclude='*.json' --exclude='qgis/**'
  \ --exclude=.git --exclude=.svn --exclude=.hg --exclude="*.cache.html" --exclude="*.nocache.js" --exclude="*.min.*" --exclude="*.map" --exclude="*.swp" --exclude="*.bak" --exclude="*.pyc" --exclude="*.class" --exclude="*.sln" --exclude="*.Master" --exclude="*.csproj" --exclude="*.csproj.user" --exclude="*.cache" --exclude="*.dll" --exclude="*.pdb" --exclude=tags --exclude="cscope.*" --exclude="*.tar.*"
  \ *

" Neovim/Vim development
autocmd BufEnter */.vim-src/* setlocal nolist

function! Cxn_py() abort
  vsplit
  terminal
  let py = isdirectory('venv') ? './venv/bin/python3' : 'python3'
  call chansend(&channel, py .. "\nimport pynvim\n")
  call chansend(&channel, "n = pynvim.attach('socket', path='".g:cxn."')\n")
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
  let nvim_path = v:progpath " executable('build/bin/nvim') ? 'build/bin/nvim' : 'nvim'
  call chansend(&channel, nvim_path." -u NORC\n")
  call chansend(&channel, ":let j=jobstart('nc -U ".v:servername."',{'rpc':v:true})\n")
  call chansend(&channel, ":call rpcrequest(j, 'nvim_set_var', 'cxn', v:servername)\n")
  call chansend(&channel, ":call rpcrequest(j, 'nvim_command', 'call Cxn_py()')\n")
endfunction
command! -nargs=* NvimCxn call Cxn(<q-args>)

silent! source ~/.vimrc.local
]]
