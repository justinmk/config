vim.cmd[=[
if has('nvim') && isdirectory(stdpath('data').'/site/pack/paqs/start/vim-fugitive')
  func! s:ctrl_g(cnt) abort
    redraw
    redir => msg | silent exe "norm! 1\<c-g>" | redir END
    " Show git branch.
    echo FugitiveHead(7) msg[2:] (a:cnt?strftime('%Y-%m-%d %H:%M',getftime(expand('%:p'))):'')
    " Show current directory.
    echo 'dir:' fnamemodify(getcwd(), ':~')
    " Show current session.
    echo 'ses:' (strlen(v:this_session) ? fnamemodify(v:this_session, ':~') : '<none>')
    " Show current context.
    " https://git.savannah.gnu.org/cgit/diffutils.git/tree/src/diff.c?id=eaa2a24#n464
    echohl ModeMsg
    echo getline(search('\v^[[:alpha:]$_]', 'bn', 1, 100))
    echohl None
  endf
  nnoremap <C-g> :<c-u>call <sid>ctrl_g(v:count)<cr>

  func! s:fug_detect() abort
    if !exists('b:git_dir')
      call FugitiveDetect()
    endif
  endfunc
endif

augroup config_fug
  autocmd!
  " Defaults for text-like buffers.
  autocmd VimEnter,BufNew * autocmd InsertEnter <buffer=abuf> ++once if &filetype ==# '' | exe 'runtime! after/ftplugin/text.vim' | endif
  autocmd FileType markdown,gitcommit runtime! after/ftplugin/text.vim

  autocmd FileType gitconfig setlocal commentstring=#\ %s
  " For the ":G log" buffer opened by the "UL" mapping.
  autocmd FileType git if get(b:, 'fugitive_type') ==# 'temp'
    \ | exe 'nnoremap <nowait><buffer><silent> <C-n> <C-\><C-n>0j:call feedkeys("p")<CR>'
    \ | exe 'nnoremap <nowait><buffer><silent> <C-p> <C-\><C-n>0k:call feedkeys("p")<CR>'
    \ | exe 'nnoremap <nowait><buffer><silent> q <C-w>q'
    \ | match Comment /  \S\+ ([^)]\+)$/
    \ | endif
  function! s:setup_gitstatus() abort
    unmap <buffer> U
  endfunction
  autocmd FileType fugitive call <SID>setup_gitstatus()
  autocmd FileType fugitive,fugitiveblame nmap <silent><buffer> q gq
  autocmd BufWinEnter * if exists("*FugitiveDetect") && empty(expand('<afile>'))|call FugitiveDetect(getcwd())|endif

  "when Vim starts in diff-mode (vim -d, git mergetool):
  "  - do/dp should not auto-fold
  autocmd VimEnter * if &diff | exe 'windo set foldmethod=manual' | endif
augroup END

nmap <expr> <C-n> &diff?']c]n':(luaeval('({pcall(require, "gitsigns")})[1]')?'<cmd>lua require("gitsigns").next_hunk({wrap=false})<cr>':']n')
nmap <expr> <C-p> &diff?'[c[n':(luaeval('({pcall(require, "gitsigns")})[1]')?'<cmd>lua require("gitsigns").prev_hunk({wrap=false})<cr>':'[n')

" version control
xnoremap <expr> D (mode() ==# "V" ? ':Linediff<cr>' : 'D')

" Blame:
nnoremap <expr>   Ub              '@_<cmd>G blame '..(v:count?'--ignore-revs-file ""':'')..'<cr>'
nnoremap <silent> 1Ub             :.,G blame<bar>call feedkeys("\<lt>cr>")<cr>
xnoremap          Ub              :G blame<cr>
" Blame "name":
nnoremap          Un              <cmd>Gitsigns blame_line<cr>

" Commit using the current file's most-recent commit-message.
nnoremap <expr>   Uc              '@_:G commit '..(v:count ? '--no-verify' : '')..' --edit -m '..shellescape(FugitiveExecute(['log', '-1', '--format=%s', '--', FugitivePath()]).stdout[0])..'<cr>'
nnoremap <silent> Ud              :<C-U>if &diff<bar>diffupdate<bar>elseif !v:count && empty(FugitiveExecute(['diff', '--', FugitivePath()]))<bar>echo 'no changes'<bar>else<bar>exe 'Gvdiffsplit'.(v:count ? ' HEAD'.repeat('^', v:count) : '')<bar>call feedkeys('<c-v><c-l>')<bar>endif<cr>
nnoremap <silent> Ue              :Gedit<cr>
nnoremap          Uf              :G show <c-r>=FugitiveExecute(['log', '-1', '--format=%h', '--', FugitivePath()]).stdout[0]<cr><cr><c-w><c-w>:G commit --fixup=<c-r>=FugitiveExecute(['log', '-1', '--format=%h', '--', FugitivePath()]).stdout[0]<cr>

" Log:
nnoremap <expr>   Ul              '@_<cmd>G log --pretty="%h%d %s  %aN (%cr)" --date=relative'.(v:count?'':' --follow -- %').'<cr>'
xnoremap          Ul              :Gclog!<cr>
nnoremap <expr>   1Ul             '@_<cmd>Gedit @<cr>'

nnoremap          U:              :G log --pretty="%h%d %s  %aN (%cr)" --date=relative 
nnoremap          Um              :G log --pretty="%h%d %s  %aN (%cr)" --date=relative -L :<C-r><C-w>:<C-r>%
nnoremap <expr>   Ur              '@_<cmd>Gread'.(v:count?(' @'.repeat('^',v:count).':%'):'').'<cr>'
nnoremap <silent> Us              :G<cr>
nnoremap <silent> Uu              :Gedit <C-R><C-A><cr>
nnoremap <silent> Uw              :call <sid>fug_detect()<bar>Gwrite!<cr>
nnoremap          Ux              :<c-u>try<bar>.GBrowse<bar>catch<bar>call feedkeys(':.GBrowse @')<bar>endtry<cr>
xnoremap          Ux              :<c-u>try<bar>'<,'>GBrowse<bar>catch<bar>call feedkeys('gv:GBrowse @')<bar>endtry<cr>
nnoremap          U.              :G  <c-r><c-w><bar>G s<home><right><right>

nmap UB Ub
nmap 1UB 1Ub
xmap UB Ub
nmap UC Uc
nmap UD Ud
nmap UE Ue
nmap UF Uf
nmap UL Ul
xmap UL Ul
nmap 1UL 1Ul
nmap UM Um
nmap UN Un
nmap UR Ur
nmap US Us
nmap UU Uu
nmap UW Uw
nmap UX Ux
xmap UX Ux

"linewise partial staging in visual-mode.
xnoremap <c-p> :diffput<cr>
xnoremap <c-o> :diffget<cr>
]=]
