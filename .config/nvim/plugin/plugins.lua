-- TODO plugins:
--
-- lldb plugin
--    1. set breakpoints...
--        vim.diagnostic.set()({namespace}, {bufnr}, {diagnostics}, {opts})
--    2. write breakpoints to a lldbinit file
--    3. start nvim server in new tmux window:
--       :Start! lldb --source lldbinit --one-line run -- ~/dev/neovim/build/bin/nvim --headless --listen ~/.cache/nvim/debug-server.pipe
--    4. start nvim client in new tmux window
--       :Start ~/dev/neovim/build/bin/nvim --remote-ui --server ~/.cache/nvim/debug-server.pipe


require 'paq' {
  'savq/paq-nvim', -- Let Paq manage itself

  -- Minimal, yet aesthetic, "screencast" tool.
  {'https://github.com/NvChad/showkeys', opt=true},

  'https://github.com/Goose97/timber.nvim',

  -- Hint: to open files start with "+" or "-" from the terminal, prefix them with "./".
  --    nvim ./-foo
  --    nvim ./+foo
  'https://github.com/lewis6991/fileline.nvim',
  -- Open files from :term in the parent Nvim.
  -- 'https://github.com/willothy/flatten.nvim',

  'https://github.com/justinmk/vim-ipmotion.git',
  'https://github.com/justinmk/vim-gtfo.git',
  'https://github.com/justinmk/vim-dirvish.git',

  {
    'glacambre/firenvim',
    build = function() vim.fn['firenvim#install'](0) end,
  },

  'https://github.com/justinmk/vim-sneak.git',

  'tpope/vim-characterize',
  'tpope/vim-apathy',
  'tpope/vim-dadbod',

  {'will133/vim-dirdiff', opt=true},
  -- gh wrapper: https://github.com/pwntester/octo.nvim
  'tpope/vim-fugitive',
  'tpope/vim-rhubarb',
  -- 'https://github.com/shumphrey/fugitive-gitlab.vim',
  'https://github.com/daliusd/ghlite.nvim',
  'https://github.com/lewis6991/gitsigns.nvim',

  'tpope/vim-surround',

  'tpope/vim-repeat',
  'tpope/vim-eunuch',
  'tpope/vim-rsi',

  'tpope/vim-unimpaired',
  'tpope/vim-endwise',
  'tommcdo/vim-lion',
  'tommcdo/vim-exchange',

  'haya14busa/vim-edgemotion',

  'tpope/vim-obsession',

  'AndrewRadev/linediff.vim',
  {'mbbill/undotree', opt=true},

  {'guns/vim-sexp', opt=true},

  {'tpope/vim-salve', opt=true},
  {'tpope/vim-fireplace', opt=true},
  'tpope/vim-dispatch',
  -- nmap yx       <Plug>(ReplSend)
  -- nmap yxx      <Plug>(ReplSendLine)
  -- xmap <Enter>  <Plug>(ReplSend)
  'justinmk/nvim-repl',

  'https://github.com/echasnovski/mini.completion',

  'https://github.com/mfussenegger/nvim-qwahl',
  -- Provides `require('tsht').nodes()`.
  'https://github.com/mfussenegger/nvim-treehopper',
  -- TODO:
  -- 'https://github.com/mfussenegger/nluarepl',
  -- 'https://github.com/mfussenegger/nvim-overfly',

  {'chrisbra/Colorizer', opt=true},

  'https://github.com/inkarkat/vim-ingo-library',
  'https://github.com/inkarkat/vim-mark',

  'junegunn/fzf',
  'https://github.com/ibhagwan/fzf-lua',

  'tpope/vim-projectionist',

  'neovim/nvim-lspconfig',

  -- requires nvim-treesitter? :(
  -- 'https://github.com/yorickpeterse/nvim-tree-pairs',

  'https://github.com/lewis6991/satellite.nvim',

  {'https://github.com/MeanderingProgrammer/render-markdown.nvim', opt=true},
}

_G._myconfig = _G._myconfig or {}
local augroup = vim.api.nvim_create_augroup('my.config', {})

vim.api.nvim_create_autocmd({'UIEnter'}, {
  group = augroup,
  callback = function()
    local client = vim.api.nvim_get_chan_info(vim.v.event.chan).client
    if client and client.name == "Firenvim" then
      vim.cmd[[
        set cmdheight=0 shortmess+=W scrolloff=0 laststatus=0 textwidth=9999
        nnoremap <expr> + '@_<cmd>set columns='..(v:count?v:count:'150')..' lines='..(v:count?v:count:'40')..'<cr>'
        nnoremap <D-v> "+p
        inoremap <D-v> <c-o>"+p
      ]]
    end
  end
})

-- Enable treesitter. For c/vimscript/markdown: https://github.com/neovim/neovim/pull/32965
vim.api.nvim_create_autocmd({'FileType'}, {
  group = augroup,
  callback = function(ev)
    if not ev.match or ev.match == '' or ev.match == 'text' then
      vim.treesitter.stop()
    end
    pcall(function() vim.treesitter.start() end)
  end
})

vim.cmd([[
  let g:sneak#label = 1
  let g:sneak#use_ic_scs = 1
  let g:sneak#absolute_dir = 1
  map <M-;> <Plug>Sneak_,
]])

vim.g.surround_indent = 0
vim.g.surround_no_insert_mappings = 1

vim.g.dispatch_no_tmux_make = 1  -- Prefer job strategy even in tmux.
-- TODO:
-- run closest zig test case: https://github.com/mfussenegger/dotfiles/commit/8e827b72e2b72e7fb240e8a270d786cffc38a2a5#diff-7d18f76b784e0cb761b7fc0a995680cf2a27b6f77031b60b854248478aed8b6fR5
-- run closest neovim lua test case via make: https://github.com/mfussenegger/dotfiles/commit/a32190b76b678517849d6da84d56836d44a22f2d#diff-f81a3d06561894224d8353f9babc6a7fa9b4962a40c191eb5c23c9cdcc6004c0R158
vim.cmd([[nnoremap mT mT:FocusDispatch VIMRUNTIME= TEST_COLORS=0 TEST_FILE=<c-r>% TEST_FILTER= TEST_TAG= make functionaltest<S-Left><S-Left><S-Left><Left>]])
-- nnoremap <silent> yr  :<c-u>set opfunc=<sid>tmux_run_operator<cr>g@

vim.cmd([[
  nnoremap <silent> *  ms:<c-u>let @/='\V\<'.escape(expand('<cword>'), '/\').'\>'<bar>call histadd('/',@/)<bar>set hlsearch<cr>
  nnoremap <silent> g* ms:<c-u>let @/='\V' . escape(expand('<cword>'), '/\')     <bar>call histadd('/',@/)<bar>set hlsearch<cr>

  " Configure https://github.com/inkarkat/vim-mark until it enrages me enough to rewrite it.
  let g:mw_no_mappings = 1
  "nnoremap <silent> m.. :exe 'Mark /\%'..line('.')..'l./'<cr>
  nnoremap <silent> m.. :exe 'Mark /\V'..escape(getline('.'), '/\')..'/'<cr>
  nnoremap <silent> m*  :exe 'Mark /\V'..escape(substitute('<c-r><c-w>', "'", "''", 'g'), '/\')..'/'<cr>
  nmap     <silent> m?  <Plug>MarkToggle
  nnoremap <silent> m<bs> :MarkClear<cr>
  nmap     <silent> ]m <Plug>MarkSearchAnyOrDefaultNext
  nmap     <silent> [m <Plug>MarkSearchAnyOrDefaultPrev
  xmap              m*  <Plug>MarkIWhiteSet
]])

vim.cmd([[
  nmap <C-j> m'<Plug>(edgemotion-j)
  nmap <C-k> m'<Plug>(edgemotion-k)
  xmap <C-j> m'<Plug>(edgemotion-j)
  xmap <C-k> m'<Plug>(edgemotion-k)
  omap <C-j> <Plug>(edgemotion-j)
  omap <C-k> <Plug>(edgemotion-k)
]])

vim.cmd([====[
  inoremap [[ [[ ]]<Left><Left><Left>
  inoremap [= [=[ ]=]<Left><Left><Left><Left>
  inoremap {<CR> {<CR>}<Esc>O
  inoremap {; {<CR>};<Esc>O
  inoremap {, {<CR>},<Esc>O
  inoremap [<CR> [<CR>]<Esc>O
  inoremap [; [<CR>];<Esc>O
  inoremap [, [<CR>],<Esc>O
]====])

-- g?: Web search
vim.keymap.set('n', 'g?', function()
  vim.ui.open(('https://google.com/search?q=%s'):format(vim.fn.expand('<cword>')))
end)
vim.keymap.set('x', 'g?', function()
  vim.ui.open(('https://google.com/search?q=%s'):format(vim.trim(table.concat(
    vim.fn.getregion(vim.fn.getpos('.'), vim.fn.getpos('v'), { type=vim.fn.mode() }), ' '))))
  vim.api.nvim_input('<esc>')
end)

vim.g.obsession_no_bufenter = 1  -- https://github.com/tpope/vim-obsession/issues/40

vim.g.linediff_buffer_type = 'scratch'

vim.g.clojure_fold = 1
vim.g.sexp_filetypes = ''
vim.g.salve_auto_start_repl = 1

vim.api.nvim_set_var('projectionist_heuristics', {
    ['package.json'] = {
      ['package.json'] = {['alternate'] = {'package-lock.json'}},
      ['package-lock.json'] = {['alternate'] = {'package.json'}},
    },
    ['*.sln'] = {
      ['*.cs'] = {['alternate'] = {'{}.designer.cs'}},
      ['*.designer.cs'] = {['alternate'] = {'{}.cs'}},
    },
    ['/*.c|src/*.c'] = {
      ['*.c'] = {['alternate'] = {'../include/{}.h', '{}.h'}},
      ['*.h'] = {['alternate'] = '{}.c'},
    },
    ['Makefile'] = {
      ['Makefile'] = {['alternate'] = 'CMakeLists.txt'},
      ['CMakeLists.txt'] = {['alternate'] = 'Makefile'},
    },
  })

-- Eager-load these plugins so we can override their settings. {{{
vim.cmd([[
  runtime! plugin/rsi.vim
]])

local function on_attach(client, bufnr)
  vim.cmd([[
  nnoremap <buffer> K   <cmd>lua vim.lsp.buf.hover()<cr>
  " Get diagnostics only for current buffer (one client):
  " d = vim.diagnostic.get(0, {namespace=vim.lsp.diagnostic.get_namespace(vim.lsp.get_clients({buf=0})[1].id)})
  " vim.fn.setqflist(vim.diagnostic.toqflist(d))
  nnoremap <buffer> grq <cmd>lua vim.diagnostic.setqflist()<cr>
  nnoremap <buffer> gO  <cmd>FzfLua lsp_document_symbols<cr>
  nnoremap <buffer> gr/ <cmd>FzfLua lsp_workspace_symbols<cr>
  nnoremap <buffer> gd  <cmd>lua vim.lsp.buf.definition()<cr>
  nnoremap <buffer> gi  <cmd>lua vim.lsp.buf.implementation()<cr>
  ]])

  vim.keymap.set('n', '<bs>', function()
    vim.diagnostic.config({ virtual_lines = not vim.diagnostic.config().virtual_lines })
    vim.lsp.inlay_hint.enable(not vim.lsp.inlay_hint.is_enabled())
  end, { buffer = 0, desc = 'Toggle verbose diagnostics and inlay_hints.' })
  vim.keymap.set('n', 'gK', function() vim.diagnostic.open_float() end, { buffer = 0, desc = 'Toggle diagnostics window.' })
end

vim.api.nvim_create_autocmd('LspAttach', {
  group = augroup,
  callback = on_attach,
})

local function config_lsp()
  vim.lsp.enable('ts_ls')
  vim.lsp.enable('bashls')

  local runtime_path = vim.split(package.path, ';')
  table.insert(runtime_path, 'lua/?.lua')
  table.insert(runtime_path, 'lua/?/init.lua')
  vim.lsp.config('lua_ls', {
    settings = {
      Lua = {
        runtime = {
          -- Tell the language server which version of Lua you're using (most likely LuaJIT in the case of Neovim)
          version = 'LuaJIT',
          -- Setup your lua path
          path = runtime_path,
        },
        diagnostics = {
          -- Get the language server to recognize the `vim` global
          globals = {'vim'},
        },
        workspace = {
          -- Make the server aware of Neovim runtime files
          library = vim.api.nvim_get_runtime_file('', true),
        },
        -- Do not send telemetry data containing a randomized but unique identifier
        telemetry = {
          enable = false,
        },
      },
    },
  })
  vim.lsp.enable('lua_ls')

  vim.lsp.config('clangd', {
    cmd = { [[/usr/local/opt/llvm/bin/clangd]] },
    -- on_exit = function(...)
    --   require'vim.lsp.log'.error('xxx on_exit: '..vim.inspect((...)))
    -- end,
  })
  vim.lsp.enable('clangd')
end

local function config_term_esc()
  vim.keymap.set('t', '<C-[>', [[<C-\><C-N>]])
  vim.keymap.set('t', '<Esc>', [[<C-\><C-N>]])

  -- :terminal-nested Nvim:
  if vim.env.NVIM then
    local function parent_chan()
      local ok, chan = pcall(vim.fn.sockconnect, 'pipe', vim.env.NVIM, {rpc=true})
      if not ok then
        vim.notify(('failed to create channel to $NVIM: %s'):format(chan))
      end
      return ok and chan or nil
    end

    local didset = false
    local chan = assert(parent_chan())
    local function map_parent(lhs)
      -- Map <Esc> and <C-[> in the parent so it gets sent to the child (this) Nvim.
      local map = vim.rpcrequest(chan, 'nvim_exec_lua', [[return vim.fn.maparg(..., 't', false, true)]], { lhs }) --[[@as table<string,any>]]
      if map.rhs == [[<C-\><C-N>]] then
        vim.rpcrequest(chan, 'nvim_exec_lua', [[vim.keymap.set('t', ..., '<Esc>', {buffer=0})]], { lhs })
        didset = true
      end
    end
    map_parent('<Esc>')
    map_parent('<C-[>')
    vim.fn.chanclose(chan)

    -- Restore the mapping(s) on VimLeave.
    if didset then
      vim.api.nvim_create_autocmd({'VimLeave'}, {
        group = augroup,
        callback = function()
          local chan2 = assert(parent_chan())
          vim.rpcrequest(chan2, 'nvim_exec2', [=[
            tunmap <buffer> <Esc>
            tunmap <buffer> <C-[>
          ]=], {})
        end,
      })
    end
  end
end

local function config_fzf()
  require('fzf-lua').setup({ fzf_colors = true })
  -- Search full-text or filenames.
  -- TODO: ignore dirs: build .git venv .vim-src .deps .vscode-test node_modules .coverage
  vim.keymap.set('n', '<M-/>', function()
    if vim.v.count == 0 then
      vim.cmd 'FzfLua files'
      return
    end
    require'fzf-lua'.live_grep({
      cmd = 'git grep --line-number --column --color=always -v "^[[:space:]]*$"'
    })
  end)

  vim.cmd[=[
    " Search MRU files
    nnoremap <silent>       <M-\> :FzfLua oldfiles<cr>
    nnoremap <silent><expr> <C-\> v:count ? 'mS:<C-U>FzfLua lines<CR>' : ':<C-U>FzfLua buffers<CR>'
    nnoremap <silent> gO    :FzfLua btags<cr>
    nnoremap <silent> gr/   :FzfLua tags<cr>
  ]=]
end

--- Map ":'<,'>s/" to ":'<,'>s/\%V".
local function cmdline_sub()
  local skip = false

  local function map_cmdline_sub()
    local cmd = vim.fn.getcmdline()
    local ok, rv = pcall(vim.api.nvim_parse_cmd, cmd, {})
    -- vim.notify(vim.inspect(rv))

    if ok and rv.cmd == 'substitute' and cmd:match("'<,'>s[^u ]") then
      skip = true
      vim.fn.setcmdline(cmd..[[\%V]])
    end
  end
  vim.api.nvim_create_autocmd('CmdlineEnter', {
    group = augroup,
    callback = function()
      skip = false
    end
  })
  vim.api.nvim_create_autocmd('CmdlineChanged', {
    group = augroup,
    callback = function()
      if not skip then
        map_cmdline_sub()
      end
    end
  })
end

local function config_theme()
  -- theme/colorscheme
  local function fix_colorscheme()
    vim.cmd[[
      if !&termguicolors
        highlight SpellBad guibg=Red guifg=White
        highlight CursorLine ctermbg=235
        highlight Comment ctermfg=gray
      endif
      highlight Visual guibg=fg guifg=bg
    ]]
  end
  vim.api.nvim_create_autocmd({'ColorScheme'}, {
    group = augroup,
    callback = fix_colorscheme
  })
  fix_colorscheme()
end

local function config_printf_mappings()
  -- printf/log util
  require('timber').setup({
    default_keymaps_enabled = false,
    log_templates = {
      default = {
        c = [[ILOG("%watcher_marker_start %s %watcher_marker_end", %log_target);]],
        lua = [[print(string.format('%watcher_marker_start %%s %watcher_marker_end', %log_target))]],
     },
     plain = {
       c = [[LOG("%insert_cursor");]],
     },
    },
    -- batch_log_templates = {},
  })
  vim.keymap.set('n', 'dP', function()
    require('timber.actions').insert_log({
      position = 'below',
      template = 'plain',
    })
  end)
  vim.keymap.set('n', 'dp', function()
    if vim.wo.diff then
      return 'dp'
    end
    return require('timber.actions').insert_log({ operator=true, position = 'below' })
  end, { expr = true })
end

local function config_tabline()
  -- 'tabline'
  vim.cmd[[highlight TabLineSel guibg=bg guifg=fg]]
  _G._myconfig.tablabel = function(n)
    local buflist = vim.fn.tabpagebuflist(n)
    local winnr = vim.fn.tabpagewinnr(n)
    local tabdir = vim.fn.getcwd(-1, n)
    local has_tabdir = vim.fn.getcwd(-1, -1) ~= tabdir
    if has_tabdir then
      return ('CWD: %s/'):format(vim.fn.fnamemodify(tabdir, ':t'))
    end
    local bufname = vim.fn.bufname(buflist[winnr])
    local isdir = bufname:sub(#bufname) == '/'
    local name = vim.fn.fnamemodify(bufname, isdir and ':h:t' or ':t') .. (isdir and '/' or '')
    name = name:len() > 20 and name:sub(1, 20) .. '…' or name
    return name == '' and 'No Name' or ' ' .. name
  end
  _G._myconfig.tabline = function()
    local s = ''
    for i = 1, vim.fn.tabpagenr('$') do
      -- Highlight group.
      local hlgroup = (i == vim.fn.tabpagenr() and '%#TabLineSel#' or '%#TabLine#')
      -- %T: set the tabpage number (for mouse clicks).
      s = s .. ('%s%%%dT %%{v:lua._myconfig.tablabel(%d)} '):format(hlgroup, i, i)
    end
    -- After last tab: Fill with TabLineFill. Reset tabpage nr. Right-align the "close" (X) button.
    return s .. '%#TabLineFill#%T%=%#TabLine#%999XX'
  end

  vim.go.tabline='%!v:lua._myconfig.tabline()'
end

local function config_term()
  -- credit: gpanders, #30415
  vim.api.nvim_create_user_command('TermHl', function()
    local b = vim.api.nvim_create_buf(false, true)
    local chan = vim.api.nvim_open_term(b, {})
    vim.api.nvim_chan_send(chan, table.concat(vim.api.nvim_buf_get_lines(0, 0, -1, false), '\n'))
    vim.api.nvim_win_set_buf(0, b)
  end, { desc = 'Highlights ANSI termcodes in curbuf' })

  -- Enable prompt sign in :terminal buffers.
  vim.api.nvim_create_autocmd('TermOpen', {
    command = 'setlocal signcolumn=auto',
  })
  vim.api.nvim_create_autocmd('TermRequest', {
    group = augroup,
    callback = function(args)
      if string.match(args.data.sequence, '^\027]133;A') then
        local lnum = args.data.cursor[1]
        vim.api.nvim_buf_set_extmark(args.buf, vim.api.nvim_create_namespace('my.terminal.prompt'), lnum - 1, 0, {
          -- Replace with sign text and highlight group of choice
          sign_text = '∙',
          sign_hl_group = 'SpecialChar',
        })
      end
    end,
  })
end

-- yankring
vim.api.nvim_create_autocmd('TextYankPost', {
  group = augroup,
  callback = function()
    if vim.v.event.operator == 'y' then
      for i = 9, 1, -1 do -- Shift all numbered registers.
        vim.fn.setreg(tostring(i), vim.fn.getreg(tostring(i - 1)))
      end
    end
  end,
})

if not vim.g.vscode then
  -- require("flatten").setup{
  --   hooks = {},
  --   nest_if_no_args = true,
  -- }
  require('satellite').setup()
  require('mini.completion').setup({})

  -- require('ghlite').setup{}
  require('gitsigns').setup{
    signs_staged_enable = false,
    current_line_blame = true,
    current_line_blame_opts = {
      virt_text_pos = 'eol_right_align', -- 'eol' | 'overlay' | 'right_align'
    },
  }
  vim.cmd([[
    hi! link GitSignsChange Normal
  ]])

  config_term_esc()
  config_term()
  config_tabline()
  config_lsp()
  config_fzf()
  config_printf_mappings()
  cmdline_sub()
  config_theme()
end

require('vim._extui').enable({})
