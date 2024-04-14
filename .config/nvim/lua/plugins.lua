-- TODO plugins:
--
-- lldb plugin
--    1. set breakpoints...
--        vim.diagnostic.set()({namespace}, {bufnr}, {diagnostics}, {opts})
--    
--    2. write breakpoints to a lldbinit file
--    3. start nvim server in new tmux window:
--       :Start! lldb --source lldbinit --one-line run -- ~/dev/neovim/build/bin/nvim --headless --listen ~/.cache/nvim/debug-server.pipe
--    4. start nvim client in new tmux window
--       :Start ~/dev/neovim/build/bin/nvim --remote-ui --server ~/.cache/nvim/debug-server.pipe


require 'paq' {
  'savq/paq-nvim', -- Let Paq manage itself

  'justinmk/vim-printf',

  -- Hint: to open files start with "+" or "-" from the terminal, prefix them with "./".
  --    nvim ./-foo
  --    nvim ./+foo
  'https://github.com/lewis6991/fileline.nvim',

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

  'tpope/vim-surround',

  'tpope/vim-dispatch',

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

  'https://github.com/echasnovski/mini.completion',
  'justinmk/nvim-repl',

  {'chrisbra/Colorizer', opt=true},

  'junegunn/fzf',
  'junegunn/fzf.vim',

  'tpope/vim-projectionist',

  -- use'mfussenegger/nvim-lsp-compl',
  'neovim/nvim-lspconfig',

  'nvim-lua/plenary.nvim',
  -- Waiting for BufNew fix: https://github.com/lewis6991/satellite.nvim/issues/33
  'https://github.com/lewis6991/satellite.nvim',
  'lewis6991/gitsigns.nvim',
}

vim.api.nvim_create_autocmd({'UIEnter'}, {
  callback = function()
    local client = vim.api.nvim_get_chan_info(vim.v.event.chan).client
    if client and client.name == "Firenvim" then
      vim.cmd[[
        nnoremap <expr> + '@_<cmd>set lines='..(v:count?v:count:'20')..'<cr>'
        nnoremap <D-v> "+p
        inoremap <D-v> <esc>"+pa
      ]]
      vim.o.laststatus = 0
      if vim.o.lines < 10 then
        vim.o.lines = 10
      end
      if vim.o.columns < 80 then
        vim.o.columns = 80
      end
    end
  end
})

-- Enable treesitter
vim.api.nvim_create_autocmd({'FileType'}, {
  callback = function(ev)
    if not ev.match or ev.match == '' or ev.match == 'text' then
      vim.treesitter.stop()
    end
    -- local lang = ev.match
    -- if vim.treesitter.get_lang(lang) ~= nil then
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
-- xnoremap <silent> R   :<c-u>call <sid>tmux_run_operator(visualmode(), 1)<CR>

vim.cmd([[
  nmap <C-j> m'<Plug>(edgemotion-j)
  nmap <C-k> m'<Plug>(edgemotion-k)
  xmap <C-j> m'<Plug>(edgemotion-j)
  xmap <C-k> m'<Plug>(edgemotion-k)
  omap <C-j> <Plug>(edgemotion-j)
  omap <C-k> <Plug>(edgemotion-k)
]])

vim.cmd([====[
  inoremap {<CR> {<CR>}<Esc>O
  inoremap {; {<CR>};<Esc>O
  inoremap {, {<CR>},<Esc>O
  inoremap [<CR> [<CR>]<Esc>O
  inoremap [[ [[<CR>]]<Esc>O
  inoremap [= [=[<CR>]=]<Esc>O
  inoremap [; [<CR>];<Esc>O
  inoremap [, [<CR>],<Esc>O
]====])

vim.g.obsession_no_bufenter = 1  -- https://github.com/tpope/vim-obsession/issues/40

vim.g.linediff_buffer_type = 'scratch'

vim.g.clojure_fold = 1
vim.g.sexp_filetypes = ''

vim.g.salve_auto_start_repl = 1

vim.cmd([[
  nmap yx       <Plug>(ReplSend)
  nmap yxx      <Plug>(ReplSendLine)
  xmap <Enter>  <Plug>(ReplSend)
  nnoremap <c-q> :Repl<CR>
]])

vim.g.fzf_command_prefix = 'Fz'

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
  -- require'lsp_compl'.attach(client, bufnr, { server_side_fuzzy_completion = true })
  vim.cmd([[
  nnoremap <buffer> K <cmd>lua vim.lsp.buf.hover()<cr>
  nnoremap <buffer> gK <cmd>lua vim.lsp.inlay_hint.enable(0, not vim.lsp.inlay_hint.is_enabled())<cr>
  " Get diagnostics only for current buffer (one client):
  " d = vim.diagnostic.get(0, {namespace=vim.lsp.diagnostic.get_namespace(vim.lsp.get_clients({buf=0})[1].id)})
  " vim.fn.setqflist(vim.diagnostic.toqflist(d))
  nnoremap <buffer> crq <cmd>lua vim.diagnostic.setqflist()<cr>
  nnoremap <buffer> crr <cmd>lua vim.lsp.buf.code_action()<cr>
  nnoremap <buffer> crn <cmd>lua vim.lsp.buf.rename()<cr>
  nnoremap <buffer> gO <cmd>lua vim.lsp.buf.document_symbol()<cr>
  nnoremap <buffer> gd <cmd>lua vim.lsp.buf.definition()<cr>
  nnoremap <buffer> gr <cmd>lua vim.lsp.buf.references()<cr>
  nnoremap <buffer> gi <cmd>lua vim.lsp.buf.implementation()<cr>
  ]])
end

-- xxx
local function idk()
  require('satellite').setup()
  require('mini.completion').setup({})

  require'lspconfig'.clangd.setup{
    cmd = { [[/usr/local/opt/llvm/bin/clangd]] },
    on_attach = on_attach,
    on_exit = function(...)
      require'vim.lsp.log'.error('xxx on_exit: '..vim.inspect((...)))
    end,
  }

  local function organize_imports()
    local params = {
      command = '_typescript.organizeImports',
      arguments = { vim.api.nvim_buf_get_name(0) },
      title = ''
    }
    vim.lsp.buf.execute_command(params)
  end
  require'lspconfig'.tsserver.setup{
    on_attach = on_attach,
    -- capabilities = capabilities,
    commands = {
      OrganizeImports = {
        organize_imports,
        description = 'Organize Imports'
      }
    }
  }

  require('gitsigns').setup()
  vim.cmd([[
    hi! link GitSignsChange Normal
  ]])

end

local function setup_lua_lsp()
  local runtime_path = vim.split(package.path, ';')
  table.insert(runtime_path, 'lua/?.lua')
  table.insert(runtime_path, 'lua/?/init.lua')

  require'lspconfig'.lua_ls.setup {
    cmd = {'lua-language-server'};
    on_attach = on_attach,
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
  }
end

local function set_esc_keymap()
  vim.cmd([[autocmd TermOpen * tnoremap <buffer> <Esc> <C-\><C-N>]])
  if 1 == vim.fn.exists('$NVIM')  then
    local function parent_chan()
      local ok, chan = pcall(vim.fn.sockconnect, 'pipe', vim.env.NVIM, {rpc=true})
      return ok and chan or nil
    end
    local chan = parent_chan()
    if not chan then return end
    -- Unmap <Esc> in the parent so it gets sent to the child (this) Nvim.
    local esc_mapdict = vim.fn.rpcrequest(chan, 'nvim_exec_lua', [[return vim.fn.maparg('<Esc>', 't', false, true)]], {})
    if esc_mapdict.rhs == [[<C-\><C-N>]] then
      vim.fn.rpcrequest(chan, 'nvim_exec_lua', [=[vim.cmd('tunmap <buffer> <Esc>')]=], {})
      vim.fn.chanclose(chan)
      vim.api.nvim_create_autocmd({'VimLeave'}, { callback = function()
        chan = parent_chan()
        if not chan then return end
        -- Restore the <Esc> mapping on VimLeave.
        vim.fn.rpcrequest(chan, 'nvim_exec_lua', [=[
          local esc_mapdict = ...
          vim.fn.mapset('t', false, esc_mapdict)
        ]=], {esc_mapdict})
        vim.fn.chanclose(chan)
      end, })
    end
  end
end

set_esc_keymap()
idk()
setup_lua_lsp()


-- Remap ":'<,'>s/" to ":'<,'>s/\%V".
local function map_cmdline_sub()
  local cmd = vim.fn.getcmdline()
  if not cmd:match("^'") then
    return
  end
  local ok, rv = pcall(vim.api.nvim_parse_cmd, cmd, {})
  if not ok or not rv.cmd == 'substitute' then
    return
  end
  if cmd:match("'<,'>s[^u ]") then
    vim.fn.setcmdline(cmd..[[\%V]])
    return true
  end
end
do
  local skip = false
  vim.api.nvim_create_autocmd('CmdlineEnter', {
    callback = function()
      skip = false
    end
  })
  vim.api.nvim_create_autocmd('CmdlineChanged', {
    callback = function()
      if not skip and map_cmdline_sub() then
        skip = true
      end
    end
  })
end

-- Detach the UI by closing the highest channel number.
-- https://github.com/neovim/neovim/issues/23093
vim.api.nvim_create_user_command('UIDetach', function()
  local maxchan = 0
  vim.iter(vim.api.nvim_list_uis()):map(function(o)
    if o.chan and not o.stdout_tty and o.chan > maxchan then
      maxchan = o.chan
    end
  end)
  if maxchan > 0 then
    vim.fn.chanclose(maxchan)
  end
end, {})

