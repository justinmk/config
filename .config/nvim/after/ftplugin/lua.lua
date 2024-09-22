vim.cmd[[
" From https://github.com/tpope/tpope/blob/master/.vimrc
setlocal includeexpr=substitute(v:fname,'\\.','/','g').'.lua'

setlocal comments-=:-- comments+=:---,:--
let b:printf_pattern = 'print(string.format("%s", %s))'

nnoremap <buffer> yxal :luafile %<cr>

inoreabbrev <buffer> lo local
inoreabbrev <buffer> lf local function()<left><left>
inoreabbrev <buffer> fu function() end<left><left><left><left>
inoreabbrev <buffer> fo (''):format()<left><left><s-left><left><left><left>

]]

vim.keymap.set('n', '<leader>log', function()
  vim.api.nvim_paste([[
local function flog(o, s)
  local ok, _inspect = pcall(require, 'inspect')
  _inspect = ok and _inspect or tostring
  local f = assert(io.open('dbg.txt','wa+'))
  f:write(('xxx: %s\n'):format(_inspect(s)))
  f:flush()
  f:close()
end
  ]], false, -1)
end, { buffer = true })

vim.keymap.set('n', '<leader>dbg', function()
  vim.api.nvim_paste([[
local function dbg_this(o, s)
  if o and (
    (o.find and o:find(s))
    or (o.name and o.name:find(s))
    or (o.keyset_name and o.keyset_name:find(s)))
  then
    error(vim.inspect(o))
  end
end
  ]], false, -1)
end, { buffer = true })
