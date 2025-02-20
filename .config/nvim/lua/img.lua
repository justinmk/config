-- credit: timg + https://github.com/neovim/neovim/issues/30889
--
-- TODO: look at https://github.com/folke/snacks.nvim/blob/main/docs/image.md
--
-- ref: https://sw.kovidgoyal.net/kitty/graphics-protocol/

local M = {}

--- Gets the current tty name. E.g. "/dev/ttys008"
---@return string|nil
local function get_tty_name()
  -- local handle = io.popen('tty 2>/dev/null')
  -- if not handle then return nil end
  -- local rv = handle:read('*a')
  -- handle:close()

  -- TODO: this is a hack, $GPG_TTY is set in my personal bashrc.
  local rv = vim.fn.trim(vim.env.GPG_TTY)
  return rv ~= '' and rv or nil
end

local TTY_NAME = assert(get_tty_name(), 'failed to read editor tty name')

local function base64_encode_file(fname)
  local f = assert(io.open(fname, 'rb'))
  local data = f:read('*a')
  f:close()
  return vim.base64.encode(data)
end

---Writes data to the editor tty.
---@param ... string|number
local function write(...)
  local handle = io.open(TTY_NAME, 'w')
  if not handle then
    error('failed to open ' .. TTY_NAME)
  end
  handle:write(...)
  handle:close()
end

local CODES = {
  BEL = '\x07', -- aka ^G
  ESC = '\x1B', -- aka ^[
}

local function move_cursor(x, y, save)
  -- if is_SSH and utils.tmux.is_tmux then
  --     -- When tmux is running over ssh, set-cursor sometimes doesn't actually get sent
  --     -- I don't know why this fixes the issue...
  --     utils.tmux.get_cursor_x()
  --     utils.tmux.get_cursor_y()
  -- end
  if save then write(CODES.ESC .. '[s') end
  write(CODES.ESC .. '[' .. y .. ';' .. x .. 'H')
  vim.uv.sleep(1)
end

local function restore_cursor()
  write(CODES.ESC .. '[u')
end

---@param opts? {data?:string, filename?:string, proto?:'iterm2'|'kitty'}
function M.show(opts)
  opts = opts or {}

  local data = opts.data
  if opts.filename then
    data = base64_encode_file(opts.filename)
  end

  -- Exit early if nothing to show
  if not data or string.len(data) == 0 then
    print('NO DATA')
    return
  end

  local proto = opts.proto or 'kitty'

  move_cursor(60, 4, true)

  if proto == 'iterm2' then
    -- NOTE: We MUST mark as inline otherwise not rendered and put in a
    --       downloads folder
    write(CODES.ESC .. ']1337') -- Begin sequence
    write(';File=inline=1')     -- Display image inline
    write(':' .. data)          -- Transmit base64 data
    write(CODES.BEL)            -- End sequence
  elseif proto == 'kitty' then
    local CHUNK_SIZE = 4096
    local pos = 1
    local DATA_LEN = string.len(data)

    -- For kitty, we need to write an image in chunks
    --
    --     Graphics codes are in this form:
    --
    --         <ESC>_G<control data>;<payload><ESC>\
    --
    --     To simultaneously transmit and display an image, we use `a=T`.
    --
    --     Chunking data (such as from over a network) requires the
    --     specification of `m=0|1`, where all chunks must have a
    --     value of `1` except the very last chunk.
    while pos <= DATA_LEN do
      write(CODES.ESC .. '_G') -- Begin sequence

      -- If at the beginning of our image, mark as a PNG to be
      -- transmitted and displayed immediately.
      -- a=T : "transmit and display"
      -- f=100 : PNG
      -- w=width
      -- h=height
      -- c=columns
      -- H=horizontal offset
      -- V=vertical offset
      -- C=1 : "don't move the cursor"
      if pos == 1 then
        write('a=T,f=100,C=1,c=44,w=1000,h=1000,')
        -- write('a=T,f=100,C=1,c=50,X=1000,Y=200,H=100,V=100,')
      end

      -- Get our specific chunk of data and increment position
      local chunk = data:sub(pos, pos + CHUNK_SIZE)
      pos = pos + CHUNK_SIZE

      -- If we are still sending chunks and not at the end
      if pos <= DATA_LEN then
        write('m=1')
      end

      -- If we have a chunk available, write it
      if string.len(chunk) > 0 then
        write(';')
        write(chunk)
      end

      write(CODES.ESC .. '\\') -- End sequence
    end
  end

  restore_cursor()
end

function M.clear_all(proto)
  proto = proto and proto or 'kitty'

  if proto == 'iterm2' then
    error()
  elseif proto == 'kitty' then
    -- Graphics codes are in this form:
    --
    --    <ESC>_G<control data>;<payload><ESC>\
    --
    -- a=d without other params means 'delete all'.
    write(CODES.ESC .. '_Ga=d;' .. CODES.ESC .. '\\')
  end

  restore_cursor()
end

M.clear_all()
-- M.show({ filename = vim.fn.expand('~/notes/talks/img/mtg-homunculus.png') })
-- write(CODES.ESC .. '[2J')

return M
