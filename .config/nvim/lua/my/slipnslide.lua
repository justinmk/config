-- TODO: https://sli.dev/
-- TODO: use this instead?: https://github.com/sotte/presenting.nvim

local saved = {}

local imgs = {}

-- Gets the dir of the current buffer.
local function buf_dir()
 return vim.fn.fnamemodify(vim.fn.expand('%'), ':p:h')
end

local function clear_imgs()
  for _, imgid in ipairs(imgs) do
    vim.ui.img.del(imgid)
  end
end

-- Searches between previous and next H1 heading ("^(#|=)").
function _G.search_cur_slide(pat)
  -- Temporarily move cursor 1 row down, in case it is on a heading already.
  vim.cmd[[norm! j]]

  local cur_row, _ = unpack(vim.api.nvim_win_get_cursor(0))
  cur_row = cur_row - 1
  -- Find previous H1 heading, or use topline.
  local start_row, _ = unpack(vim.fn.searchpos('^#[^#]', 'nb'))
  start_row = start_row > 0 and (start_row - 1) or vim.fn.line('w0')
  -- Find next H1 heading, or use botline.
  local end_row, _ = unpack(vim.fn.searchpos('^#[^#]', 'ne'))
  end_row = end_row > 0 and (end_row - 1) or vim.fn.line('w$')

  -- Now, search for the pattern within that section/range.
  local matches = {}
  local lines = vim.api.nvim_buf_get_lines(0, start_row, end_row + 1, false)
  for _, line in ipairs(lines) do
    local m = line:match(pat)
    if m then
      table.insert(matches, m)
    end
  end

  -- Restore cursor.
  vim.cmd[[norm! k]]

  return matches
end

--- Gets the image data for the current slide, if any.
local function get_slide_img()
  -- Markdown image: "![alt](/img/foo/bar.png)"
  local relpath = search_cur_slide('^!%[.-%]%((.-)%)')[1]
  if not relpath then
    -- "img: ./foo/bar.png"
    local el = search_cur_slide('^img: .*')[1]
    if not el then
      return nil
    end
    relpath = vim.trim(el:gsub('img: ', ''))
  end

  local fullpath
  if relpath:sub(1, 1) == '/' then
    -- Site-absolute path: resolve against Hugo project root's static/ dir.
    local root = vim.fs.root(0, { 'hugo.yaml', 'hugo.toml', 'config.yaml', 'config.toml' })
    fullpath = root and vim.fs.joinpath(root, 'static', relpath:sub(2)) or relpath
  else
    fullpath = vim.fs.joinpath(buf_dir(), relpath)
  end
  assert(vim.fn.filereadable(fullpath) ~= 0, ('img read failed: "%s"'):format(fullpath))
  return vim.fn.readblob(fullpath)
end

local function try_show_img()
  local img = get_slide_img()
  if not img then
    return
  end
  vim.cmd[[redraw]]
  -- vim.cmd[[sleep 100m]]
  local imgid = vim.ui.img.set(img, { row = 5, col = 60,  height = 20, zindex = 50 })
  table.insert(imgs, imgid)
end

function _G.slides_clear()
  -- vim.cmd[[mapclear <buffer>]]
  vim.cmd[[setlocal laststatus=2]]
  clear_imgs()
  vim.fn.clearmatches()
  vim.o.tabline = saved.tabline
  vim.o.ruler = saved.ruler
end

function _G.slides_setopt()
  vim.o.tabline = ' '
  vim.o.ruler = false
  vim.o.virtualedit = 'all'
  vim.cmd[[
    setlocal laststatus=0 cmdheight=1 scrolloff=0 signcolumn=no
    setlocal textwidth=53
  ]]
end

function _G.show_cur_slide()
  _G.slides_setopt()

  if vim.fn.hlID('SlidesHide') == 0 then
    -- use folds instead of highlight
  else
    vim.cmd[=[
      " Hide all lines after the current slide
      exe 'match SlidesHideAfterSlide /\%>'.(line('w0')-1).'l\n\(#[^#]\|==\)[^\n]*\n\_.\{-}\zs\n.*\n\(#[^#]\|==\)\+\_.*/'
      " Hide "img:…" lines.
      2match SlidesHideImg /^img: .*/
      " Hide "http://…" lines.
      3match SlidesHideUrl /^https:.*/
    ]=]
  end

  vim.cmd[[doautocmd FileType]] -- Trigger hlheadings.lua:hlheadings_do_hl().
  clear_imgs()
  try_show_img()
end

function _G.Slides()
  saved.tabline = vim.o.tabline
  saved.ruler = vim.o.ruler

  vim.cmd[[
    lua slides_clear()

    tab split
    lua slides_setopt()
    4vnew
    setlocal nocursorline winfixwidth
    wincmd p

    "let editing = !!a:0
    "if editing
    "  setlocal colorcolumn=54,67 textwidth=53
    "  exe printf('hi ColorColumn gui%sg=#555555 cterm%sg=235', 'bf'[&bg], 'bf'[&bg])
    "endif

    hi markdownError ctermbg=NONE ctermfg=NONE guifg=NONE guibg=NONE

    " For cterm.
    let bg = synIDattr(synIDtrans(hlID('Normal')), 'bg', 'cterm')
    let bg = !empty(bg) ? bg : (&bg==#'dark'?'black':'white')
    " Hide slides before/after the current one.
    exe 'hi SlidesHide ctermbg='..bg..' ctermfg='..bg..' cterm=nocombine guibg=bg guifg=bg gui=nocombine'
    hi link SlidesHideImg SlidesHide
    hi link SlidesHideUrl SlidesHide
    hi link SlidesHideAfterSlide SlidesHide

    hi! link EndOfBuffer SlidesHide
    hi! link WinSeparator SlidesHide
    hi Visual guifg=bg guibg=fg

    nnoremap <buffer><silent> <Right> :keeppatterns /\v^(#[^#]<bar>\=\=+)<CR>zt<C-Y>50<bar>:call v:lua.show_cur_slide()<CR>
    nnoremap <buffer><silent> <Left>   :keeppatterns ?\v^(#[^#]<bar>\=\=+)<CR>zt<C-Y>50<bar>:call v:lua.show_cur_slide()<CR>
    let old_ctrl_l = substitute(maparg('<c-l>', 'n', 0), '|', '<bar>', 'g')
    exe "nnoremap <buffer><silent><expr> <C-L> ':call v:lua.slides_clear()<cr>'.."..old_ctrl_l
  ]]
end

vim.cmd[[
  command! -nargs=? Slides lua Slides()
]]
