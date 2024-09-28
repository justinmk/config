---@return integer,integer,string
local function getbufline(node, bufnr, offset)
  offset = offset or 0
  local line1, _, line2, _ = node:range()
  line1 = line1 - offset
  line2 = line2 + offset
  local lines = vim.fn.getbufline(bufnr, line1 + 1, line2 + 1)
  return line1, line2, table.concat(lines, '\n')
end

local function visit(root, level, lang_tree, bufnr, fn)
  assert(root and level and lang_tree and bufnr and fn)
  local node_name = (root.named and root:named()) and root:type() or nil

  if root:child_count() > 0 then
    for node, _ in root:iter_children() do
      if node:named() then
        visit(node, level + 1, lang_tree, bufnr, fn)
      end
    end
  end

  local startline, endline, text = getbufline(root, bufnr)
  fn(root, node_name, startline, endline, text)
end

---@param fn fun(node, name, startline, endline, text)
local function ts_traverse(bufnr, fn)
  local lang_tree = assert(vim.treesitter.get_parser(bufnr, nil, { error = false }))
  for _, tree in ipairs(lang_tree:trees()) do
    visit(tree:root(), 0, tree, bufnr, fn)
  end
  lang_tree:destroy()
end

--- Highlights a line across the entire viewport.
--- HACK: copies the line text into a virt_text overlay.
local function hl_line(bufnr, linenr, text, hlgroup)
  -- local line = vim.api.nvim_buf_get_lines(bufnr, linenr, linenr + 1, false)
  local ns = vim.api.nvim_create_namespace('my_heading_higlight')
  vim.api.nvim_buf_set_extmark(bufnr, ns, linenr, 0, {
      virt_text = {{ text .. (' '):rep(200), hlgroup }},
      virt_text_pos = 'overlay',
      -- hl_mode = 'combine',
  })
end

vim.api.nvim_create_autocmd({'FileType'}, {
  pattern = 'help',
  group = vim.api.nvim_create_augroup('help_heading', { clear=true }),
  callback = function(ev)
    -- Highlight h1,h2 in :help docs.
    ts_traverse(0, function(node, name, startline, endline, text)
      if name == 'h1' or name == 'h2' or name == 'h3' then
        local hlgroup = name == 'h1' and 'MyH1' or 'MyH2'
        text = text:find('[-=]') and '' or text
        hl_line(0, startline, text, hlgroup)
      end
    end)
  end
})

vim.cmd[[hi MyH1 cterm=underline gui=underdouble]]
vim.cmd[[hi MyH2 cterm=underdouble gui=underdotted]]
