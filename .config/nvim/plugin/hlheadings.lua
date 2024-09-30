---@return integer,integer,string
local function getbufline(node, bufnr, offset)
  offset = offset or 0
  local line1, _, line2, _ = node:range()
  line1 = line1 - offset
  line2 = line2 + offset
  local lines = vim.fn.getbufline(bufnr, line1 + 1, line2 + 1)
  return line1, line2, table.concat(lines, '\n')
end

--- Recursively applies `fn` to all nodes in tree starting at `root`.
local function ts_traverse_descendants(root, level, lang_tree, bufnr, fn)
  assert(root and level and lang_tree and bufnr and fn)
  local node_name = (root.named and root:named()) and root:type() or nil

  if root:child_count() > 0 then
    for node, _ in root:iter_children() do
      if node:named() then
        ts_traverse_descendants(node, level + 1, lang_tree, bufnr, fn)
      end
    end
  end

  local startline, endline, text = getbufline(root, bufnr)
  fn(root, node_name, startline, endline, text)
end

--- Traverses all treesitter trees in the given buffer.
---
---@param fn fun(node, name, startline, endline, text)
local function ts_traverse(bufnr, fn)
  local lang_tree = assert(vim.treesitter.get_parser(bufnr, nil, { error = false }))
  for _, tree in ipairs(lang_tree:trees()) do
    ts_traverse_descendants(tree:root(), 0, tree, bufnr, fn)
  end
  lang_tree:destroy()
end

--- Overlays a highlight on a line.
--- HACK: copies the line text into a virt_text overlay.
local function hl_line(bufnr, linenr, text, ns, hlgroup, width)
  local line = text and text or vim.api.nvim_buf_get_lines(bufnr, linenr, linenr + 1, false)[1]
  local linelen = vim.fn.strdisplaywidth(line)
  vim.api.nvim_buf_set_extmark(bufnr, ns, linenr, 0, {
      virt_text = {{ line .. (' '):rep(math.max(0, width - linelen)), hlgroup }},
      virt_text_pos = 'overlay',
      hl_mode = 'combine',
  })
end

local ns = vim.api.nvim_create_namespace('my_heading_highlight')
vim.cmd[[hi MyH1 ctermbg=DarkGrey guisp=fg guibg=#3b3b3b]] -- cterm=underdouble gui=underdouble guisp=fg
vim.cmd[[hi MyH2 ctermbg=DarkGrey guisp=fg guibg=#3a3a3a]] -- cterm=underdouble gui=underdouble guisp=fg

vim.api.nvim_create_autocmd({'FileType'}, {
  pattern = 'markdown',
  group = vim.api.nvim_create_augroup('config_helpheadings', { clear=true }),
  callback = function(ev)
    -- vim.cmd[[hi clear @markup.heading.1.delimiter.vimdoc]]
    -- vim.cmd[[hi clear @markup.heading.2.delimiter.vimdoc]]
    -- local curmarks = vim.api.nvim_buf_get_extmarks(0, vim.api.nvim_create_namespace('my_heading_highlight'), 0, -1, {})
    vim.api.nvim_buf_clear_namespace(0, ns, 0, -1)
    local width = 78

    -- Highlight headings in markdown docs.
    ts_traverse(0, function(node, name, startline, endline, text)
      local hlgroup = 'MyH2' -- name == 'atx_heading' and 'MyH1' or 'MyH2'
      if name == 'setext_heading' or name == 'atx_heading' then
        hl_line(0, startline, nil, ns, hlgroup, width)
      elseif name == 'setext_h1_underline' or name == 'setext_h2_underline' then
        hl_line(0, startline, '', ns, 'Normal', width)
      end
    end)
  end
})
