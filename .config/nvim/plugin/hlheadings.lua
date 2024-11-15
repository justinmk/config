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
local function hl_line(bufnr, linenr, hide, ns, hlgroup, minlen)
  local line = vim.api.nvim_buf_get_lines(bufnr, linenr, linenr + 1, false)[1]
  if not line then
    return
  end
  local len = vim.fn.strdisplaywidth(line)
  -- Scrub markup chars.
  line = line:gsub('^%s*[#]+', ' ')
  local pad_len = math.max(minlen and 0 or len, (minlen or 0) - len)
  vim.api.nvim_buf_set_extmark(bufnr, ns, linenr, 0, {
      virt_text = {{ (hide and '' or line) .. (' '):rep(pad_len), hlgroup }},
      virt_text_pos = 'overlay',
      hl_mode = 'combine',
  })
end

local ns = vim.api.nvim_create_namespace('my_heading_highlight')
vim.cmd[[
  autocmd VimEnter,OptionSet background if v:option_new ==# 'dark'
    \| hi MyH1 ctermfg=white ctermbg=DarkGrey guisp=fg guifg=white guibg=#3b3b3b
    \| hi MyH2 ctermfg=white ctermbg=DarkGrey guisp=fg guifg=white guibg=#3a3a3a
    \| else
    \| hi MyH1 ctermfg=black ctermbg=LightGrey guisp=fg guifg=black guibg=LightGrey
    \| hi MyH2 ctermfg=black ctermbg=LightGrey guisp=fg guifg=black guibg=LightGrey
    \| endif
  hi MyH1 ctermfg=white ctermbg=DarkGrey guisp=fg guifg=white guibg=#3b3b3b
]] -- cterm=underdouble gui=underdouble guisp=fg

vim.api.nvim_create_autocmd({'FileType'}, {
  pattern = 'markdown',
  group = vim.api.nvim_create_augroup('config_helpheadings', { clear=true }),
  callback = function(ev)
    -- vim.cmd[[hi clear @markup.heading.1.delimiter.vimdoc]]
    -- vim.cmd[[hi clear @markup.heading.2.delimiter.vimdoc]]
    -- local curmarks = vim.api.nvim_buf_get_extmarks(0, vim.api.nvim_create_namespace('my_heading_highlight'), 0, -1, {})
    vim.api.nvim_buf_clear_namespace(0, ns, 0, -1)
    local minlen = 78

    -- Highlight headings in markdown docs.
    ts_traverse(0, function(node, name, startline, endline, text)
      local hlgroup = 'MyH2' -- name == 'atx_heading' and 'MyH1' or 'MyH2'
      if name == 'setext_heading' or name == 'atx_heading' then
        hl_line(0, startline, false, ns, hlgroup, minlen)
      -- elseif name == 'setext_h1_underline' or name == 'setext_h2_underline' then
      --   hl_line(0, startline, true, ns, 'Normal')
      end
    end)
  end
})
