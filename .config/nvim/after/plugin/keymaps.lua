-- Unmap ]C [C from vim-unimpaired
vim.cmd[[
  if !empty(maparg('[C'))
    unmap [C
    unmap [CC
    unmap ]C
    unmap ]CC
  endif
]]
