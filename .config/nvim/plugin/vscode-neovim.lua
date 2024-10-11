if not vim.g.vscode then
  return
end

vim.cmd[[
  nnoremap <silent> gd <Cmd>lua require('vscode-neovim').call(vim.v.count > 0 and 'typescript.goToSourceDefinition' or 'editor.action.revealDefinition')<CR>
  nnoremap <silent> gD <Cmd>lua require('vscode-neovim').call('editor.action.goToImplementation')<CR>
  nnoremap <silent> gri <Cmd>lua require('vscode-neovim').call('references-view.findImplementations')<CR>
  nnoremap <silent> <delete> <Cmd>lua require('vscode-neovim').call('editor.debug.action.toggleBreakpoint')<CR>
   nnoremap <silent> gO <Cmd>lua require('vscode-neovim').call('workbench.action.gotoSymbol')<CR>
  "nnoremap <silent> gO <Cmd>lua require('vscode-neovim').call('outline.focus')<CR>
  nnoremap <silent> gr/ <Cmd>lua require('vscode-neovim').call('workbench.action.showAllSymbols')<CR>
  nnoremap <silent> - <Cmd>lua require('vscode-neovim').call('workbench.files.action.showActiveFileInExplorer')<CR>
  nnoremap <silent> <c-b> <Cmd>lua require('vscode-neovim').call('workbench.action.showAllEditorsByMostRecentlyUsed')<CR>
  nnoremap <silent> <c-n> <Cmd>lua require('vscode-neovim').call('workbench.action.editor.nextChange')<CR>
  nnoremap <silent> <c-p> <Cmd>lua require('vscode-neovim').call('workbench.action.editor.previousChange')<CR>

  nnoremap <silent> UD <Cmd>lua require('vscode-neovim').call('git.openChange')<CR>
  nnoremap <silent> UW <Cmd>lua require('vscode-neovim').call('git.stage')<CR>
  nnoremap <silent> UB <Cmd>lua require('vscode-neovim').call('gitlens.toggleFileBlame')<CR>
]]
