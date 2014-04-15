" %VS120COMNTOOLS% => C:\Program Files (x86)\Microsoft Visual Studio 12.0\Common7\Tools\
" $COMSPEC /k $VS120COMNTOOLS."vsvars32.bat"

setlocal tabstop=4 shiftwidth=4 copyindent

"TODO: implement unite sources for:
"        <m-o> OmniSharpFindSymbol/OmniSharpFindMembers (?)
"        gD    OmniSharpFindType
"        <bs>  OmniSharpGetCodeActions
if exists("g:OmniSharp_loaded")
  setlocal omnifunc=OmniSharp#Complete

  nnoremap <buffer> gd    :<c-u>OmniSharpGotoDefinition<cr>
  nnoremap <buffer> gr    :<c-u>OmniSharpFindUsages<cr>
  nnoremap <buffer> gI    :<c-u>OmniSharpFindImplementations<cr>
  nnoremap <buffer> cri   :<c-u>JavaImportOrganize<cr>
  nnoremap <buffer> crr   :<c-u>OmniSharpRename<cr>
  nnoremap <buffer> cpA   :<c-u>OmniSharpAddToProject<cr>
  nnoremap <buffer> cpR   :<c-u>OmniSharpReloadSolution<cr>
  nnoremap <buffer> @!    :<c-u>OmniSharpBuildAsync<cr>
  nnoremap <buffer> K     :<c-u>OmniSharpDocumentation<cr>
  nnoremap <buffer> gk    :<c-u>OmniSharpTypeLookup<cr>
  nnoremap <buffer> <m-o> :<c-u>OmniSharpFindMembers<cr>
  nnoremap <buffer> <bs>  :<c-u>OmniSharpFixIssue<cr>
  nnoremap <buffer> !e    :<c-u>OmniSharpFindSyntaxErrors<cr>
  nnoremap <buffer>   :<c-u>JavaCorrect<cr>

  command! -buffer Format OmniSharpCodeFormat
endif

