" %VS120COMNTOOLS% => C:\Program Files (x86)\Microsoft Visual Studio 12.0\Common7\Tools\
" $COMSPEC /k $VS120COMNTOOLS."vsvars32.bat"

"open current file in powershell ISE
nnoremap <buffer> yxG   :<c-u>Start powershell_ise.exe %<cr>

