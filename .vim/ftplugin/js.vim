command! -buffer -range=% Format let b:winview = winsaveview() | execute <line1> . "," . <line2> . "!js-beautify -f - -j -B -s " . &shiftwidth | call winrestview(b:winview)

