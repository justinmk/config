command! -buffer -range=% Format let b:winview = winsaveview() | execute <line1> . "," . <line2> . "!js-beautify -f - -j -B -s " . &shiftwidth | call winrestview(b:winview)

let b:printf_pattern = 'console.log(`%s=${%s}`);'
