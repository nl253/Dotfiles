

function! Convert()
    AsyncRun call system('pandoc -s -o '.expand('%:p:r').'html -t html --html-q-tags --self-contained '.expand('%:p')) 
endfunction



