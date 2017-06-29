
command! HLCW execute 'match Boolean "'.expand("<cWORD>").'"'

au! CursorMoved HLCW
