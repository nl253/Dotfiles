if executable("hoogle")
    com! HoogleSignatureOn au! CursorHold <buffer> call HoogleSignature()
endif
