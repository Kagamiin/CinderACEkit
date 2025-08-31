
NEXT_ERROR_CODE .set $80

.macro deferr name
    .ident (name) = NEXT_ERROR_CODE
    .global .ident (name)
    NEXT_ERROR_CODE .set NEXT_ERROR_CODE + 1
.endmacro

ERR_OK = $00
.global ERR_OK
deferr "ERR_FILE_ID_INVALID"
deferr "ERR_LIBRARY_NOT_FOUND"
deferr "ERR_FUNCTION_INDEX_OUT_OF_BOUNDS"
deferr "ERR_OUT_OF_MEMORY"
