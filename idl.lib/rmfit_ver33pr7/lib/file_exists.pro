; ----------------------------------------------------------------------------
; Verify that file FILENAME exists.  If FILENAME is an array of filenames,
; then this function returns TRUE (1) only if all files in the list exist.
; ----------------------------------------------------------------------------
FUNCTION FILE_EXISTS, filename

    found = 1    
    FOR i = 0, N_ELEMENTS (filename) - 1 DO BEGIN
    
        check = FINDFILE (filename[i], COUNT = count)
        IF (count EQ 0) THEN found = 0 
    
    ENDFOR
  
    RETURN, (found NE 0)

END
