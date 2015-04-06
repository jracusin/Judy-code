
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;+
;
; NAME:
;    FEXIST
;       
; PURPOSE:
;    Check if a file exists. This now uses built-in function file_test()
;    for versions of IDL >= 5.4  For efficiency the user should use that
;    function if possible.  Otherwise a test using the openr function is 
;    used. In that case directories cannot be checked.
;
; CALLING SEQUENCE:
;    file_exists=fexist(file, _extra=_extra)
;
; INPUTS: 
;    file: A full path filename.
;
; OPTIONAL INPUTS:
;    _extra: For IDL >= 5.4 these are the options to the file_test() function
;
; KEYWORD PARAMETERS:
;    NONE
;       
; OUTPUTS: 
;    1 or 0: 1 means the file exists, 0 means the file does not exist.
;
; OPTIONAL OUTPUTS:
;    NONE
;
; EXAMPLE:
;    outfile='test.dat'
;    ;; don't overwrite existing file
;    if fexist(file) then outfile = 'some other name'
;    openw, lun, file, /get_lun
;    ....etc.
;
; CALLED ROUTINES:
;    FILE_TEST (IDL >= 5.4)
;    OPENR
;    FREE_LUN
;
;	
;
; REVISION HISTORY:
;    07-DEC-2000 Creation. Erin Scott Sheldon
;    01-MAR-2003 Added use of FILE_TEST function
;       
;                                      
;-                                       
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

FUNCTION fexist, file, _extra=_extra

  IF N_params() EQ 0 THEN BEGIN 
     message,'ERROR: no input file.  -Syntax: check=fexist(file, _extra=_extra)'
  ENDIF 

  IF float(!version.release) GE 5.4 THEN BEGIN 
      return, file_test(file, _extra=_extra)
  ENDIF ELSE BEGIN 
      error=0
      openr, lun, file, /get_lun,error=error
      IF error NE 0 THEN BEGIN
          return,0 
      ENDIF ELSE BEGIN
          free_lun,lun
          return,1
      ENDELSE 
  ENDELSE 

END 

