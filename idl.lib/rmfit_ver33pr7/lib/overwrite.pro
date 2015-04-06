; -----------------------------------------------------------------------------
;+
; NAME:
;     OVERWRITE (FUNCTION)
;
; PURPOSE:
;     A dialog widget that verifies the user wants to overwrite a 
;     specified file.  
;
; CALLING SEQUENCE:
;     overwrite = OVERWRITE (filename)
;
; INPUTS:
;     filename : name of the file to check
;
; OUTPUTS:
;     overwrite
;         INTEGER (0 or 1) specifying if the file can be overwritten.
;         If the requested file does not exist, the function returns true (1).
;
; KEYWORDS:
;
;     MESSAGE
;         Set this keyword to a STRING or STRARR to be displayed in the
;         overwrite confirmation dialog.  If a STRARR is used, each element 
;         will appear on a separate line.
;
;         You can use the variable "FILENAME" in the message string to 
;         access the input filename. 
;
;         DEFAULT = ['File exists: ' + filename, 'Ok to overwrite?']
; 
;     TITLE
;         A STRING containing the title of the dialog.
;
;     PARENT
;         A widget ID specifying the parent of the dialog.
;
; EXAMPLES:
;
;     f = 'mydata.dat'
;     overwrite = OVERWRITE (f)
;     IF (overwrite) THEN PRINT, 'Ok to overwrite.'
;
;     f = 'moredata.dat'
;     ; The message text will be "Clobber moredata.dat?"
;     overwrite = OVERWRITE (f, MESSAGE = 'Clobber FILENAME?')
;     IF (overwrite) THEN PRINT, 'Ok to overwrite.'
;
; MODIFICATION HISTORY:
;
;     Written, 1999 September, Robert.Mallozzi@msfc.nasa.gov
;
;-
; -----------------------------------------------------------------------------


FUNCTION OVERWRITE, filename, MESSAGE = message, PARENT = parent, TITLE = title

    check = FINDFILE (filename, COUNT = count)
    
    ans = 'Yes'
    IF (count NE 0) THEN BEGIN
      
       IF (N_ELEMENTS (message) EQ 0) THEN BEGIN

          msg = ['File exists: ' + filename, 'Ok to overwrite?']

       ENDIF ELSE BEGIN

          n   = N_ELEMENTS (message)
          msg = STRARR (n)
          
          FOR i = 0, n - 1 DO BEGIN
              
              msg[i] = message[i]
              s = STRPOS (STRLOWCASE (msg[i]), 'filename')
          
              IF (s NE -1) THEN $          
                 msg[i] = STRMID (msg[i], 0, s) + filename + $
                          STRMID (msg[i], s + 8)

          ENDFOR
       
       ENDELSE
       
       ans = DIALOG_MESSAGE (/QUESTION, msg, $
           DIALOG_PARENT = parent, TITLE = title)
    
    ENDIF

    RETURN, (ans EQ 'Yes')
    
END
