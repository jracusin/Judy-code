; -----------------------------------------------------------------------------
;+
; NAME:
;
;     Error (OBJECT)
;
; PURPOSE:
;
;     This class encapsulates error handling 
;
; CALLING SEQUENCE:
;
;     errorHandler = OBJ_NEW ('Error')
;
; INPUTS:
;
;     NONE
;
; KEYWORDS:
;
;     NONE
;
; PUBLIC METHODS:
;     
;     print (PROCEDURE) - print an error message
;
;         Inputs: MSG  : A STRING or STRARR containing the message to print.
;                        If an array is given, each element is output to 
;                        a new line.
;
;                 DEST : This optional parameter can be supplied to redirect
;                        the output (the default is to print to the standard
;                        output).
;
;                        Set DEST to a file logical unit number (LONG) to 
;                        direct the output to an open file.
;
;                        Set DEST to a string to direct the output
;                        to a file.  The output can be appended to the
;                        named file by setting the APPEND keyword.
;
;                        Set DEST to a valid widget ID to direct the output
;                        to widget (WIDGET_TEXT or WIDGET_LABEL).  If a
;                        WIDGET_LABEL is used, only the zeroth element of
;                        MSG will be displayed (WIDGET_LABELs do not support
;                        STRARR).  The output can be appended to a WIDGET_TEXT
;                        by setting the APPEND keyword.
;
;                        Use the ALERT method if you want to show an error
;                        dialog box.
;
;        Outputs: NONE
;       Keywords: APPEND : If DEST is a filename (string) or a WIDGET_TEXT id,
;                        set this keyword to append the MSG.
;
;     halt (PROCEDURE) - Abort program execution by stopping the 
;                        IDL interpreter.
;
;         Inputs: NONE
;        Outputs: NONE
;       Keywords: NONE
;
;     alert (FUNCTION) - Show an error message in a popup dialog box
;
;         Inputs: MSG  : A STRING or STRARR containing the message to print.
;                        If an array is given, each element is output to 
;                        a new line.
;        Outputs: The text string of the button that was selected.
;       Keywords: <Any DIALOG_MESSAGE keywords>
;                 The default is to set the ERROR keyword.
;
; MODIFICATION HISTORY:
;
;     Written, 2000 Apr, Robert.Mallozzi@msfc.nasa.gov
;
;-
; -----------------------------------------------------------------------------



; -----------------------------------------------------------------------------
; Constructor
; -----------------------------------------------------------------------------
FUNCTION Error::init
    
    RETURN, 1

END


; -----------------------------------------------------------------------------
; Destructor
; -----------------------------------------------------------------------------
PRO Error::cleanup

END


; -----------------------------------------------------------------------------
; Print an error message
; -----------------------------------------------------------------------------
PRO Error::print, msg, dest, APPEND = append

    isLUN    = 0
    isFile   = 0
    isWidget = 0
    
    n = N_ELEMENTS (msg)

    IF (N_ELEMENTS (dest) NE 0) THEN BEGIN
    
       ;== Check if DEST is a valid file unit number
        
       s = FSTAT (dest)
       isLUN = s.open
          
        
       ;== Check if DEST is a filename
       
       isFile = SIZE (dest, /TNAME) EQ 'STRING'


       ;== Check if DEST is a valid widget
       
       isWidget = WIDGET_INFO (dest, /VALID)

    
       IF (isLUN) THEN BEGIN
          FOR i = 0, n - 1 DO PRINTF, dest, msg[i] 
       ENDIF

       IF (isFile) THEN BEGIN
          OPENW, fl, dest, /GET_LUN, APPEND = append
          FOR i = 0, n - 1 DO PRINTF, fl, msg[i] 
          CLOSE, fl
          FREE_LUN, fl
       ENDIF

       IF (isWidget) THEN BEGIN

          IF (WIDGET_INFO (dest, /NAME) EQ 'TEXT') THEN BEGIN
             WIDGET_CONTROL, dest, SET_VALUE = msg, APPEND = append 
          ENDIF ELSE $
          IF (WIDGET_INFO (dest, /NAME) EQ 'LABEL') THEN BEGIN
             WIDGET_CONTROL, dest, SET_VALUE = msg[0]
          ENDIF ELSE $
             MESSAGE, 'Widget must be of type TEXT or LABEL.'

       ENDIF

    ENDIF ELSE BEGIN
    
       FOR i = 0, n - 1 DO PRINT, msg[i] 
       
    ENDELSE
    
    
END


; -----------------------------------------------------------------------------
; Stop program execution
; -----------------------------------------------------------------------------
PRO Error::halt

    STOP

END


; -----------------------------------------------------------------------------
; Show a popup dialog box
; -----------------------------------------------------------------------------
FUNCTION Error::alert, msg, _EXTRA = extra

    RETURN, DIALOG_MESSAGE (msg, _EXTRA = extra)

END


; -----------------------------------------------------------------------------
; Object to handle errors
; -----------------------------------------------------------------------------
PRO Error__define

    obj = { Error, $

        _error_null : 0B $

    }

END
