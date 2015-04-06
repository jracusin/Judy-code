; ----------------------------------------------------------------------------
;+
; NAME:
;
;     Lookup (OBJECT)
;
; PURPOSE:
;
;     This class implements a data selection lookup object.
;
; INPUTS:
;
;     filename : A STRING containing a lookup filename
;
; KEYWORDS:
;
;     NONE
;
; PUBLIC METHODS:
;
;
; MODIFICATION HISTORY:
;
;     Written, 2000 March, Robert.Mallozzi@msfc.nasa.gov
;
;     03/23/2010 RDP: Added a 'FREE_LUN, FL' to balance an 'OPENR, FL,... /GET_LUN',
;                since we were running out of LUNs for long sessions.
;-
; ----------------------------------------------------------------------------


; ----------------------------------------------------------------------------
; Constructor
; ----------------------------------------------------------------------------
FUNCTION Lookup::init, FILENAME = filename

    self.File = OBJ_NEW ('File')
            
    IF (N_ELEMENTS (filename) NE 0) THEN BEGIN
      
       self.File->set, filename
       
       self->read, ERROR = error          
       IF (error) THEN BEGIN
          MESSAGE, /CONT, 'Failed to read lookup file: ' + STRING (filename)
          RETURN, 0
       ENDIF
        
    ENDIF

    RETURN, 1

END


; ----------------------------------------------------------------------------
; Destructor
; ----------------------------------------------------------------------------
PRO Lookup::cleanup

    OBJ_DESTROY, self.File

END


; ----------------------------------------------------------------------------
; Return current lookup filename
; ----------------------------------------------------------------------------
FUNCTION Lookup::filename, _EXTRA = extra

    RETURN, self.File->get (_EXTRA = extra)
    
END


; ----------------------------------------------------------------------------
; Set current lookup filename
; ----------------------------------------------------------------------------
PRO Lookup::setFilename, name, $
    INTERACTIVE = interactive, ERROR = error, _EXTRA = extra

    self.File->set, name, $
        INTERACTIVE = interactive, ERROR = error, _EXTRA = extra
    
END


; ----------------------------------------------------------------------------
; Read a lookup file
; ----------------------------------------------------------------------------
PRO Lookup::read, FILENAME = filename, AUTOREAD = autoRead, ERROR = error

    error = 0
        
    IF (N_ELEMENTS (filename) NE 0) THEN BEGIN
    
       IF (NOT FILE_EXISTS (filename)) THEN BEGIN
          error = 1
          RETURN
       ENDIF
    
    ENDIF ELSE BEGIN
                  
       filename = self.File->get ()

       IF (filename[0] EQ '') THEN BEGIN

          path = self.Reader->filename (/PATH)
          root = self.Reader->filename (/ROOT)

          f = path + root + '.lu'

          IF (KEYWORD_SET (autoRead)) THEN BEGIN

             filename = f

          ENDIF ELSE BEGIN

             filename = DIALOG_PICKFILE ( $
                 TITLE = 'Select a Lookup File for Reading', $
                 PATH = !RM_PARAMS.lastPath, GET_PATH = lastPath, $
                 /MUST_EXIST, FILE = f, FILTER = '*.lu')
             IF (filename EQ '') THEN RETURN
             ; User may have slipped up! Filename is only a path:
             IF ((STRPOS(filename, '/', /REVERSE_SEARCH) + 1) EQ STRLEN(filename)) $
                 THEN RETURN
             ; If the user accidently puts a space in front of the filename...
             retpos = STRPOS(filename, STRING(10B))
             IF (retpos NE 0) THEN BEGIN
                filename = STRMID(filename, retpos + 1)
             ENDIF
             
             !RM_PARAMS.lastPath = lastPath

          ENDELSE

       ENDIF

    ENDELSE
    
    
    OPENR, FL, filename, ERROR = error, /GET_LUN
	CLOSE, fl
    FREE_LUN, FL
    IF (error NE 0) THEN BEGIN
       msg = 'File open failed: ' + filename
       GOTO, BAD_TABLE
    ENDIF
    
 
 
 
    RETURN
    
BAD_TABLE: 

    MESSAGE, /CONTINUE, msg
    error = 1
    
END


; ----------------------------------------------------------------------------
; Write a lookup file
; ----------------------------------------------------------------------------
PRO Lookup::write, ERROR = error

    error = 0
    
    filename = self.File->get ()
        
    IF (filename[0] EQ '') THEN BEGIN
       MESSAGE, /CONTINUE, 'No lookup filename is set.'
       error = 1
       RETURN
    ENDIF


END


; ----------------------------------------------------------------------------
; Definition
; ----------------------------------------------------------------------------
PRO Lookup__define

    obj = { LOOKUP, $

        File   : OBJ_NEW () $
        
    }

END
