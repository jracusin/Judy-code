; ----------------------------------------------------------------------------
;+
; NAME:
;
;     Response (OBJECT)
;
; PURPOSE:
;
;     Base class object that encapsulates a detector response function.
;
; CALLING SEQUENCE:
;
;     This class is not meant to be instantiated directly.  Clients should
;     override methods in this class to handle the response function of 
;     their specific detector.
;
; INPUTS:
;
;     NONE
;
; KEYWORDS:
;
;     NONE
;
; INHERITS:
;
;     NONE
;
; DEPENDENCIES:
;
;     file__define.pro
;
; METHODS:
;     
;     read (PROCEDURE) - Read a response file
;         VIRTUAL METHOD: Must be overridden by subclasses         
;
;     response (FUNCTION) - Return response data
;         VIRTUAL METHOD: Must be overridden by subclasses         
;
;     filename (FUNCTION) - Return data filename
;         Inputs: NONE
;        Outputs: STRING containing the current data filename
;       Keywords: NONE
;
;     setFilename (PROCEDURE) - Set data filename
;         Inputs: STRING containing the new filename
;        Outputs: NONE
;       Keywords: INTERACTIVE: Select filenames from a dialog box.
;                 Any keywords valid for DIALOG_PICKFILE
;                 ERROR: Set to a named variable to return 1 if the file
;                 was successfully set, else 0.  If this keyword is set,
;                 no input STRING is required. 
;
; MODIFICATION HISTORY:
;
;     1.0 : Written, 1999 August, Robert.Mallozzi@msfc.nasa.gov
;
;-
; ----------------------------------------------------------------------------


; ----------------------------------------------------------------------------
; Constructor
; ----------------------------------------------------------------------------
FUNCTION Response::init

    self.File = OBJ_NEW ('File')
    
    RETURN, 1

END


; ----------------------------------------------------------------------------
; Destructor
; ----------------------------------------------------------------------------
PRO Response::cleanup

    OBJ_DESTROY, self.File
    PTR_FREE, self.header
    PTR_FREE, self.response
    
END    
    

; ----------------------------------------------------------------------------
; Read a response file
; ----------------------------------------------------------------------------
PRO Response::read

    ON_ERROR, 2
    MESSAGE, 'You forgot to override the READ method.'

END

;------------------------------------------------------------------------------
; Display a PHAII header
;------------------------------------------------------------------------------
PRO Response::showHeader, extension, _EXTRA = extra

    IF (NOT PTR_VALID (self.header)) THEN $
         RETURN 

    filename = self->filename (/BASENAME)

    IF (N_ELEMENTS (extension) EQ 0) THEN $
       extension = 0    
    
    myTags = N_ELEMENTS(TAG_NAMES(*self.header) ) - 1
    IF extension GT myTags THEN extension = myTags
         
    
    CASE (extension) OF
             
       0: w = DIALOG_TEXT ((*self.header).ext0, $
              TITLE = filename + ' (EXT 0)', /SAVE_OPTION, _EXTRA = extra)

       1: w = DIALOG_TEXT ((*self.header).ext1, $
              TITLE = filename + ' (EXT 1)', /SAVE_OPTION, _EXTRA = extra)

       2: w = DIALOG_TEXT ((*self.header).ext2, $
              TITLE = filename + ' (EXT 2)', /SAVE_OPTION, _EXTRA = extra)

       ELSE: MESSAGE, /CONTINUE, 'Unknown extension: ' + STRING (extension)
       
    ENDCASE
       

END

; ----------------------------------------------------------------------------
; Return object data.  
;
; WARNING: Allows return of pointer-to-data, which violates
; encapsulation and can compromise the object internal state.
; ----------------------------------------------------------------------------
FUNCTION Response::header, POINTER = pointer

    IF (NOT PTR_VALID (self.header)) THEN $
       RETURN, 0
       
    RETURN, KEYWORD_SET (pointer) ? self.header : *self.header

END

; ----------------------------------------------------------------------------
; Return response data
; ----------------------------------------------------------------------------
FUNCTION Response::response

    RETURN, *self.response
    
END


; ----------------------------------------------------------------------------
; Return current data filename
; ----------------------------------------------------------------------------
FUNCTION Response::filename, _EXTRA = extra

    RETURN, self.File->get (_EXTRA = extra)
    
END


; ----------------------------------------------------------------------------
; Set current data filename
; ----------------------------------------------------------------------------
PRO Response::setFilename, name, $
    INTERACTIVE = interactive, ERROR = error, _EXTRA = extra

    lastPath = !RM_PARAMS.lastPath
    IF (name EQ '' OR name EQ 'none') THEN BEGIN ; Destroy the old response file reference:
        OBJ_DESTROY, self.File
        self.file = OBJ_NEW('File')
    ENDIF
    self.File->set, name, $
        INTERACTIVE = interactive, ERROR = error, $
        _EXTRA = extra
        
    !RM_PARAMS.lastPath = lastPath
    
END


; ----------------------------------------------------------------------------
; Object definition
; ----------------------------------------------------------------------------
PRO Response__define

    obj = { RESPONSE, $

        File     : OBJ_NEW (), $
        header   : PTR_NEW (), $   ; Pointer to a header structure
        response : PTR_NEW ()  $

    }

END
