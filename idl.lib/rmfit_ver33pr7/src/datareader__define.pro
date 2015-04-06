; ----------------------------------------------------------------------------
;+
; NAME:
;     DataReader (OBJECT)
;
; PURPOSE:
;     Base class object that encapsulates a data reader.
;
; CALLING SEQUENCE:
;     This class is not meant to be instantiated directly.  Clients should
;     override methods in this class to handle the dataset of their 
;     specific reader.  See class BfitsReader for an example.
;
; INPUTS:
;     filename (OPTIONAL) : STRING name of a data file
;
; KEYWORDS:
;     NONE
;
; INHERITS:
;     NONE
;
; AGGREGATE OBJECTS:
;     File
;
; DEPENDENCIES:
;     file__define.pro
;     file_exists.pro
;
; PUBLIC METHODS:
;     
;     read (PROCEDURE) - Read a data file
;         VIRTUAL METHOD: Must be overridden by subclasses         
;
;     filename (FUNCTION) - Return data filename
;         Inputs: NONE
;        Outputs: STRING containing the current data filename
;       Keywords: 
;
;         * NOT ALL OF THE FOLLOWING KEYWORDS APPLY FOR ALL OS.  For example, 
;           DEVICE and VERSION do not apply under UNIX, and are returned as 
;           null.
;
;           BASENAME  : Filename with leading path removed
;
;           PATH      : Filename path
;                       UNIX    : path + basename is the full filename
;                       VMS     : device + path + basename is the full filename
;                       WINDOWS : device + path + basename is the full filename
;                       MACOS   : device + path + basename is the full filename
;
;           DIRECTORY : Filename directory (same as PATH)
;
;           ROOT      : Filename with leading path and file extension removed.  
;                       An extension consists of all characters after and 
;                       including the last "."
;
;           EXTENSION : Filename extension.  An extension consists of all 
;                       characters after and including the last "."
;
;           DEVICE    : Filename device
;
;           NODE      : VMS only: The node name
;
;           VERSION   : VMS only: Filename version.  A version consists of 
;                       all characters after and including the last ";"
;
;     setFilename (PROCEDURE) - Set data filename
;         Inputs: STRING containing the new filename
;        Outputs: NONE
;       Keywords: INTERACTIVE: Select filenames from a dialog box.
;                     If this keyword is set, no input is required. 
;                     Any keywords valid for DIALOG_PICKFILE may also be used.
;                 ERROR: Set to a named variable to return 1 if the file
;                     was successfully set, else return 0.  
;
; MODIFICATION HISTORY:
;
;     Written, 1999 August, Robert.Mallozzi@msfc.nasa.gov
;
;-
; ----------------------------------------------------------------------------


; ----------------------------------------------------------------------------
; Constructor
; ----------------------------------------------------------------------------
FUNCTION DataReader::init, filename

	;== Assume Poisson statistics to begin with:
	self.poisson = 1
  
    IF (N_ELEMENTS (filename) NE 0) THEN BEGIN
     
       IF (NOT FILE_EXISTS (filename)) THEN BEGIN
          MESSAGE, /CONTINUE, 'File not found: ' + STRING (filename)
          RETURN, 0
       ENDIF

       self.File = OBJ_NEW ('File', filename)
		 
       self->read, ERROR = error
       
       IF (error) THEN RETURN, 0
       
    ENDIF ELSE BEGIN
    
       self.File = OBJ_NEW ('File')

    ENDELSE
        
    RETURN, 1

END


; ----------------------------------------------------------------------------
; Destructor
; ----------------------------------------------------------------------------
PRO DataReader::cleanup

    PTR_FREE, self.header
    PTR_FREE, self.data
    PTR_FREE, self.units
    
    OBJ_DESTROY, self.File
    
END    


; ----------------------------------------------------------------------------
; Read a data file
; ----------------------------------------------------------------------------
PRO DataReader::read, ERROR = error

    error = 0
    ON_ERROR, 2
    MESSAGE, 'You forgot to override the READ method.'

END


; ----------------------------------------------------------------------------
; Return current data filename
; ----------------------------------------------------------------------------
FUNCTION DataReader::filename, _EXTRA = extra

    RETURN, self.File->get (_EXTRA = extra)
    
END


; ----------------------------------------------------------------------------
; Set current data filename
; ----------------------------------------------------------------------------
PRO DataReader::setFilename, name, $
    INTERACTIVE = interactive, ERROR = error, _EXTRA = extra

    lastPath = !RM_PARAMS.lastPath
    self.File->set, name, $
        INTERACTIVE = interactive, ERROR = error, $
        _EXTRA = extra
        
    !RM_PARAMS.lastPath = lastPath

END


; -----------------------------------------------------------------------------
; Query if filename has been set
; -----------------------------------------------------------------------------
FUNCTION DataReader::haveFile & RETURN, self.File->haveFile () & END


; ----------------------------------------------------------------------------
; Return object data.  
;
; WARNING: Allows return of pointer-to-data, which violates
; encapsulation and can compromise the object internal state.
; ----------------------------------------------------------------------------
FUNCTION DataReader::header, POINTER = pointer

    IF (NOT PTR_VALID (self.header)) THEN $
       RETURN, 0
       
    RETURN, KEYWORD_SET (pointer) ? self.header : *self.header

END

FUNCTION DataReader::data, POINTER = pointer

    IF (NOT PTR_VALID (self.data)) THEN $
       RETURN, 0

    RETURN, KEYWORD_SET (pointer) ? self.data : *self.data

END

FUNCTION DataReader::units, POINTER = pointer

    IF (NOT PTR_VALID (self.units)) THEN $
       RETURN, 0

    RETURN, KEYWORD_SET (pointer) ? self.units : *self.units

END

FUNCTION DataReader::getPoisson

	RETURN, self.poisson

END

PRO DataReader::setPoisson, isPoisson

	self.poisson = FIX(isPoisson)

END


; ----------------------------------------------------------------------------
; Object definition
; ----------------------------------------------------------------------------
PRO DataReader__define

    obj = { DATAREADER, $

        Poisson: 0         , $   ; Data consistent with Poisson statistics
        File   : OBJ_NEW (), $   ; Data file name object
        header : PTR_NEW (), $   ; Pointer to a header structure
        data   : PTR_NEW (), $   ; Pointer to a data structure
        units  : PTR_NEW ()  $   ; Pointer to a structure containing data units
                
    }

END
