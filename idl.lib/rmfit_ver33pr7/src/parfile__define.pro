; -----------------------------------------------------------------------------
;+
; NAME:
;     Parfile (OBJECT)
;
; PURPOSE:
;     An object for accessing parameter FITS file data  
;
; CALLING SEQUENCE:
;     file = OBJ_NEW ('Parfile', filename, /INTERACTIVE])
;
; INPUTS:
;     filename: Scalar filename
;
; KEYWORDS:
;     INTERACTIVE
;         Select filename(s) interactively when the object is 
;         instantiated.  If this keyword is set, any valid 
;         DIALOG_PICKFILE keywords may also be used.
;
; INHERITS:
;     File
;
; PUBLIC METHODS:
;     
;
; EXAMPLE:
;
;     ;== Create an instance of a file object
;     
;     file = OBJ_NEW ('ParFile', '/path/to/file/root.ext')
;
;     ;== Obtain some file properties
;     
;     basename = file->get (/BASENAME)      
;     path     = file->get (/PATH)          
;     root     = file->get (/ROOT)          
;     ext      = file->get (/EXTENSION)
;     version  = file->get (/VERSION)    ; null for UNIX
;
;     ;== All done
;     
;     OBJ_DESTROY, file
;
; MODIFICATION HISTORY:
;
;     Written, 2002 Nov, Rob.Preece@nsstc.nasa.gov
;
;-
; -----------------------------------------------------------------------------



; -----------------------------------------------------------------------------
; Constructor
; -----------------------------------------------------------------------------
FUNCTION ParFile::init, name, _EXTRA = extra
    
    OK = self->File::init (name, FILTER = '*.par', $
         PATH = !RM_PARAMS.lastPath, GET_PATH = lastPath, $
        _EXTRA = extra)
    ;self.labelList = PTR_NEW (self->getLabelList ())
    !RM_PARAMS.lastPath = lastPath
    RETURN, 1

END


; -----------------------------------------------------------------------------
; Destructor
; -----------------------------------------------------------------------------
PRO ParFile::cleanup

    ;PTR_FREE, self.data
    IF PTR_VALID (self.labelList) THEN PTR_FREE, self.labelList
    self->File::cleanup

END

; -----------------------------------------------------------------------------
; Accessor for labelList
; -----------------------------------------------------------------------------
FUNCTION ParFile::getLabelList

    IF NOT PTR_VALID (self.labelList) THEN BEGIN
        myFile = self->File::get (/PATH) + self->File::get (/BASENAME)
        header2 = headfits (myfile, EXT = 2)
        throwAway = sxpar (header2, 'TTYPE*', COMMENT = labelList)
        nPars = N_ELEMENTS (labelList) - 1
        self.labelList = PTR_NEW (labelList[1: nPars])
    ENDIF
    RETURN, *self.labelList 

END

; -----------------------------------------------------------------------------
; Object that stores a filename
; -----------------------------------------------------------------------------
PRO ParFile__define

    obj = { PARFILE, INHERITS FILE,   $

        ;data       : PTR_NEW (), $         
        labelList  : PTR_NEW () $        

    }

END
