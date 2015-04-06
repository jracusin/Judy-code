; ----------------------------------------------------------------------------
;+
; NAME:
;
;     Detector (OBJECT)
;
; PURPOSE:
;
;     This class implements a detector object.
;
; INPUTS:
;
;     filename : A STRING containing a detector data file in FITS format
;
; KEYWORDS:
;
;     READER : The type of data reader to invoke. Must inherit from the 
;              base DATAREADER object.
;
;     RESPONSE : The type of DRM reader to invoke. Must inherit from the
;              base RESPONSE object.
;
; PUBLIC METHODS:
;
;
; MODIFICATION HISTORY:
;
;     11/07/2002: RDP. Added new keywords READER and RESPONSE to invoke 
;     different types of data readers than BFITS. The default reader is
;     BFITSREADER.
;
;     Written, 2000 March, Robert.Mallozzi@msfc.nasa.gov
;
;-
; ----------------------------------------------------------------------------


; ----------------------------------------------------------------------------
; Constructor
; ----------------------------------------------------------------------------
FUNCTION Detector::init, filename, READER = reader, RESPONSE = response, $
         _EXTRA = extra

    ;== Create Detector data objects
    IF NOT KEYWORD_SET (READER) THEN reader = 'BfitsReader'
    self.DataReader = OBJ_NEW (reader, filename)      ;'BfitsReader'
    IF (NOT OBJ_VALID (self.DataReader)) THEN BEGIN
       ;== Pass error up the chain:  MESSAGE, /CONTINUE, 'You have not chosen a PHA FITS file.'
       RETURN, 0
    ENDIF
    IF NOT KEYWORD_SET (RESPONSE) THEN response = 'BfitsResponse'
    self.Response   = OBJ_NEW (response)                      ;'BfitsResponse'
    self.Lookup     = OBJ_NEW ('Lookup')        
;    self.Lightcurve = OBJ_NEW ('Lightcurve', self.DataReader)
;    self.Spectrum   = OBJ_NEW ('Spectrum',   self.DataReader)    
;    self.Background = OBJ_NEW ('Background', self.DataReader)

    ;== Create the Singleton MFIT object for spectral fitting and display  
    ;== This object is shared by all instances of Detector

    self.Fitter = OBJ_SINGLETON ('Mfit', INFO_FILE = !MFIT.INFO_FILE, _EXTRA = extra)

    RETURN, 1

END


; ----------------------------------------------------------------------------
; Destructor
; ----------------------------------------------------------------------------
PRO Detector::cleanup

    OBJ_DESTROY, self.DataReader   
    OBJ_DESTROY, self.Response     
    OBJ_DESTROY, self.Lookup       
;    OBJ_DESTROY, self.Lightcurve   
;    OBJ_DESTROY, self.Spectrum     
;    OBJ_DESTROY, self.Background   
    ;OBJ_DESTROY, self.Fitter       


END


; ----------------------------------------------------------------------------
; Change response readers:
; ----------------------------------------------------------------------------
FUNCTION Detector::switchResponse, response, filename
	
    OBJ_DESTROY, self.Response     
    self.Response   = OBJ_NEW (response, filename) 
    RETURN, self.Response

END


; ----------------------------------------------------------------------------
; Return object references
; ----------------------------------------------------------------------------

FUNCTION Detector::dataReader & RETURN, self.DataReader & END
;FUNCTION Detector::lightcurve & RETURN, self.Lightcurve & END
;FUNCTION Detector::spectrum   & RETURN, self.Spectrum   & END
;FUNCTION Detector::background & RETURN, self.Background & END
FUNCTION Detector::response   & RETURN, self.Response   & END
FUNCTION Detector::lookup     & RETURN, self.Lookup     & END
FUNCTION Detector::fitter     & RETURN, self.Fitter     & END


; ----------------------------------------------------------------------------
; Definition
; ----------------------------------------------------------------------------
PRO Detector__define

    obj = { DETECTOR, $

        DataReader : OBJ_NEW (), $
;        Lightcurve : OBJ_NEW (), $
;        Spectrum   : OBJ_NEW (), $
;        Background : OBJ_NEW (), $
        Response   : OBJ_NEW (), $
        Lookup     : OBJ_NEW (), $
        Fitter     : OBJ_NEW ()  $
        
    }

END
