; ----------------------------------------------------------------------------
;+
; NAME:
;
;     MultiDetector (OBJECT)
;
; PURPOSE:
;
;     This class implements a multiple detector object with more than one spectral
;     history.
;
; INPUTS:
;
;     filename : A STRING containing a detector data file in FITS format
;
; KEYWORDS (in _EXTRA):
;
;     READER : The type of data reader to invoke. Must inherit from the 
;              base DATAREADER object.
;
;     RESPONSE : The type of DRM reader to invoke. Must inherit from the
;              base RESPONSE object.
;
; INHERITS:
;
;     DETECTOR
;
; PUBLIC METHODS:
;
;     getNumDetectors (FUNCTION) - Return the number of detectors in data file
;         Inputs: NONE
;        Outputs: Integer
;       Keywords: NONE
;
;     getDetList (FUNCTION) - Return the list of detectors to plot
;         Inputs: NONE
;        Outputs: Integer array of zeros and ones; the ones represent detectors
;                 to plot.
;       Keywords: NONE
;
;     setDetList (PROCEDURE) - Set the list of detectors to plot
;         Inputs: newDetList: Integer array of ones and zeros; the ones represent 
;                 detectors to plot.
;        Outputs: NONE 
;       Keywords: NONE
;
; MODIFICATION HISTORY:
;
;     Written, 2004 September, Rob.Preece@nsstc.nasa.gov
;
;-
; ----------------------------------------------------------------------------



; ----------------------------------------------------------------------------
; Constructor
; ----------------------------------------------------------------------------
FUNCTION MultiDetector::init, filename, _EXTRA = extra 

    ;== Set the detector object first
    
    ok = self->DETECTOR::INIT (filename, _EXTRA = extra)
             
    ;== Obtain the number of detectors in the file

    self.numDetectors = 0L
    detName = STRARR(self.numDetectors)
    
    ;== Are the detectors in synch (do their energies 'match')?

    self.detsInSynch = 0L
    
    ;== The number of the detector currently being displayed

    self.currentDet = 0L
    
    ;== Set up the list of detectors to plot; assume all to start with:
    ;   detList->names () will b set to the detector names
    ;   detList->data () will be set to whether to plot this detector (0 or 1)
    self.detList = OBJ_NEW ('List')
    FOR i = 0, self.numDetectors - 1 DO BEGIN
        self.detList->add, detName[i], 1
    ENDFOR
    
    RETURN, 1
      
END


; ----------------------------------------------------------------------------
; Destructor
; ----------------------------------------------------------------------------
PRO MultiDetector::cleanup

    OBJ_DESTROY, self.detList
    self->DETECTOR::cleanup
    
END


; ----------------------------------------------------------------------------
; Accessors and Setters
; ----------------------------------------------------------------------------
FUNCTION MultiDetector::getNumDetectors

    RETURN, self.numDetectors
    
END

FUNCTION MultiDetector::getDetList

    RETURN, *self.detList
    
END

PRO MultiDetector::setDetList, newDetList

    newNum = N_ELEMENTS(newDetList)
    IF (newNum NE self.numDetectors) THEN RETURN
    PTR_FREE, self.detList
    
END

; ----------------------------------------------------------------------------
; Definition of MultiDetector
; ----------------------------------------------------------------------------
PRO MultiDetector__define

    obj = { MultiDetector, INHERITS DETECTOR, $
      
        numDetectors     : 0L,         $    ; Number of detectors in the list
        detsInSynch      : 0L,         $    ; All detectors cover same energies
        currentDet       : 0L,         $    ; Which detector is being plotted
        detList          : OBJ_NEW ()  $    ; List of detectors

    }
    
END
