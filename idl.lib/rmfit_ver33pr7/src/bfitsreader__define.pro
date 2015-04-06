; ----------------------------------------------------------------------------
;+
; NAME:
;
;     BfitsReader (OBJECT)
;
; PURPOSE:
;
;     An object for reading BATSE BFITS data.
;
; CALLING SEQUENCE:
;
;     obj = OBJ_NEW ('BfitsReader' [, filename])
;
; INPUTS:
;
;     filename (OPTIONAL) : STRING denoting the BFITS file to read.  If
;         this parameter is given, the file will be read when the object
;         is instantiated.
;
; KEYWORDS:
;
;     NONE
;
; INHERITS:
;
;     DataReader
;
; AGGREGATE OBJECTS:
;
;     NONE
;
; DEPENDENCIES:
;
;     datareader__define.pro
;     dialog_text.pro
;
; PUBLIC METHODS:
;     
;     read (PROCEDURE) - Read a BFITS file
;         Inputs: NONE
;        Outputs: NONE 
;       Keywords: ERROR : Set to a named variable to return 0 if the read
;                 was successful, or 1 if the read failed.
;
;     header (FUNCTION) - Return FITS header data
;         Inputs: NONE
;        Outputs: STRUCTURE of FITS header data
;       Keywords: POINTER : Set this keyword to return a pointer to data
;                     instead of a copy.  Clients should not alter pointer data.
;
;     data (FUNCTION) - Return event data
;         Inputs: NONE
;        Outputs: STRUCTURE of raw data (times, rates, etc)
;       Keywords: POINTER : Set this keyword to return a pointer to data
;                     instead of a copy.  Clients should not alter pointer data.
;
;     units (FUNCTION) - Return event data units
;         Inputs: NONE
;        Outputs: STRUCTURE of the raw data units
;       Keywords: POINTER : Set this keyword to return a pointer to data
;                     instead of a copy.  Clients should not alter pointer data.
;
;     showHeader (PROCEDURE) - View header information in a dialog widget
;         Inputs: extension - BFITS table extension (0, 1, or 2)
;        Outputs: NONE
;       Keywords: DIALOG_PARENT: Set this keyword to the widget ID of a widget 
;                     over which the dialog should be positioned, else the 
;                     dialog is centered on the display.
;
;     showNotes (PROCEDURE) - View miscellaneous notes in a dialog widget
;         Inputs: NONE
;        Outputs: NONE
;       Keywords: DIALOG_PARENT: Set this keyword to the widget ID of a widget 
;                     over which the dialog should be positioned, else the 
;                     dialog is centered on the display.
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
FUNCTION BfitsReader::init, filename
       
    IF (NOT self->dataReader::init (filename)) THEN $
       RETURN, 0

    IF (N_ELEMENTS (file) NE 0) THEN BEGIN

       self->read, ERROR = error

       IF (error EQ 1) THEN BEGIN
       
          MESSAGE, /CONTINUE, 'Failed to read file: ' + STRING (filename)
          RETURN, 0
       
       ENDIF

    ENDIF
     
    RETURN, 1

END


; ----------------------------------------------------------------------------
; Destructor
; ----------------------------------------------------------------------------
PRO BfitsReader::cleanup

    self->dataReader::cleanup
        
END


; ----------------------------------------------------------------------------
; Read BFITS data
; ----------------------------------------------------------------------------
PRO BfitsReader::read, ERROR = error

    error = 0
    self.dataValid = 0
    
    IF (NOT self->haveFile ()) THEN BEGIN
       error = 1
       RETURN
    ENDIF
    
    filename = self->filename ()    

    MESSAGE, /RESET_ERROR_STATE    
    ON_IOERROR, READ_ERROR
    
    ;== Read FITS headers and data tables

    table0 = MRDFITS (filename, 0, header0, /SILENT)
    table1 = MRDFITS (filename, 1, header1, /SILENT)
    table2 = MRDFITS (filename, 2, header2, /SILENT)
    
    self.zenAngle  = (table1.det_s_zn)[0]
    
    ;== Verify BATSE BFITS filetype

    filetype = SXPAR (header0, 'FILETYPE')
    IF (STRUPCASE (filetype) NE 'BATSE BURST SPECTRA') THEN BEGIN
       MESSAGE, /CONTINUE, 'Incorrect file type: ' + filetype
       error = 1
       RETURN
    ENDIF
    
    ;== Filter negative error rates 

; TODO: Is this necessary ?

    idx = WHERE (table2.errors LT 0.0, zeroCount)
    IF (zeroCount NE 0) THEN BEGIN
       MESSAGE, /CONTINUE, 'Negative error rates in FITS file set to 0.'
       table2.errors = table2.errors > 0.0
    ENDIF

    ;== BFITS files can store summed rates from multiple detectors, in 
    ;== which case the thresholds for each detector are stored in a 
    ;== 2-dimensional array.  In this case, average the thresholds over 
    ;== these detectors.  

    s = SIZE (table1.e_edges)
    numDets = (s[0] EQ 1) ? 1 : (s)[2]
    numChan = s[1] - 1

    IF (numDets EQ 1) THEN BEGIN
       aveThresholds = table1.e_edges
    ENDIF ELSE BEGIN
       aveThresholds = TOTAL (table1.e_edges, 2) / numDets
    ENDELSE

    thresholds       = FLTARR (2, numChan, /NOZERO)
    thresholds[0, *] = aveThresholds[0:numChan - 1]
    thresholds[1, *] = aveThresholds[1:numChan]
    

    ;== Assign class member variables
    
    notes = self->readNotes (header0, header2)

    PTR_FREE, self.header
    self.header = PTR_NEW ({  $

        ext0  : header0, $
        ext1  : header1, $
        ext2  : header2, $
        notes : notes    $

    })    

    PTR_FREE, self.data
    self.data = PTR_NEW ({ $
        
        times  : table2.times,  $
        rates  : table2.rates,  $
        errors : table2.errors, $
        eedges : thresholds     $

    })
        
    PTR_FREE, self.units
    self.units = PTR_NEW ({ $

        times  : STRTRIM (SXPAR (header2, 'TUNIT1'), 2), $
        rates  : STRTRIM (SXPAR (header2, 'TUNIT2'), 2), $
        errors : STRTRIM (SXPAR (header2, 'TUNIT2'), 2), $
        eedges : STRTRIM (SXPAR (header1, 'TUNIT4'), 2)  $

    })
   
    self.dataValid = 1
     
    RETURN 
                       
READ_ERROR:    

    IF (!ERROR_STATE.CODE LT 0) THEN BEGIN
       error = 1
       MESSAGE, /CONTINUE, !ERROR_STATE.MSG
    ENDIF
    
END


;------------------------------------------------------------------------------
; PRIVATE: Extract some useful notes from headers
;------------------------------------------------------------------------------
FUNCTION BfitsReader::readNotes, h0, h2

    ; COMPILE_OPT HIDDEN
        
    s = 'Event: ' + STRTRIM (SXPAR (h0, 'OBJECT'), 2)
    notes = s
          
    s = 'BATSE Trigger: ' + STRTRIM (SXPAR (h0, 'BATSE_TR'), 2)
    notes = [notes, s]

    select  = STRUPCASE (STRTRIM (SXPAR (h2, 'DSELECT'), 2))
    bselect = BYTE (select)

    detName = STRTRIM (SXPAR (h2, 'DET_MODE'), 2) + '#'
    
    idx = WHERE (bselect EQ (byte ('N'))[0], cnt)
    IF (cnt GT 0) THEN $
       bselect[idx] = BYTE ('-')
       
    idx = WHERE (bselect EQ (byte ('Y'))[0], cnt)
    FOR i = 0, cnt - 1 DO BEGIN
        j = idx[i]
        bselect[j] = BYTE (STRTRIM (7 - j, 2))
        detName = detName + STRTRIM (7 - j, 2)
    ENDFOR
    
    self.detName = detName
    select = STRING (bselect)    
    s = 'Detectors: ' + STRTRIM (SXPAR (h2, 'DET_MODE'), 2) + ' ' + select
    notes = [notes, s]

    s = 'Data types: ' + STRTRIM (SXPAR (h2, 'DATATYPE'), 2)
    notes = [notes, s]

    s = 'Start TJD: ' + STRTRIM (LONG (SXPAR (h2, 'BASETIME')), 2)
    notes = [notes, s]

    s = 'Start sec: ' + STRTRIM (SXPAR (h0, 'TRIG-TIM'), 2)
    notes = [notes, s]

    s = 'File version: ' + STRTRIM (SXPAR (h0, 'MNEMONIC'), 2)
    notes = [notes, s]
    
    RETURN, notes

END


;------------------------------------------------------------------------------
; Display a BFITS header
;------------------------------------------------------------------------------
PRO BfitsReader::showHeader, extension, _EXTRA = extra

    IF (NOT self.dataValid) THEN $
       RETURN

    filename = self->filename (/BASENAME)

    IF (N_ELEMENTS (extension) EQ 0) THEN $
       extension = 0    
       
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


;------------------------------------------------------------------------------
; Display notes
;------------------------------------------------------------------------------
PRO BfitsReader::showNotes, _EXTRA = extra

    IF (NOT self.dataValid) THEN $
       RETURN

    filename = self->filename (/BASENAME)

    w = DIALOG_TEXT ((*self.header).notes, $
        TITLE = filename, /SAVE_OPTION, _EXTRA = extra)

END


;------------------------------------------------------------------------------
; Get Unique Detector ID String
;------------------------------------------------------------------------------
FUNCTION BfitsReader::getDetName

     RETURN, self.detName

END


;------------------------------------------------------------------------------
; Get Zenith Angle
;------------------------------------------------------------------------------
FUNCTION BfitsReader::getZenAngle

     RETURN, self.zenAngle

END


; ----------------------------------------------------------------------------
; Object definition
; ----------------------------------------------------------------------------
PRO BfitsReader__define

    obj = { BFITSREADER, INHERITS DATAREADER, $

        detName   : '', $
        dataValid : 0, $
        zenAngle  : 0. $
        
    }

END
