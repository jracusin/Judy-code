; ----------------------------------------------------------------------------
;+
; NAME:
;     CDFReader (OBJECT)
;
; PURPOSE:
;     An object for reading BATSE CDF data.
;
; CALLING SEQUENCE:
;     obj = OBJ_NEW ('CDFReader', filename)
;
; INPUTS:
;     filename : STRING denoting the CDF file to read
;
; KEYWORDS:
;     READ : If filename is supplied, set this keyword to read the file
;            when the object is instantiated.
;
; INHERITS:
;     DataReader
;
; AGGREGATE OBJECTS:
;     NONE
;
; DEPENDENCIES:
;     datareader__define.pro
;     dialog_text.pro
;
; PUBLIC METHODS:
;     
;     read (PROCEDURE) - Read a CDF file
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
; MODIFICATION HISTORY:
;
;     Written, 1999 December, Robert.Mallozzi@msfc.nasa.gov
;
    ; Flags indicating abnormal conditions
    ; BYTE 1:
    ;    BIT 0: DISCLA overflow in some detectors
    ;    BIT 1: GRO reorientation in progress
    ;    BIT 2: LAD gains out of balance
    ;    BIT 3: Background variations on timescales < 32 s in some LAD(s)
    ;    BIT 4: Background variations on timescales > 32 s in some LAD(s)
    ;    BIT 5: 1 kb telemetry data
    ;    BIT 6: spare
    ;    BIT 7: spare
    ; BYTE 2: spare
    ; BYTE 3: BIT i = background variation on timescales < 32 s in LAD i
    ; BYTE 4: BIT i = background variation on timescales > 32 s in LAD i

;-
; ----------------------------------------------------------------------------



; ----------------------------------------------------------------------------
; Constructor
; ----------------------------------------------------------------------------
FUNCTION CDFReader::init, filename, READ = read
       
    IF (NOT self->dataReader::init (filename)) THEN $
       RETURN, 0

    IF (KEYWORD_SET (read)) THEN BEGIN

       self->read, ERROR = error

       IF (error) THEN BEGIN
          MESSAGE, /CONTINUE, 'Failed to read file: ' + STRING (filename)
          RETURN, 0
       ENDIF

    ENDIF
     
    RETURN, 1

END


; ----------------------------------------------------------------------------
; Destructor
; ----------------------------------------------------------------------------
PRO CDFReader::cleanup

    self->dataReader::cleanup
        
END


; ----------------------------------------------------------------------------
; Read CDF data
; ----------------------------------------------------------------------------
PRO CDFReader::read, ERROR = error

    error = 0
    
    filename = self->filename ()    
    IF (filename[0] EQ '') THEN BEGIN
       error = 1
       RETURN
    ENDIF

    MESSAGE, /RESET_ERROR_STATE    
    ON_IOERROR, READ_ERROR

    ; Read FITS headers and data tables

    table0 = MRDFITS (filename, 0, header0, /SILENT)
    table1 = MRDFITS (filename, 1, header1, /SILENT)
    table2 = MRDFITS (filename, 2, header2, /SILENT)

    ; Verify BATSE CDF filetype

    filetype = STRUPCASE (SXPAR (header0, 'FILETYPE'))
    IF ((filetype NE 'BATSE_CONT') AND (filetype NE 'BATSE_DISCLA')) THEN BEGIN
       MESSAGE, /CONTINUE, 'Incorrect file type: ' + filetype
       error = 1
       RETURN
    ENDIF

    ; Assign class member variables
    
    PTR_FREE, self.header
    self.header = PTR_NEW ({  $

        ext0 : header0, $
        ext1 : header1, $
        ext2 : header2  $

    })    

    PTR_FREE, self.data
    self.data = PTR_NEW ({ $

        ext0 : table0, $
        ext1 : table1, $
        ext2 : table2  $

    })
        
;    PTR_FREE, self.units
;    self.units = PTR_NEW ({ $
;
;        times  : STRTRIM (SXPAR (header2, 'TUNIT1'), 2), $
;        rates  : STRTRIM (SXPAR (header2, 'TUNIT2'), 2), $
;        errors : STRTRIM (SXPAR (header2, 'TUNIT2'), 2), $
;        eedges : STRTRIM (SXPAR (header1, 'TUNIT4'), 2)  $
;
;    })
   
    self.dataValid = 1
                   
READ_ERROR:    

    IF (!ERROR_STATE.CODE LT 0) THEN BEGIN
       error = 1
       MESSAGE, /CONTINUE, !ERROR_STATE.MSG
    ENDIF
    
END


;------------------------------------------------------------------------------
; Display headers
;------------------------------------------------------------------------------
PRO CDFReader::showHeader, extension, _EXTRA = extra

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
; Sum a subset of the eight BATSE LADs
;------------------------------------------------------------------------------
FUNCTION CDFReader::sumDetectors, subset

    IF (NOT self.dataValid) THEN BEGIN
       MESSAGE, /CONTINUE, 'No data available.'
       RETURN, 0
    ENDIF
    
    IF (N_ELEMENTS (subset) EQ 0) THEN $
       subset = INDGEN (8)
     
    IF ((MIN (subset) LT 0) OR (MAX (subset) GT 7)) THEN BEGIN
       MESSAGE, /CONTINUE, 'Requested subset out of available range [0-7].'
       RETURN, 0
    ENDIF
    
    counts = (*self.data).ext2.counts[subset, *, *]
    counts = TOTAL (counts, 1)
    
    RETURN, counts

END


; ----------------------------------------------------------------------------
; Object definition
; ----------------------------------------------------------------------------
PRO CDFReader__define

    obj = { cdfReader, INHERITS DATAREADER, $

        dataValid : 0 $
    }


END
