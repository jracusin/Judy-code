; ----------------------------------------------------------------------------
;+
; NAME:
;
;     PHAReader (OBJECT)
;
; PURPOSE:
;
;     An object for reading PHA FITS data.
;
; CALLING SEQUENCE:
;
;     obj = OBJ_NEW ('PHAReader' [, filename])
;
; INPUTS:
;
;     filename (OPTIONAL) : STRING denoting the PHA file to read.  If
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
;     read (PROCEDURE) - Read a PHA file
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
;         Inputs: extension - PHA table extension (0, 1, or 2)
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
;     Adapted from BFITSReader, written 1999 August, Robert.Mallozzi@msfc.nasa.gov
;
;-
; ----------------------------------------------------------------------------



; ----------------------------------------------------------------------------
; Constructor
; ----------------------------------------------------------------------------
FUNCTION PHAReader::init, filename
       
    IF (NOT self->dataReader::init (filename)) THEN BEGIN
       self->dataReader::cleanup
       RETURN, 0
       ENDIF

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
PRO PHAReader::cleanup

    self->dataReader::cleanup
        
END


; ----------------------------------------------------------------------------
; Read PHA data
; ----------------------------------------------------------------------------
PRO PHAReader::read, ERROR = error

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
    
    ;== Verify FITS filetype
    filetype = SXPAR (header1, 'EXTNAME')
    IF (STRUPCASE (filetype) NE 'SPECTRUM') THEN BEGIN
        ;== May be an INTEGRAL PHA file
        newHDR = HEADFITS (filename, EXTEN = 2)
        filetype = STRTRIM(SXPAR (newHDR, 'EXTNAME'), 2)
        IF (filetype NE 'SPI.-PHA1-SPE') AND $
           (filetype NE 'ISRG-PHA1-SPE') AND (filetype NE 'SPECTRUM') THEN BEGIN
            MESSAGE, /CONTINUE, 'Incorrect file type: ' + filetype
            error = 1
            RETURN
        ENDIF
        ;=== Try reversing the extensions:
        table2 = table1
        header2 = header1
        table1 = MRDFITS (filename, 2, header1, /SILENT)
    ENDIF
    
    ; See if there is a response file listed:
    self.rmfFile = ''
    foundRMF = 0
    respPos = STRPOS(header1, 'RESPFILE')
    jj = WHERE(respPos GE 0, cnt)
    IF (cnt NE 0) THEN BEGIN
        responseFile = STRLOWCASE(STRTRIM(SXPAR (header1, 'RESPFILE'), 2))
        IF (responseFile NE 'none') THEN BEGIN
			;c = FINDFILE (responseFile, COUNT = cnt)
			foundRMF = 1
			self.rmfFile = responseFile
	    ENDIF
	ENDIF

    ; See if there is an background file listed:
    self.backFile = ''
    backPos = STRPOS(header1, 'BACKFILE')
    jj = WHERE(backPos GE 0, cnt)
    IF (cnt NE 0) THEN BEGIN
        backFile = STRLOWCASE(STRTRIM(SXPAR (header1, 'BACKFILE'), 2))
        IF (backFile NE 'none') THEN BEGIN
			;c = FINDFILE (responseFile, COUNT = cnt)
			self.backFile = backFile
	    ENDIF
	ENDIF

	IF N_ELEMENTS(header2) NE 0 THEN BEGIN
		;numChan = N_ELEMENTS(table1.rate)
		numChan = SXPAR (header1, 'DETCHANS')
		myChanEdges = FLTARR(numChan + 1) 
		myChanEdges[0:numChan - 1] = table2.e_min
		myChanEdges[1:numChan] = table2.e_max
	ENDIF ELSE BEGIN
		IF foundRMF THEN BEGIN
			myResponse = OBJ_NEW('RMFResponse', responseFile)
			self.rmfFile = responseFile
			respStruct = myResponse->response()
			myChanEdges = respStruct.chan_edges
			OBJ_DESTROY, myResponse
		ENDIF ELSE BEGIN  ;== No response file: fake the energy edges
			self.rmfFile = ''
			numChan = SXPAR (header1, 'DETCHANS')
			myChanEdges = FINDGEN(numChan + 1) + 1.0
			;myChanEdges = FINDGEN(N_ELEMENTS(table1.rate) + 1) + 1.0
		ENDELSE
    ENDELSE

    numChan = N_ELEMENTS(myChanEdges) - 1

    thresholds       = FLTARR (2, numChan, /NOZERO)
    thresholds[0, *] = myChanEdges[0:numChan - 1]
    thresholds[1, *] = myChanEdges[1:numChan]
    IF (thresholds[0, 0] LE 0.0) THEN thresholds[0, 0] = 0.1 * thresholds[1, 0] 
    
    exposure = SXPAR (header1, 'EXPOSURE', COUNT = ecnt)
	IF (ecnt EQ 0) THEN BEGIN
		tstart = SXPAR (header1, 'TSTART', COUNT = tcnt)
		IF (tcnt = 0) THEN BEGIN
			MESSAGE, /CONTINUE, 'There is no way to determine the exposure in this file.'
			RETURN
		ENDIF
		tstop = SXPAR (header1, 'TSTOP')
		exposure = tstop - tstart
	ENDIF
    
    ;== Assign class member variables
    notes = self->readNotes (header1)

    PTR_FREE, self.header
    self.header = PTR_NEW ({  $

        ext0  : header0, $
        ext1  : header1, $
        ext2  : 0,       $
        notes : notes    $

    })    
    
    columns = TAG_NAMES(table1)
    mm = WHERE(columns EQ 'COUNTS', ccnt)
    IF (ccnt GT 0) THEN BEGIN
		tZero = SXPAR (header1, 'TZERO' + STRTRIM(mm[0] + 1, 2))
		theRates = (table1.counts + tZero) / exposure
    ENDIF ELSE BEGIN
    	theRates = table1.rate
    ENDELSE
    
    ;== 01/25/10 RDP: Not necessary; the rsp file is also scaled!
	;npix = SXPAR (header1, 'NGOODPIX', COUNT = pcnt)
	;IF (pcnt EQ 0) THEN npix = 1
	therates = theRates ;* npix
	total_err = table1.stat_err ;* npix
    ;== Make the units correct in case of COUNTS data:
    IF (ccnt GT 0) THEN BEGIN
		total_err = total_err / exposure
    ENDIF
    
    ;== Filter negative error rates and NANs:
    idx = WHERE (total_err LT 0.0, zeroCount)
    IF (zeroCount NE 0) THEN BEGIN
       MESSAGE, /CONTINUE, 'Negative error rates in FITS file set to 0.'
       total_err = total_err > 0.0
    ENDIF
    
    ;== See about the SYS_ERR
    sysTest = WHERE (TAG_NAMES(table1) EQ 'SYS_ERR', sysCnt)
    IF (sysCnt GT 0) THEN BEGIN
         temp = (theRates * table1.sys_err)^2 + total_err * total_err
         total_err = SQRT(temp)
         ;== Flag the non-Poisson errors for spectrum::plot
         self.poisson = 0
    ENDIF
	;== 03/25/2010 RDP: Make the test for POISERR = F:
	poiTest = SXPAR (header1, 'POISSERR', COUNT = pcnt)
	IF (pcnt NE 0) THEN BEGIN 
		IF (NOT poiTest) THEN self.poisson = 0
	ENDIF

    PTR_FREE, self.data
    self.data = PTR_NEW ({ $
        
        times  : [0.0, exposure], $
        rates  : theRates,        $
        errors : total_err,       $
        eedges : thresholds       $

    })
        
    PTR_FREE, self.units
    self.units = PTR_NEW ({ $

        times  : 's', $ ;STRTRIM (SXPAR (header2, 'TUNIT1'), 2), $
        rates  : STRTRIM (SXPAR (header1, 'TUNIT2'), 2), $
        errors : STRTRIM (SXPAR (header1, 'TUNIT3'), 2), $
        eedges : 'keV'  $ ;STRTRIM (SXPAR (header1, 'TUNIT4'), 2)  $

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
FUNCTION PHAReader::readNotes, h1

    ; COMPILE_OPT HIDDEN
        
    s = 'Event: ' + STRTRIM (SXPAR (h1, 'OBJECT'), 2)
    notes = s
          
    s = 'Telescope: ' + STRTRIM (SXPAR (h1, 'TELESCOP'), 2)
    notes = [notes, s]
    
    s = STRTRIM (SXPAR (h1, 'INSTRUME'), 2)
    s = 'Instrument: ' + s
    notes = [notes, s]

    detName = STRTRIM (SXPAR (h1, 'DETNAM'), 2)
    s = 'Detector: ' + detName
    notes = [notes, s]
    self.detName = detName

    s = 'Data types: ' + 'PHA'
    notes = [notes, s]

    RETURN, notes

END


;------------------------------------------------------------------------------
; Display a PHA header
;------------------------------------------------------------------------------
PRO PHAReader::showHeader, extension, _EXTRA = extra

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

;       2: w = DIALOG_TEXT ((*self.header).ext2, $
;              TITLE = filename + ' (EXT 2)', /SAVE_OPTION, _EXTRA = extra)

       ELSE: MESSAGE, /CONTINUE, 'Unknown extension: ' + STRING (extension)
       
    ENDCASE
       

END


;------------------------------------------------------------------------------
; Display notes
;------------------------------------------------------------------------------
PRO PHAReader::showNotes, _EXTRA = extra

    IF (NOT self.dataValid) THEN $
       RETURN

    filename = self->filename (/BASENAME)

    w = DIALOG_TEXT ((*self.header).notes, $
        TITLE = filename, /SAVE_OPTION, _EXTRA = extra)

END


;------------------------------------------------------------------------------
; Get RMF File Name from the PHA file
;------------------------------------------------------------------------------
FUNCTION PHAReader::getRMFFile

    RETURN, self.RMFFile

END


;------------------------------------------------------------------------------
; Get Background File Name from the PHA file
;------------------------------------------------------------------------------
FUNCTION PHAReader::getBackFile, RENEW = renew, NONINTERACTIVE = noninteractive

	backFileName = KEYWORD_SET(RENEW) ? '' : self.backFile
	
	;== Abort, abort, abort:
	IF KEYWORD_SET(NONINTERACTIVE) THEN RETURN, backFileName
	
	; This routine has been called because we need a background file:
	IF (backFileName EQ '' OR backFileName EQ 'none') THEN BEGIN
		path = self->filename (/PATH)
		root = self->filename (/ROOT)
		
		f = path + root + '.bak'
		backFile = DIALOG_PICKFILE ( $
			/MUST_EXIST, FILTER = '*.bak', $ ;DIALOG_PARENT = self.widgetID.top, $
            PATH = !RM_PARAMS.lastPath, GET_PATH = lastPath, $
			TITLE = 'Select Background File for Dataset "' + root + '.pha"')
			
		foundBack = 1
		
    	IF (backFile[0] EQ '') THEN BEGIN
    		 foundBack = 0
    		 ;RETURN    
    	ENDIF
        ; User may have slipped up! Filename is only a path:
        IF ((STRPOS(backFile, '/', /REVERSE_SEARCH) + 1) EQ $
             STRLEN(backFile)) THEN BEGIN
    		 foundBack = 0
             ;RETURN
        ENDIF

        ; If the user accidently puts a space in front of the filename...
        retpos = STRPOS(backFile, STRING(10B))
        IF (retpos NE 0) THEN BEGIN
           backFile = STRMID(backFile, retpos + 1)
        ENDIF
        
        IF (foundBack) THEN self.backFile = backFile
        
	ENDIF
	
    RETURN, self.backFile

END


;------------------------------------------------------------------------------
; Get Unique Detector ID String
;------------------------------------------------------------------------------
FUNCTION PHAReader::getDetName

     RETURN, self.detName

END


; ----------------------------------------------------------------------------
; Object definition
; ----------------------------------------------------------------------------
PRO PHAReader__define

    obj = { PHAREADER, INHERITS DATAREADER, $

        detName   : '', $
        rmfFile   : '', $
        backFile  : '', $
        dataValid : 0   $
        
    }

END
