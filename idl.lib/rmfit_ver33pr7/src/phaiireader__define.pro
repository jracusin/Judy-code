; ----------------------------------------------------------------------------
;+
; NAME:
;
;     PHAIIReader (OBJECT)
;
; PURPOSE:
;
;     An object for reading PHAII data.
;
; CALLING SEQUENCE:
;
;     obj = OBJ_NEW ('PHAIIReader' [, filename])
;
; INPUTS:
;
;     filename (OPTIONAL) : STRING denoting the PHAII file to read.  If
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
;     !RM_PARAMS: defined in RMFIT__DEFINE.PRO
;
; PUBLIC METHODS:
;     
;     read (PROCEDURE) - Read a PHAII file
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
;         Inputs: extension - PHAII table extension (0, 1, or 2)
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
;     Created from PHAREADER, 2005 April 1, Rob.Preece@nnstc.nasa.gov
;     PHAREADER Written, 1999 August, Robert.Mallozzi@msfc.nasa.gov
;
;-
; ----------------------------------------------------------------------------



; ----------------------------------------------------------------------------
; Constructor
; ----------------------------------------------------------------------------
FUNCTION PHAIIReader::init, filename
       
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
PRO PHAIIReader::cleanup

    self->dataReader::cleanup
        
END


; ----------------------------------------------------------------------------
; Read PHAII data
; ----------------------------------------------------------------------------
PRO PHAIIReader::read, ERROR = error

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
    filetype = SXPAR (header2, 'EXTNAME')
    IF (STRUPCASE (filetype) NE 'SPECTRUM') THEN BEGIN
        ;== May be a switched file
        temp = table2
        table2 = table1
        table1 = temp
        
        htemp = header2
        header2 = header1
        header1 = htemp
    ENDIF

    IF ((SIZE(table2))[1] EQ 1) THEN BEGIN
       error = 1
       self->setfilename,''
       joke = DIALOG_MESSAGE(/ERROR, 'PHAII files with single spectra are not supported. Use the PHA Type 1 format.')
       RETURN
    ENDIF
    
    ;== Filter negative error rates 

    exposure = table2.exposure
;    idx = WHERE (deltat EQ 0.0, zeroCount) 
;    IF (zeroCount NE 0) THEN BEGIN 
;       MESSAGE, /CONTINUE, 'Zero exposure in FITS file set to 1.' 
;       deltat[idx] = 1.0 
;    ENDIF 

    ;TODO: unneccessary?

    ; See if there is an response file listed:
    self.rmfFile = ''
    foundRMF = 0
    respPos = STRPOS(header2, 'RESPFILE')
    jj = WHERE(respPos GE 0, cnt)
    IF (cnt NE 0) THEN BEGIN
        responseFile = SXPAR (header2, 'RESPFILE')
        IF (STRLOWCASE(STRTRIM(responseFile, 2)) NE 'none') THEN BEGIN
			c = FINDFILE (!rm_params.lastpath+responseFile, COUNT = cnt)
			foundRMF = 1
	    ENDIF
	ENDIF
	
;	IF (CNT EQ 0) THEN BEGIN
;		path = self->filename (/PATH)
;		root = self->filename (/ROOT)
;		
;		f = path + root + '.rmf'
;		responseFile = DIALOG_PICKFILE ( $
;			/MUST_EXIST, FILTER = ['*.rmf','*.rsp','*.RMF','*.RSP'], $ 
;            PATH = !RM_PARAMS.lastPath, GET_PATH = lastPath, $
;			TITLE = 'Select Response File for Dataset "' + root + '.fit"')
;			
;    	IF (responseFile[0] EQ '') THEN BEGIN
;    		 foundRMF = 0
;    		 ;RETURN    
;    	ENDIF
;        ; User may have slipped up! Filename is only a path:
;        IF ((STRPOS(responseFile, '/', /REVERSE_SEARCH) + 1) EQ $
;             STRLEN(responseFile)) THEN BEGIN
;    		 foundRMF = 0
;             ;RETURN
;        ENDIF
;
;        ; If the user accidently puts a space in front of the filename...
;        retpos = STRPOS(responseFile, STRING(10B))
;        IF (retpos NE 0) THEN BEGIN
;           responseFile = STRMID(responseFile, retpos + 1)
;        ENDIF
;    	
;    	!RM_PARAMS.lastPath = lastPath
;    	
;	ENDIF

    IF foundRMF THEN BEGIN
;        myResponse = OBJ_NEW('RMFResponse', responseFile[0])
        self.rmfFile = responseFile
;        OBJ_DESTROY, myResponse
    ENDIF ELSE BEGIN  ;== No response file
        self.rmfFile = ''
    ENDELSE

    numChan = SXPAR (header2, 'DETCHANS')

    thresholds       = FLTARR (2, numChan, /NOZERO)
    thresholds[0, *] = table1.e_min
    thresholds[1, *] = table1.e_max
    IF (thresholds[0, 0] LE 0.0) THEN thresholds[0, 0] = 0.1 * thresholds[1, 0] 

    tZero = 0  ;32768 ;SXPAR (header2, 'TZERO1')
    columns = SXPAR (header2, 'TTYPE*', COUNT = tcnt)
    
    tstart = SXPAR (header2, 'TRIGTIME')
    numTimes = N_ELEMENTS(table2.time)
    theTimes = DBLARR(2, numTimes)
    theTimes[0, *] = table2.time    ; + tstart
    mm = WHERE(STRTRIM(columns,2) EQ 'ENDTIME', ccnt)
    IF (ccnt GT 0) THEN BEGIN
		theTimes[1, *] = table2.endtime
    ENDIF ELSE BEGIN
    	nn = WHERE(STRTRIM(columns,2) EQ 'TIME_STOP', scnt)
		IF (scnt GT 0) THEN BEGIN
			theTimes[1, *] = table2.time_stop
		ENDIF ELSE BEGIN
			theTimes[1, 0: numTimes - 2] = theTimes[0, 1:*]
			theTimes[1, numTimes - 1] = theTimes[1, numTimes - 2] + 1.
		ENDELSE
    ENDELSE
     ; + tstart

	cunit = 'count'
	tunit = 's'
	useCounts = 0
	FOR mm = 0, tcnt - 1 DO BEGIN
		IF (STRTRIM(columns[mm], 2) EQ 'COUNTS') THEN BEGIN
			tZero = SXPAR (header2, 'TZERO' + STRTRIM(mm + 1, 2))
			cunit = STRTRIM(SXPAR (header2, 'TUNIT' + STRTRIM(mm + 1, 2)), 2)
			useCounts = 1
			IF cunit EQ '0' THEN cunit = 'count'
		ENDIF
		IF (STRTRIM(columns[mm], 2) EQ 'TIME') THEN BEGIN
			tunit = STRTRIM(SXPAR (header2, 'TUNIT' + STRTRIM(mm + 1, 2)), 2)
			timeNotRectified = theTimes[0,0] GT 1.0E7
			IF (timeNotRectified) THEN theTimes -= tstart
		ENDIF
	ENDFOR
	
	IF useCounts THEN BEGIN
		therates = float(table2.counts + tZero)
    ENDIF ELSE BEGIN
		;== 01/25/10 RDP: Not necessary; the rsp file is also scaled!
    	;npix = SXPAR (header2, 'NGOODPIX', COUNT = pcnt)
    	;IF (pcnt EQ 0) THEN npix = 1
		therates = table2.rate ;* npix
		theerrors = table2.stat_err ;* npix
    ENDELSE

    deltat = thetimes[1, *] - thetimes[0, *]
    nexp = where(deltat LE 0.016, co_nexp)
    ; We can have some zero-length accumulations after the trigger; eliminate them:
    IF co_nexp GT 0 THEN BEGIN
    	pexp = where(deltat GT 0.016, co_nexp)
    	exposure = exposure[pexp]
    	theTimes = theTimes[*, pexp]
    	therates = therates[*, pexp]
    ENDIF
    eexp = where(exposure LE 0.016, co_eexp)
    IF co_eexp GT 0 THEN BEGIN
    	pexp = where(exposure GT 0.016, co_nexp)
    	exposure = exposure[pexp]
    	theTimes = theTimes[*, pexp]
    	therates = therates[*, pexp]
    ENDIF
	IF useCounts THEN BEGIN
		theerrors = SQRT(therates)
		FOR jj = 0, numChan - 1 DO BEGIN
			therates[jj, *] = therates[jj, *] / exposure
			theerrors[jj, *] = theerrors[jj, *] / exposure
		ENDFOR
	ENDIF
	;== 03/25/2010 RDP: Make the test for POISERR = F:
	poiTest = SXPAR (header2, 'POISSERR', COUNT = pcnt)
	IF (pcnt NE 0) THEN BEGIN 
		IF (NOT poiTest) THEN self.poisson = 0
	ENDIF
    
    ;== Assign class member variables
    notes = self->readNotes (header0, header1, header2)

    PTR_FREE, self.header
    self.header = PTR_NEW ({  $

        ext0  : header0, $
        ext1  : header1, $
        ext2  : header2, $
        notes : notes    $

    })    

    PTR_FREE, self.data
    self.data = PTR_NEW ({ $
        
        times  : thetimes,  $
        rates  : therates,  $
        errors : theerrors, $
        liveti : exposure,  $
        eedges : thresholds     $

    })
        
    countsunit = cunit + ' / ' + tunit
;    countsunit = STRTRIM (SXPAR (header2, 'TUNIT1'), 2) + ' / ' + $
;                 STRTRIM (SXPAR (header2, 'TUNIT2'), 2)
    PTR_FREE, self.units
    self.units = PTR_NEW ({ $

;        times  : STRTRIM (SXPAR (header2, 'TUNIT5'), 2), $
        times  : tunit,                                  $
        rates  : countsunit,                             $
        errors : countsunit,                             $
        eedges : STRTRIM (SXPAR (header1, 'TUNIT2'), 2)  $

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
FUNCTION PHAIIReader::readNotes, h0, h1, h2

    ; COMPILE_OPT HIDDEN
        
    s = '  '
    notes = s
    
    kk = STRPOS(h0, 'OBJECT')
    found = WHERE(kk EQ 0, oco)
    IF (oco GT 0) THEN BEGIN
        s = 'Event: ' + STRTRIM (SXPAR (h0, 'OBJECT'), 2)
        notes = [notes, s]
    ENDIF
          
    s = 'Telescope: ' + STRTRIM (SXPAR (h0, 'TELESCOP'), 2)
    notes = [notes, s]
    
    instrument = STRTRIM (SXPAR (h0, 'INSTRUME'), 2)
    s = 'Instrument: ' + instrument
    notes = [notes, s]
    
    detName = STRTRIM (SXPAR (h0, 'DETNAM'), 2)
    IF (instrument EQ 'LAT') THEN BEGIN
    	tempF = STRUPCASE(STRTRIM (SXPAR (h0, 'FILENAME'), 2))
    	lll = STRPOS(tempF, 'FRONT')
    	IF (lll GT 0) THEN detName = 'FRONT' ELSE detName = 'BACK' 
    ENDIF
    s = 'Detector: ' + detName
    notes = [notes, s]
    self.detName = detName

    kk = STRPOS(h0, 'DATATYPE')
    found = WHERE(kk EQ 0, fco)
    IF (fco GT 0) THEN BEGIN
        s = 'Data types: ' + STRTRIM (SXPAR (h0, 'DATATYPE'), 2)
    ENDIF ELSE BEGIN
        s = 'Data types: ' + 'PHAII'
    ENDELSE
    notes = [notes, s]
    
    tstart = 0
    tstart = SXPAR (h0, 'TSTART')
    IF (tstart NE 0) THEN BEGIN
        s = 'TSTART: ' + STRTRIM (STRING (tstart), 2)
		notes = [notes, s]
    ENDIF

    RETURN, notes[1:*]

END


;------------------------------------------------------------------------------
; Display a PHAII header
;------------------------------------------------------------------------------
PRO PHAIIReader::showHeader, extension, _EXTRA = extra

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
PRO PHAIIReader::showNotes, _EXTRA = extra

    IF (NOT self.dataValid) THEN $
       RETURN

    filename = self->filename (/BASENAME)

    w = DIALOG_TEXT ((*self.header).notes, $
        TITLE = filename, /SAVE_OPTION, _EXTRA = extra)

END


;------------------------------------------------------------------------------
; Set RMF File Name for the PHA file
;------------------------------------------------------------------------------
PRO PHAIIReader::setRMFFile, newRMFFile

    self.RMFFile = newRMFFile

END


;------------------------------------------------------------------------------
; Get RMF File Name from the PHA file
;------------------------------------------------------------------------------
FUNCTION PHAIIReader::getRMFFile

    RETURN, self.RMFFile

END

;------------------------------------------------------------------------------
; Get Unique Detector ID String
;------------------------------------------------------------------------------
FUNCTION PHAIIReader::getDetName

     RETURN, self.detName

END


; ----------------------------------------------------------------------------
; Object definition
; ----------------------------------------------------------------------------
PRO PHAIIReader__define

    obj = { PHAIIREADER, INHERITS DATAREADER, $

        detName   : '', $
        rmfFile   : '', $
        dataValid : 0   $
        
    }

END
