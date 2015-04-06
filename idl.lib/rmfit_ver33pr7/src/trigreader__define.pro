; ----------------------------------------------------------------------------
;+
; NAME:
;
;     TRIGReader (OBJECT)
;
; PURPOSE:
;
;     An object for reading PHAII data.
;
; CALLING SEQUENCE:
;
;     obj = OBJ_NEW ('TRIGReader' [, filename])
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
;     Created from PHAREADER, 2008 April 8, Rob.Preece@nasa.gov
;     PHAREADER Written, 1999 August, Robert.Mallozzi@msfc.nasa.gov
;
;-
; ----------------------------------------------------------------------------



; ----------------------------------------------------------------------------
; Constructor
; ----------------------------------------------------------------------------
FUNCTION TRIGReader::init, filename
       
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
PRO TRIGReader::cleanup

    PTR_FREE, self.quaternion
    PTR_FREE, self.earthPos
    PTR_FREE, self.locations
    
    self->dataReader::cleanup
        
END


; ----------------------------------------------------------------------------
; Read PHAII data
; ----------------------------------------------------------------------------
PRO TRIGReader::read, newDetString = newDetString, FINETIME = fine, ERROR = error

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
    table2 = MRDFITS (filename, 5, header2, /SILENT)
    ;== Do this only once per session:
    IF (NOT PTR_VALID(self.locations)) THEN BEGIN
		table3 = MRDFITS (filename, 3, header3, /SILENT)
		PTR_FREE, self.locations
    	self.locations = PTR_NEW(table3)
    ENDIF
    
    ;== Verify FITS filetype
    filetype = SXPAR (header2, 'EXTNAME')
    
	notes = self->readNotes (header0, header1, header2)
	
	IF KEYWORD_SET(newDetString) THEN BEGIN
		self.detname = newDetString
		;print, 'Changing det lookup!'
	ENDIF

	numChan = 8
    
    tstart = SXPAR (header0, 'TRIGTIME')
    myStarts = table2.time    - tstart
    myStops  = table2.endtime - tstart
    
    ;== Find the Earth Position and Quaternion Info closest to the trigger:
    myZero = MIN(myStops, minIdx, /ABSOLUTE)

    myDelta  = myStops - myStarts
    IF KEYWORD_SET(FINE) THEN BEGIN
;		OKidx = WHERE(myDelta LT 0.10 AND myStops LT 0.51)
		temp1 = WHERE(myDelta LT 0.10)
		temp2 = WHERE(myDelta GT 0.10 AND myDelta LT 1.0)
		midTi = (myStarts[temp1] + myStops[temp1])/2.
		OKidx = 0
		FOR hh = 0, N_ELEMENTS(temp2) - 1 DO BEGIN
			trialIdx = WHERE(midTi GT mystarts[temp2[hh]] AND midTi LT mystops[temp2[hh]], tryCo)
			IF (tryCo EQ 4) THEN BEGIN
				OKidx = [OKidx, temp1[trialIdx]]
			ENDIF ELSE BEGIN
				OKidx = [OKidx, temp2[hh]]
			ENDELSE			
		END
		OKidx = OKidx[1: *]
		temp3 = WHERE(myDelta GT 1.0 AND myDelta LT 2.0)
		midT3 = (myStarts[temp2] + myStops[temp2])/2.
		FOR hh = 0, N_ELEMENTS(temp3) - 1 DO BEGIN
			trialIdx = WHERE(midT3 GT mystarts[temp3[hh]] AND midT3 LT mystops[temp3[hh]], tryCo3)
			IF (tryCo3 GT 0) THEN BEGIN
				;OKidx = [OKidx, temp2[trialIdx]]
				IF (tryCo3 NE 4) THEN BEGIN 
					OKidx = OKidx[tryCo3:*]
					OKidx = [OKidx, temp3[hh]]
				ENDIF
				BREAK
			ENDIF ELSE BEGIN
				OKidx = [OKidx, temp3[hh]]
			ENDELSE			
		END
		
		FOR hh = N_ELEMENTS(temp3) - 1, 0, -1 DO BEGIN
			trialIdx = WHERE(midT3 GT mystarts[temp3[hh]] AND midT3 LT mystops[temp3[hh]], tryCo3)
			IF (tryCo3 GT 0) THEN BEGIN
				;OKidx = [OKidx, temp2[trialIdx]]
				IF (tryCo3 NE 4) THEN BEGIN 
					FOR ii = 0, tryCo3 - 1 DO BEGIN
						temp4 = WHERE(OKIdx EQ temp2[trialIdx[ii]], nFound, $
						              COMPLEMENT = compIdx, NCOMPLEMENT = Ncomp)
						IF (nFound GT 0) THEN OKidx = OKidx[compIdx]
					END
					OKidx = [OKidx, temp3[hh]]
				ENDIF
				BREAK
			ENDIF ELSE BEGIN
				OKidx = [temp3[hh], OKidx]
			ENDELSE			
		END
;		OKidx = [OKidx, WHERE((myStops LT -0.20 OR myStarts GE 0.40) AND $
;		                myDelta GT 0.10 AND myDelta LT 1.0)]
		;OKidx = [OKidx, WHERE((myStops LT -1.0 OR myStarts GT 2.0) AND $
		;                myDelta GT 1.0 AND myDelta LT 2.0)]  ;
		min_one_sec = myStarts[MIN(OKidx)] + 0.1
		max_one_sec = myStops[MAX(OKidx)]  - 0.1
		OKidx = [OKidx, WHERE(myStops LE min_one_sec OR myStarts GE max_one_sec)]
		;OKidx = [OKidx, WHERE(myStops LT -8.0 OR myStarts GT 62.0)]
		
    ENDIF ELSE BEGIN
		;OKidx = WHERE(myStops LE -8.0 OR myStarts GE 62.0)
		OKidx = WHERE(myDelta GT 1.0 AND myDelta LT 2.0)  ;OKidx, 
		min_one_sec = myStarts[MIN(OKidx)]
		max_one_sec = myStops[MAX(OKidx)]
		OKidx = [OKidx, WHERE(myStops LE min_one_sec OR myStarts GE max_one_sec)]
    ENDELSE
	OKidx = OKidx[SORT(myStarts[OKidx])]
    myNumTimes = N_ELEMENTS(OKidx)
    theTimes = DBLARR(2, myNumTimes)
    theTimes[0, *] = myStarts[OKidx]
    theTimes[1, *] = myStops[OKidx]
    tempRates = (table2.rate)[*, *, OKidx]

    PTR_FREE, self.quaternion
    PTR_FREE, self.earthPos
    self.quaternion = PTR_NEW ((table2.scattitd)[*, OKidx])  ; 
    self.earthPos = PTR_NEW ((table2.eic)[*, OKidx])         ; 
    tempRates = REFORM(tempRates, 8, 14, myNumTimes, /OVERWRITE)
    detArr = BYTE(self.detname)
    useDets = where(detArr EQ 49, useCnt)
    IF (useCnt EQ 0) THEN BEGIN
        useDets = INDGEN(14)
        myDets = INTARR(14)
        myDets[useDets] = 1
        myNewDetName = STRING(myDets, FORMAT = '(14i1)')
        self.detname = myNewDetName 
		myDets = ['0','1','2','3','4','5','6','7','8','9','a','b','B0','B1']
		blanks = REPLICATE('-',14)
		detPos = STRSPLIT(myNewDetName, '1') - 1
		detPos = detPos[WHERE(detPos > 0)]
		blanks[detPos] = myDets[detPos]
		displayName = STRJOIN(blanks)
        notes[3] = 'Detector: ' + displayName
    ENDIF
    theRates = TOTAL(tempRates[*, useDets, *], 2)

	;== Determine the energy scale to use:    
    thresholds       = FLTARR (2, numChan, /NOZERO)
    thresholds[0, *] = [3.4, 10.0, 22.0, 44.0, 95.0, 300.0, 500.0, 800.0]
    thresholds[1, *] = [10.0, 22.0, 44.0, 95.0, 300.0, 500.0, 800.0, 2000.]
    IF MIN(useDets) GT 11 THEN BEGIN
		thresholds[0, *] = [150., 400.0, 850.0, 1500.0, 3000.0, 5500.0, 10000.0, 20000.0]
		thresholds[1, *] = [400.0, 850.0, 1500.0, 3000.0, 5500.0, 10000., 20000., 50000.]
    ENDIF

    columns = SXPAR (header2, 'TTYPE*', COUNT = tcnt)

	cunit = 'count'
	tunit = 's'
	FOR mm = 0, tcnt - 1 DO BEGIN
		IF (STRTRIM(columns[mm], 2) EQ 'TIME') THEN BEGIN
			tunit = STRTRIM(SXPAR (header2, 'TUNIT' + STRTRIM(mm + 1, 2)), 2)
		ENDIF
	ENDFOR
    
    theerrors = therates
    exposure = myDelta[OKidx]

    FOR jj = 0, numChan - 1 DO BEGIN
        tempRates = SQRT(therates[jj, *] * exposure)
        theerrors[jj, *] = tempRates / exposure
    ENDFOR

    ;== Assign class member variables

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
        eedges : 'keV'                                   $

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
FUNCTION TRIGReader::readNotes, h0, h1, h2

    ; COMPILE_OPT HIDDEN
        
    s = '  '
    notes = s
    
    kk = STRPOS(h0, 'OBJECT')
    found = WHERE(kk EQ 0, oco)
    IF (oco GT 0) THEN BEGIN
        myObject = STRTRIM (SXPAR (h0, 'OBJECT'), 2)
        s = 'Event: ' + myObject
        notes = [notes, s]
        self.Object = myObject
    ENDIF
          
    s = 'Telescope: ' + STRTRIM (SXPAR (h0, 'TELESCOP'), 2)
    notes = [notes, s]
    
    s = STRTRIM (SXPAR (h0, 'INSTRUME'), 2)
    s = 'Instrument: ' + s
    notes = [notes, s]
    
    detName = STRTRIM (SXPAR (h0, 'DET_MASK'), 2) ;+ '00'

	myDets = ['0','1','2','3','4','5','6','7','8','9','a','b','B0','B1']
	blanks = REPLICATE('-',14)
	myDetArray = BYTE(detname)
	IF (N_ELEMENTS(myDetArray) EQ 12) THEN BEGIN
		detName = detName + '00'
		myDetArray = BYTE(detname)
	ENDIF

	currChoice = WHERE(myDetArray EQ 49)
	; Handle the BGO triggers until the TRIGDAT_Reader is released:
	IF (currChoice[0] EQ -1) THEN currChoice = [12, 13]
	blanks[currChoice] = myDets[currChoice]
	displayName = STRJOIN(blanks)
    
    s = 'Detector: ' + displayName
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
    tstart = SXPAR (h0, 'TRIGTIME')
    IF (tstart NE 0) THEN BEGIN
        s = 'TRIGTIME: ' + STRTRIM (STRING (LONG(tstart)), 2)
		notes = [notes, s]
    ENDIF
    
    myRA = STRTRIM (STRING (SXPAR (h0, 'RA_OBJ')), 2)
    s = 'Source RA: ' + myRA
    notes = [notes, s]
    self.RA = myRA
    
    myDec = STRTRIM (STRING (SXPAR (h0, 'DEC_OBJ')), 2)
    s = 'Source Dec: ' + myDec
    notes = [notes, s]
    self.Dec = myDec
    
    myErr = STRTRIM (STRING (SXPAR (h0, 'ERR_RAD')), 2)
    s = 'Loc Error: ' + myErr
    notes = [notes, s]
    self.Err = myErr

    RETURN, notes[1:*]

END


;------------------------------------------------------------------------------
; Display a PHAII header
;------------------------------------------------------------------------------
PRO TRIGReader::showHeader, extension, _EXTRA = extra

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
PRO TRIGReader::showNotes, _EXTRA = extra

    IF (NOT self.dataValid) THEN $
       RETURN

    filename = self->filename (/BASENAME)

    w = DIALOG_TEXT ((*self.header).notes, $
        TITLE = filename, /SAVE_OPTION, _EXTRA = extra)

END


;------------------------------------------------------------------------------
; Get Unique Detector ID String
;------------------------------------------------------------------------------
FUNCTION TRIGReader::getDetName
     RETURN, self.detName
END

;------------------------------------------------------------------------------
; Get Source Right Ascension Value
;------------------------------------------------------------------------------
FUNCTION TRIGReader::getSourceRA
     RETURN, self.RA
END

;------------------------------------------------------------------------------
; Get Source Declination Value
;------------------------------------------------------------------------------
FUNCTION TRIGReader::getSourceDec
     RETURN, self.Dec
END
;------------------------------------------------------------------------------
; Get the official Oject name
;------------------------------------------------------------------------------
FUNCTION TRIGReader::getObject
     RETURN, self.Object
END

;------------------------------------------------------------------------------
; Get FSW localizations
;------------------------------------------------------------------------------
FUNCTION TRIGReader::getLocations
	RETURN, self.locations
END

;------------------------------------------------------------------------------
; Get Spacecraft attitude
;------------------------------------------------------------------------------
FUNCTION TRIGReader::getQuaternion, atTime
	theTimes = (*self.data).times
	myStops = theTimes[1, *] - atTime
	myZero = MIN(myStops, minIdx, /ABSOLUTE)
	myQuats = (*self.quaternion)[*, minIdx]
	RETURN, myQuats
END

;------------------------------------------------------------------------------
; Get Earth position:
;------------------------------------------------------------------------------
FUNCTION TRIGReader::getEarthPos, atTime
	theTimes = (*self.data).times
	myStops = theTimes[1, *] - atTime
	myZero = MIN(myStops, minIdx, /ABSOLUTE)
	myEarthPos = (*self.earthPos)[*, minIdx]
	RETURN, myEarthPos 
END

;------------------------------------------------------------------------------
; Set Unique Detector ID String
;------------------------------------------------------------------------------
PRO TRIGReader::setDetName, newDetString
     self.detName = newDetString
END

;------------------------------------------------------------------------------
; Set Source Right Ascension Value
;------------------------------------------------------------------------------
PRO TRIGReader::setSourceRA, newRA
     self.RA = newRA
END

;------------------------------------------------------------------------------
; Set Source Declination Value
;------------------------------------------------------------------------------
PRO TRIGReader::setSourceDec, newDec
     self.Dec = newDec
END

; ----------------------------------------------------------------------------
; Object definition
; ----------------------------------------------------------------------------
PRO TRIGReader__define

    obj = { TRIGReader, INHERITS DATAREADER, $

        detName   : '',         $
        RA        : '',         $
        Dec       : '',         $
        Err       : '',         $
        Object    : '',         $
        locations : PTR_NEW (), $   ;FLTARR(4)
        quaternion: PTR_NEW (), $   ;FLTARR(4)
        earthPos  : PTR_NEW (), $   ;FLTARR(3)
        dataValid : 0    $
        
    }

END
