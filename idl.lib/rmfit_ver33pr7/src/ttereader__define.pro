; ----------------------------------------------------------------------------
;+
; NAME:
;
;     TTEReader (OBJECT)
;
; PURPOSE:
;
;     An object for reading PHAII data.
;
; CALLING SEQUENCE:
;
;     obj = OBJ_NEW ('TTEReader' [, filename])
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
FUNCTION TTEReader::init, filename
       
    IF (NOT self->dataReader::init (filename)) THEN $
       RETURN, 0

    IF (N_ELEMENTS (file) NE 0) THEN BEGIN

       self->read, ERROR = error

       IF (error EQ 1) THEN BEGIN
       
          MESSAGE, /CONTINUE, 'Failed to read file: ' + STRING (filename)
          RETURN, 0
       
       ENDIF

    ENDIF
    
    ;== Start out clean: no mods to the time lookup:
    self.dataDirty = 0
     
    RETURN, 1

END


; ----------------------------------------------------------------------------
; Destructor
; ----------------------------------------------------------------------------
PRO TTEReader::cleanup

	;== We are going to write out the time bins for future reference:
	IF (self.dataDirty EQ 1) THEN BEGIN
		self->writeTiFile
;		IF (PTR_VALID(self.data)) THEN BEGIN 
; 
;			TimeBins = (*self.data).times 
; 
;			path = self->filename (/PATH) 
;			root = self->filename (/ROOT) 
;			f = path + root + '.ti' 
; 
;			filename = f 
;			 
;			OPENW, FL, filename, ERROR = status, /GET_LUN 
;			IF (status EQ 0) THEN BEGIN 
;				NumTimes = N_ELEMENTS(TimeBins[0, *]) + 1 
;			   PRINTF, FL, NumTimes 
;			   PRINTF, FL, TimeBins[0, *], TimeBins[1, NumTimes - 2] 
;			   FREE_LUN, FL 
;			 
;			ENDIF 
;           
;		ENDIF 
	ENDIF 

	PTR_FREE, self.myTimes
	PTR_FREE, self.myChans

    self->dataReader::cleanup
        
END


; ----------------------------------------------------------------------------
; Rebin EVENT data
; ----------------------------------------------------------------------------
PRO TTEReader::writeTiFile, LOOKUPFILENAME = lookupFileName

		IF (PTR_VALID(self.data)) THEN BEGIN

			TimeBins = (*self.data).times

			IF KEYWORD_SET(LOOKUPFILENAME) THEN BEGIN
				dotpos = STRPOS(lookupFileName, '.', /REVERSE_SEARCH)
				IF (dotpos NE 0) THEN BEGIN
					f = STRMID(lookupFileName, 0, dotpos) + '.ti'
				ENDIF ELSE RETURN
			ENDIF ELSE BEGIN
				path = self->filename (/PATH)
				root = self->filename (/ROOT)
				f = path + root + '.ti'
			ENDELSE

			filename = f
			
			OPENW, FL, filename, ERROR = status, /GET_LUN
			IF (status EQ 0) THEN BEGIN
				NumTimes = N_ELEMENTS(TimeBins[0, *]) + 1L
			   PRINTF, FL, NumTimes
			   PRINTF, FL, TimeBins[0, *], TimeBins[1, NumTimes - 2]
               CLOSE, fl
			   FREE_LUN, FL
			
			ENDIF
          
		ENDIF
        
END


; ----------------------------------------------------------------------------
; Rebin EVENT data
; ----------------------------------------------------------------------------
PRO TTEReader::accumulator, intervals, thresh, backErr, targetSNR, bins
	;== Cache the data we don't want to change:
	IF (PTR_VALID(self.data)) THEN BEGIN
		oldTimes = (*self.data).times
		oldRates = (*self.data).rates
		oldErrors = (*self.data).errors
		oldLiveti = (*self.data).liveti
		thresholds = (*self.data).eedges
	ENDIF ELSE RETURN
	
	numChan = (SIZE(thresholds))[2]

    mySpectrum = TRANSPOSE(lonarr(numChan))
    theRates = TRANSPOSE(findgen(numChan))
    start_time = intervals[0]
    curr_time = start_time
    myNumTimes = N_ELEMENTS(intervals) - 1
    end_time = intervals[myNumTimes]
    timeInterval = WHERE(*self.myTimes GE start_time AND *self.myTimes LT end_time)
    theEvents = (*self.myTimes)[timeInterval]
    theChans = (*self.myChans)[timeInterval]
    myNumEVT = N_ELEMENTS(theEvents)
    
	myTimes = [start_time]
    evtDead = self.evt_dead

	accum = 0L
	ti_idx = 0L
	accum_prev = start_time
	exposure = [0.0d]
	
	WHILE (1) DO BEGIN
;print,"  "
;print,accum_cur,accum_end
		noise = 0.0
		signal = 0.0
		REPEAT BEGIN 
			IF (ti_idx GE myNumEVT - 1) THEN BREAK
			mySpectrum[0, theChans[ti_idx]]++ 
;print,theChans[ti_idx],theEvents[ti_idx]
			accum_cur = theEvents[++ti_idx]
			deltat = accum_cur - accum_prev
			accum++
			signal    = accum / deltat - thresh
			;sigmaBack = backErrs[theIdx] ;/ edges[theIdx]
			sigmaObs  = SQRT(accum) / deltat
			noise     = SQRT(sigmaObs * sigmaObs + backErr * backErr)
			
		ENDREP UNTIL ((signal / noise) GE targetSNR)
;print,accum
		theRates = [theRates, float(mySpectrum)]
		deadtime = TOTAL(mySpectrum) * evtDead
		exposure = [exposure, accum_cur - accum_prev - deadtime]
		myTimes = [myTimes, accum_cur]
		mySpectrum[0, *] = 0
		accum = 0L
		accum_prev = accum_cur
		IF (ti_idx GE myNumEVT - 1) THEN BREAK
	ENDWHILE

	exposure = exposure[1:*]
	theRates = theRates[1:*,*]
	theRates = TRANSPOSE(theRates)
    theerrors = SQRT(therates)
    tiTop = N_ELEMENTS(myTimes) - 1
    newTimes = DBLARR(2, tiTop)
    newTimes[0, *] = myTimes[0: tiTop - 1]
    newTimes[1, *] = myTimes[1: *]
    
    FOR jj = 0, numChan - 1 DO BEGIN
        therates[jj, *] = therates[jj, *] / exposure
        theerrors[jj, *] = theerrors[jj, *] / exposure
    ENDFOR
    
    tempTimes = TRANSPOSE(newTimes)
    tempRates = TRANSPOSE(theRates)
    tempErrors = TRANSPOSE(theerrors)
    tempLiveti = TRANSPOSE(exposure)
    
    IF (bins[0] GT 1) THEN BEGIN
		tempTimes = [TRANSPOSE(oldTimes[*, 0: bins[0] - 1]), tempTimes]
		tempRates = [TRANSPOSE(oldRates[*, 0: bins[0] - 1]), tempRates]
		tempErrors = [TRANSPOSE(oldErrors[*, 0: bins[0] - 1]), tempErrors]
		tempLiveti = [oldLiveti[0: bins[0] - 1], tempLiveti]
    ENDIF
    
    IF (bins[1] LT (SIZE(oldTimes))[2] - 1) THEN BEGIN
		tempTimes = [tempTimes, TRANSPOSE(oldTimes[*,bins[1] + 1: *])]
		tempRates = [tempRates, TRANSPOSE(oldRates[*,bins[1] + 1: *])]
		tempErrors = [tempErrors, TRANSPOSE(oldErrors[*,bins[1] + 1: *])]
		tempLiveti = [tempLiveti, oldLiveti[bins[1] + 1: *]]
    ENDIF
    
    oldTimes = TRANSPOSE(tempTimes)
    oldRates = TRANSPOSE(tempRates)
    oldErrors = TRANSPOSE(tempErrors)
	
    PTR_FREE, self.data
    self.data = PTR_NEW ({ $
        
        times  : oldTimes,  $
        rates  : oldRates,  $
        errors : oldErrors, $
        liveti : tempLiveti,$
        eedges : thresholds $

    })
        
	;== We've rebinned the TTE data from the default; lookup table is dirty...
    self.dataDirty = 1
        
END


; ----------------------------------------------------------------------------
; Rebin EVENT data
; ----------------------------------------------------------------------------
PRO TTEReader::rebinner, intervals, bins

	;== Cache the data we don't want to change:
	IF (PTR_VALID(self.data)) THEN BEGIN
		oldTimes = (*self.data).times
		oldRates = (*self.data).rates
		oldErrors = (*self.data).errors
		oldLiveti = (*self.data).liveti
		thresholds = (*self.data).eedges
	ENDIF ELSE RETURN
	
	numChan = (SIZE(thresholds))[2]

    mySpectrum = TRANSPOSE(lonarr(numChan))
    theRates = TRANSPOSE(findgen(numChan))
    start_time = intervals[0]
    curr_time = start_time
    myNumTimes = N_ELEMENTS(intervals) - 1
    end_time = intervals[myNumTimes]
    timeInterval = WHERE(*self.myTimes GE start_time AND *self.myTimes LT end_time)
    theEvents = (*self.myTimes)[timeInterval]
    theChans = (*self.myChans)[timeInterval]
    myNumEVT = N_ELEMENTS(theEvents)
    
    myTimes = DBLARR(2, myNumTimes)
	myTimes[0, *] = intervals[0: myNumTimes - 1]
	myTimes[1, *] = intervals[1: *]
    deltat = myTimes[1, *] - myTimes[0, *]
    exposure = deltat
    evtDead = self.evt_dead

	ti_idx = 0L
	FOR accum = 0, myNumTimes - 1 DO BEGIN
		accum_end = myTimes[1, accum]
		accum_cur = myTimes[0, accum]
;print,"  "
;print,accum_cur,accum_end
		REPEAT BEGIN 
			;== 08/18/09 RDP: Fixed up the logic, thanks to an observation by Michael Briggs!
			;== If the the interval doesn't have a count, plese don't put one there!
			;== NB: Any change here *must* be also changed in "read" as well!
			IF (theEvents[ti_idx] GT accum_end) THEN BREAK
			mySpectrum[0, theChans[ti_idx]]++ 
			IF (ti_idx EQ myNumEVT - 1) THEN BREAK
;print,theChans[ti_idx],theEvents[ti_idx]
			accum_cur = theEvents[++ti_idx]
		ENDREP UNTIL (accum_cur GE accum_end)
		theRates = [theRates, float(mySpectrum)]
		deadtime = TOTAL(mySpectrum) * evtDead
		exposure[accum] = deltat[accum] - deadtime
		mySpectrum[0, *] = 0
	ENDFOR
	
	theRates = theRates[1:*,*]
	theRates = TRANSPOSE(theRates)
    theerrors = SQRT(therates)
    FOR jj = 0, numChan - 1 DO BEGIN
        therates[jj, *] = therates[jj, *] / exposure
        theerrors[jj, *] = theerrors[jj, *] / exposure
    ENDFOR
    
    tempTimes = TRANSPOSE(myTimes)
    tempRates = TRANSPOSE(theRates)
    tempErrors = TRANSPOSE(theerrors)
    tempLiveti = TRANSPOSE(exposure)
    
    IF (bins[0] GT 1) THEN BEGIN
		tempTimes = [TRANSPOSE(oldTimes[*, 0: bins[0] - 1]), tempTimes]
		tempRates = [TRANSPOSE(oldRates[*, 0: bins[0] - 1]), tempRates]
		tempErrors = [TRANSPOSE(oldErrors[*, 0: bins[0] - 1]), tempErrors]
		tempLiveti = [oldLiveti[0: bins[0] - 1], tempLiveti]
    ENDIF
    
    IF (bins[1] LT (SIZE(oldTimes))[2] - 1) THEN BEGIN
		tempTimes = [tempTimes, TRANSPOSE(oldTimes[*,bins[1] + 1: *])]
		tempRates = [tempRates, TRANSPOSE(oldRates[*,bins[1] + 1: *])]
		tempErrors = [tempErrors, TRANSPOSE(oldErrors[*,bins[1] + 1: *])]
		tempLiveti = [tempLiveti, oldLiveti[bins[1] + 1: *]]
    ENDIF
    
    oldTimes = TRANSPOSE(tempTimes)
    oldRates = TRANSPOSE(tempRates)
    oldErrors = TRANSPOSE(tempErrors)
	
    PTR_FREE, self.data
    self.data = PTR_NEW ({ $
        
        times  : oldTimes,  $
        rates  : oldRates,  $
        errors : oldErrors, $
        liveti : tempLiveti, $
        eedges : thresholds $

    })
        
	;== We've rebinned the TTE data from the default; lookup table is dirty...
    self.dataDirty = 1

END


; ----------------------------------------------------------------------------
; Read EVENT data
; ----------------------------------------------------------------------------
PRO TTEReader::read, ACCUM = accum, TIME_FILE = time_file, NOTIMEFILE = notimefile, ERROR = error

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
    table3 = MRDFITS (filename, 3, header3, /SILENT)
    
    ;== Verify FITS filetype
    filetype = SXPAR (header2, 'EXTNAME')
    
    ; See if there is an response file listed:
    self.rmfFile = ''
    foundRMF = 0
    respPos = STRPOS(header2, 'RESPFILE')
    jj = WHERE(respPos GE 0, cnt)
    IF (cnt NE 0) THEN BEGIN
        responseFile = SXPAR (header2, 'RESPFILE')
        IF (STRLOWCASE(STRTRIM(responseFile, 2)) NE 'none') THEN BEGIN
			c = FINDFILE (!rm_params.lastpath+responseFile, COUNT = respcnt)
			IF (respcnt GT 0) THEN BEGIN
				foundRMF = 1
			ENDIF
	    ENDIF
	ENDIF
	
    IF foundRMF THEN BEGIN
;        myResponse = OBJ_NEW('RMFResponse', responseFile[0])
        self.rmfFile = responseFile
;        OBJ_DESTROY, myResponse
    ENDIF ELSE BEGIN  ;== No response file
        self.rmfFile = ''
    ENDELSE
    
    IF KEYWORD_SET(ACCUM) THEN BEGIN
    	self.accum = accum
		self.dataDirty = 1
    ENDIF ELSE BEGIN
    	self.accum = 0.064d
	ENDELSE
	
    numChan = SXPAR (header2, 'DETCHANS')
	evtDead = SXPAR (header2, 'EVT_DEAD')
	self.evt_dead = evtDead

    thresholds       = FLTARR (2, numChan, /NOZERO)
    thresholds[0, *] = table1.e_min
    thresholds[1, *] = table1.e_max
    
    mySpectrum = TRANSPOSE(lonarr(numChan))
    theRates = TRANSPOSE(findgen(numChan))
	PTR_FREE, self.myTimes
	PTR_FREE, self.myChans
    self.myTimes = PTR_NEW(table2.time)
    self.myChans = PTR_NEW(table2.pha)
    start_time = (*self.myTimes)[0]
    curr_time = start_time
    myNumEVT = N_ELEMENTS(*self.myTimes)
    end_time = (*self.myTimes)[myNumEVT - 1]
    
	path = self->filename (/PATH)
	root = self->filename (/ROOT)
	IF KEYWORD_SET(TIME_FILE) THEN ti_filename = time_file $
							ELSE ti_filename = path + root + '.ti'
    c = FINDFILE (ti_filename, COUNT = cnt)
			
    ; No lookup file; use defualt binning:
    IF (cnt EQ 0) OR (KEYWORD_SET(ACCUM) OR KEYWORD_SET(NOTIMEFILE)) THEN BEGIN
		myAccum = self.accum
		myNumTimes = FLOOR ((end_time - start_time) / myAccum)
		theTimes = DBLARR(2, myNumTimes)
		FOR ti = 0L, myNumTimes - 1 DO theTimes[0, ti] = start_time + ti * myAccum

		;== Get the time rectified to the trigger time (= 0.0):
		trigOffset = MIN(ABS(theTimes[0,*]),tind)
		theTimes -= theTimes[0, tind]
		theTimes[1, 0: myNumTimes - 2] = theTimes[0, 1:*]
		theTimes[1, myNumTimes - 1] = end_time
    ; Read in lookup from file for arbitrary binning:
    ENDIF ELSE BEGIN
 		OPENR, FL, ti_filename, ERROR = status, /GET_LUN
     	readTimes = 0L
		READF, FL, readTimes
		temp_ti = DBLARR(readTimes)
		myNumTimes = readTimes - 1L
     	READF, FL, temp_ti
		;== CCR #255: 03/29/2010 RDP: We need to close the file!
		CLOSE, fl
		FREE_LUN, FL
		theTimes = DBLARR(2, myNumTimes)
		theTimes[0, *] = temp_ti[0: myNumTimes - 1L]
		theTimes[1, *] = temp_ti[1: myNumTimes]
    ENDELSE
    
    deltat = theTimes[1, *] - theTimes[0, *]
    exposure = TRANSPOSE(deltat)
	
	ti_idx = 0L
	FOR accum = 0L, myNumTimes - 1 DO BEGIN
		accum_end = theTimes[1, accum]
		accum_cur = theTimes[0, accum]
		REPEAT BEGIN 
			;== 08/18/09 RDP: Fixed up the logic, thanks to an observation by Michael Briggs!
			;== If the the interval doesn't have a count, please don't put one there!
			;== NB: Any change here *must* be also changed in "rebinner" as well!
			IF ((*self.myTimes)[ti_idx] GT accum_end) THEN BREAK
			mySpectrum[0, (*self.myChans)[ti_idx]]++ 
			IF (ti_idx EQ myNumEVT - 1) THEN BREAK
			accum_cur = (*self.myTimes)[++ti_idx]
		ENDREP UNTIL (accum_cur GE accum_end)
		theRates = [theRates, float(mySpectrum)]
		deadtime = TOTAL(mySpectrum) * evtDead
		exposure[accum] = deltat[accum] - deadtime
		mySpectrum[0, *] = 0
	ENDFOR

	theRates = theRates[1:*,*]
	theRates = TRANSPOSE(theRates)
    tstart = SXPAR (header0, 'TRIGTIME')
    columns = SXPAR (header2, 'TTYPE*', COUNT = tcnt)

	cunit = 'count'
	tunit = 's'
	FOR mm = 0, tcnt - 1 DO BEGIN
		IF (STRTRIM(columns[mm], 2) EQ 'TIME') THEN BEGIN
			tunit = STRTRIM(SXPAR (header2, 'TUNIT' + STRTRIM(mm + 1, 2)), 2)
		ENDIF
	ENDFOR
    
    nexp = where(deltat LE 1.0E-6, co_nexp)
    ; We can have some zero-length accumulations after the trigger; eliminate them:
    IF co_nexp GT 0 THEN BEGIN
    	pexp = where(deltat GT 0.00001, co_nexp)
    	exposure = exposure[pexp]
    	theTimes = theTimes[*, pexp]
    	therates = therates[*, pexp]
    ENDIF
    theerrors = SQRT(therates)
    FOR jj = 0, numChan - 1 DO BEGIN
        therates[jj, *] = therates[jj, *] / exposure
        theerrors[jj, *] = theerrors[jj, *] / exposure
    ENDFOR

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
FUNCTION TTEReader::readNotes, h0, h1, h2

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
    
    s = STRTRIM (SXPAR (h0, 'INSTRUME'), 2)
    s = 'Instrument: ' + s
    notes = [notes, s]
    
    detName = STRTRIM (SXPAR (h0, 'DETNAM'), 2)
    s = 'Detector: ' + detName
    notes = [notes, s]
    self.detName = detName

    kk = STRPOS(h0, 'DATATYPE')
    found = WHERE(kk EQ 0, fco)
    IF (fco GT 0) THEN BEGIN
        s = 'Data types: ' + STRTRIM (SXPAR (h0, 'DATATYPE'), 2)
    ENDIF ELSE BEGIN
        s = 'Data types: ' + 'EVENT'
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
PRO TTEReader::showHeader, extension, _EXTRA = extra

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
PRO TTEReader::showNotes, _EXTRA = extra

    IF (NOT self.dataValid) THEN $
       RETURN

    filename = self->filename (/BASENAME)

    w = DIALOG_TEXT ((*self.header).notes, $
        TITLE = filename, /SAVE_OPTION, _EXTRA = extra)

END


;------------------------------------------------------------------------------
; Set RMF File Name for the PHA file
;------------------------------------------------------------------------------
PRO TTEReader::setRMFFile, newRMFFile

    self.RMFFile = newRMFFile

END


;------------------------------------------------------------------------------
; Get RMF File Name from the PHA file
;------------------------------------------------------------------------------
FUNCTION TTEReader::getRMFFile

    RETURN, self.RMFFile


END

;------------------------------------------------------------------------------
; Get Unique Detector ID String
;------------------------------------------------------------------------------
FUNCTION TTEReader::getDetName

     RETURN, self.detName

END

;------------------------------------------------------------------------------
; Get the event deadtime
;------------------------------------------------------------------------------
FUNCTION TTEReader::getEvtDead

     RETURN, self.evt_dead

END


; ----------------------------------------------------------------------------
; Object definition
; ----------------------------------------------------------------------------
PRO TTEReader__define

    obj = { TTEReader, INHERITS DATAREADER, $

        detName   : '',         $
        rmfFile   : '',         $
        evt_dead  : 0.0,        $
        accum     : 0.0d,       $
        myTimes   : PTR_NEW (), $
        myChans   : PTR_NEW (), $
        dataValid : 0,          $
        dataDirty : 0           $
        
    }

END
