; ----------------------------------------------------------------------------
;+
; NAME:
;
;     WritePHAFile (Lib)
;
; PURPOSE:
;
;     Code that will output PHA data in the proper FITS format.
;
; CALLING SEQUENCE:
;
;     WritePHAFile, filename, FITSheader, numbins, channels, totalTime, tint, 
;               history, errors, verstring, BACKFILE = backfile
;
; INPUTS:
;
;     filename   : STRING containing a FITS filename
;
;     FITSheader : STRING ARRAY containing a valid FITS header (no checking!)
;
;     numbins    : The number of PHA channels for the data (history & errors)
;
;     channels   : 2X1 INTEGER ARRAY of the valid channel limits
;
;     totalTime  : FLOAT containing the exposure
;
;     tint       : 2X1 FLOAT ARRAY of the accumulation start and stop times
;
;     ebins      : 2Xnumbins FLOAT ARRAY of the energy loss thresholds
;
;     history    : numbins element FLOAT ARRAY containing the PHA rate date
;
;     errors     : numbins element FLOAT ARRAY containing the PHA rate uncertanties
;
;     verstring  : STRING containing the version number of rmfit
;
; KEYWORDS:
;
;     BACKFILE: A STRING that names any background file to be associated with the 
;               file to be created.
;
;     RESPFILE: A STRING that names any detector redistribution matrix file to be  
;               associated with the file to be created.
;
; DEPENDENCIES:
;
;     NONE
;
; MODIFICATION HISTORY:
;
;     Written, 2005 Feb, Rob.Preece@nsstc.nasa.gov
;
;-
; ----------------------------------------------------------------------------
PRO WritePHAFile, filename, FITSheader, numbins, channels, totalTime, tint, $
    ebins, history, errors, verstring, BACKGROUND = background, BACKFILE = backfile, RESPFILE = respfile

    ;== Create the new PRIMARY header
    fxhmake, hdr, /extend, /date, /initialize
;    fxaddpar, hdr, 'CONTENT', 'PHA SPECTRUM', 'SPECTRUM extension (at least) exists'
    fxaddpar, hdr, 'CREATOR', 'writephafile:rmfit ' + !RM_PARAMS.VERSION, $
                   'Software and version creating file'
;    fxaddpar, hdr, 'HDUCLASS', 'OGIP', 'format conforms to OGIP standard'
;    fxaddpar, hdr, 'HDUVERS', '1.0.0'
;    fxaddpar, hdr, 'HDUCLAS1', 'PHA', 'PHA dataset (OGIP-92-007)'
    pathEnd = STRPOS(filename, '/', /REVERSE_SEARCH)
    IF (pathEnd NE -1) THEN BEGIN
    	shortName = STRMID(filename, pathEnd + 1)
    ENDIF ELSE BEGIN
    	shortName = filename
    ENDELSE
    fxaddpar, hdr, 'FILENAME', shortName, 'Name of this file'
    
    IF KEYWORD_SET(BACKGROUND) THEN filetype = 'GBM BACK' $
                               ELSE filetype = 'SPECTRUM'
    fxaddpar, hdr, 'FILETYPE', filetype, $
                   'Name for this type of FITS file'
    fxaddpar, hdr, 'FILE-VER', '1.0.0', 'Name for the format of this filetype'
    origin = sxpar(FITSheader, 'ORIGIN')
    fxaddpar, hdr, 'ORIGIN', origin, 'Name of organization making file'
    mission = sxpar(FITSheader, 'TELESCOP')
    fxaddpar, hdr, 'TELESCOP', mission, 'mission/satellite name'
    instrument = sxpar(FITSheader, 'INSTRUME')
    fxaddpar, hdr, 'INSTRUME', instrument, 'instrument/detector name'
    detname = ' '
    ww = STRPOS(FITSheader, 'DETNAM')
    wnum = WHERE(ww NE -1, co)
    IF (co NE 0) THEN detname = sxpar(FITSheader, 'DETNAM')
    fxaddpar, hdr, 'DETNAM', detname, 'specific detector name'
    observer = sxpar(FITSheader, 'OBSERVER')
    fxaddpar, hdr, 'OBSERVER', observer, 'Name of instrument PI'
    ;fxaddpar, hdr, 'AUTHOR', 'Preece'
    theobject = sxpar(FITSheader, 'OBJECT')
    theobject = STRING(theobject)
    fxaddpar, hdr, 'MJDREFI', 51910., 'Reference epoch MJD date, integer part'
    sxaddpar, hdr, 'MJDREFF', 7.428703703703703D-4, 'Reference epoch MJD, fractional part',f='(e21.15)'
    fxaddpar, hdr, 'TIMESYS', 'TT', 'Time system used in time keywords'
    fxaddpar, hdr, 'TIMEUNIT', 's', 'Time unit used in TSTART, TSTOP and TRIGTIME'
    IF (theObject NE '-1') THEN BEGIN ;We have a trigger file
		fxaddpar, hdr, 'OBJECT', theobject, 'Object name'
		;CHECK TO SEE IF WE ARE PARSING BATSE OR GBM DATA:
		td = STRPOS(FITSheader, 'TRIG-DAY')
        tnum = WHERE(td NE -1, tco)
        IF (tco NE 0) THEN BEGIN ; It's BATSE:
        	trigDay = sxpar(FITSheader, 'TRIG-DAY')
        	trigDay = trigDay * 1000.
			trigSec = sxpar(FITSheader, 'TRIG-TIM')
			obsStart = DOUBLE(trigSec + tInt[0])
			obsStop  = DOUBLE(trigSec + tInt[1])
			trigFrac = trigSec / 24. /60 /60
			timeObs = trigFrac + tint[0] / 24. /60 /60
			timeStr = DATE_CONV(trigDay + trigFrac, 'FITS')
			dateObs = DATE_CONV(trigDay + timeObs, 'FITS')
			timeEnd = trigFrac + tint[1] / 24. /60 /60
			dateEnd = DATE_CONV(trigDay + timeEnd, 'FITS')
		    ;fxaddpar, hdr, 'TIMEZERO', trigSec, 'Seconds of day for event trigger'
			objectRA = sxpar(FITSheader, 'OBJCTRA')
			objectDEC = sxpar(FITSheader, 'OBJCTDEC')
			trigcomm = 'Seconds of day for event trigger'
		ENDIF ELSE BEGIN
			trigSec = sxpar(FITSheader, 'TRIGTIME')
			obsStart = DOUBLE(trigSec + tInt[0])
			obsStop  = DOUBLE(trigSec + tInt[1])
			dateObs = FITSTime(obsStart)
			dateEnd = FITSTime(obsStop)
			;dateObs = sxpar(FITSheader, 'DATE-OBS')
			;dateEnd = sxpar(FITSheader, 'DATE-END')
			objectRA = sxpar(FITSheader, 'RA_OBJ')
			objectDEC = sxpar(FITSheader, 'DEC_OBJ')
			trigcomm = 'Trigger time since MJDREF'
		ENDELSE
		fxaddpar, hdr, 'RADECSYS', 'FK5', 'Stellar reference frame'
		fxaddpar, hdr, 'EQUINOX', 2000.0, 'Equinox for RA and Dec'
		fxaddpar, hdr, 'RA_OBJ', objectRA, 'J2000 RA of source, degrees'
		fxaddpar, hdr, 'DEC_OBJ', objectDEC, 'J2000 DEC of source, degrees'
		fxaddpar, hdr, 'TRIGTIME', trigSec, trigcomm,f='(f19.7)'
	ENDIF ELSE BEGIN ; Don't have the trigger info: just add what we do have:
		dateObs = sxpar(FITSheader, 'DATE-OBS')
		dateEnd = sxpar(FITSheader, 'DATE-END')
		timeStr = dateObs
	ENDELSE
	fxaddpar, hdr, 'TSTART', obsStart, 'Start time of spectral accumulation',f='(f19.7)'
	fxaddpar, hdr, 'TSTOP',  obsStop,  'Stop time of spectral accumulation',f='(f19.7)'
	fxaddpar, hdr, 'DATE-OBS', dateObs, 'Date of start of observation'
	fxaddpar, hdr, 'DATE-END', dateEnd, 'Date of end of observation'
    fits_add_checksum, hdr
    
    fxwrite, filename, hdr
    
    ;== EBOUNDS extension: one row for each energy channel...
    fxbhmake, eh, numBins, /DATE

    ;== Unique name for this FITS extension
    fxaddpar, eh, 'EXTNAME', 'EBOUNDS', 'Name of this binary table extension'
    fxaddpar, eh, 'ORIGIN', origin, 'Name of organization making file'
    fxaddpar, eh, 'TELESCOP', mission, 'mission/satellite name'
    ;instrument = sxpar(FITSheader, 'INSTRUME')
    fxaddpar, eh, 'INSTRUME', instrument, 'instrument/detector name'
    fxaddpar, eh, 'OBSERVER', observer, 'Name of instrument PI'
    fxaddpar, eh, 'DETNAM', detname, 'specific detector name'
    fxaddpar, eh, 'MJDREFI', 51910., 'Reference epoch MJD date, integer part'
    sxaddpar, eh, 'MJDREFF', 7.428703703703703D-4, 'Reference epoch MJD, fractional part',f='(e21.15)'
    fxaddpar, eh, 'TIMESYS', 'TT', 'Time system used in time keywords'
    fxaddpar, eh, 'TIMEUNIT', 's', 'Time unit used in TSTART, TSTOP and TRIGTIME'
    IF (theObject NE '-1') THEN BEGIN ;We have a trigger file
		fxaddpar, eh, 'OBJECT', theobject, 'Object name'
		fxaddpar, eh, 'RADECSYS', 'FK5', 'Stellar reference frame'
		fxaddpar, eh, 'EQUINOX', 2000.0, 'Equinox for RA and Dec'
		fxaddpar, eh, 'RA_OBJ', objectRA, 'J2000 RA of source, degrees'
		fxaddpar, eh, 'DEC_OBJ', objectDEC, 'J2000 DEC of source, degrees'
		fxaddpar, eh, 'TRIGTIME', trigSec, trigcomm,f='(f19.7)'
	ENDIF 
	fxaddpar, eh, 'TSTART', obsStart, 'Start time of spectral accumulation',f='(f19.7)'
	fxaddpar, eh, 'TSTOP', obsStop, 'Stop time of spectral accumulation',f='(f19.7)'
	fxaddpar, eh, 'DATE-OBS', dateObs, 'Date of start of observation'
	fxaddpar, eh, 'DATE-END', dateEnd, 'Date of end of observation'
    fxaddpar, eh, 'FILTER', 'none', 'Instrument filter in use (if any)'

    fxaddpar, eh, 'CHANTYPE', 'PHA', 'channel type (PHA, PI etc.)'
    fxaddpar, eh, 'DETCHANS', numBins, 'total number possible channels'
    fxaddpar, eh, 'HDUCLASS', 'OGIP', 'format conforms to OGIP standard'
    fxaddpar, eh, 'HDUCLAS1', 'RESPONSE', 'PHA dataset (OGIP-92-007)'
    fxaddpar, eh, 'HDUCLAS2', 'EBOUNDS'
    fxaddpar, eh, 'HDUVERS', '1.2.0', 'Format version number'
    fxaddpar, eh, 'EXTVER', 1, 'FITS format template version'
    
    ;= Create the data columns:
    comment = 'Pulse Height Analyzer (PHA) Channel'
    fxbaddcol, id, eh, 1, 'CHANNEL', comment
    fxaddpar, eh, 'TLMIN1', 0, 'Channel numbers are non-negative', after='TTYPE1'
    fxaddpar, eh, 'TLMAX1', numBins - 1, 'Greater than the number of channels', after='TLMIN1'
    comment = 'Lower edge of energy loss threshold'
    fxbaddcol, id, eh, 1.0, 'E_MIN', comment
    fxaddpar, eh, 'TUNIT2', 'keV', after='TFORM2'
    min_e_min = MIN(ebins[0, *], MAX = max_e_min)
    fxaddpar, eh, 'TLMIN2', min_e_min, 'Energies are non-negative', after='TTYPE2'
    fxaddpar, eh, 'TLMAX2', max_e_min, 'Largest possible energy', after='TLMIN2'
    comment = 'Upper edge of energy loss threshold'
    fxbaddcol, id, eh, 1.0, 'E_MAX', comment
    fxaddpar, eh, 'TUNIT3', 'keV', after='TFORM3'
    min_e_max = MIN(ebins[1, *], MAX = max_e_max)
    fxaddpar, eh, 'TLMIN3', min_e_max, 'Energies are non-negative', after='TTYPE3'
    fxaddpar, eh, 'TLMAX3', max_e_max, 'Largest possible energy', after='TLMIN3'

    ebdStr = {CHANNEL: 1, E_MIN: 1.0, E_MAX:1.0}
    ebdStr = REPLICATE(ebdStr, numBins)
    
    ebdStr.channel = indgen(numBins) + channels[0]
    ebdStr.E_MIN = TRANSPOSE(ebins[0, *])
    ebdStr.E_MAX = TRANSPOSE(ebins[1, *])
    
    ;fits_add_checksum, eh, ebins
    MWRFITS, ebdStr, filename, eh, /NO_COMMENT

    ;== Rate data table: one row for each energy channel...
    fxbhmake, ch, numBins, /DATE

    ;== Unique name for this FITS extension
    fxaddpar, ch, 'EXTNAME', 'SPECTRUM', 'Name of this binary table extension'
    fxaddpar, ch, 'ORIGIN', origin, 'Name of organization making file'
    fxaddpar, ch, 'TELESCOP', mission, 'mission/satellite name'
    ;instrument = sxpar(FITSheader, 'INSTRUME')
    fxaddpar, ch, 'INSTRUME', instrument, 'instrument/detector name'
    fxaddpar, ch, 'OBSERVER', observer, 'Name of instrument PI'
    fxaddpar, ch, 'DETNAM', detname, 'specific detector name'
    fxaddpar, ch, 'FILTER', 'none', 'Instrument filter in use (if any)'
    fxaddpar, ch, 'EXPOSURE', totalTime, 'exposure time (in seconds)'
    
    bfilename = 'none'
    IF KEYWORD_SET(BACKFILE) THEN bfilename = backfile
    fxaddpar, ch, 'BACKFILE', bfilename, 'associated background filename'
    fxaddpar, ch, 'BACKSCAL', 1.0, 'background file scaling factor'
    fxaddpar, ch, 'CORRFILE', 'none', 'associated correction filename'
    fxaddpar, ch, 'CORRSCAL', 1.0, 'correction file scaling factor'
    rfilename = 'none'
    IF KEYWORD_SET(RESPFILE) THEN rfilename = respfile
    fxaddpar, ch, 'RESPFILE', rfilename, 'associated redistribution matrix filename'
    fxaddpar, ch, 'ANCRFILE', 'none', 'associated ancilary response filename'
    fxaddpar, ch, 'AREASCAL', 1.0, 'Effective area scaling factor'
    ; 11/07/08 RDP: Not needed if STAT_ERR is provided!
    ;fxaddpar, ch, 'POISSERR', 'T', 'Poisson errors are applicable'
    fxaddpar, ch, 'SYS_ERR', 0, 'no systematic error specified'
    fxaddpar, ch, 'QUALITY', 0, 'no systematic error specified'
    fxaddpar, ch, 'GROUPING', 0, 'Grouping flag'
    
    fxaddpar, ch, 'HDUCLASS', 'OGIP', 'format conforms to OGIP standard'
    fxaddpar, ch, 'HDUCLAS1', 'SPECTRUM', 'PHA dataset (OGIP-92-007)'
    hduClas2 = 'NET'
    bkgComm = 'NET counts PHA Spectrum'
    IF KEYWORD_SET(BACKGROUND) THEN BEGIN
        hduClas2 = 'BKG'
        bkgComm = 'Bkgd-subtracted PHA Spectrum'
    ENDIF
    fxaddpar, ch, 'HDUCLAS2', hduClas2, bkgComm
    fxaddpar, ch, 'HDUCLAS3', 'RATE', 'PHA data stored in count/s'
    fxaddpar, ch, 'HDUCLAS4', 'PHA:I', 'Single PHA dataset'
    fxaddpar, ch, 'HDUVERS', '1.2.0', 'Format version number'
    fxaddpar, ch, 'HDUVERS1', '1.1.0', 'Version of format (OGIP memo OGIP-92-007a)'
    fxaddpar, ch, 'HISTORY', 'RATE data created by ' + verstring
    
    fxaddpar, ch, 'CHANTYPE', 'PHA', 'channel type (PHA, PI etc.)'
    fxaddpar, ch, 'DETCHANS', N_ELEMENTS(history), 'total number possible channels'
    
    ;trigDay = sxpar(FITSheader, 'TRIG-DAT')
    IF (theObject NE '-1') THEN BEGIN ;We have a trigger file
		fxaddpar, ch, 'OBJECT', theobject, 'Object name'
		fxaddpar, ch, 'RADECSYS', 'FK5', 'Stellar reference frame'
		fxaddpar, ch, 'EQUINOX', 2000.0, 'Equinox for RA and Dec'
		fxaddpar, ch, 'RA_OBJ', objectRA, 'J2000 RA of source, degrees'
		fxaddpar, ch, 'DEC_OBJ', objectDEC, 'J2000 DEC of source, degrees'
		fxaddpar, ch, 'TRIGTIME', trigSec, trigcomm,f='(f19.7)'
	ENDIF 
    fxaddpar, ch, 'TSTART', obsStart, 'Start time of spectral accumulation',f='(f19.7)'
    fxaddpar, ch, 'TSTOP', obsStop, 'Stop time of spectral accumulation',f='(f19.7)'
    fxaddpar, ch, 'DATE-OBS', dateObs, 'Date of start of observation'
	fxaddpar, ch, 'DATE-END', dateEnd, 'Date of end of observation'
    ;trigSec = sxpar(FITSheader, 'TRIG-TIM')
    ;fxaddpar, ch, 'TZERO', trigSec, 'Seconds of day for event trigger'
    fxaddpar, ch, 'MJDREFI', 51910., 'Reference epoch MJD date, integer part'
    sxaddpar, ch, 'MJDREFF', 7.428703703703703D-4, 'Reference epoch MJD, fractional part',f='(e21.15)'
    fxaddpar, ch, 'TIMESYS', 'TT', 'Time system used in time keywords'
    fxaddpar, ch, 'TIMEUNIT', 's', 'Time unit used in TSTART, TSTOP and TRIGTIME'
    
    fxaddpar, ch, 'EXTVER', 1, 'FITS format template version'
    
    ;= Create the data columns:
    comment = 'Pulse Height Analyzer (PHA) Channel'
    fxbaddcol, id, ch, 1, 'CHANNEL', comment
    comment = 'Counts per second per channel'
    fxbaddcol, id, ch, 1.0, 'RATE', comment
    fxaddpar, ch, 'TUNIT2', 'count/s', after='TFORM2'
    comment = 'Statistical error on RATE'
    fxbaddcol, id, ch, 1.0, 'STAT_ERR', comment
    fxaddpar, ch, 'TUNIT3', 'count/s', after='TFORM3'
    fxaddpar, ch, 'TLMIN1', 0, 'Channel numbers are non-negative', after='TTYPE1'
    fxaddpar, ch, 'TLMAX1', numbins - 1, 'Greater than the number of channels', after='TLMIN1'
    
    ;fxbcreate, unit_out, filename, ch
    specStr = {CHANNEL: 1, RATE: 1.0, STAT_ERR:1.0}
    specStr = REPLICATE(specStr, numBins)
    
    specStr.channel = indgen(numBins) + channels[0]
    specStr.RATE = history
    specStr.STAT_ERR = errors
    
    ;fits_add_checksum, ch, specStr
    ;= Write out the data for the extension:
    MWRFITS, specStr, filename, ch, /NO_COMMENT
        
    ;== GTI extension: one row for the selection or one for each background interval...
    numTimes = 1
    fxbhmake, gti, numTimes, /DATE

    ;== Unique name for this FITS extension
    fxaddpar, gti, 'EXTNAME', 'GTI', 'Name of this binary table extension'
    fxaddpar, gti, 'ORIGIN', origin, 'Name of organization making file'
    fxaddpar, gti, 'TELESCOP', mission, 'mission/satellite name'
    fxaddpar, gti, 'INSTRUME', instrument, 'instrument/detector name'
    fxaddpar, gti, 'OBSERVER', observer, 'Name of instrument PI'
    fxaddpar, gti, 'DETNAM', detname, 'specific detector name'
    fxaddpar, gti, 'HDUCLASS', 'OGIP', 'format conforms to OGIP standard'
    fxaddpar, gti, 'HDUCLAS1', 'GTI', 'PHA dataset (OGIP-92-007)'
    fxaddpar, gti, 'HDUVERS', '1.2.0', 'Format version number'
    fxaddpar, gti, 'EXPOSURE', totalTime, 'exposure time (in seconds)'
    IF (theObject NE '-1') THEN BEGIN ;We have a trigger file
		fxaddpar, gti, 'OBJECT', theobject, 'Object name'
		fxaddpar, gti, 'RADECSYS', 'FK5', 'Stellar reference frame'
		fxaddpar, gti, 'EQUINOX', 2000.0, 'Equinox for RA and Dec'
		fxaddpar, gti, 'RA_OBJ', objectRA, 'J2000 RA of source, degrees'
		fxaddpar, gti, 'DEC_OBJ', objectDEC, 'J2000 DEC of source, degrees'
		fxaddpar, gti, 'TRIGTIME', trigSec, trigcomm,f='(f19.7)'
	ENDIF 
    fxaddpar, gti, 'TSTART', obsStart, 'Start time of spectral accumulation',f='(f19.7)'
    fxaddpar, gti, 'TSTOP', obsStop, 'Stop time of spectral accumulation',f='(f19.7)'
    fxaddpar, gti, 'DATE-OBS', dateObs, 'Date of start of observation'
	fxaddpar, gti, 'DATE-END', dateEnd, 'Date of end of observation'
    ;trigSec = sxpar(FITSheader, 'TRIG-TIM')
    ;fxaddpar, gti, 'TZERO', trigSec, 'Seconds of day for event trigger'
    fxaddpar, gti, 'MJDREFI', 51910., 'Reference epoch MJD date, integer part'
    sxaddpar, gti, 'MJDREFF', 7.428703703703703D-4, 'Reference epoch MJD, fractional part',f='(e21.15)'
    fxaddpar, gti, 'TIMESYS', 'TT', 'Time system used in time keywords'
    fxaddpar, gti, 'TIMEUNIT', 's', 'Time unit used in TSTART, TSTOP and TRIGTIME'
    fxaddpar, gti, 'EXTVER', 1, 'FITS format template version'
    
    ;= Create the data columns:
    comment = 'Beginning time of accumulation'
    fxbaddcol, id, gti, 1.0, 'START', comment
    fxaddpar, gti, 'TUNIT1', 's', after='TFORM1'
    fxaddpar, gti, 'TZERO1', trigSec, '[s] Offset, equal to TRIGTIME', after='TTYPE1'
    comment = 'Ending time of accumulation'
    fxbaddcol, id, gti, 1.0, 'STOP', comment
    fxaddpar, gti, 'TUNIT2', 's', after='TFORM2'
    fxaddpar, gti, 'TZERO2', trigSec, '[s] Offset, equal to TRIGTIME', after='TTYPE2'

    gtiStr = {START: 1.0d, STOP:1.0d}
    gtiStr = REPLICATE(gtiStr, numTimes)
    
    gtiStr.START = tint[0]
    gtiStr.STOP = tint[1]

    ;fits_add_checksum, gti, gtiStr
    
    MWRFITS, gtiStr, filename, gti, /NO_COMMENT

END; WritePHAFile
