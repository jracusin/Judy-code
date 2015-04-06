; ----------------------------------------------------------------------------
;+
; NAME:
;
;     rmfResponse (OBJECT)
;
; PURPOSE:
;
;     This object encapsulates a detector response function in XSPEC format.
;     The response data are stored in FITS format (rmf) files.
;
; CALLING SEQUENCE:
;
;     obj = OBJ_NEW ('rmfResponse' [, filename])
;
; INPUTS:
;
;     filename (OPTIONAL) : STRING denoting the response file to read.  If
;         this parameter is given, the file will be read when the object
;         is instantiated.
;
; KEYWORDS:
;
;     NONE
;
; INHERITS:
;
;     Response
;
; DEPENDENCIES:
;
;     file__define.pro
;     response__define.pro
;
; METHODS:
;     
;     read (PROCEDURE) - Read a response file
;         Inputs: FILENAME : STRING denoting the response data filename
;        Outputs: NONE 
;       Keywords: ERROR : Set to a named variable to return 0 if the read
;                 was successful, or 1 if the read failed.
;
;     response (FUNCTION) - Return response data
;         Inputs: NONE
;        Outputs: The response data
;       Keywords: NONE
;
; MODIFICATION HISTORY:
;
;     1.0 : Written, 1999 August, Robert.Mallozzi@msfc.nasa.gov
;
;-
; ----------------------------------------------------------------------------


; ----------------------------------------------------------------------------
; Constructor
; ----------------------------------------------------------------------------
FUNCTION rmfResponse::init, filename, READ = read

    status = self->response::init ()

    IF (N_ELEMENTS (filename) NE 0) THEN BEGIN
      
       self.File->set, filename   
       
       self->read, ERROR = error          
       IF (error) THEN BEGIN
          MESSAGE, /CONT, 'Failed to read response file: ' + STRING (filename)
          self->setfilename,''
          RETURN, 0
       ENDIF
        
    ENDIF
    
    RETURN, status

END


; ----------------------------------------------------------------------------
; Destructor
; ----------------------------------------------------------------------------
PRO rmfResponse::cleanup

    PTR_FREE, self.time_list 

    PTR_FREE, self.chan_energy 
    PTR_FREE, self.chan_width  
    PTR_FREE, self.chan_edges

    PTR_FREE, self.phot_energy
    PTR_FREE, self.phot_width     
    PTR_FREE, self.phot_edges

    self->response::cleanup

END    
    

; ----------------------------------------------------------------------------
; Read an XSPEC RSP/RMF FITS file
; ----------------------------------------------------------------------------
PRO rmfResponse::read, DATAREADER = dataReader, ERROR = error, TINT = tint

    error = 0

    filename = self.File->get ()
    
	CATCH, error_status
	IF error_status NE 0 THEN BEGIN
		MFIT = { MFIT_SYSTEM, $
		
			PARAM_FILE : '', $
			SCRIPT_FILE : '', $
			INFO_FILE : '', $
			HELP_PATH : ''     $
			
		}    
		READ_ONLY = 1
		DEFSYSV, '!MFIT', MFIT, READ_ONLY
		CATCH, /CANCEL 
	ENDIF
	
    ; Assume it is in the current directory:
    CD, !RM_PARAMS.lastpath
    c = FINDFILE (filename, COUNT = cnt)   
    
    IF (filename[0] EQ '' OR cnt EQ 0) THEN BEGIN
       MESSAGE, /CONTINUE, 'No response filename is set.'
       error = 1
       RETURN
    ENDIF

    MESSAGE, /RESET_ERROR_STATE    
    ON_IOERROR, READ_ERROR

    ;== Read FITS headers and data tables

    header0 = HEADFITS (filename, EXTEN = 0)
    header1 = HEADFITS (filename, EXTEN = 1)
    header2 = HEADFITS (filename, EXTEN = 2)
    
    IF KEYWORD_SET(datareader) THEN BEGIN
		;== Check the sizes:
		mychan = SXPAR(header1, 'DETCHANS')
		myHDRs = datareader->header()
		nchan = SXPAR(myHDRs.ext1, 'DETCHANS')
	
		IF (nchan NE mychan) THEN BEGIN
		   joke = DIALOG_MESSAGE('DRM is the wrong size: expected ' + STRTRIM(STRING(nchan),2) + $
								 ' but found ' + STRTRIM(STRING(mychan), 2))
		   error = 1
		   self->setfilename,''
		   RETURN
		ENDIF   
    
		;== Do some more checking:
		hedrs = datareader->header()
		myDataDet = STRTRIM(SXPAR(hedrs.ext0, 'DETNAM'), 2)
		myRSPDet  = STRTRIM(SXPAR(header0, 'DETNAM'), 2)
		;== Not found:
		IF (SIZE(myDataDet))[1] NE 7 THEN myDataDet = 'DATA'
		IF (SIZE(myRSPDet))[1] NE 7 THEN myRSPDet = 'RSP'
		;== Catch some odd cases:
		IF (myDataDet EQ 'LAT') THEN myDataDet = 'LAT TKR'
		
		IF (myDataDet NE myRSPDet) THEN BEGIN
		   joke = DIALOG_MESSAGE('DETNAMs do not agree: Data File: ' + myDataDet + $
								 ' RSP File: ' + myRSPDet)
		ENDIF   
    ENDIF

    ;== Read data
    ;== Sometimes, the LAT DRMs do not have a valid entry for DRM_NUM:
    tempNumDRM = SXPAR(header0, 'DRM_NUM')
    IF (SIZE(tempNumDRM))[1] EQ 7 THEN self.numDRM = 1 $
    ELSE self.numDRM = tempNumDRM

    ;== Old-type DRM has no numDRM keyword...
    IF self.numDRM EQ 0 THEN self.numDRM = 1
    
    IF (self.numDRM GT 1) AND (NOT PTR_VALID(self.time_list)) THEN BEGIN
    	timeList = FLTARR(self.numDRM)
    	trigTime = SXPAR(header0,"TRIGTIME")
    	FOR tt = 2, self.numDRM + 1 DO BEGIN
    		tempHdr = HEADFITS (filename, EXTEN = tt)
    		drmTime = SXPAR(tempHdr,"TSTART") - trigTime
;print, drmtime
    		timeList[tt - 2] = drmtime
    	ENDFOR
    	self.time_list = PTR_NEW(timeList)
    ENDIF
    
    ok = self->read_compressed (filename, $
        num_ebins, num_chan, e_edges, c_edges, $
        resp_matrix, TINT = tint)

    IF (NOT ok) THEN BEGIN
       ;MESSAGE, /CONTINUE, 'Failed to read compressed DRM.'
       error = 1
       RETURN
    ENDIF   

    ;== Single detector
    
    drm_dim1 = num_ebins - 1L
    drm_dim2 = num_chan - 1L
    drm_dim3 = 0L                      ; TODO: Individual multiple detectors

    PTR_FREE, self.chan_energy
    self.chan_energy = PTR_NEW ( $
        (c_edges[1:num_chan] + c_edges[0:num_chan - 1]) / 2.0)
    
    PTR_FREE, self.chan_width           
    self.chan_width = PTR_NEW ( $
        c_edges[1:num_chan] - c_edges[0:num_chan - 1])
    
    PTR_FREE, self.phot_energy
    self.phot_energy = PTR_NEW ( $
        (e_edges[0:num_ebins - 1] + e_edges[1:num_ebins]) / 2.0)
    
    PTR_FREE, self.phot_width
    self.phot_width  = PTR_NEW ( $
        e_edges[1: num_ebins] - e_edges[0: num_ebins - 1])

    PTR_FREE, self.response
    self.response = PTR_NEW (resp_matrix)
        
    PTR_FREE, self.chan_edges
    self.chan_edges = PTR_NEW (c_edges)
    
    PTR_FREE, self.phot_edges
    self.phot_edges = PTR_NEW (e_edges)

    PTR_FREE, self.header
    self.header = PTR_NEW ({  $

        ext0  : header0, $
        ext1  : header1, $
        ext2  : header2  $

    })    

READ_ERROR:    

    IF (!ERROR_STATE.CODE LT 0) THEN BEGIN
    	IF (!ERROR_STATE.CODE EQ -104) THEN BEGIN
			MESSAGE, /CONTINUE, /RESET
			RETURN
		ENDIF
       error = 1
       MESSAGE, /CONTINUE, !ERROR_STATE.MSG
    ENDIF

END


; ----------------------------------------------------------------------------
; Read an RMF DRM stored in compressed format
; ----------------------------------------------------------------------------
FUNCTION rmfResponse::read_compressed, filename, $
    num_ebins, num_chan, e_edges, c_edges, resp_matrix, TINT = tint, REX_IDX = rex_idx

   
    HEADER = HEADFITS(FILENAME)
    
    ;Loop through extensions until the right one is found:
    ebExt = 0
    found = 0
    REPEAT BEGIN
        ebExt += 1
        newHDR = HEADFITS (filename, EXTEN = ebExt)
        ebExtName = STRTRIM(SXPAR (newHDR, 'EXTNAME'), 2)
        IF (ebExtName EQ 'EBOUNDS') OR (ebExtName EQ 'SPI.-EBDS-SET') OR $
           (ebExtName EQ 'PHT_EDGE') OR (ebExtName EQ 'ISRG-EBDS-SET') THEN found = 1
    ENDREP UNTIL found
    FXBOPEN, UNIT, FILENAME, ebExtName, EH
    FXBREAD, UNIT, E_LO, 'E_MIN'
    FXBREAD, UNIT, E_HI, 'E_MAX'
    FXBCLOSE, UNIT
    
    NCHAN = N_ELEMENTS(E_LO)
    THRESHOLDS = FLTARR(NCHAN + 1)
    THRESHOLDS[0] = E_LO[0]
    THRESHOLDS[1: *] = E_HI
    
    num_chan = NCHAN
    c_edges = THRESHOLDS
    
    ;== Extension name has not been nailed down sufficiently...
    rspExt = 0
    found = 0
    IF (self.numDRM EQ 1) THEN BEGIN
		REPEAT BEGIN
			rspExt += 1
			newHDR = HEADFITS (filename, EXTEN = rspExt)
			extName = STRTRIM(SXPAR (newHDR, 'EXTNAME'), 2)
			IF (extName EQ 'SPECRESP MATRIX') OR $
			   (extName EQ 'MATRIX') OR (extName EQ 'SPI.-RMF1-RSP') OR $
			   (extName EQ 'ISRG-RMF1-RSP') THEN found = 1
		ENDREP UNTIL found
    ENDIF ELSE BEGIN
    	IF KEYWORD_SET(TINT) THEN BEGIN
    		;== Start simply - average the time interval:
    		targetT = TOTAL(tint) / 2.0
			resVal = MIN(ABS(*self.time_list - targetT), minInd)
			;== time_list is zero-indexed and the matrices start at ext=2
			minInd += 2
    	ENDIF ELSE IF KEYWORD_SET(REX_IDX) THEN BEGIN
    		;== The caller has a specific response matrix extension in mind:
    		minInd = rex_idx
    	ENDIF ELSE BEGIN
			;== Find the matrix closest to the trigger:
    		targetT = 0.0
			resVal = MIN(ABS(*self.time_list - targetT), minInd)
			;== time_list is zero-indexed and the matrices start at ext=2
			minInd += 2
    	ENDELSE
		rspExt = minInd
;print,rspExt
    ENDELSE
    		
	self.currDRM = rspExt
	;newHDR = HEADFITS (filename, EXTEN = 1)
    ;extName = STRTRIM(SXPAR (newHDR, 'EXTNAME'), 2)
    ;IF (extName EQ 'EBOUNDS') THEN BEGIN
    ;    newHDR = HEADFITS (filename, EXTEN = 2)
    ;    extName = STRTRIM(SXPAR (newHDR, 'EXTNAME'), 2)
    ;ENDIF
    
    matrix_tbl = MRDFITS(FILENAME, rspExt, RH)
    ;FXBOPEN, UNIT, FILENAME, extName, RH

    NPHOTS = SXPAR(RH, 'NAXIS2')
    ;FXBREAD, UNIT, E_LO, 'ENERG_LO'
    E_LO = matrix_tbl.ENERG_LO
    ;FXBREAD, UNIT, E_HI, 'ENERG_HI'
    E_HI = matrix_tbl.ENERG_HI
    ;FXBREAD, UNIT, N_GRP, 'N_GRP'
    N_GRP = matrix_tbl.N_GRP
    ;FXBREAD, UNIT, N_CHAN, 'N_CHAN'
    N_CHAN = matrix_tbl.N_CHAN
    ;FXBREAD, UNIT, F_CHAN, 'F_CHAN'
    F_CHAN = matrix_tbl.F_CHAN
    DRM = FLTARR(NPHOTS, NCHAN)
 
    GRP_SWITCH = (SIZE(F_CHAN))[0]                      ; Possibly several groups per row...
    IF (GRP_SWITCH EQ 1) THEN BEGIN
        junk = WHERE(F_CHAN EQ 0, jco)
        IF (jco GT 0) THEN F_CHAN = F_CHAN + 1  ; Asuume zero-indexed!
    ENDIF

    FOR I = 0, NPHOTS - 1 DO BEGIN
       IF (N_GRP[I] GT 0) THEN BEGIN
          ;FXBREAD, UNIT, DATA, 'MATRIX ', I + 1         ; Data are 1-indexed in FITS files!
          DATA = (matrix_tbl.MATRIX)[*,I]
          IF (GRP_SWITCH EQ 1) THEN BEGIN               ; `DATA' is contiguous
             DRM[I, F_CHAN[I] - 1: N_CHAN[I] + F_CHAN[I] - 2] = DATA
             ENDIF $
          ELSE BEGIN                                    ; `DATA' consists of N_GRP[i] groups
             IDX = 0
             FOR J = 0, N_GRP[I] - 1 DO BEGIN
                DRM[I, F_CHAN[J, I] - 1: N_CHAN[J, I] + F_CHAN[J, I] - 2] = DATA[IDX: IDX + N_CHAN[J, I] - 1]
                IDX = IDX + N_CHAN[J, I]
                ENDFOR
             ENDELSE
          ENDIF
       ENDFOR
    ;FXBCLOSE, UNIT
    
    E_IN = FLTARR(NPHOTS + 1)
    E_IN[0] = E_LO[0]
    E_IN[1: *] = E_HI
    
    num_ebins = NPHOTS
    e_edges = E_IN
    
    resp_matrix = DRM

    RETURN, 1

END


; ----------------------------------------------------------------------------
; Return response data
; ----------------------------------------------------------------------------
FUNCTION rmfResponse::response, TINT = tint, NORM_HIST = norm_hist, TIME_BINS = time_bins

    IF (self.numDRM GT 1) THEN BEGIN
    	IF KEYWORD_SET(TINT) THEN BEGIN
    		IF KEYWORD_SET(NORM_HIST) THEN BEGIN
    			;== We want to weight the matrices by counts:
    			weightV = [0.0]
				weightS = 0.0
				weightD = [0]
				tiList = *self.time_list
				currDRM = -1
    			FOR hi = 0, N_ELEMENTS(norm_hist) - 1 DO BEGIN
    				myTime = TOTAL(time_bins[*, hi]) / 2.0
    				wResult = MIN(ABS(tiList - myTime), minInd)
    				IF (currDRM EQ -1) THEN BEGIN 
						currDRM = minInd
						weightD = [weightD, currDRM]
					ENDIF
    				IF (currDRM EQ minInd) THEN BEGIN
    					weightS += norm_hist[hi]
    				ENDIF ELSE BEGIN
    					currDRM = minInd
    					weightV = [weightV, weightS]
    					weightD = [weightD, currDRM]
    					weightS = norm_hist[hi]
    				ENDELSE
    			ENDFOR
    			weightV = [weightV, weightS]
    			weightV = weightV[1:*]
    			weightD = weightD[1:*]
print,"Sum of the weight vector: ",total(weightV)
print,"DRMs in the weight sum: ",weightD
;stop
    			mySize = SIZE(*self.response)
    			myResponse = FLTARR(mySize[1], mySize[2])
    			filename = self.File->get ()
    			FOR wi = 0, N_ELEMENTS(weightD) - 1 DO BEGIN
					ok = self->read_compressed (filename, $
						num_ebins, num_chan, e_edges, c_edges, $
						resp_matrix, REX_IDX = weightD[wi] + 2)
					myResponse = myResponse + resp_matrix * weightV[wi]
    			ENDFOR

    			PTR_FREE, self.response
    			self.response = PTR_NEW(myResponse)
    		ENDIF ELSE BEGIN
				;== Start simply - average the time interval:
				targetT = TOTAL(tint) / 2.0
				;== Find the matrix closest to the trigger:
				resVal = MIN(ABS(*self.time_list - targetT), minInd)
				;== time_list is zero-indexed and the matrices start at ext=2
				rspExt = minInd + 2
				
				;== Now check to see whether the current matrix is valid:
				IF (rspExt NE self.currDRM) THEN BEGIN
					self->read, ERROR = error, TINT = tint
					IF (ERROR) THEN BEGIN
						MESSAGE, /CONT, 'Failed to read response file'
						self->setfilename,''
						RETURN, 0
					ENDIF
				ENDIF
			ENDELSE
    	ENDIF 
	ENDIF
	
    resp = { $

        chan_energy : *self.chan_energy, $ 
        chan_width  : *self.chan_width,  $  
        chan_edges  : *self.chan_edges,  $  
        phot_energy : *self.phot_energy, $
        phot_width  : *self.phot_width,  $ 
        phot_edges  : *self.phot_edges,  $ 
        drm         : *self.response     $

    }
    
    RETURN, resp
    
END

; ----------------------------------------------------------------------------
; Object definition
; ----------------------------------------------------------------------------
PRO rmfResponse__define

    obj = { RMFRESPONSE, INHERITS RESPONSE, $

        numDRM      : 0         , $ 
        currDRM     : 0         , $ 
        time_list   : PTR_NEW (), $ 

        chan_energy : PTR_NEW (), $ 
        chan_width  : PTR_NEW (), $
        chan_edges  : PTR_NEW (), $
         
        phot_energy : PTR_NEW (), $
        phot_width  : PTR_NEW (), $
        phot_edges  : PTR_NEW ()  $

    }

END
