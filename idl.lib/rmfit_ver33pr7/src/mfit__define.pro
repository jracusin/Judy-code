; ----------------------------------------------------------------------------
;+
; NAME:
;     Mfit
;
; PURPOSE:
;     Interface to Levenberg-Marquardt least-squares FORTRAN fitting
;     library MFIT, originally developed by Michael S. Briggs
;
; CALLING SEQUENCE:
;     o = OBJ_SINGLETON ('Mfit')
;
; INPUTS:
;     NONE
;
; KEYWORDS:
;     NONE
;
; INHERITS:
;     Singleton
;
; METHODS:
;
;     addDetector (PROCEDURE) - Add a detector dataset
;         Inputs : 
;        Outputs : 
;       Keywords :
;
;     deleteDetector (PROCEDURE) - Remove a detector dataset
;         Inputs : 
;        Outputs : 
;       Keywords :
;
;     fitModel (PROCEDURE) - Perform a fit
;         Inputs : 
;        Outputs : 
;       Keywords :
;
;     evalModel (FUNCTION) - Return model curve after a fit has been done
;         Inputs : 
;        Outputs : 
;       Keywords :
;
; MODIFICATION HISTORY:
;
;     Written, 1998 November, Robert.Mallozzi@msfc.nasa.gov
;
;-
; ----------------------------------------------------------------------------


; ----------------------------------------------------------------------------
; Constructor
; ----------------------------------------------------------------------------
FUNCTION Mfit::init, INFO_FILENAME = info_filename, _EXTRA = extra

    IF (N_ELEMENTS  (info_filename) EQ 0) THEN BEGIN
       MESSAGE, /CONTINUE, 'Missing keyword: INFO_FILENAME'
       RETURN, 0
    ENDIF

    ok = self->Singleton::init ()
    
    self.haveBatch       = 0L  ; Assume no batch fit results to start with
    self.lastFitWasBatch = 0L  ; Assume no batch fit results to start with
    
   ; *****  The values MAX_TERMS, MAX_PPT and MAX_PARAM must agree in three files !!  *****
   ; *****  The three files: mfit.F95, mfit_cparams.h and mfit__define.pro            *****
    
    ; TBD: parse the mfit_cparams.h header file to make sure these match!
;    self.MAX_DET       = 8L
    self.MAX_TERMS     = 55L
    self.NUM_ADD_TERMS = 39L
    self.MAX_PARAM     = 20L
    self.MAX_PPT       = 10L
;    self.MAX_CHAN      = 252L
;    self.MAX_EBINS     = 350L
;    self.MAX_PLOT      = 6L * self.MAX_CHAN + 1

    ;== Read the photon term info file

    self.termInfo        = OBJ_SINGLETON ('MFIT_termInfo', $
              INFO_FILE      = info_Filename,          $
              MAX_TERMS      = self.MAX_TERMS,         $
              MAX_PPT        = self.MAX_PPT            $
              )

    self.FitModel     = OBJ_NEW ('FitModel')
    self.PhotonModel  = OBJ_NEW ('PhotonModel', TERM_INFO = self.termInfo)
    self.DetectorList = OBJ_NEW ('List')
    self.BatchFitList = OBJ_NEW ('List')
    self.Error        = OBJ_NEW ('Error')
    
    RESTORE, !MFIT.script_file
    self.Log = OBJ_NEW ('Widget_Logger', TITLE = 'Fit Log', $
        XSIZE = 100, YSIZE = 30, MAP = 0)
        ;, GROUP_LEADER = self.widgetID.top)
    
    s = GET_SCREEN_SIZE ()
    self.Display = OBJ_NEW ('MfitDisplay', YSIZE = s[1] * 0.6, _EXTRA = extra)
    self.Display->setLogger,       self.Log
    self.Display->setFitter,       self
	
	; Presume Chi^2 to start with:
    self.statistic = 0L;
        
    RETURN, ok

END


; ----------------------------------------------------------------------------
; Destructor
; ----------------------------------------------------------------------------
PRO Mfit::cleanup

    PTR_FREE, self.chi1DVals
    PTR_FREE, self.chi1DParm
    PTR_FREE, self.fitModelInfo

    OBJ_DESTROY,  self.TermInfo

    numDet = self.DetectorList->count ()
    names  = self.DetectorList->names ()
    
    FOR i = 0, numDet - 1 DO BEGIN
   
        det = self.DetectorList->data (names[i])
        STRUCT_FREE, det
    
    ENDFOR
    
    OBJ_DESTROY, self.DetectorList

    numFit = self.BatchFitList->count ()
    batNames  = self.BatchFitList->names ()
    
    FOR i = 0, numFit - 1 DO BEGIN
   
        fit = self.BatchFitList->data (batNames[i])
        STRUCT_FREE, fit
    
    ENDFOR

    OBJ_DESTROY, self.BatchFitList
    
    OBJ_DESTROY, self.PhotonModel
    OBJ_DESTROY, self.Log
    OBJ_DESTROY, self.Display
    OBJ_DESTROY, self.FitModel
    OBJ_DESTROY, self.Error
    
    self->Singleton::cleanup
        
END    


;------------------------------------------------------------------------------
; Register a detector's display.  These are keyed by their unique data filename.
;------------------------------------------------------------------------------
PRO Mfit::registerDetector, Display
    
    Detector = Display->detector()
    Reader   = Detector->dataReader ()
    filename = Reader->filename ()
      
    det = { MFIT_DETECTOR }
    det.Detector     = Detector
    det.Display      = Display
    det.registerOnly = 1L
    
    self.DetectorList->add, filename, det
    self.numDet = self.numDet + 1
    self.TermInfo->adjustEffCorrTerm, self.numDet
    
END


;------------------------------------------------------------------------------
; Add a detector dataset.  Detectors are keyed by their unique data filename.
;------------------------------------------------------------------------------
PRO Mfit::addDetector, Display, $
    timeInt, energyInt, fitChannels, obsCrate, $
    backCrate, backCsig, liveTime
    
    Detector = Display->detector()
    Reader   = Detector->dataReader ()
    filename = Reader->filename ()
    
    header   = Reader->header (/POINTER)
    myDetName = STRING(Reader->getDetName())
    myDetNum = -1
    IF (myDetName NE '0') THEN BEGIN
		isBGO = STRPOS(myDetName, "BGO")
		isNAI = STRPOS(myDetName, "NAI")
		isGBM = isBGO + isNAI
		IF (isGBM GT -2) THEN READS, myDetName, myDetNum, FORMAT = '(4x,I)'
		IF isBGO EQ 0 THEN myDetNum += 12
    ENDIF
    
    Response   = Detector->response ()
    rspHead = Response->header()
    numDRM = SXPAR(rspHead.ext0, "DRM_NUM")
        
    IF (numDRM GT 1) THEN BEGIN
		;== Get the total count history:
		combHist  = Display.Lightcurve->combinedHist()
		combTimes = display.Lightcurve->combinedTimes ()
		COUNT_SPAN, combTimes, TRANSPOSE(timeInt), hidx
		myHist = combHist[hidx]
		myHist /= TOTAL(myHist)
;print, myHist
		IF (N_ELEMENTS(hidx) GT 1) THEN BEGIN
			myTimes = (combTimes)[*, hidx]
			;print, myTimes
			resp = Response->response (TINT = timeInt, NORM_HIST = myHist, TIME_BINS = myTimes)    
		ENDIF ELSE BEGIN
			resp = Response->response (TINT = timeInt)    
		ENDELSE
    ENDIF ELSE BEGIN
		resp = Response->response ()    
    ENDELSE
    numChan    = (SIZE (resp.drm))[2]
    numEbins   = (SIZE (resp.drm))[1]
    IF OBJ_ISA (Reader, 'BFITSReader') THEN BEGIN
        chanOffset = FIX (SXPAR ((*header).ext2, 'LO_CHAN'))
        resFrac    = FLOAT (SXPAR ((*header).ext2, 'RF_511'))
        resExp     = FLOAT (SXPAR ((*header).ext2, 'R_EXP'))
    ENDIF ELSE BEGIN    ;Reasonable values TBD
        chanOffset = 0
        resFrac    = 0.2
        resExp     = -0.15
    ENDELSE
            
    det = { MFIT_DETECTOR }
    
    det.Detector   = Detector
    det.Display    = Display
    det.registerOnly = 0L
    IF OBJ_VALID(Display.background) THEN $
	     det.haveBack  = Display.background->haveModel() $
    ELSE det.haveBack  = 0 
    det.timeInt    = FLOAT (timeInt)
    det.energyInt  = FLOAT (energyInt)
    det.numChan    = LONG (numChan)
    det.numEbins   = LONG (numEbins)
    det.chanOffset = LONG (chanOffset)  
    det.detIndex   = LONG (myDetNum)
    det.obsCrate   = PTR_NEW (obsCrate)
    det.backCrate  = PTR_NEW (backCrate)
    det.backCsig   = PTR_NEW (backCsig)
    det.liveTime   = PTR_NEW (liveTime)
    det.resp       = PTR_NEW (resp)
    det.fitChan    = LONG (fitChannels)
    det.resFrac    = FLOAT (resFrac)
    det.resExp     = FLOAT (resExp)
    det.respName   = Response->filename()
            
    ;== Add this detector to the list
    
    self.DetectorList->add, filename, det
    self.numDet = self.numDet + 1
    self.TermInfo->adjustEffCorrTerm, self.numDet
    
END


;------------------------------------------------------------------------------
; Delete a detector dataset
;------------------------------------------------------------------------------
PRO Mfit::deleteDetector, filename

    det = self.DetectorList->data (filename, ERROR = error)
    IF (error) THEN $
       RETURN

    IF (NOT det.registerOnly) THEN BEGIN
	    PTR_FREE, det.obsCrate 
	    PTR_FREE, det.backCrate
	    PTR_FREE, det.backCsig 
	    PTR_FREE, det.liveTime 
	    PTR_FREE, det.resp
    ENDIF
   ; STRUCT_FREE, det
            
    self.DetectorList->delete, filename, ERROR = error
    IF (NOT error) THEN $
       self.numDet = self.numDet - 1
       
    self.TermInfo->adjustEffCorrTerm, self.numDet

END


;------------------------------------------------------------------------------
; Check if a detector is in the list
;------------------------------------------------------------------------------
FUNCTION Mfit::inList, filename

    RETURN, self.DetectorList->inList (filename)

END


;------------------------------------------------------------------------------
; Print keys of all loaded detectors
;------------------------------------------------------------------------------
PRO Mfit::printDetectors & self.DetectorList->print & END


;------------------------------------------------------------------------------
; Return number of loaded detectors
;------------------------------------------------------------------------------
FUNCTION Mfit::numDetectors & RETURN, self.numDet & END



;------------------------------------------------------------------------------
; Fit Template: let the user select a model and have MFIT evaluate it:
;------------------------------------------------------------------------------
PRO Mfit::fitTemplate, numChan, fitChan, liveTime, chanEnergy, $
                 chanWidth, model_cnt_rate, status

    status = 0
    ;== Select photon model terms
    
    saveNames = self.PhotonModel->names ()
    self.PhotonModel->clear
    self.PhotonModel->select, FIT_STAT = self.statistic 
    self.statistic = self.PhotonModel->statchoice()
    
    numTerms = self.PhotonModel->count ()
    IF (numTerms LT 1) THEN BEGIN
       IF (numTerms NE N_ELEMENTS(saveNames)) THEN BEGIN
          ;== Check first that there *is* a saved model:
          IF (SIZE(saveNames))[0] NE 0 THEN BEGIN
              ;User has cancelled, restore the saved models:
              self.PhotonModel->setModel, saveNames, dummy
          ENDIF ELSE BEGIN
              status = 1
              RETURN
          ENDELSE
       ENDIF ELSE BEGIN
          MESSAGE, /CONTINUE, 'No model terms selected.'
       ENDELSE
       status = 1
       RETURN
    ENDIF
    
    ;== Number of detectors currently loaded
    numDet = self.DetectorList->count ()
    names  = self.DetectorList->names ()

    IF (numDet LT 1) THEN BEGIN
       MESSAGE, /CONTINUE, 'No detector data available.'
       status = 1
       RETURN
    ENDIF

    ;== Get the data from each detector
    model = self.fitModel->model()
    use_det = model.useDet
    
    jj = WHERE (use_det EQ 1, co)
    IF (co EQ 0) THEN BEGIN
        status = 1
        RETURN        ;== All datasets were cancelled
    ENDIF
        
    numTerms = self.PhotonModel->count ()
    IF (numTerms LT 1) THEN BEGIN
       MESSAGE, /CONTINUE, 'No model terms selected.'
       status = 1
       RETURN
    ENDIF

    ;== Set up model selection arrays
    
    termInfo = self.termInfo->getInfo()
    
    term_used   = LONARR (self.MAX_TERMS + 1)
    param_vary  = LONARR (self.MAX_TERMS, self.MAX_PPT)
    param       = FLTARR (self.MAX_TERMS, self.MAX_PPT)
    param_uncer = FLTARR (self.MAX_TERMS, self.MAX_PPT)
    covar       = FLTARR (self.MAX_PARAM, self.MAX_PARAM)
    alpha       = covar
    param       = *termInfo.param_defaults

    ;== Retrieve a copy of the photon model.  This action returns a
    ;== copy of photon model term objects that must be freed below.
    
    photonModel = self.PhotonModel->model ()

    ;== Set up data arrays for MFIT
    
    ;== Get the maximum number of channels and energy bins
    
    numChan = LONARR (numDet)
    numEbin = LONARR (numDet)
    FOR i = 0, numDet - 1 DO BEGIN   
        numChan[i] = (self.DetectorList->data (names[i])).numChan
        numEbin[i] = (self.DetectorList->data (names[i])).numEbins
    ENDFOR
    maxChan = MAX (numChan)
    maxEbin = MAX (numEbin)

    chanEnergy = FLTARR (maxChan, numDet)
    chanWidth  = FLTARR (maxChan, numDet)
    photEnergy = FLTARR (maxEbin, numDet)
    photWidth  = FLTARR (maxEbin, numDet)
    drm        = FLTARR (maxEbin, maxChan, numDet)
    
    obsCrate   = FLTARR (maxChan, numDet)
    backCrate  = FLTARR (maxChan, numDet)
    backCsig   = FLTARR (maxChan, numDet)
    liveTime   = FLTARR (maxChan, numDet)
    
    resFrac    = FLTARR (numDet)
    resExp     = FLTARR (numDet)
    fitChan    = LONARR (2, numDet)        
    chanOffset = LONARR (numDet)
    
    FOR i = 0, numDet - 1 DO BEGIN   
    
        nChan1 = numChan[i] - 1 
        nEbin1 = numEbin[i] - 1

        resp = *(self.DetectorList->data (names[i])).resp        
                
        chanEnergy[0:nChan1, i] = resp.chan_energy 
        chanWidth [0:nChan1, i] = resp.chan_width
        photEnergy[0:nEbin1, i] = resp.phot_energy 
        photWidth [0:nEbin1, i] = resp.phot_width 
        drm[0:nEbin1, 0:nChan1, i] = resp.drm 
    
        obsCrate[0:nChan1, i]  = *(self.DetectorList->data (names[i])).obsCrate
        backCrate[0:nChan1, i] = *(self.DetectorList->data (names[i])).backCrate
        backCsig[0:nChan1, i]  = *(self.DetectorList->data (names[i])).backCsig
        liveTime[0:nChan1, i]  = *(self.DetectorList->data (names[i])).liveTime

        resFrac[i]    = (self.DetectorList->data (names[i])).resFrac
        resExp[i]     = (self.DetectorList->data (names[i])).resExp        
        fitChan[*, i] = (self.DetectorList->data (names[i])).fitChan
        chanOffset[i] = 0;(self.DetectorList->data (names[i])).chanOffset        
    
    ENDFOR

    ;== More input variables
    
    rel_converg       = 0.001
    abs_converg       = 0.01
    max_tries         = 90L
    enable_undet      = 0L

    num_terms_avail   =  termInfo.num_terms_avail 
    num_add_terms     =  termInfo.num_add_terms
    nt_add_line       =  termInfo.nt_add_line
    num_param_of_term = *termInfo.num_param_of_term
    rel_change_limit  = *termInfo.rel_change_limit
    abs_change_limit  = *termInfo.abs_change_limit
    undet_priority    = *terminfo.undet_priority
    undet_rel_req     = *terminfo.undet_rel_req
    undet_abs_req     = *terminfo.undet_abs_req
    high_ud_priority  = LONG(MAX(undet_priority))


    ;== Output variables (must be predefined for the DLM call)
    
    use_max_plot = 6*maxChan + 1
    
    have_fit               = 1L
    fit_err                = 0L
    model_chisq            = 0.0
    dof                    = 0L
    num_vary               = 0L
    model_cnt_rate         = FLTARR (maxChan, self.MAX_TERMS + 1, numDet)
    phot_obs_rate          = FLTARR (maxChan, numDet)
    phot_obs_sig           = FLTARR (maxChan, numDet)
    nu_f_nu_data           = FLTARR (maxChan, numDet)
    nu_f_nu_sig            = FLTARR (maxChan, numDet)
    nu_f_nu_model          = FLTARR (use_max_plot, numDet)
    model_energy           = FLTARR (use_max_plot, numDet)
    model_phot_rate        = FLTARR (use_max_plot, self.MAX_TERMS + 1, numDet)
    model_phot_rate_bychan = FLTARR (maxChan, numDet)
    model_cr_vari          = FLTARR (maxChan, numDet)
    data_cr_vari           = FLTARR (maxChan, numDet)
    mfit_ver_string        = "hello"

    ;== Perform the fit

    have_model   = 1L
    term_used[0] = 1L
        
    ;== First get the term and parameter names:
    FOR i = 0, numTerms - 1 DO BEGIN
        
        idx = photonModel[i]->index ()
        np  = photonModel[i]->nParams ()                 
        p   = photonModel[i]->params ()

        term_used[idx] = 1L
           
        ;== Set the default values
        param[idx - 1, 0: np - 1] = p.value
        param_vary[idx - 1, 0: np - 1] = LONG (0)

    ENDFOR

    ;== Free the copy of the photon model
    
    FOR i = 0, numTerms - 1 DO $
         OBJ_DESTROY, photonModel[i]

	MFIT, $ 
        self.statistic + 1L,                                          $
		numDet,            numChan,           numEbin,                $
		;maxChan,           maxEbin,
		chanEnergy,             $
		chanWidth,         obsCrate,          backCrate,              $
		backCsig,          liveTime,          photEnergy,             $
		photWidth,         drm,               resFrac,                $
		resExp,            param_vary,        fitChan,                $
		use_det,           term_used,         num_terms_avail,        $
		num_add_terms,     nt_add_line,       num_param_of_term,      $
		rel_change_limit,  abs_change_limit,  rel_converg,            $
		abs_converg,       max_tries,                                 $
        enable_undet,      undet_priority,    undet_rel_req,          $
        undet_abs_req,     high_ud_priority,                          $
        chanOffset,             $
        param,             $ ;have_model,        have_fit,               $
        fit_err,           model_chisq,       dof,                    $
        num_vary,          param_uncer,       alpha,   covar,         $
		model_cnt_rate,    phot_obs_rate,     phot_obs_sig,           $
		nu_f_nu_data,      nu_f_nu_sig,       nu_f_nu_model,          $
		model_energy,      model_phot_rate,   model_phot_rate_bychan, $
		model_cr_vari,     data_cr_vari,      mfit_ver_string
    
    ;== Restore the fit!
    self.PhotonModel->setModel, saveNames, dummy
    
END

;------------------------------------------------------------------------------
; Map out the chi square region for one or more parameters of interest
;------------------------------------------------------------------------------
PRO Mfit::fitChiSq, theSelection, theRanges, numSteps, displaySigma, numInterest, $
         CHI1D = chi1D, CHI2D = chi2d, INTERVAL = interval, _EXTRA = extra

    junk = WHERE (theSelection, numSelected)
    IF (numSelected LT 1) THEN BEGIN
       MESSAGE, /CONTINUE, 'No parameters were selected.'
       RETURN
    ENDIF

    IF KEYWORD_SET (chi1d) THEN BEGIN
    	IF KEYWORD_SET(interval) THEN BEGIN
			chiValues = FLTARR (numSteps, numSelected)
			paramValues = FLTARR (numSteps, numSelected)
    	ENDIF ELSE BEGIN
			chiValues = FLTARR (numSteps)
			paramValues = FLTARR (numSteps)
    	ENDELSE
    ENDIF ELSE BEGIN
        chiValues = FLTARR (numSteps, numSteps)
        paramValues = FLTARR (numSteps, 2)
    ENDELSE
    
    ;== Number of detectors currently loaded
    numDet = self.DetectorList->count ()
    names  = self.DetectorList->names ()

    IF (numDet LT 1) THEN BEGIN
       MESSAGE, /CONTINUE, 'No detector data available.'
       RETURN
    ENDIF

    ;== Get the data from each detector
    model = self.fitModel->model()
    use_det = model.useDet
    
    jj = WHERE (use_det EQ 1, co)
    IF (co EQ 0) THEN RETURN        ;== All datasets were cancelled
        
    numTerms = self.PhotonModel->count ()
    IF (numTerms LT 1) THEN BEGIN
       MESSAGE, /CONTINUE, 'No model terms selected.'
       RETURN
    ENDIF

    ;== Set up model selection arrays
    
    termInfo = self.termInfo->getInfo()
    
    term_used   = LONARR (self.MAX_TERMS + 1)
    param_vary  = LONARR (self.MAX_TERMS, self.MAX_PPT)
    param       = FLTARR (self.MAX_TERMS, self.MAX_PPT)
    param_uncer = FLTARR (self.MAX_TERMS, self.MAX_PPT)
    covar       = FLTARR (self.MAX_PARAM, self.MAX_PARAM)
    alpha       = covar
    param       = *termInfo.param_defaults

    ;== Retrieve a copy of the photon model.  This action returns a
    ;== copy of photon model term objects that must be freed below.
    
    photonModel = self.PhotonModel->model ()

    ;== Preliminary header text for Fit Log
    msg = [' ', STRING (REPLICATE (BYTE ('-'), 100))]
    self.Log->append, msg

    FOR i = 0, numDet - 1 DO BEGIN
   
        det = self.DetectorList->data (names[i])
                
        ts = STRCOMPRESS (STRING (det.timeInt, $
            FORMAT = '(F10.3, ": ", F10.3, " s")'))
        
        es = STRCOMPRESS (STRING (det.energyInt, $
            FORMAT = '(G, ": ", G, " keV")'))
    
        cs = STRCOMPRESS (STRING (det.fitChan, $
            FORMAT = '(I, ": ", I)'))
    
        detStatus = (use_det[i] EQ 1) ? ' INCLUDED' : ' OMITTED'
        msg = [ $
            '==> Dataset      : #' + STRTRIM (i, 2) + detStatus, $
            '==> Data file    : ' + names[i], $
            '==> Response file: ' + det.respName, $
            '==> Fit interval : ' + ts + ', ' + es + ', channels ' + cs, $
            ' ']
        self.Log->append, msg

    ENDFOR


    ;== Set up data arrays for MFIT
    
    ;== Get the maximum number of channels and energy bins
    
    numChan = LONARR (numDet)
    numEbin = LONARR (numDet)
    FOR i = 0, numDet - 1 DO BEGIN   
        numChan[i] = (self.DetectorList->data (names[i])).numChan
        numEbin[i] = (self.DetectorList->data (names[i])).numEbins
    ENDFOR
    maxChan = MAX (numChan)
    maxEbin = MAX (numEbin)

    chanEnergy = FLTARR (maxChan, numDet)
    chanWidth  = FLTARR (maxChan, numDet)
    photEnergy = FLTARR (maxEbin, numDet)
    photWidth  = FLTARR (maxEbin, numDet)
    drm        = FLTARR (maxEbin, maxChan, numDet)
    
    obsCrate   = FLTARR (maxChan, numDet)
    backCrate  = FLTARR (maxChan, numDet)
    backCsig   = FLTARR (maxChan, numDet)
    liveTime   = FLTARR (maxChan, numDet)
    
    resFrac    = FLTARR (numDet)
    resExp     = FLTARR (numDet)
    fitChan    = LONARR (2, numDet)        
    chanOffset = LONARR (numDet)
    
    FOR i = 0, numDet - 1 DO BEGIN   
    
        nChan1 = numChan[i] - 1 
        nEbin1 = numEbin[i] - 1

        resp = *(self.DetectorList->data (names[i])).resp        
                
        chanEnergy[0:nChan1, i] = resp.chan_energy 
        chanWidth [0:nChan1, i] = resp.chan_width
        photEnergy[0:nEbin1, i] = resp.phot_energy 
        photWidth [0:nEbin1, i] = resp.phot_width 
        drm[0:nEbin1, 0:nChan1, i] = resp.drm 
    
        obsCrate[0:nChan1, i]  = *(self.DetectorList->data (names[i])).obsCrate
        backCrate[0:nChan1, i] = *(self.DetectorList->data (names[i])).backCrate
        backCsig[0:nChan1, i]  = *(self.DetectorList->data (names[i])).backCsig
        liveTime[0:nChan1, i]  = *(self.DetectorList->data (names[i])).liveTime

        resFrac[i]    = (self.DetectorList->data (names[i])).resFrac
        resExp[i]     = (self.DetectorList->data (names[i])).resExp        
        fitChan[*, i] = (self.DetectorList->data (names[i])).fitChan
        chanOffset[i] = 0;(self.DetectorList->data (names[i])).chanOffset        
    
    ENDFOR

    ;== More input variables
    
    rel_converg       = 0.001
    abs_converg       = 0.01
    max_tries         = 90L
    enable_undet      = 0L

    num_terms_avail   =  termInfo.num_terms_avail 
    num_add_terms     =  termInfo.num_add_terms
    nt_add_line       =  termInfo.nt_add_line
    num_param_of_term = *termInfo.num_param_of_term
    rel_change_limit  = *termInfo.rel_change_limit
    abs_change_limit  = *termInfo.abs_change_limit
    undet_priority    = *terminfo.undet_priority
    undet_rel_req     = *terminfo.undet_rel_req
    undet_abs_req     = *terminfo.undet_abs_req
    high_ud_priority  = LONG(MAX(undet_priority))

    ;== Output variables (must be predefined for the DLM call)
    
    use_max_plot = 6*maxChan + 1
    
    have_fit               = 1L
    fit_err                = 0L
    model_chisq            = 0.0
    dof                    = 0L
    num_vary               = 0L
    model_cnt_rate         = FLTARR (maxChan, self.MAX_TERMS + 1, numDet)
    phot_obs_rate          = FLTARR (maxChan, numDet)
    phot_obs_sig           = FLTARR (maxChan, numDet)
    nu_f_nu_data           = FLTARR (maxChan, numDet)
    nu_f_nu_sig            = FLTARR (maxChan, numDet)
    nu_f_nu_model          = FLTARR (use_max_plot, numDet)
    model_energy           = FLTARR (use_max_plot, numDet)
    model_phot_rate        = FLTARR (use_max_plot, self.MAX_TERMS + 1, numDet)
    model_phot_rate_bychan = FLTARR (maxChan, numDet)
    model_cr_vari          = FLTARR (maxChan, numDet)
    data_cr_vari           = FLTARR (maxChan, numDet)
    mfit_ver_string        = "hello"

    chisq_save = model.chisq
    dof_save   = model.dof

    ;== Perform the fit
 
    self.Log->append, '==> Performing Chi^2 Mapping...' 
;    self.Log->append, ' '
    
    found = 0
    foundIDX = WHERE (theSelection, countFound)
    
    ;== If somehow we have an invalid number of terms selected:
    ;IF (countFound GT 2) THEN foundIDX = foundIDX[0:1]
    
    have_model   = 1L
    term_used[0] = 1L
        
    termInfo = self.termInfo->getInfo()
    allowed_sign = *(termInfo.allowed_sign)
    
    IF KEYWORD_SET(interval) THEN BEGIN
    	errorIntNames = STRARR(numSelected)
    	errorIntBestP = FLTARR(numSelected)
    	idx1Arr = INTARR(numSelected)
    	j1Arr   = INTARR(numSelected)
    ENDIF
    
    ;== First get the term and parameter names:
    FOR i = 0, numTerms - 1 DO BEGIN
        
        idx = photonModel[i]->index ()
        np  = photonModel[i]->nParams ()                 
        p   = photonModel[i]->params ()

        term_used[idx] = 1L
           
        ;== Set the default values; the changing ones we will do below
        param[idx - 1, 0: np - 1] = model.param[idx - 1, 0: np - 1]
        param_vary[idx - 1, 0: np - 1] = LONG ((p.fixed) EQ 0)

        FOR j = 0, np - 1 DO BEGIN
            IF (theSelection[i, j] EQ 1) THEN BEGIN
                found = found + 1
                 
                tempName = STRTRIM (photonModel[i]->name ()) + ': '
                tempName = tempName + STRTRIM (p[j].name, 2)
                tempJunk = STRTRIM (p[j].units, 2)            ;May be dimensionless
                IF (tempJunk NE '') THEN tempName = tempName + ' (' + tempJunk + ')'
                
                ;We will need these later; NB: no checking if found > 2!
                IF KEYWORD_SET (INTERVAL) THEN BEGIN
					IF (found GE 1) THEN BEGIN
						errorIntNames[found - 1] = tempName 
						errorIntBestP[found - 1] = model.param[idx - 1, j]
						self.param1dName = tempName 
						self.bestChi1DParam = model.param[idx - 1, j]
						;== Some values *must* be positive!
						IF (allowed_sign[idx - 1, j] EQ 'P') THEN $
							theRanges[i, j, *] = theRanges[i, j, *] > 0.0
						fixValue = theRanges[i, j, 0] + $
								  (theRanges[i, j, 1] - theRanges[i, j, 0]) * $
								  FINDGEN (numSteps) / numSteps
						paramValues[*, found - 1] = fixValue
						idx1Arr[found - 1] = idx
						j1Arr[found - 1]   = j
					ENDIF
                ENDIF ELSE BEGIN
					IF (found EQ 1) THEN BEGIN
						self.param1dName = tempName 
						self.bestChi1DParam = model.param[idx - 1, j]
						;== Some values *must* be positive!
						IF (allowed_sign[idx - 1, j] EQ 'P') THEN $
							theRanges[i, j, *] = theRanges[i, j, *] > 0.0
						fixValue = theRanges[i, j, 0] + $
								  (theRanges[i, j, 1] - theRanges[i, j, 0]) * $
								  FINDGEN (numSteps) / numSteps
						paramValues[*, 0] = fixValue
						idx1 = idx
						j1   = j
					ENDIF ELSE BEGIN
						self.param2dName = tempName
						self.bestChi2DParam = model.param[idx - 1, j]
						fixValue = theRanges[i, j, 0] + $
								  (theRanges[i, j, 1] - theRanges[i, j, 0]) * $
								  FINDGEN (numSteps) / numSteps
						paramValues[*, 1] = fixValue
						idx2 = idx
						j2   = j
					ENDELSE
                ENDELSE

                ;self.Log->append, 'TERM: ' + tempName
                
            ENDIF
        ENDFOR
    ENDFOR
    
    param_vary_save = param_vary

    self.Log->show

    ;== We need to process the chi2d as a double loop
    IF KEYWORD_SET (chi1d) THEN numYSteps = 1 $
                           ELSE numYSteps = numSteps

    ;== We need to process the multiple error intervals as a double loop
    IF KEYWORD_SET (interval) THEN numYSteps = numSelected
        
    valSelect = 0
    FOR l = 0, numYSteps - 1 DO BEGIN
    
		IF KEYWORD_SET (chi2d) THEN BEGIN
            self.Log->append, 'STEP' + STRTRIM (STRING (l)) + ':' + STRING (l * numSteps)
            ENDIF

        IF KEYWORD_SET (INTERVAL) THEN BEGIN
        	idx1 = idx1Arr[l]
        	j1   = j1Arr[l]
        	valSelect = l
        ENDIF
	
        FOR k = 0, numSteps - 1 DO BEGIN

            ; 3/31/03 RDP: reset the parameter values to original fit values:
            param = model.param
            param_vary = param_vary_save
	
            IF KEYWORD_SET (chi2d) THEN BEGIN
                param[idx2 - 1, j2] = paramValues[l, 1]
                param_vary[idx2 - 1, j2] = LONG (0)
                ;self.Log->append, 'STEP' + STRTRIM (STRING (l)) + ':' + STRING (l * numSteps)
                ENDIF
    
            param[idx1 - 1, j1] = paramValues[k, valSelect]
            param_vary[idx1 - 1, j1] = LONG (0)

            REPEAT BEGIN
                MFIT, $ 
                self.statistic + 1L,                                          $
                numDet,            numChan,           numEbin,                $
                ;maxChan,           maxEbin,
                chanEnergy,             $
                chanWidth,         obsCrate,          backCrate,              $
                backCsig,          liveTime,          photEnergy,             $
                photWidth,         drm,               resFrac,                $
                resExp,            param_vary,        fitChan,                $
                use_det,           term_used,         num_terms_avail,        $
                num_add_terms,     nt_add_line,       num_param_of_term,      $
                rel_change_limit,  abs_change_limit,  rel_converg,            $
                abs_converg,       max_tries,                                 $
                enable_undet,      undet_priority,    undet_rel_req,          $
                undet_abs_req,     high_ud_priority,                          $
                chanOffset,             $
				param,             $ ;have_model,        have_fit,               $
				fit_err,           model_chisq,       dof,                    $
				num_vary,          param_uncer,       alpha,   covar,         $
                model_cnt_rate,    phot_obs_rate,     phot_obs_sig,           $
                nu_f_nu_data,      nu_f_nu_sig,       nu_f_nu_model,          $
                model_energy,      model_phot_rate,   model_phot_rate_bychan, $
                model_cr_vari,     data_cr_vari,     mfit_ver_string

            ENDREP UNTIL (NOT (KEYWORD_SET (batch) AND (fit_err EQ 1)))
    
            chiValues[k, l] = model_chisq
        ENDFOR
    ENDFOR

    self.Log->append, '==> Map completed at ' + SYSTIME () 
             
    ;== Safely arrived here; store the results
    IF KEYWORD_SET (chi1d) THEN BEGIN
        self.haveChi1D = 1
        self.haveChi2D = 0
    ENDIF ELSE BEGIN
        self.haveChi1D = 0
        self.haveChi2D = 1
    ENDELSE
    
    IF KEYWORD_SET(INTERVAL) THEN BEGIN
;		sigLevels = FLTARR (10)
		sigLevels = [1.00,  4.00,  9.00]
		
;		nineLevels = FLTARR (10)
		nineLevels = [ 2.70,  6.63,  10.83]
		delta_chi = displaySigma EQ 0 ? sigLevels[numInterest] : nineLevels[numInterest]
		print, "Delta Chi^sq", delta_chi
		nineLabels = [' 90%', ' 99%', ' 99.9%']
		info_str = displaySigma EQ 0 ? '(' + STRTRIM(STRING(numInterest+1),2) + ' Sigma)' : $
		                               '(' + nineLabels[numInterest] + ')'
		
		foundAgain = 0
		idx1 = idx1Arr[foundAgain]
		j1   = j1Arr[foundAgain]
		
       FOR i = 0, numTerms - 1 DO BEGIN

           term = photonModel[i]

           termName   = term->name ()        
           index      = term->index ()
           nParams    = term->nParams ()
           termParams = term->params ()
           
           msg = [' ', 'TERM: ' + termName, ' ']
           self.Log->append, msg
           
           FOR j = 0, nParams - 1 DO BEGIN

               name  = termParams[j].name
               units = termParams[j].units

               value = model.param[index - 1, j]
               vary  = (param_vary[index - 1, j] EQ 1) ? 'VARY' : ' FIX'
               IF (idx1 EQ index AND j1 EQ j) THEN BEGIN
                   ;value = errorIntBestP[foundAgain]
                   minchi = chisq_save + delta_chi
                   temp_chi = MIN(chivalues[*, foundAgain] ,minind)
                   IF (minind NE 0) THEN BEGIN
					   result=interpol(paramvalues[0:minind-1, foundAgain],chivalues[0:minind-1, foundAgain],[minchi])
					   error_lo = ABS(result[0] - value)
                   ENDIF ELSE BEGIN
					   error_lo = 0.0
                   ENDELSE
                   IF (minind NE numSteps - 1) THEN BEGIN
					   result=interpol(paramvalues[minind:*, foundAgain],chivalues[minind:*, foundAgain],[minchi])
					   error_hi = ABS(result[0] - value)
                   ENDIF ELSE BEGIN
					   error_hi = 0.0
                   ENDELSE
                   foundAgain++
                   IF foundAgain LT N_ELEMENTS(idx1Arr) THEN BEGIN
						idx1 = idx1Arr[foundAgain]
						j1   = j1Arr[foundAgain]
				   ENDIF
				   msg = STRING (name, 'INTRVL', value, error_lo, error_hi, units, info_str, $
					   FORMAT = '(A20, A6, G12.4, " -   ", G12.3, " + ", G12.3, 1X, A, 1X, A)')
               ENDIF ELSE BEGIN
	               error = model.param_uncer[index - 1, j]
				   msg = STRING (name, vary, value, error, units, $
					   FORMAT = '(A20, 1X, A4, 1X, G12.4, " +/- ", G12.3, 1X, A)')
               ENDELSE

               self.Log->append, msg

           ENDFOR 
       ENDFOR   
    
       CASE (self.statistic) OF
       
		   0: BEGIN
			   chiProb = 1.d - chisqr_pdf(chisq_save, dof_save)
			   cs = STRCOMPRESS (STRING (chisq_save, dof_save, chisq_save / dof_save, chiProb, $ 
			   FORMAT = '("CHISQ = ", G12.5, ", DOF = ", I, ", REDUCED CHISQ = ", G12.7,", PROB = ", G12.4)'))
		   END
		   
		   1: BEGIN
			   cs = STRCOMPRESS (STRING (chisq_save, dof_save, $ 
			   FORMAT = '("-2 LOG (LIKELIHOOD) = ", G12.5, ", DOF = ", I)'))
		   END
		   
		   2: BEGIN
			   chiProb = 1.d - chisqr_pdf(chisq_save, dof_save)
			   cs = STRCOMPRESS (STRING (chisq_save, dof_save, chisq_save / dof_save, chiProb, $ 
			   FORMAT = '("Castor C-STAT  = ", G12.5, ", DOF = ", I, ", REDUCED C-STAT = ", G12.7,", PROB = ", G12.4)'))
		   END
       
       ENDCASE
       
       msg = [ $
           ' ', $
           '==> ' + cs]
       self.Log->append, msg
       
	   ;== 'Photon Flux (ph/s-cm^2)'
	   energyInt = self.display->get_fluenceInt()
	   ps = STRCOMPRESS (STRING (self.pFlux, self.pErr, energyInt[0], energyInt[1], $ 
	   FORMAT = '("Photon Flux = ", G12.5, " +/- ", G12.2, " ph/s-cm^2 in the interval:", G12.4, ": ", G12.5, " keV")'))
	   msg = '==> ' + ps
	   self.Log->append, msg
   
	   ;== 'Energy Flux (erg/s-cm^2)'
	   ps = STRCOMPRESS (STRING (self.eFlux, self.eErr, energyInt[0], energyInt[1], $ 
	   FORMAT = '("Energy Flux = ", G12.5, " +/- ", G12.2, " erg/s-cm^2 in the interval:", G12.4, ": ", G12.5, " keV")'))
	   msg = ['==> ' + ps, ' ']
	   self.Log->append, msg       

    ENDIF
    
    PTR_FREE, self.chi1DVals
    PTR_FREE, self.chi1DParm
    self.chi1DVals = PTR_NEW (chiValues)
    self.chi1DParm = PTR_NEW (paramValues)
    
    IF (NOT KEYWORD_SET (Batch)) THEN BEGIN
	    ;== Free up the plot range info:
	    
	    myDisplay = self.display
	    myPlotter = myDisplay.plotter
	    rangeListCount = myPlotter.rangeList->count()
	    IF (rangeListCount NE 0) THEN myPlotter.rangeList->clear
    ENDIF

    ;== Free the copy of the photon model
    
    FOR i = 0, numTerms - 1 DO $
         OBJ_DESTROY, photonModel[i]

END

;------------------------------------------------------------------------------
; Fit the user model to several data intervals in a batch sequence
;------------------------------------------------------------------------------
PRO Mfit::fitBatch, status, BATCHPLOT = batchPlot, _EXTRA = extra
        
    ;== Need to communicate failure back to the caller:
    status = 0
    
    ;== Number of detectors currently loaded
    
    numDet = self.DetectorList->count ()
    names  = self.DetectorList->names ()

    IF (numDet LT 1) THEN BEGIN
       MESSAGE, /CONTINUE, 'No detector data available.'
       status = 1
       RETURN
    ENDIF

    ;== Get the span data from each detector
    
    numTimes   = INTARR (numDet)
    resolution = FLTARR (numDet)
    ;det        = PTRARR (numDet)
    display    = OBJARR (numDet)
    lightCurve = OBJARR (numDet)
    doRebin    = 0
    
    ;== Save the flux info:
	pFlux_Save = self->getPFlux()    
	eFlux_Save = self->getEFlux()    
	pErr_Save  = self->getPErr()    
	eErr_Save  = self->getEErr()    
	bFlux_Save = self->getBFlux()    
	dFlux_Save = self->getDFlux()    
	bErr_Save  = self->getBErr()    
	dErr_Save  = self->getDErr()    
    
    FOR i = 0, numDet - 1 DO BEGIN
   
        det = self.DetectorList->data (names[i])
        display[i] = det.display
        lightCurve[i] = (display[i]).lightcurve
        theSpan = lightcurve[i]->timeSpan()
        combTimes = lightcurve[i]->combinedTimes (/POINTER)
        COUNT_SPAN, *combTimes, theSpan, tidx
        ntTemp = N_ELEMENTS (tidx)

        IF (ntTemp EQ 1) THEN BEGIN
            MESSAGE, /CONTINUE, 'Only one time bin selected.'
            status = 1
            RETURN
        ENDIF
        
        ;== We need a hint for rebinning each dataset, should it be necessary
        numTimes[i] = ntTemp
        tempTimes = (*combTimes)[1, tidx] - (*combTimes)[0, tidx]
        resolution[i] = MIN (tempTimes) / 2.
        
        ;== Test to see that all datasets have the same number of selections
        IF (i GT 0) THEN BEGIN
            IF (ntTemp NE numTimes[0]) THEN BEGIN
                doRebin = 1
            ENDIF
        ENDIF
    ENDFOR
    
    ;== We need to rebin some of the data sets
    IF doRebin THEN BEGIN
        msg = ['Each dataset must have the same number of time selections. ', $
              'If RMFIT tries to find a common time binning, ', $
              'it will change the time binnings of some of your datasets. ', $
              'Shall RMFIT attempt to correct this?']
        ans = DIALOG_MESSAGE( msg, /QUESTION, TITLE = 'MFIT Message')
        
        IF (ans EQ 'No') THEN BEGIN
            status = 1
            RETURN
        ENDIF
        
        ;== Get the index of the dataset with the minimum number of time bins
        minBins = MIN (numTimes, target)
        
        ;== We should bin to the dataset with the minimum number of bins
        targetSpan = lightcurve[target]->timeSpan()
        targetTimes = lightcurve[target]->combinedTimes (/POINTER)
        COUNT_SPAN, *targetTimes, targetSpan, targetIDX
        
        hitMe = INTARR (numDet)
        
        FOR i = 0, minBins - 1 DO BEGIN
            targetInterval = (*targetTimes)[*, targetIDX[i]]
            FOR j = 0, numDet - 1 DO BEGIN
                ;== Skip the target dataset
                IF (j NE target) THEN BEGIN
                    ;== Do only those datasets that need rebinning
                    IF (numTimes[j] NE minBins) THEN BEGIN
                        display[j]->map
                        
                        ;== Some things should be done only once
                        IF (i EQ 0) THEN BEGIN
                            display[j]->setPlotDomain, 'TIME'
                            display[j]->rebin, /fullresolution
                        ENDIF
                        myInterval = [targetInterval[0] + resolution[j], $
                                      targetInterval[1] - resolution[j]]
                        display[j]->rebin, haveInterval = myInterval
                        
                        ;== Revisit this detector later to set the selection
                        hitMe[j] = 1
                    ENDIF
                ENDIF
            ENDFOR  ;j
           
        ENDFOR      ;i
        
        ;== Finally, set up the new selections
        FOR i = 0, numDet - 1 DO BEGIN
            IF hitMe[i] THEN BEGIN
                display[i]->map
                display[i]->selectSource, HAVESELECTION = targetSpan
            ENDIF
        ENDFOR
    ENDIF
        
    ;== Test to see that all datasets have the same number of selections
    ;We do this a second time, in case the binning algorithm messed up...
    FOR i = 0, numDet - 1 DO BEGIN
        theSpan = lightcurve[i]->timeSpan()
        combTimes = lightcurve[i]->combinedTimes (/POINTER)
        COUNT_SPAN, *combTimes, theSpan, tidx
        numTimes[i] = N_ELEMENTS (tidx)
    ENDFOR
    
    IF (MAX (numTimes) NE MIN (numTimes)) THEN BEGIN
        msg = 'Each dataset must have the same number ' +$
              'of time selections:' + STRING(FIX (MIN (numtimes)))
        ans = DIALOG_MESSAGE( msg, /ERROR, TITLE = 'MFIT Error')
        status = 1
        RETURN
    ENDIF
        
    FOR i = 0, numDet - 1 DO BEGIN
   
        ;== First time through, create history arrays
        ntTemp = MIN (numTimes)
        IF (i EQ 0) THEN BEGIN      
            burstHistory = FLTARR (ntTemp, numDet)
            burstHistErr = FLTARR (ntTemp, numDet)
        ENDIF
        
        ;== Stuff the data for batch fitting
        theSpan = lightcurve[i]->timeSpan()
        combTimes = lightcurve[i]->combinedTimes (/POINTER)
        COUNT_SPAN, *combTimes, theSpan, tidx
        
        combHistory = lightcurve[i]->combinedHist (/POINTER)
        burstHistory[*, i] = (*combHistory)[tidx]

        combHistErr = lightcurve[i]->combinedHistErr (/POINTER)
        burstHistErr[*, i] = (*combHistErr)[tidx]
        
    ENDFOR
    
    ;== Select photon model terms
    numTerms = self.PhotonModel->count ()
    termNames = self.PhotonModel->names ()
    IF (numTerms LT 1) THEN BEGIN
       self.PhotonModel->clear
       self.PhotonModel->select, FIT_STAT = self.statistic 
       self.statistic = self.PhotonModel->statchoice()
       numTerms = self.PhotonModel->count ()
       ;== This could always happen:
       IF (numTerms LT 1) THEN BEGIN
           self.display->setStatus, 'No model terms were selected.', 5
           status = 1
           RETURN
       ENDIF
    ENDIF
    
    ;== Get rid of any current batch fit, if necessary
    numFit = self.BatchFitList->count ()
    fitnames  = self.BatchFitList->names ()
    
    FOR i = 0, numFit - 1 DO BEGIN
   
        fit = self.BatchFitList->data (fitnames[i])
        STRUCT_FREE, fit
    
    ENDFOR
    OBJ_DESTROY, self.BatchFitList
    self->haveBatch, 0L
    
    ;== Retrieve a copy of the photon model.  This action returns a
    ;== copy of photon model term objects that must be freed below.
    
    photonModel = self.PhotonModel->model ()

    totalParms = 0
    FOR i = 0, numTerms - 1 DO BEGIN
        np  = photonModel[i]->nParams ()                 
        totalParms = totalParms + np
    ENDFOR   
    paramArr = FLTARR (totalParms)
    perrsArr = FLTARR (totalParms)
    
    ;== Make sure that the log doesn't fill up
    myText = self.Log->getText()
    lineNo = N_ELEMENTS (myText)
    IF (lineNo GT 500) THEN BEGIN
         myText = myText[lineNo - 499: *]
         self.Log->clear
         self.Log->append, myText
    ENDIF
              
    ;== Ready to start fitting
    
    self.BatchFitList = OBJ_NEW ('List')
    
    ;== Save the last fit model and info
    dataSaved = 0
    IF self.fitModel->haveFit () THEN BEGIN
        tmodel = self.fitModel->model()
        tFitInfo = *self.fitModelInfo
        self.PhotonModel->setModel, termNames, photonModel
        dataSaved = 1
    ENDIF
    
    self.Log->setClearButton, 'Cancel', /MANAGED
    myLog = self.log
    myClearID = WIDGET_INFO (myLog.clearID, FIND_BY_UNAME = 'WIDGET_LOGGER_CLEARID')
    IF (myClearID EQ 0) THEN $
       MESSAGE, 'Failed to find WIDGET_LOGGER id in widget hierarchy.'
    
	self.Log->append, ['', STRING(REPLICATE(BYTE('-'), 100)), $
	                   '==> Batch Fitting: Fit Results are Suppressed.', $
	                   '==> Issue the command: "Fit Results:"->"Dump Results to Log" to see the results.','']

	energyInt = self.display->get_fluenceInt()
	
    FOR k = 0, numTimes[0] - 1 DO BEGIN
    
        fitTimes = FLTARR (2, numDet)
        fitEnergies = FLTARR (2, numDet)
 
        ;== Call MFIT for batch fitting
        ; Print spectrum number on the log for each time interval:
        sp_count = k + 1
        self.Log->append, ['Spectrum #' + STRTRIM(sp_count, 1)]
        self->fitModel, /BATCH, ENERGYINT = energyInt, SELECTBYINDEX = k
        
        FOR i = 0, numDet - 1 DO BEGIN
           det = self.DetectorList->data (names[i])
           fitTimes[*, i] = det.timeInt
           fitEnergies[*, i] = det.energyInt
        ENDFOR

        fitName = 'BATCH' + STRTRIM (STRING (K), 2)
        model = self.FitModel->Model()
        
        ;== Try getting 'Cancel' Events
        event = WIDGET_EVENT(myClearID, /NOWAIT)
        IF (event.id EQ myClearID) THEN BEGIN
            ;== Cancelling! Clear everything up!
            self.fitModel->setHaveFit, 0
            ;== Copy previous fit results to the Model structure
            IF dataSaved THEN BEGIN
                self.FitModel->setModel, $
                     
                tmodel.numDet,       tmodel.havemodel,   tmodel.havefit,    tmodel.chisq,     $
                tmodel.dof,          tmodel.fitErr,      tmodel.useDet,     tmodel.term_used, $
                tmodel.fitChannels,  tmodel.numChan,     tmodel.chanEnergy, tmodel.chanWidth, $
                tmodel.obsCrate,     tmodel.backCrate,   tmodel.backCsig,   tmodel.liveTime,  $
                tmodel.modelCntRate, tmodel.photObsRate, tmodel.photObsSig,                   $
                tmodel.nuFNuData,    tmodel.nuFNuSig,    tmodel.nuFNuModel,                   $
                tmodel.modelEnergy,  tmodel.modelPhotRate, tmodel.modelPhotRateByChan,        $
                tmodel.modelCRVari,  tmodel.dataCRVari,  tmodel.names,      tmodel.param,     $
                tmodel.param_uncer
        
                PTR_FREE, self.fitModelInfo
                self.fitModelInfo = PTR_NEW (tFitInfo)
                self.PhotonModel->setModel, termNames, photonModel
                
                ;== Set up the 'previous' data. Only selects the integrated data selection, 
                ;if the user has done anything else, we're not going to reflect it...
                FOR i = 0, numDet - 1 DO BEGIN
           
                    det = self.DetectorList->data (names[i])
                    det.display->getSpectra, cancel, /SELECTIONS
                
                ENDFOR
    
				;== Restore the flux info:
				self.PFlux = pFlux_Save    
				self.EFlux = eFlux_Save    
				self.PErr  = pErr_Save    
				self.EErr  = eErr_Save    
				self.BFlux = bFlux_Save     
				self.DFlux = dFlux_Save     
				self.BErr  = bErr_Save     
				self.DErr  = dErr_Save     
                
            ENDIF
            
            self.Log->setClearButton, 'Clear'
        
            FOR i = 0, numTerms - 1 DO $
                OBJ_DESTROY, photonModel[i]
                
            status = 1
            
            RETURN

        ENDIF

        ;== If the user cancelled the Dataset choice dialog, then no model has been
        ; fit. In that case, self.FitModel->Model() returns 0, not a valid structure.
        IF ((SIZE (model))[0] EQ 0) OR (NOT self.FitModel->haveFit()) THEN BEGIN
            ;== Leave no dangling objects
            FOR i = 0, numTerms - 1 DO $
                OBJ_DESTROY, photonModel[i]
                
            self.display->setStatus, 'No Datasets were selected.', 5
            status = 1
            RETURN
        ENDIF
        
        ;== Load up the struct with the batch fit info
        fit = { MFIT_BATCHINFO }
        
        fit.timeInt        = PTR_NEW (fitTimes)
        fit.energyInt      = PTR_NEW (fitEnergies)
        fit.chisq          = model.chisq
        fit.dof            = model.dof
        fit.netCrate       = PTR_NEW (model.photObsRate)
        fit.netCsig        = PTR_NEW (model.photObsSig)
        fit.modelCntRate   = PTR_NEW (model.modelPhotRateBychan)
        
        myFitChan = model.fitChannels
        FOR ii = 0, numDet - 1 DO BEGIN
            myFitChan[0, ii] = MIN (WHERE (model.modelEnergy[*, ii]))
            myFitChan[1, ii] = MAX (WHERE (model.modelEnergy[*, ii]))
        ENDFOR
        success = reorderData (numDet, model.numChan, model.modelEnergy, $
                  REFORM (model.modelPhotRate[*, 0, *]), myFitChan, $
                  model.param[self.NUM_ADD_TERMS, *], fullEnergy, fullModel) ;, /WIDE

        fit.modelEnergy    = PTR_NEW (fullEnergy)
        fit.modelPhotRate  = PTR_NEW (fullModel)
   
        theTot = 0
        FOR i = 0, numTerms - 1 DO BEGIN        ;== Pack up the fit results
            idx = photonModel[i]->index ()
            np  = photonModel[i]->nParams ()                 

            paramArr[theTot: theTot + np - 1] = model.param[idx - 1, 0: np - 1]
            perrsArr[theTot: theTot + np - 1] = model.param_uncer[idx - 1, 0: np - 1]
            theTot = theTot + np
        ENDFOR   

        fit.param       = PTR_NEW (paramArr)
        fit.param_uncer = PTR_NEW (perrsArr)
		fit.pFlux       = self->getPFlux()
		fit.eFlux       = self->getEFlux()
		fit.pErr        = self->getPErr()
		fit.eErr        = self->getEErr()
		fit.BFlux       = self->getBFlux()
		fit.BEFlux      = self->getBEFlux()
		fit.DFlux       = self->getDFlux()
		fit.BErr        = self->getBErr() 
		fit.BEErr       = self->getBEErr() 
		fit.DErr        = self->getDErr() 

        self.BatchFitList->add, fitname, fit

    ENDFOR

    self.Log->append, ['  ', '==> Batch Fit Done...']

    self->haveBatch, 1L       ;== We have batch fit results, if we've gotten this far
    self.lastFitWasBatch = 1L ;== We have done a batch fit, so the REFIT data are dirty
    
    ;== Copy previous fit results to the Model structure
    IF dataSaved THEN BEGIN
        self.FitModel->setModel, $
             
        tmodel.numDet,       tmodel.havemodel,   tmodel.havefit,    tmodel.chisq,     $
        tmodel.dof,          tmodel.fitErr,      tmodel.useDet,     tmodel.term_used, $
        tmodel.fitChannels,  tmodel.numChan,     tmodel.chanEnergy, tmodel.chanWidth, $
        tmodel.obsCrate,     tmodel.backCrate,   tmodel.backCsig,   tmodel.liveTime,  $
        tmodel.modelCntRate, tmodel.photObsRate, tmodel.photObsSig,                   $
        tmodel.nuFNuData,    tmodel.nuFNuSig,    tmodel.nuFNuModel,                   $
        tmodel.modelEnergy,  tmodel.modelPhotRate, tmodel.modelPhotRateByChan,        $
        tmodel.modelCRVari,  tmodel.dataCRVari,  tmodel.names,      tmodel.param,     $
        tmodel.param_uncer

        PTR_FREE, self.fitModelInfo
        self.fitModelInfo = PTR_NEW (tFitInfo)
        self.PhotonModel->setModel, termNames, photonModel
        
        ;== Set up the 'previous' data. Only selects the integrated data selection, 
        ;if the user has done anything else, we're not going to reflect it...
;        FOR i = 0, numDet - 1 DO BEGIN 
;    
;            det = self.DetectorList->data (names[i]) 
;            det.display->getSpectra, cancel, /BATCH, /SELECTIONS 
;         
;        ENDFOR 
		
		;== Restore the flux info:
		self.PFlux = pFlux_Save    
		self.EFlux = eFlux_Save    
		self.PErr  = pErr_Save    
		self.EErr  = eErr_Save    
		self.BFlux = bFlux_Save     
		self.DFlux = dFlux_Save     
		self.BErr  = bErr_Save     
		self.DErr  = dErr_Save     
        
    ENDIF
    
    self.Log->setClearButton, 'Clear'

    FOR i = 0, numTerms - 1 DO $
        OBJ_DESTROY, photonModel[i]

	bDisplay = self.display
	bPlotter = bDisplay.BatchPlotter
	bPlotter->setDurStatus, 0

    IF KEYWORD_SET (BATCHPLOT) THEN BEGIN          
        ;== called from PHADISPLAY, so we need some extra handling
        bPlotter->clearLabelList
        bDisplay->setPlotOptions, batchPlot
        bDisplay->plotModel, /BATCHPLOT, _EXTRA = extra
    ENDIF

END

;------------------------------------------------------------------------------
; Fit the user model
;------------------------------------------------------------------------------
PRO Mfit::fitModel, SELECT_MODEL = select_model, BATCH = batch, $
                    REFIT = refit, ENERGYINTERVAL = energyInt, $
                    FITPLOT = fitplot, _EXTRA = extra
        
    ;== Number of detectors currently loaded
    
    numDet = self.DetectorList->count ()
    names  = self.DetectorList->names ()

    IF (numDet LT 1) THEN BEGIN
       MESSAGE, /CONTINUE, 'No detector data available.'
       RETURN
    ENDIF

    ;== Get the data from each detector
    use_det = LONARR (numDet) + 1
    IF ((NOT KEYWORD_SET (REFIT)) OR (self.lastFitWasBatch EQ 1L)) THEN BEGIN
        ;self.display->setStatus, 'Accumulating spectral data, please wait.', 5, /REVERT
        FOR i = 0, numDet - 1 DO BEGIN
            
            RESTORE, !MFIT.script_file                                                               ; AMG
            numfits=N_ELEMENTS(model[0,*])                                                           ;
            phaNum=i+1                                                                               ;
            SAVE, file=!MFIT.script_file, script, lcFile, rspFile, luFile, phaNum, model, vals, $    ;
                       statistic, eInterval, numfits, iter, scriptVerbose, table                     ;
                       
            det = self.DetectorList->data (names[i])
			;== Cancel the REFIT since the BATCH fit made the last data set dirty:
            IF (self.lastFitWasBatch EQ 1L) THEN BEGIN
                det.display->getSpectra, cancel, /SELECTIONS, _EXTRA = extra
            ENDIF ELSE det.display->getSpectra, cancel, BATCH = batch, _EXTRA = extra
            IF cancel THEN RETURN  ;use_det[i] = 0
        
        ENDFOR
		;== 4/15/2010 RDP: Oops! We were failing on joint fits because lastFitWasBatch was 
		;   being set in the loop!
		self.lastFitWasBatch = 0L
    ENDIF
    ;== Mark all available detectors for inclusion in fit
    
    ;use_det = [1L]
    RESTORE, !MFIT.script_file                                                                      ; AMG
    IF (numDet GT 1) AND (NOT script) THEN BEGIN                                                    ;
    
        IF (NOT KEYWORD_SET (Batch) OR NOT self.fitModel->haveFit()) THEN BEGIN
         
            use_det = DIALOG_CHECKLIST (names, INITIAL = use_det, $
                      TITLE = 'Select Datasets for Fitting', /NONEXCLUSIVE) 
            IF TOTAL (use_det) EQ 0 THEN RETURN
             
		ENDIF ;ELSE BEGIN
;	    
;            model = self.fitModel->model()
;            ;use_det = model.useDet
;	        
;        ENDELSE
    ENDIF
    
    IF (SIZE(model))[N_ELEMENTS(SIZE(model))-2] EQ 10 THEN Nmodels=N_ELEMENTS(model) ELSE $       ; AMG
        Nmodels=N_ELEMENTS(model[0,*])                                                            ;
    IF script THEN mm=Nmodels ELSE mm=1                                                           ;
    FOR numfits=0, mm-1 DO BEGIN                                                                  ;
      numfits_temp=numfits                                                                        ;
    RESTORE, !MFIT.script_file                                                                    ;
    numfits=numfits_temp                                                                          ;
    SAVE, FILE = !MFIT.script_file, script, lcFile, rspFile, luFile, phaNum, model, vals, $       ;
                 statistic, eInterval, numfits, iter, scriptVerbose, table                        ; 
    
    jj = WHERE (use_det EQ 1, co)
    IF (co EQ 0) THEN RETURN        ;== All datasets were cancelled
    
    ;== Check to see whether we should allow the user to select log likelihood:
    useLikelihood = 1
    FOR i = 0, numDet - 1 DO BEGIN
		det = self.DetectorList->data (names[i])
        IF (det.haveBack EQ 0) THEN BEGIN
        	useLikelihood = 0
        	BREAK
        ENDIF
    ENDFOR

    ;== Select photon model terms
    
    saveNames = self.PhotonModel->names ()
    IF (KEYWORD_SET (select_model)) THEN BEGIN
       self.PhotonModel->clear                                                                                                                     ;
       RESTORE, !MFIT.script_file                                                                 ; AMG
       IF script THEN self.statistic=statistic                                                    ;
       self.PhotonModel->select, FIT_STAT = self.statistic
       ;== CCR #256 RDP: The logic was backwards: we set the statistic no matter what; then 
       ; if there is no background, we force the fitter to use chi^2:
	   self.statistic = self.PhotonModel->statchoice()
       IF NOT useLikelihood THEN $
		   self.statistic = 0L
    ENDIF
    
    numTerms = self.PhotonModel->count ()
    IF (numTerms LT 1) THEN BEGIN
       IF (numTerms NE N_ELEMENTS(saveNames)) THEN BEGIN
          ;== Check first that there *is* a saved model:
          IF (SIZE(saveNames))[0] NE 0 THEN BEGIN
              ;User has cancelled, restore the saved models:
              self.PhotonModel->setModel, saveNames, dummy
          ENDIF ELSE RETURN
       ENDIF ELSE BEGIN
          MESSAGE, /CONTINUE, 'No model terms selected.'
       ENDELSE
       RETURN
    ENDIF
    
    ;== Set up the integration intervals:
    durInt = self.display->get_durationInt()
    BFITInt = [50., 300.]

    ;== Set up model selection arrays
    
    termInfo = self.termInfo->getInfo()
    
    term_used   = LONARR (self.MAX_TERMS + 1)
    param_vary  = LONARR (self.MAX_TERMS, self.MAX_PPT)
    param       = FLTARR (self.MAX_TERMS, self.MAX_PPT)
    param_uncer = FLTARR (self.MAX_TERMS, self.MAX_PPT)
    covar       = FLTARR (self.MAX_PARAM, self.MAX_PARAM)
    alpha       = covar
    param       = *termInfo.param_defaults

    ;== Retrieve a copy of the photon model.  This action returns a
    ;== copy of photon model term objects that must be freed below.
    
    photonModel = self.PhotonModel->model ()

	have_model   = 1L
	term_used[0] = 1L
	
    FOR i = 0, numTerms - 1 DO BEGIN
        
        idx = photonModel[i]->index ()
        np  = photonModel[i]->nParams ()                 
        p   = photonModel[i]->params ()

        term_used[idx] = 1L
        param[idx - 1, 0: np - 1] = p.value
        param_vary[idx - 1, 0: np - 1] = LONG (p.fixed EQ 0)

    ENDFOR   
    ;== Preliminary header text for Fit Log
    
    msg = [' ', STRING (REPLICATE (BYTE ('-'), 100))]
    IF (NOT KEYWORD_SET (batch)) THEN $
        self.Log->append, msg
    
    ;fitModelInfo = [msg]

    FOR i = 0, numDet - 1 DO BEGIN
   
        det = self.DetectorList->data (names[i])
                
        ts = STRCOMPRESS (STRING (det.timeInt, $
            FORMAT = '(F10.3, ": ", F10.3, " s")'))
        
        es = STRCOMPRESS (STRING (det.energyInt, $
            FORMAT = '(G, ": ", G, " keV")'))
    
        cs = STRCOMPRESS (STRING (det.fitChan, $
            FORMAT = '(I, ": ", I)'))
    
        detStatus = (use_det[i] EQ 1) ? ' INCLUDED' : ' OMITTED'
        msg = [ $
            '==> Dataset      : #' + STRTRIM (i, 2) + detStatus, $
            '==> Data file    : ' + names[i], $
            '==> Response file: ' + det.respName, $
            '==> Fit interval : ' + ts + ', ' + es + ', channels ' + cs, $
            ' ']
        IF (NOT KEYWORD_SET (batch)) THEN self.Log->append, msg
        
        ;fitModelInfo = [fitModelInfo, msg]

    ENDFOR


    ;== Set up data arrays for MFIT
    
    ;== Get the maximum number of channels and energy bins
    
    numChan = LONARR (numDet)
    numEbin = LONARR (numDet)
    FOR i = 0, numDet - 1 DO BEGIN   
        numChan[i] = (self.DetectorList->data (names[i])).numChan
        numEbin[i] = (self.DetectorList->data (names[i])).numEbins
    ENDFOR
    maxChan = MAX (numChan)
    maxEbin = MAX (numEbin)

    chanEnergy = FLTARR (maxChan, numDet)
    chanWidth  = FLTARR (maxChan, numDet)
    photEnergy = FLTARR (maxEbin, numDet)
    photWidth  = FLTARR (maxEbin, numDet)
    drm        = FLTARR (maxEbin, maxChan, numDet)
    
    obsCrate   = FLTARR (maxChan, numDet)
    backCrate  = FLTARR (maxChan, numDet)
    backCsig   = FLTARR (maxChan, numDet)
    liveTime   = FLTARR (maxChan, numDet)
    
    resFrac    = FLTARR (numDet)
    resExp     = FLTARR (numDet)
    fitChan    = LONARR (2, numDet)        
    chanOffset = LONARR (numDet)
    detIndex   = LONARR (numDet)
    
    FOR i = 0, numDet - 1 DO BEGIN   
    
        nChan1 = numChan[i] - 1 
        nEbin1 = numEbin[i] - 1

        resp = *(self.DetectorList->data (names[i])).resp        
                
        chanEnergy[0:nChan1, i] = resp.chan_energy 
        chanWidth [0:nChan1, i] = resp.chan_width
        photEnergy[0:nEbin1, i] = resp.phot_energy 
        photWidth [0:nEbin1, i] = resp.phot_width 
        drm[0:nEbin1, 0:nChan1, i] = resp.drm 
    
        obsCrate[0:nChan1, i]  = *(self.DetectorList->data (names[i])).obsCrate
        backCrate[0:nChan1, i] = *(self.DetectorList->data (names[i])).backCrate
        backCsig[0:nChan1, i]  = *(self.DetectorList->data (names[i])).backCsig
        liveTime[0:nChan1, i]  = *(self.DetectorList->data (names[i])).liveTime

        resFrac[i]    = (self.DetectorList->data (names[i])).resFrac
        resExp[i]     = (self.DetectorList->data (names[i])).resExp        
        fitChan[*, i] = (self.DetectorList->data (names[i])).fitChan
        chanOffset[i] = 0;(self.DetectorList->data (names[i])).chanOffset        
        detIndex[i]   = (self.DetectorList->data (names[i])).detIndex
    
    ENDFOR

	;== FOR MICHAEL: Use the following variable as input to MFIT
	;print, detIndex
	;== More input variables
    
    rel_converg       = 0.001
    abs_converg       = 0.01
    max_tries         = 90L
    fix_undet         = self.PhotonModel->fixUndetChoice()
    enable_undet      = (KEYWORD_SET (Batch)) ? fix_undet : 0L

    num_terms_avail   =  termInfo.num_terms_avail 
    num_add_terms     =  termInfo.num_add_terms
    nt_add_line       =  termInfo.nt_add_line
    num_param_of_term = *termInfo.num_param_of_term
    rel_change_limit  = *termInfo.rel_change_limit
    abs_change_limit  = *termInfo.abs_change_limit
    undet_priority    = *terminfo.undet_priority
    undet_rel_req     = *terminfo.undet_rel_req
    undet_abs_req     = *terminfo.undet_abs_req
    high_ud_priority  = LONG(MAX(undet_priority))


    ;== Output variables (must be predefined for the DLM call)
    
    use_max_plot = 6*maxChan + 1
    
    have_fit               = 1L
    fit_err                = 0L
    model_chisq            = 0.0
    dof                    = 0L
    num_vary               = 0L
    model_cnt_rate         = FLTARR (maxChan, self.MAX_TERMS + 1, numDet)
    phot_obs_rate          = FLTARR (maxChan, numDet)
    phot_obs_sig           = FLTARR (maxChan, numDet)
    nu_f_nu_data           = FLTARR (maxChan, numDet)
    nu_f_nu_sig            = FLTARR (maxChan, numDet)
    nu_f_nu_model          = FLTARR (use_max_plot, numDet)
    model_energy           = FLTARR (use_max_plot, numDet)
    model_phot_rate        = FLTARR (use_max_plot, self.MAX_TERMS + 1, numDet)
    model_phot_rate_bychan = FLTARR (maxChan, numDet)
    model_cr_vari          = FLTARR (maxChan, numDet)
    data_cr_vari           = FLTARR (maxChan, numDet)
    mfit_ver_string        = "hello"

    ;== Perform the fit
 
    IF (NOT KEYWORD_SET (batch)) THEN self.Log->append, '==> Fitting data...' 
    self.Log->show
    
    ;== Maximum number of attempts: 10
    numTries = 0
    ;REPEAT BEGIN
    BATLOOP: MFIT, $ 
        self.statistic + 1L,                                          $
        numDet,            numChan,           numEbin,                $
        ;maxChan,           maxEbin,
        chanEnergy,             $
        chanWidth,         obsCrate,          backCrate,              $
        backCsig,          liveTime,          photEnergy,             $
        photWidth,         drm,               resFrac,                $
        resExp,            param_vary,        fitChan,                $
        use_det,           term_used,         num_terms_avail,        $
        num_add_terms,     nt_add_line,       num_param_of_term,      $
        rel_change_limit,  abs_change_limit,  rel_converg,            $
        abs_converg,       max_tries,                                 $
        enable_undet,      undet_priority,    undet_rel_req,          $
        undet_abs_req,     high_ud_priority,  chanOffset,             $
        param,             $ ;have_model,        have_fit,               $
        fit_err,           model_chisq,       dof,                    $
        num_vary,          param_uncer,       alpha,   covar,         $
        model_cnt_rate,    phot_obs_rate,     phot_obs_sig,           $
        nu_f_nu_data,      nu_f_nu_sig,       nu_f_nu_model,          $
        model_energy,      model_phot_rate,   model_phot_rate_bychan, $
        model_cr_vari,     data_cr_vari,      mfit_ver_string
        
    numTries = numTries+1
     
    IF (KEYWORD_SET (batch) AND ((fit_err EQ 1) AND (numTries LE 9))) THEN GOTO, BATLOOP
    ;ENDREP UNTIL ((NOT (KEYWORD_SET (batch))) AND (fit_err EQ 0) OR (numTries GT 9))
    
    ;== Compress some of the really big data structures:
    termArr = INDGEN (N_ELEMENTS (term_used))
    termInd = termArr [WHERE (term_used)]
    
    IF (fit_err LT 10 AND num_vary GT 0) THEN BEGIN   ;Trap errors computing covariance NE 444
        nans = WHERE (FINITE (covar, /NAN), coNaN)
        IF (coNaN GT 0) THEN BEGIN ; bad covariance array!
            numCovar = 1
            myCovar  = FLTARR (1)
			myAlpha  = FLTARR (1)
        ENDIF ELSE BEGIN
            covarInd = WHERE (covar)
            numCovar = N_ELEMENTS (WHERE (covar[0, *]))    ; + 1
            numInd = N_ELEMENTS(covarInd)
            IF (numInd EQ numCovar^2) THEN BEGIN
                myCovar  = REFORM (covar [covarInd], numCovar, numCovar)
                myAlpha  = REFORM (alpha [covarInd], numCovar, numCovar)
            ENDIF ELSE BEGIN
                numCovar = 1
                myCovar  = FLTARR (1)
                myAlpha  = FLTARR (1)
            ENDELSE
        ENDELSE
    ENDIF ELSE BEGIN
        numCovar = 1
        myCovar  = FLTARR (1)
		myAlpha  = FLTARR (1)
    ENDELSE
         
    ;== Copy fit results to the Model structure

    self.FitModel->setModel, $
             
        numDet,        have_model,    have_fit,        model_chisq,         $
        dof,           fit_err,       use_det,         term_used,           $
        fitChan,       numChan,       chanEnergy,      chanWidth,           $
        obsCrate,      backCrate,     backCsig,        liveTime,            $
        model_cnt_rate[*,termInd,*],  phot_obs_rate,   phot_obs_sig,        $
        nu_f_nu_data,  nu_f_nu_sig,   nu_f_nu_model,   model_energy,        $
        model_phot_rate[*,termInd,*],            model_phot_rate_bychan,    $
        model_cr_vari, data_cr_vari,  names,           param,               $
        param_uncer
    
;--------------------------------------------------------------------------------------------------
  IF (fit_err LT 10) AND ((N_ELEMENTS(MyCovar) GT 1) OR (MyCovar[0,0] NE 0.)) THEN BEGIN          ; AMG
    Total_phDeriv=0.                                                                               ;
    Total_enDeriv=0.                                                                               ;
    Total_bfDeriv=0.                                                                               ;
    Total_beDeriv=0.                                                                               ;
    Total_duDeriv=0.                                                                               ;

    FOR i=0, numTerms-1 DO BEGIN                                                                  ;
      term=photonModel[i]                                                                         ;
      nParams=term->nParams()                                                                     ;
      index=term->index()                                                                         ;
      param_mask=param_vary[index-1, 0:nParams-1]                                                 ;
      param_index=WHERE(param_mask EQ 1)                                                          ;
      If param_index[0] NE -1 THEN nParams=N_ELEMENTS(param_index) ELSE CONTINUE                  ;
      phDeriv=FLTARR(nParams)                                                                     ;
      enDeriv=FLTARR(nParams)                                                                     ;
      bfDeriv=FLTARR(nParams)                                                                     ;
      beDeriv=FLTARR(nParams)                                                                     ;
      duDeriv=FLTARR(nParams)                                                                     ;
      FOR j=0, nParams-1 DO BEGIN                                                                 ;
        parval=param[index-1, param_index[j]]                                                     ;
        IF parval NE 0. THEN BEGIN                                                                ;
          temp_u=1.005*parval                                                                     ;
          temp_l=0.995*parval                                                                     ;
        ENDIF ELSE BEGIN                                                                          ;
          temp_u=+0.001                                                                           ;
          temp_l=-0.001                                                                           ;
        ENDELSE                                                                                   ;
                                                                                                  ;
        epsilon=[temp_u, temp_l]                                                                  ;
        phflux=FLTARR(2)                                                                          ;
        enflux=FLTARR(2)                                                                          ;
        bfflux=FLTARR(2)                                                                          ;
        beflux=FLTARR(2)                                                                          ;
        duflux=FLTARR(2)                                                                          ;
                                                                                                  ;
        FOR k=0,1 DO BEGIN                                                                        ;
          param_vary_t=param_vary & param_vary_t[*,*]=0                                           ;
          param_t=param & param_t[index-1, param_index[j]]=epsilon[k]                             ;
          fit_err_t=fit_err & model_chisq_t=model_chisq                                           ;
          dof_t=dof & num_vary_t=0                                                                ;
          param_uncer_t=param_uncer & alpha_t=alpha                                               ;
          covar_t=covar & model_cnt_rate_t=model_cnt_rate                                         ;
          phot_obs_rate_t=phot_obs_rate & phot_obs_sig_t=phot_obs_sig                             ;
          nu_f_nu_data_t=nu_f_nu_data & nu_f_nu_sig_t=nu_f_nu_sig                                 ;
          nu_f_nu_model_t=nu_f_nu_model & model_energy_t=model_energy                             ;
          model_phot_rate_t=model_phot_rate                                                       ;
          model_phot_rate_bychan_t=model_phot_rate_bychan                                         ;
          model_cr_vari_t=model_cr_vari & data_cr_vari_t=data_cr_vari                             ;
                                                                                                  ;
          MFIT,                                                         $                         ;
          self.statistic + 1L,                                          $                         ;
          numDet,            numChan,           numEbin,                $                         ;
          ;maxChan,          maxEbin,                                                             ;
          chanEnergy,                                                   $                         ;
          chanWidth,         obsCrate,          backCrate,              $                         ;
          backCsig,          liveTime,          photEnergy,             $                         ;
          photWidth,         drm,               resFrac,                $                         ;
          resExp,            param_vary_t,      fitChan,                $                         ;
          use_det,           term_used,         num_terms_avail,        $                         ;
          num_add_terms,     nt_add_line,       num_param_of_term,      $                         ;
          rel_change_limit,  abs_change_limit,  rel_converg,            $                         ;
          abs_converg,       max_tries,                                 $                         ;
          enable_undet,      undet_priority,    undet_rel_req,          $                         ;
          undet_abs_req,     high_ud_priority,  chanOffset,             $                         ;
          param_t,           $ ;have_model,     have_fit,               $                         ;
          fit_err_t,         model_chisq_t,     dof_t,                  $                         ;
          num_vary_t,        param_uncer_t,     alpha_t,   covar_t,     $                         ;
          model_cnt_rate_t,  phot_obs_rate_t,   phot_obs_sig_t,         $                         ;
          nu_f_nu_data_t,    nu_f_nu_sig_t,     nu_f_nu_model_t,        $                         ;
          model_energy_t,    model_phot_rate_t, model_phot_rate_bychan_t, $                       ;
          model_cr_vari_t,   data_cr_vari_t,    mfit_ver_string                                   ;
                                                                                                  ;
          myFitChan = fitChan                                                                     ;
          FOR ii = 0, numDet - 1 DO BEGIN                                                         ;
            myFitChan[0, ii] = MIN (WHERE (model_energy[*, ii]))                                  ;
            myFitChan[1, ii] = MAX (WHERE (model_energy[*, ii]))                                  ;
          ENDFOR                                                                                  ;
                                                                                                  ;
          success = reorderData (numDet, numChan, model_energy_t, $                               ;
                     REFORM (model_phot_rate_t[*, 0, *]), myFitChan, $                            ;
                     param[self.NUM_ADD_TERMS, *], fullEnergy, fullModel)                         ;
          phflux[k]=IntegratePhotonModel(fullEnergy, fullModel, EnergyInt)                        ;
          bfflux[k]=IntegratePhotonModel(fullEnergy, fullModel, BFITInt)                        ;
          beflux[k]=IntegratePhotonModel(fullEnergy, fullModel, BFITInt, /ENERGY_INT)*1.602e-9                         ;
          duflux[k]=IntegratePhotonModel(fullEnergy, fullModel, DurInt)                        ;
          enflux[k]=IntegratePhotonModel(fullEnergy, fullModel, EnergyInt, /ENERGY_INT)*1.602e-9  ;
                                                                                                  ;
        ENDFOR                                                                                    ;
                                                                                                  ;
        phDeriv[j]=(phflux[0] - phflux[1])/(epsilon[0]-epsilon[1])                                ;
        enDeriv[j]=(enflux[0] - enflux[1])/(epsilon[0]-epsilon[1])                                ;
        bfDeriv[j]=(bfflux[0] - bfflux[1])/(epsilon[0]-epsilon[1])                                ;
        beDeriv[j]=(beflux[0] - beflux[1])/(epsilon[0]-epsilon[1])                                ;
        duDeriv[j]=(duflux[0] - duflux[1])/(epsilon[0]-epsilon[1])                                ;
                                                                                                  ;
      ENDFOR                                                                                      ;
                                                                                                  ;
      Total_phDeriv=[Total_phDeriv,phDeriv]                                                       ;
      Total_enDeriv=[Total_enDeriv,enDeriv]                                                       ;
      Total_bfDeriv=[Total_bfDeriv,phDeriv]                                                       ;
      Total_beDeriv=[Total_beDeriv,phDeriv]                                                       ;
      Total_duDeriv=[Total_duDeriv,duDeriv]                                                       ;
                                                                                                  ;
    ENDFOR                                                                                        ;
                                                                                                  ;
    Total_phDeriv=Total_phDeriv[1:*]                                                              ;
    Total_enDeriv=Total_enDeriv[1:*]                                                              ;
    Total_bfDeriv=Total_bfDeriv[1:*]                                                              ;
    Total_beDeriv=Total_beDeriv[1:*]                                                              ;
    Total_duDeriv=Total_duDeriv[1:*]                                                              ;
    phfluxErr=SQRT(Total_phDeriv##MyCovar##TRANSPOSE(Total_phDeriv))                              ;
    enfluxErr=SQRT(Total_enDeriv##MyCovar##TRANSPOSE(Total_enDeriv))                              ;
    bffluxErr=SQRT(Total_bfDeriv##MyCovar##TRANSPOSE(Total_bfDeriv))                              ;
    befluxErr=SQRT(Total_beDeriv##MyCovar##TRANSPOSE(Total_beDeriv))                              ;
    dufluxErr=SQRT(Total_duDeriv##MyCovar##TRANSPOSE(Total_duDeriv))                              ;
    self.pErr=phfluxErr                                                                           ;
    self.eErr=enfluxErr                                                                           ;
    self.bErr=bffluxErr                                                                           ;
    self.beErr=befluxErr                                                                           ;
    self.dErr=dufluxErr                                                                           ;
  ENDIF ELSE self.pErr=(self.eErr=0.)                                                                                       ;
;--------------------------------------------------------------------------------------------------    
     msg = '==> ' + mfit_ver_string +  ': Fit completed at ' + SYSTIME ()
     IF (NOT KEYWORD_SET (batch)) THEN self.Log->append, msg
     fitModelInfo = [msg]
           
    ;== Update status log
     
    IF ((fit_err LT 10) OR (fit_err EQ 444)) THEN BEGIN
     
       FOR i = 0, numTerms - 1 DO BEGIN

           term = photonModel[i]

           termName   = term->name ()        
           index      = term->index ()
           nParams    = term->nParams ()
           termParams = term->params ()
           
           msg = [' ', 'TERM: ' + termName, ' ']
           IF (NOT KEYWORD_SET (batch)) THEN self.Log->append, msg
           
           fitModelInfo = [fitModelInfo, msg]

           FOR j = 0, nParams - 1 DO BEGIN

               name  = termParams[j].name
               units = termParams[j].units

               value = param[index - 1, j]
               error = param_uncer[index - 1, j]
               vary  = (param_vary[index - 1, j] EQ 1) ? 'VARY' : ' FIX'

               msg = STRING (name, vary, value, error, units, $
                   FORMAT = '(A20, 1X, A4, 1X, G12.4, " +/- ", G12.3, 1X, A)')
               IF (NOT KEYWORD_SET (batch)) THEN self.Log->append, msg
               
               fitModelInfo = [fitModelInfo, msg]

           ENDFOR 
           
           IF (NOT KEYWORD_SET (batch)) THEN BEGIN
               ;== Copy the fit result into the termInfo data
               self.termInfo->setParams, index, nParams, param[index - 1, *], $
                              param_vary[index - 1, *]
               ;self->haveBatch, 0L    ;== Just in case...
           ENDIF

       ENDFOR   
    
       CASE (self.statistic) OF
       
		   0: BEGIN
			   chiProb = 1.d - chisqr_pdf(model_chisq, dof)
			   cs = STRCOMPRESS (STRING (model_chisq, dof, model_chisq / dof, chiProb, $ 
			   FORMAT = '("CHISQ = ", G12.5, ", DOF = ", I, ", REDUCED CHISQ = ", G12.7,", PROB = ", G12.4)'))
		   END
		   
		   1: BEGIN
			   cs = STRCOMPRESS (STRING (model_chisq, dof, $ 
			   FORMAT = '("-2 LOG (LIKELIHOOD) = ", G12.5, ", DOF = ", I)'))
		   END
		   
		   2: BEGIN
			   ;chiProb = 1.d - chisqr_pdf(model_chisq, dof)
			   cs = STRCOMPRESS (STRING (model_chisq, dof, $ ;model_chisq / dof, chiProb, 
			   FORMAT = '("Castor C-STAT = ", G12.5, ", DOF = ", I)'))
		   END
       
       ENDCASE
       
       msg = [ $
           ' ', $
           '==> ' + cs]
       IF (NOT KEYWORD_SET (batch)) THEN self.Log->append, msg
       
       fitModelInfo = [fitModelInfo, msg]
       
       IF KEYWORD_SET (energyInt) THEN BEGIN
           ;== 'Photon Flux (ph/s-cm^2)'
           myFitChan = fitChan
           FOR ii = 0, numDet - 1 DO BEGIN
               myFitChan[0, ii] = MIN (WHERE (model_energy[*, ii]))
               myFitChan[1, ii] = MAX (WHERE (model_energy[*, ii]))
           ENDFOR
           success = reorderData (numDet, numChan, model_energy, $
                     REFORM (model_phot_rate[*, 0, *]), myFitChan, $
                     param[self.NUM_ADD_TERMS, *], fullEnergy, fullModel) ;, /WIDE)

           self.pFlux = (pFlux = integratePhotonModel(fullEnergy, fullModel, EnergyInt))
           self.bFlux = integratePhotonModel(fullEnergy, fullModel, BFITInt)
			self.beFlux = IntegratePhotonModel(fullEnergy, fullModel, BFITInt, /ENERGY_INT)*1.602e-9                         ;
           self.dFlux = integratePhotonModel(fullEnergy, fullModel, DurInt)
;           self.pErr = (pErr = pFlux * param_uncer[termInd[1] - 1, 0] / param[termInd[1] - 1, 0] / 2.0)
           ps = STRCOMPRESS (STRING (pFlux, self.pErr, energyInt[0], energyInt[1], $ 
           FORMAT = '("Photon Flux = ", G12.5, " +/- ", G12.2, " ph/s-cm^2 in the interval:", G12.4, ": ", G12.5, " keV")'))
           msg = '==> ' + ps
           IF (NOT KEYWORD_SET (batch)) THEN self.Log->append, msg
       
           ;fitModelInfo = [fitModelInfo, msg]
           
           ;== 'Energy Flux (erg/s-cm^2)'
           self.eFlux = (eFlux = integratePhotonModel(fullEnergy, fullModel, $
                   EnergyInt, /ENERGY_INT) * 1.602e-9 ); erg / keV
;           self.eErr = (eErr = eFlux * param_uncer[termInd[1] - 1, 0] / param[termInd[1] - 1, 0] / 2.0)
           ps = STRCOMPRESS (STRING (eFlux, self.eErr, energyInt[0], energyInt[1], $ 
           FORMAT = '("Energy Flux = ", G12.5, " +/- ", G12.2, " erg/s-cm^2 in the interval:", G12.4, ": ", G12.5, " keV")'))
           msg = '==> ' + ps
           IF (NOT KEYWORD_SET (batch)) THEN self.Log->append, msg
           
           ;== Make sure integration range is valid:
           IF (fullEnergy[0] GT energyInt[0]) OR $
              (fullEnergy[N_ELEMENTS(fullEnergy) - 1] LT energyInt[1]) THEN BEGIN
              m = '******'
              IF (NOT KEYWORD_SET (batch)) THEN self.Log->append, m
              m = 'WARNING: Flux and Fluence calculations are incorrect!'
              IF (NOT KEYWORD_SET (batch)) THEN self.Log->append, m
              m = 'Fluence interval should be changed to lie within the data range.'
              IF (NOT KEYWORD_SET (batch)) THEN self.Log->append, m
              m = '******'
              IF (NOT KEYWORD_SET (batch)) THEN self.Log->append, m
           ENDIF
       
           ;fitModelInfo = [fitModelInfo, msg]
       ENDIF
    
    ENDIF 
    
    IF (fit_err NE 0) THEN BEGIN

        CASE (fit_err) OF
	
            1   : m = 'WARNING: Mild non-convergence.'
            2   : m = 'WARNING: Negative photon model.'
            9   : m = 'WARNING: Negative count rate model.'           
            10  : m = 'ERROR: Severe non-convergence.'           
            111 : m = 'ERROR: Nonlinear fit failed.'           
            222 : m = 'ERROR: Some unknown error.'
            333 : m = 'ERROR: No varying parameters in model.'
            444 : m = 'ERROR: Failed to compute parameter covariance.'
	   
            ELSE: m = 'ERROR: status = ' + STRTRIM (fit_err, 2)
	
        ENDCASE
	   
        msg = [ $
            ' ', $
            '***', $
            '*** ' + m, $
            '***' $
            ]    
            IF (NOT KEYWORD_SET (batch)) THEN self.Log->append, msg
            
            ;fitModelInfo = [fitModelInfo, msg]

    ENDIF
    
    IF (NOT KEYWORD_SET (Batch) AND NOT (fit_err EQ 444)) THEN BEGIN
	    IF (num_vary EQ 0) THEN BEGIN
	        msg = [' ', $
		    ' *** WARNING! : All parameters were fixed ' + $
		    'so there is no covariance matrix...']
	    ENDIF ELSE BEGIN
			;== Print the covariance info:
			msg = [' ','    The Normed Covariance Matrix = Correlation Coefficient Matrix:']
			self.Log->append, msg
			;fitModelInfo = [fitModelInfo, msg]
			
			mySqrt = FLTARR (numCovar, numCovar)
			FOR i = 0, numCovar - 1 DO FOR j = 0, numCovar - 1 DO $
				mySqrt[i,j] = myCovar [i,j] / SQRT (ABS (myCovar[i,i]) * ABS (myCovar[j,j]))
				
			FOR i = 0, numCovar - 1 DO BEGIN 
				myString = ' '
				FOR j = 0, numCovar - 1 DO $
					myString = myString + STRING (mySqrt [i,j], FORMAT = '(F7.3)')
				self.Log->append, myString
				;fitModelInfo = [fitModelInfo, myString]
			ENDFOR
			
			;myMatrix = INVERT (myCovar, myStatus)
			;IF (myStatus NE 0) THEN BEGIN
			;IF ((myStatus NE 0) AND (myStatus NE 2)) THEN BEGIN
			;    msg = [' ',' *** ERROR! : Failed to obtain an inverse for the covariance matrix!']
		   ; ENDIF ELSE BEGIN
			warning = ''
            msg = [warning,' ','    The global correlation coefficients of the varying parameters are:']
            myString = ' '
            FOR i = 0, numCovar - 1 DO BEGIN
                rProd = 1./ (myCovar[i,i] * myAlpha[i,i])
                IF rProd LE 1.0 THEN rProd = SQRT (1. - rProd)
                myString = myString + STRING (rProd, FORMAT = '(F7.3)')
            ENDFOR
            msg = [msg, myString]
        ENDELSE
        self.Log->append, msg
        ;fitModelInfo = [fitModelInfo, msg]
        
        ;== Free up the plot range info:  
        myDisplay = self.display
        myPlotter = myDisplay.plotter
        rangeListCount = myPlotter.rangeList->count()
        IF (rangeListCount NE 0) THEN myPlotter.rangeList->clear
        
    ENDIF
    
    PTR_FREE, self.fitModelInfo
    self.fitModelInfo = PTR_NEW (fitModelInfo)

    ;== Free the copy of the photon model
    
    FOR i = 0, numTerms - 1 DO $
         OBJ_DESTROY, photonModel[i]
         
    IF KEYWORD_SET (FITPLOT) THEN BEGIN
        self.display->setPlotOptions, fitPlot
        self.display->plotModel, _EXTRA = extra
    ENDIF
 ENDFOR                                                                                           ; AMG

END


;------------------------------------------------------------------------------
; Return the model fit and other relevant stuff
;------------------------------------------------------------------------------
FUNCTION Mfit::getFitModel     & RETURN, self.fitModel      & END
FUNCTION Mfit::getPhotonModel  & RETURN, self.photonModel   & END
FUNCTION Mfit::getDetectorList & RETURN, self.detectorList  & END
FUNCTION Mfit::getBatchFitList & RETURN, self.batchFitList  & END
FUNCTION Mfit::getTermInfo     & RETURN, self.termInfo      & END
FUNCTION Mfit::getFitModelInfo & RETURN, *self.fitModelInfo & END
FUNCTION Mfit::getNumAddTerms  & RETURN, self.NUM_ADD_TERMS & END
FUNCTION Mfit::getStatistic    & RETURN, self.statistic     & END
FUNCTION Mfit::getPFlux        & RETURN, self.pFlux         & END
FUNCTION Mfit::getEFlux        & RETURN, self.eFlux         & END
FUNCTION Mfit::getPErr         & RETURN, self.pErr          & END
FUNCTION Mfit::getEErr         & RETURN, self.eErr          & END
FUNCTION Mfit::getBFlux        & RETURN, self.bFlux         & END
FUNCTION Mfit::getBEFlux       & RETURN, self.beFlux        & END
FUNCTION Mfit::getDFlux        & RETURN, self.dFlux         & END
FUNCTION Mfit::getBErr         & RETURN, self.bErr          & END
FUNCTION Mfit::getBEErr        & RETURN, self.beErr         & END
FUNCTION Mfit::getDErr         & RETURN, self.dErr          & END

;------------------------------------------------------------------------------
; We have a batch fit - notify the other responsible parties
;------------------------------------------------------------------------------
PRO Mfit::haveBatch, tf

    self.haveBatch = tf
    myDisplay = self.Display
    myBatch = myDisplay.BatchPlotter
    myBatch->setBatchStatus, tf
    myBatch->setDurStatus, 0L

END


; ----------------------------------------------------------------------------
; We have a single fit - notify the other responsible parties
; ----------------------------------------------------------------------------
PRO Mfit::haveFit, tf

    self.FitModel->setHaveFit, tf
    
END


; ----------------------------------------------------------------------------
; Do we have a 1D chisq result?
; ----------------------------------------------------------------------------
FUNCTION Mfit::haveChi1D

    RETURN, self.haveChi1D
    
END


; ----------------------------------------------------------------------------
; Do we have a 2D chisq result?
; ----------------------------------------------------------------------------
FUNCTION Mfit::haveChi2D

    RETURN, self.haveChi2D
    
END

; ----------------------------------------------------------------------------
; Object definition
; ----------------------------------------------------------------------------
PRO Mfit__define

    tmpl = { MFIT_DETECTOR, $

        Detector        : OBJ_NEW (), $             
        Display         : OBJ_NEW (), $             
        registerOnly    : 0L,         $
        timeInt         : FLTARR (2), $
        energyInt       : FLTARR (2), $
        numChan         : 0L,         $
        numEbins        : 0L,         $
        chanOffset      : 0L,         $
        haveBack        : 0L,         $
        detIndex        : 0L,         $
        obsCrate        : PTR_NEW (), $
        backCrate       : PTR_NEW (), $
        backCsig        : PTR_NEW (), $
        liveTime        : PTR_NEW (), $
        resp            : PTR_NEW (), $
        fitChan         : LONARR (2), $
        resFrac         : 0.0,        $
        resExp          : 0.0,        $
        respName         : ''         $
    }
    
    tmpl = { MFIT_BATCHINFO, $
        timeInt             : PTR_NEW (), $
        energyInt           : PTR_NEW (), $
        chisq               : 0.0,        $
        dof                 : 0L,         $
        netCrate            : PTR_NEW (), $
        netCsig             : PTR_NEW (), $
        modelCntRate        : PTR_NEW (), $
        modelEnergy         : PTR_NEW (), $
        modelPhotRate       : PTR_NEW (), $
        param               : PTR_NEW (), $
        param_uncer         : PTR_NEW (), $
        pFlux               : 0.0,        $
        eFlux               : 0.0,        $
        pErr                : 0.0,        $
        eErr                : 0.0,        $
        bFlux               : 0.0,        $
        beFlux              : 0.0,        $
        dFlux               : 0.0,        $
        bErr                : 0.0,        $
        beErr               : 0.0,        $
        dErr                : 0.0         $
    }
                     
    obj = { MFIT, INHERITS SINGLETON, $

        Error             : OBJ_NEW (),        $ ; Error handler

        numDet            : 0L,                $ ; Current number of detectors
        haveBatch         : 0L,                $ ; Do we have batch fit results?
        lastFitWasBatch   : 0L,                $ ; Was the last fit done a batch fit?
        haveChi1D         : 0L,                $ ; Do we have 1D chisq results?
        haveChi2D         : 0L,                $ ; Do we have 2D chisq results?
        statistic         : 0L,                $ ; 1: chi^2, 2: likelihood, 3: C-Stat
        bestChi1DParam    : 0.,                $ ; Fitted parameter value
        bestChi2DParam    : 0.,                $ ; Second fitted parameter value
        param1DName       : '',                $ ; Fitted parameter name
        param2DName       : '',                $ ; Second fitted parameter name
        chi1DVals         : PTR_NEW (),        $ ; Store the 1D chisq values
        chi1DParm         : PTR_NEW (),        $ ; Store the 1D chisq param values
        fitModelInfo      : PTR_NEW (),        $ ; String array to store the model fit info
        FitModel          : OBJ_NEW (),        $ ; Model fit results
        PhotonModel       : OBJ_NEW (),        $ ; User photon model
        DetectorList      : OBJ_NEW (),        $ ; List of detectors
        BatchFitList      : OBJ_NEW (),        $ ; List of batch fit results
        Log               : OBJ_NEW (),        $ ; WIDGET_LOGGER object
        Display           : OBJ_NEW (),        $ ; MfitDisplay object        
        TermInfo          : OBJ_NEW (),        $ ; Photon term info
        
        pFlux             : 0.,                $ ; Calculated fluxes and errors:
        eFlux             : 0.,                $ ; 
        pErr              : 0.,                $ ; 
        eErr              : 0.,                $ ; 
        bFlux             : 0.0,               $
        beFlux            : 0.0,               $
        dFlux             : 0.0,               $
        bErr              : 0.0,               $
        bEErr             : 0.0,               $
        dErr              : 0.0,               $

        ;MAX_DET           : 0L, $                ; MFIT fixed array sizes
        MAX_TERMS         : 0L, $
        NUM_ADD_TERMS     : 0L, $
        MAX_PARAM         : 0L, $
        MAX_PPT           : 0L  $
        ;MAX_CHAN          : 0L, $
        ;MAX_EBINS         : 0L, $
        ;MAX_PLOT          : 0L  $

    }


END
