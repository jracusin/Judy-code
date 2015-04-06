; ----------------------------------------------------------------------------
;+
; NAME:
;     Spectrum (OBJECT)
;
; PURPOSE:
;     An object to encapsulate a detector energy spectrum.
;
; CALLING SEQUENCE:
;     obj = OBJ_NEW ('Spectrum', Reader)
;
; INPUTS:
;     Reader : A reference to a DataReader object.  This object typically
;         will be a subclass of DataReader that encapsulates the data
;         for a specific detector.
;
; KEYWORDS:
;     NONE
;
; INHERITS:
;     PHA
;
; DEPENDENCIES:
;     pha__define.pro
;     nearest_edge.pro
;
; PUBLIC METHODS:
;     
;     plot (PROCEDURE) - Plot the spectral data on the current device
;         Inputs: NONE
;        Outputs: NONE 
;       Keywords: OPLOT - overplot on the current device
;                 Any valid IDL PLOT keywords
;
;     oplot (PROCEDURE) - Overplot the spectral data on the current device
;         Inputs: NONE
;        Outputs: NONE 
;       Keywords: Any valid IDL OPLOT keywords
;
;     integrate (FUNCTION) - Integrate the spectrum over time bins
;         Inputs: NONE, but uses current timeSpan, timeLookup arrays
;                 to obtain integration intervals.  Set the setTimeSpan
;                 and setTimeLookup methods, inherited from class PHA
;        Outputs: integratedSpec - the integrated spectral data
;                 integratedSpecErr - errors on integratedSpec
;       Keywords: TOTALTIME - set this keyword to a named variable to return
;                 the total integrated time
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
FUNCTION Spectrum::init, Reader
   
    IF (NOT self->pha::init (Reader)) THEN $
       RETURN, 0
       
    self->update, /FULL

    RETURN, 1

END


; ----------------------------------------------------------------------------
; Destructor
; ----------------------------------------------------------------------------
PRO Spectrum::cleanup

    PTR_FREE, self.combinedThresholds
    PTR_FREE, self.combinedSpec
    PTR_FREE, self.combinedSpecErr

    PTR_FREE, self.integratedSpec
    PTR_FREE, self.integratedSpecErr

    self->pha::cleanup

END    


; ----------------------------------------------------------------------------
; Combine
;
; Set the keyword FULL_RESET to reset both x and y axis ranges.  By default,
; only the y axis range is reset in order to preserve the user's current
; x range setting.
; ----------------------------------------------------------------------------
PRO Spectrum::combine, spectrum, spectrumErr, combThresholds, $
    FULL_RESET = full_reset

    data = self.Reader->data (/POINTER)

    self->PHA::combine, $
        *self.integratedSpec, *self.integratedSpecErr, $
        (*data).eedges, *self.energyLookup, $
        spectrum, spectrumErr, combThresholds

    PTR_FREE, self.combinedThresholds
    PTR_FREE, self.combinedSpec
    PTR_FREE, self.combinedSpecErr

    self.combinedThresholds = PTR_NEW (combThresholds)
    self.combinedSpec       = PTR_NEW (spectrum)
    self.combinedSpecErr    = PTR_NEW (spectrumErr)

    ;== Reset axis ranges

    IF (KEYWORD_SET (full_reset)) THEN $
       self.range[0, *] = [MIN (combThresholds), MAX (combThresholds)]

    ind = WHERE (spectrum GT 0.0)
    IF (ind[0] EQ -1) THEN BEGIN
    	self.range[1, *] = [1.0, 10.]
    ENDIF ELSE self.range[1, *] = [MIN (spectrum[ind]),  MAX (spectrum[ind])]


END


; ----------------------------------------------------------------------------
; Set the default ranges based upon the current default selected ranges
; ----------------------------------------------------------------------------
PRO Spectrum::update, spectrumRange, FULL = full

    self->integrateTime
    self->combine, spectrum, spectrumErr, combThresholds

    ;== Set data ranges

    idx = WHERE (spectrum GT 0.0)
    IF KEYWORD_SET (full) THEN $
        self.default[0, *] = [MIN (combThresholds), MAX (combThresholds)]
    self.default[1, *] = [MIN (spectrum[idx]),  MAX (spectrum[idx])]

    IF (N_ELEMENTS (spectrumRange) NE 0) THEN BEGIN
        self.range = spectrumRange
    ENDIF 

END


; ----------------------------------------------------------------------------
; Plot data
; ----------------------------------------------------------------------------
PRO Spectrum::plot, OPLOT = oplot, $
    FGCOLOR = fgColor, BGCOLOR = bgColor, DRAWCOLOR = drawColor, $
    _EXTRA = extra

    units = self.Reader->units (/POINTER)

    IF (N_ELEMENTS (fgColor) EQ 0)   THEN fgColor   = !P.COLOR
    IF (N_ELEMENTS (bgColor) EQ 0)   THEN bgColor   = !P.BACKGROUND
    IF (N_ELEMENTS (drawColor) EQ 0) THEN drawColor = !P.COLOR
    
    myLogValues = self->logStatus()
    
    IF (NOT KEYWORD_SET (oplot)) THEN BEGIN

       ;== Provide default axis titles.  These can be overridden with the
       ;== _EXTRA keyword in the PLOT method call.
       
       ue = STRTRIM ((*units).eedges, 2)
       xTitle = 'Energy (' + ue + ')'
       
       ur = STRTRIM ((*units).rates, 2)
       yTitle = 'Rate (' + ur + '-' + ue + ')'

       
       PLOT, XLOG = myLogValues[0], YLOG = myLogValues[1], /NODATA, /YNOZERO, $
           self.range[0, *], self.range[1, *], $
           XTITLE = xTitle, YTITLE = yTitle, $
           XMARGIN = [12, 3], $
           COLOR = fgColor, BACKGROUND = bgColor, $
           _EXTRA = extra

    ENDIF
   
    ;== Plot data points
    mySpec = *self.combinedSpec
    combEnrg = *self.combinedThresholds
    myDelta = combEnrg[1, *] - combEnrg[0, *]
    poissonIsTrue = self.reader->getPoisson()
    IF poissonIsTrue THEN BEGIN
		myErrs = (SQRT(mySpec * self.livetime * myDelta) / myDelta) / self.livetime
    ENDIF ELSE BEGIN
		myerrs=*self.combinedSpecErr    
	ENDELSE
	
    self->drawHistory, combEnrg, mySpec, myErrs, $
        COLOR = drawColor, XLOG = myLogValues[0], YLOG = myLogValues[1], _EXTRA = extra

END


; ----------------------------------------------------------------------------
; Overplot data
; ----------------------------------------------------------------------------
PRO Spectrum::oplot, _EXTRA = extra

    self->plot, /OPLOT, _EXTRA = extra

END

    
; ----------------------------------------------------------------------------
; Overplot selected regions.  Regions specified by energySpan are used by 
; default unless a SPAN is requested.
; ----------------------------------------------------------------------------
PRO Spectrum::plotSpan, SPAN = span, ERASE = erase, _EXTRA = extra

    IF (N_ELEMENTS (span) EQ 0) THEN span = self->energySpan ()
    
    IF (KEYWORD_SET (erase)) THEN BEGIN
       
       self->PHA::plotSpan, $
           self->combinedThresholds (), self->combinedSpec (), span, $
           COLOR = !P.BACKGROUND, _EXTRA = extra

    ENDIF ELSE BEGIN
    
       self->PHA::plotSpan, $
           self->combinedThresholds (), self->combinedSpec (), span, $
           _EXTRA = extra

    ENDELSE
    
END

; ----------------------------------------------------------------------------
; Integrate a Spectrum over time.
;
; This method can be used in two ways:
;
;    1. By default, calling the integrate method will integrate the
;       Spectrum over the selected time range, and set the internal 
;       object data members self.integratedSpec, self.integratedSpecErr, 
;       and self.liveTime
;
;    2. To use this method as a utility method to integrate on
;       the fly, set the keywords SPECTRUM, ERRORS, LIVETIME to named 
;       variables to return the integrated spectrum, errors and the total
;       livetime.  If any of these keywords are set, the internal object 
;       members are not altered.
;
; INPUTS:
;
;    SPAN (optional) : specify an time span over which to integrate, 
;                      otherwise the time energy span is used
;
; KEYWORDS:
; 
;    LIVETIME : see note above
;    SPECTRUM : see note above
;    ERRORS   : see note above
;
; ----------------------------------------------------------------------------
PRO Spectrum::integrateTime, span, $
    SPECTRUM = spectrum, ERRORS = errors, LIVETIME = livetime

    data = self.Reader->data (/POINTER)
    timeLookup = self->timeLookup ()

    n = N_ELEMENTS (timeLookup) - 1
    combinedTimes = [ $
        ((*data).times)[0, timeLookup[0:n - 1]], $
        ((*data).times)[1, timeLookup[1:n] - 1]  $
    ]
    
    ;== Convert convex hull of timeSpan to indices

    IF (N_ELEMENTS (span) EQ 0) THEN $
       span = *self.timeSpan
    
    t_range = INTARR (2)
    numBins = (SIZE (timeLookup))[1] - 1
    edges = NEAREST_EDGE (combinedTimes, span[0, *], t_range)
    t_range = [timeLookup[t_range[0]],timeLookup[t_range[1] + 1 < numBins] - 1]

    n = SIZE ((*data).rates)
    spectra_t     = FLTARR (n[1])
    spectra_t_err = FLTARR (n[1])
    
    totalTime = 0.0

    ;== Number of selected time intervals
    
    num_t = (SIZE (t_range))[1] - 1
    
    ;== Check for multiple time intervals (possibly discontiguous)
    
    num_i = (SIZE (span))[1] - 1
    
    IF (num_i GT 1) THEN BEGIN

       num_t = num_i
       t_range = INTARR (2, num_t)

       FOR i = 0, num_t - 1 DO BEGIN

           indices = INTARR (2)
           edges = NEAREST_EDGE (combinedTimes, span[i + 1, *], indices)
           indices = [timeLookup[indices[0]],timeLookup[indices[1] + 1] - 1]
           t_range[*, i] = indices

       ENDFOR       

    ENDIF     

	;== Check for the presence of exposure data:
	haveExposure = 0
	IF (WHERE(tag_names(*data) EQ 'LIVETI') GT 0) THEN haveExposure = 1

    ;== Loop over number of intervals, integrating each interval over time
    FOR i = 0, num_t - 1 DO BEGIN
        ;== 03/15/10 RDP: this was never correct; to be largely rewritten!
;        nind = WHERE (((*data).errors)[*, t_range[0, i]: t_range[1, i]] NE 0., ncount)
;        live_t = ((*data).rates)[*, t_range[0, i]: t_range[1, i]] / $
;                (((*data).errors)[*, t_range[0, i]: t_range[1, i]] * $
;                 ((*data).errors)[*, t_range[0, i]: t_range[1, i]])
;        rate_size = (SIZE((*data).rates))[1]
;        IF (ncount EQ 0) THEN BEGIN
;           live_t[nind] = ((*data).times)[1, nind] - $          ;/ rate_size
;                          ((*data).times)[0, nind]              ; / rate_size
;        ENDIF
		IF haveExposure THEN live_t = ((*data).liveti)[t_range[0, i]: t_range[1, i]] $
		;== CCR #256: 03/30/2010 RDP: We need to TRANSPOSE live_t!
        ELSE live_t = TRANSPOSE(((*data).times)[1, t_range[0, i]: t_range[1, i]] - $          ;/ rate_size
                                ((*data).times)[0, t_range[0, i]: t_range[1, i]])              ; / rate_size
;        dt = TOTAL (live_t[nind], 1) / ncount                   ;rate_size
        dt = live_t                  ; TOTAL (live_t, 1) / rate_size
        totalTime = totalTime + TOTAL(dt)

        IF (SIZE (dt))[0] EQ 0 THEN BEGIN
           spectra_t = spectra_t + ((*data).rates)[*, t_range[0, i]: t_range[1, i]] * dt
           err = ((*data).errors)[*, t_range[0, i]: t_range[1, i]] * dt
        ENDIF ELSE BEGIN
           spectra_t = spectra_t + ((*data).rates)[*, t_range[0, i]: t_range[1, i]] # dt
           err = ((*data).errors)[*, t_range[0, i]: t_range[1, i]] # dt
        ENDELSE

        spectra_t_err = spectra_t_err + (err * err)
       
    ENDFOR

    ;== Set object internal data members or return the results        
    IF (ARG_PRESENT (spectrum) OR ARG_PRESENT (errors) OR $
       ARG_PRESENT (liveTime)) THEN BEGIN

       spectrum = spectra_t / totalTime
       errors   = SQRT (spectra_t_err) / totalTime
       liveTime = totalTime
       
    ENDIF ELSE BEGIN
       
       PTR_FREE, self.integratedSpec
       PTR_FREE, self.integratedSpecErr

       self.integratedSpec    = PTR_NEW (spectra_t / totalTime)
       self.integratedSpecErr = PTR_NEW (SQRT (spectra_t_err) / totalTime)

       self.liveTime = totalTime
   
    ENDELSE
     
END    


; ----------------------------------------------------------------------------
; Return class data
;
; WARNING: Allows return of pointer-to-data, which violates
; encapsulation and can compromise the object internal state.
; ----------------------------------------------------------------------------
FUNCTION Spectrum::combinedThresholds, POINTER = pointer

    RETURN, (KEYWORD_SET (pointer) ? self.combinedThresholds : $
        *self.combinedThresholds)
    
END

FUNCTION Spectrum::combinedSpec, POINTER = pointer

    RETURN, (KEYWORD_SET (pointer) ? self.combinedSpec : *self.combinedSpec)
    
END

FUNCTION Spectrum::combinedSpecErr, POINTER = pointer

    RETURN, (KEYWORD_SET (pointer) ? self.combinedSpecErr: *self.combinedSpecErr)
    
END

FUNCTION Spectrum::liveTime

    RETURN, self.liveTime
    
END

FUNCTION Spectrum::logStatus

    RETURN, self.logStatus
    
END

PRO Spectrum::setLogStatus, status

    siz = SIZE(status)
    IF (siz[0] EQ 1) AND (siz[1] EQ 2) THEN BEGIN
        IF (status[0] LT 2) AND (status[1] LT 2) AND $
           (status[0] GE 0) AND (status[1] GE 0) THEN $
            self.logStatus = status
    ENDIF

END

;------------------------------------------------------------------------------
; Bin by Significance to a target SNR
;------------------------------------------------------------------------------
PRO Spectrum::binBySignificance, backgroundObJ, thresholds, targetSNR, $
              ERROR = error

    error = 0
    data = self.Reader->data (/POINTER)
    eedges = (*data).eedges
   
    lookup = *self.energyLookup
    lookTop = lookup[N_ELEMENTS (lookup) - 1] + 1
    lookup = LINDGEN (lookTop)       ;== Start out fresh
    backgroundObJ->PHA::setEnergyLookup, lookup, /INITIALIZE

    self->PHA::combine, $
        *self.integratedSpec, *self.integratedSpecErr, $
        eedges, lookup, $
        spectrum, spectrumErr, combEdges, LIVETIME = self.livetime  ;== Count / s - keV

    span = self->PHA::energySpan()
    COUNT_SPAN, combEdges, span[0, *], tidx
    
    bgModel = backgroundObJ->haveModel()
    IF (NOT bgModel) THEN BEGIN
        error = 1
	RETURN
    ENDIF
    
    backgroundObJ->integrate, thresholds, self->PHA::timeSpan(), $
                   self->PHA::timeLookup(), /TIME,               $
                   SPECTRUM = backspectrum, ERRORSPECTRUM = backErrs
                                          ;== Count / s 
    deltaE = combEdges[1, *] - combEdges[0, *]
    backspectrum = backspectrum / deltaE
    backErrs     = backErrs / deltaE
    
    top    = MAX (tidx)
    bottom = MIN (tidx)
    index  = 0
    done   = 0
    WHILE (NOT done) DO BEGIN
        spInd = lookup[bottom + index]
        signal = 0.0
        noise  = 0.0
        binSNR = self->PHA::calcSNR(spInd, spectrum, spectrumErr, $
                 backspectrum, backErrs, signal, noise, LIVETIME = self.livetime)
        WHILE ((binSNR LT (0.8 * targetSNR)) AND (NOT done)) DO BEGIN
            index = index + 1
            spInd = lookup[bottom + index]
            IF ((bottom + index + 1) GE top) THEN BEGIN
                done = 1
            ENDIF ELSE BEGIN
                binSNR = self->PHA::calcSNR(spInd, spectrum, spectrumErr, $
                         backspectrum, backErrs, signal, noise, LIVETIME = self.livetime)
            ENDELSE
        ENDWHILE
        
        IF ((binSNR LT targetSNR) AND (NOT done)) THEN BEGIN
            index = index + 1
            spInd = lookup[bottom + index]
            IF ((bottom + index + 1) GE top) THEN BEGIN
                done = 1
            ENDIF ELSE BEGIN
                binSNR = self->PHA::calcSNR(spInd, spectrum, spectrumErr, $
                         backspectrum, backErrs, signal, noise, LIVETIME = self.livetime)
                IF (binSNR GT (1.2 * targetSNR)) THEN BEGIN
                    index = index - 1
                ENDIF
            ENDELSE
        ENDIF
        
        IF (index GT 0) THEN BEGIN
            IF (top LE (bottom + index + 1)) THEN BEGIN
                index = top - bottom 
                done = 1
            ENDIF
            lookup = [lookup[0: bottom], lookup[bottom + index + 1: *]]
            nChannels = N_ELEMENTS (lookup) - 1
            bottom = bottom + 1
            top    = top - index
            index  = 0
        ENDIF ELSE BEGIN
            bottom = bottom + 1
            IF (bottom EQ top) THEN done = 1
            index  = 0
        ENDELSE
            
    ENDWHILE

    self->setEnergyLookup, lookup
    self->integrateTime
    self->combine

END


;------------------------------------------------------------------------------
; Rebin
;------------------------------------------------------------------------------
PRO Spectrum::rebin,                 $
    SELECTIONS     = selections,     $
    FULLRESOLUTION = fullresolution, $
    INTERVAL       = interval,       $
    NBINS          = nbins,          $
    BYHALF         = byHalf,         $
    REFINESINGLE   = refineSingle,   $
    SINGLEBIN      = singleBin,      $
    DIALOG_PARENT  = dialog_parent
    
    IF (KEYWORD_SET (selections)) THEN BEGIN

       span   = self->energySpan ()
       lookup = self->energyLookup ()
         
       regions = SIZE (span)
         
       IF (regions[1] - 1 GT 0) THEN BEGIN

          lower = 1
          upper = regions[1] - 1

       ENDIF ELSE BEGIN

          lower = 0
          upper = 0

       ENDELSE

       idx = INTARR(2)
       FOR i = upper, lower, -1 DO BEGIN

           ER = NEAREST_EDGE (self->combinedThresholds (), span[i, *], idx)
           lookup = [lookup[0:idx[0]], lookup[idx[1] + 1: *]]

       ENDFOR
       
       self->setEnergyLookup, lookup
      
    ENDIF ; selections
    
    
    IF (KEYWORD_SET (fullResolution)) THEN BEGIN

       self->setEnergyLookup, /INITIALIZE

    ENDIF ; full resolution
     

    IF (KEYWORD_SET (nbins)) THEN BEGIN
    
       span   = TRANSPOSE ([[interval], [interval]])
       lookup = self->energyLookup ()
       ER = NEAREST_EDGE (self->combinedThresholds (), span[0, *], idx)
	  LOOP_TOP = (idx[1] - idx[0] + 1) / nbins
	  
	  FOR LOOP = 0, (LOOP_TOP - 1) DO BEGIN
	         lookup = [lookup[0: idx[0] + LOOP], $
	                  lookup[idx[0] + LOOP + nbins: *]]
	       ENDFOR

       self->setEnergyLookup, lookup
       self->integrateTime
       self->combine
	       
       RETURN ; INTERVAL will also be set; we don't want to fall through
       
    ENDIF ; bin every n bins
     

    IF (KEYWORD_SET (refineSingle)) THEN BEGIN
    
       span   = TRANSPOSE ([[singleBin], [singleBin]])
       lookup = self->energyLookup ()
       ER = NEAREST_EDGE (self->combinedThresholds (), span[0, *], idx)
       j0 = lookup[idx[0]]
       j1 = lookup[idx[0] + 1]
       n = j1 - j0 - 1
	  
	  IF (n GT 0) THEN BEGIN
	         lookup = [lookup[0: idx[0]], j0 + INDGEN (n) + 1, $
	                  lookup[idx[0] + 1: *]]
	       ENDIF

       self->setEnergyLookup, lookup
       
    ENDIF ; refine a single bin 
     

    IF (KEYWORD_SET (byHalf)) THEN BEGIN
    
       span   = TRANSPOSE ([[singleBin], [singleBin]])
       lookup = self->energyLookup ()
       ER = NEAREST_EDGE (self->combinedThresholds (), span[0, *], idx)
       j0 = lookup[idx[0]]
       j1 = lookup[idx[0] + 1]
       n = j1 - j0 - 1
	  
	  IF (n GT 0) THEN BEGIN
	         lookup = [lookup[0: idx[0]], j0 + n / 2 + 1, $
	                  lookup[idx[0] + 1: *]]
	       ENDIF

       self->setEnergyLookup, lookup
       
    ENDIF ; refine by half
     

    IF (KEYWORD_SET (interval)) THEN BEGIN

;       span   = TRANSPOSE ([[interval], [interval]])
       lookup = self->energyLookup ()
         
;       regions = SIZE (span)
;         
;       IF (regions[1] - 1 GT 0) THEN BEGIN
;
;          lower = 1
;          upper = regions[1] - 1
;
;       ENDIF ELSE BEGIN
;
;          lower = 0
;          upper = 0
;
;       ENDELSE

       idx = INTARR(2)
;       FOR i = upper, lower, -1 DO BEGIN

           ER = NEAREST_EDGE (self->combinedThresholds (), interval, idx)
           IF idx[1] LT idx[0] THEN idx[1] = idx[0]
           lookup = [lookup[0:idx[0]], lookup[idx[1] + 1: *]]

;       ENDFOR
       
       self->setEnergyLookup, lookup
      
    ENDIF ; interval

    self->integrateTime
    self->combine

END


; ----------------------------------------------------------------------------
; Object definition
; ----------------------------------------------------------------------------
PRO Spectrum__define

    obj = { SPECTRUM, INHERITS PHA, $

        combinedThresholds : PTR_NEW (), $
        combinedSpec       : PTR_NEW (), $
        combinedSpecErr    : PTR_NEW (), $

        integratedSpec     : PTR_NEW (), $
        integratedSpecErr  : PTR_NEW (), $
        
        ;== XY Axes Log status
        
        logStatus          : [0, 0],     $
        
        ;== Scalar livetime, averaged over non-zero energy bins
        
        liveTime           : 0.0         $     
           
    }

END
