; ----------------------------------------------------------------------------
;+
; NAME:
;
;     Lightcurve (OBJECT)
;
; PURPOSE:
;
;     An object to encapsulate a detector lightcurve.
;
; CALLING SEQUENCE:
;
;     obj = OBJ_NEW ('Lightcurve', Reader)
;
; INPUTS:
;
;     Reader : A reference to a DataReader object.  This object typically
;         will be a subclass of DataReader that encapsulates the data
;         for a specific detector.
;
; KEYWORDS:
;
;     NONE
;
; INHERITS:
;
;     PHA
;
; DEPENDENCIES:
;
;     pha__define.pro
;     nearest_edge.pro
;
; PUBLIC METHODS:
;     
;     plot (PROCEDURE) - Plot the lightcurve data on the current device
;         Inputs: NONE
;        Outputs: NONE 
;       Keywords: OPLOT - overplot on the current device
;                 Any valid IDL PLOT keywords
;
;     oplot (PROCEDURE) - Overplot the lightcurve data on the current device
;         Inputs: NONE
;        Outputs: NONE 
;       Keywords: Any valid IDL OPLOT keywords
;
;     integrate (FUNCTION) - Integrate the lightcurve over energy channels
;         Inputs: NONE, but uses current energySpan, energyLookup arrays
;                 to obtain integration intervals.  See the setEnergySpan
;                 and setEnergyLookup methods, inherited from class PHA
;        Outputs: integratedHist - the integrated lightcurve data
;                 integratedHistErr - errors on integratedHist
;       Keywords: TOTALENERGY - set this keyword to a named variable to return
;                 the total integrated energy
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
FUNCTION Lightcurve::init, Reader
   
    IF (NOT self->pha::init (Reader)) THEN $
       RETURN, 0
    
    self->update, /FULL

    RETURN, 1

END


; ----------------------------------------------------------------------------
; Destructor
; ----------------------------------------------------------------------------
PRO Lightcurve::cleanup

    PTR_FREE, self.combinedTimes 
    PTR_FREE, self.combinedHist
    PTR_FREE, self.combinedHistErr

    PTR_FREE, self.integratedHist
    PTR_FREE, self.integratedHistErr

    self->pha::cleanup

END   


; ----------------------------------------------------------------------------
; Combine
;
; Set the keyword FULL_RESET to reset both x and y axis ranges.  By default,
; only the y axis range is reset in order to preserve the user's current
; x range setting.
; ----------------------------------------------------------------------------
PRO Lightcurve::combine, history, historyErr, combTimes, $
    FULL_RESET = full_reset

    data = self.Reader->data (/POINTER)

    self->PHA::combine, $
        *self.integratedHist, *self.integratedHistErr, $
        (*data).times, *self.timeLookup, $
        history, historyErr, combTimes

    PTR_FREE, self.combinedTimes
    PTR_FREE, self.combinedHist
    PTR_FREE, self.combinedHistErr

    self.combinedTimes   = PTR_NEW (combTimes)
    self.combinedHist    = PTR_NEW (history)
    self.combinedHistErr = PTR_NEW (historyErr)

    ;== Reset axis ranges
    
    IF (KEYWORD_SET (full_reset)) THEN $
       self.range[0, *] = [MIN (combTimes), MAX (combTimes)]
    
    self.range[1, *] = [MIN (history),   MAX (history)]

END


; ----------------------------------------------------------------------------
; Set the default ranges based upon the current default selected ranges
; ----------------------------------------------------------------------------
PRO Lightcurve::update, historyRange, FULL = full

    self->integrateEnergy
    self->combine, history, historyErr, combTimes
    
    ;== Set data ranges
    IF KEYWORD_SET (full) THEN $
        self.default[0, *] = [MIN (combTimes), MAX (combTimes)]
    self.default[1, *] = [MIN (history),   MAX (history)] 

    IF (N_ELEMENTS (historyRange) NE 0) THEN BEGIN
        self.range = historyRange 
        self.range[1, 1] = historyRange[1, 1] > self.default[1, 1]
    ENDIF 

END


; ----------------------------------------------------------------------------
; Plot data
; ----------------------------------------------------------------------------
PRO Lightcurve::plot, OPLOT = oplot, $
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

       u = STRTRIM ((*units).times, 2)
       xTitle = 'Time Since TSTART (' + u + ')' 

       u = STRTRIM ((*units).rates, 2)
       yTitle = 'Rate (' + u + ')'

       PLOT, XLOG = myLogValues[0], YLOG = myLogValues[1], /NODATA, /YNOZERO, $
           self.range[0, *], self.range[1, *], $
           XTITLE = xTitle, YTITLE = yTitle, $
           XMARGIN = [12, 3], $
           COLOR = fgColor, BACKGROUND = bgColor, $
           _EXTRA = extra

    ENDIF
   
    ;== Plot data points

    self->drawHistory, $
        *self.combinedTimes, *self.combinedHist, *self.combinedHistErr, $
        COLOR = drawColor, XLOG = myLogValues[0], YLOG = myLogValues[1], _EXTRA = extra
 
END


; ----------------------------------------------------------------------------
; Overplot data
; ----------------------------------------------------------------------------
PRO Lightcurve::oplot, _EXTRA = extra

    self->plot, /OPLOT, _EXTRA = extra

END


; ----------------------------------------------------------------------------
; Overplot selected regions.  Regions specified by timeSpan are used by 
; default unless a SPAN is requested.
; ----------------------------------------------------------------------------
PRO Lightcurve::plotSpan, SPAN = span, ERASE = erase, _EXTRA = extra

    IF (N_ELEMENTS (span) EQ 0) THEN span = self->timeSpan ()
    
    IF (KEYWORD_SET (erase)) THEN BEGIN
       
       self->PHA::plotSpan, $
           self->combinedTimes (), self->combinedHist (), span, $
           COLOR = !P.BACKGROUND, _EXTRA = extra

    ENDIF ELSE BEGIN
    
       self->PHA::plotSpan, $
           self->combinedTimes (), self->combinedHist (), span, $
           _EXTRA = extra

    ENDELSE
    
END


; ----------------------------------------------------------------------------
; Integrate a lightcurve over energy.
;
; This method can be used in two ways:
;
;    1. By default, calling the integrate method will integrate the
;       Lightcurve over the selected energy range, and set the internal 
;       object data members self.integratedHist and self.integratedHistErr.
;
;    2. To use this method as a utility method to integrate on
;       the fly, set the keywords HISTORY, ERRORS to named variables to
;       return the integrated lightcurve and errors.  If any of these
;       keywords are set, the internal object members are not altered.
;
; INPUTS:
;
;    SPAN (optional) : specify an energy span over which to integrate, 
;                      otherwise the current energy span is used
;
; KEYWORDS:
; 
;    PERENERGY   : return counts / time-energy (default is counts / time)    
;    TOTALENERGY : return the total energy
;    HISTORY     : see note above
;    ERRORS      : see note above
;
; ----------------------------------------------------------------------------
PRO Lightcurve::integrateEnergy, span, $
    TOTALENERGY = totalEnergy, PERENERGY = perEnergy, $
    HISTORY = history, ERRORS = errors

    data = self.Reader->data (/POINTER)

    n = N_ELEMENTS (*self.energyLookup) - 1
    combinedThresholds = [ $
        ((*data).eedges)[0, (*self.energyLookup)[0:n - 1]], $
        ((*data).eedges)[1, (*self.energyLookup)[1:n] - 1]  $
    ]
    
    n = SIZE ((*data).rates)
    i_rates  = FLTARR (n[2])
    i_errors = FLTARR (n[2])
    
    totalEnergy = 0.0

    lower = 0
    upper = 0
    
    IF (N_ELEMENTS (span) EQ 0) THEN $
       span = *self.energySpan
       
    n_regions = SIZE (span)
    
    IF ((n_regions[1] - 1) GT 0) THEN BEGIN

       lower = 1
       upper = n_regions[1] - 1

    ENDIF

    range = INTARR (2)
    numBins = (SIZE (*self.energyLookup))[1] - 1
    FOR i = lower, upper DO BEGIN

        r = NEAREST_EDGE (combinedThresholds, span[i, *], range)
        range[0] = (*self.energyLookup)[range[0]]
        range[1] = (*self.energyLookup)[range[1] + 1 < numBins] - 1

;        FOR j = range[0], range[1] DO BEGIN
;            
;	    deltaEnergy = ((*data).eedges)[1, j] - ((*data).eedges)[0, j]
;            totalEnergy = totalEnergy + deltaEnergy
;            i_rates = i_rates + ((*data).rates)[j, *]
;            err = ((*data).errors)[j, *]
;            i_errors = i_errors + (err * err)
;        
;	ENDFOR
		deltaEnergy = ((*data).eedges)[1, range[0]: range[1]] - $
					  ((*data).eedges)[0, range[0]: range[1]]
		totalEnergy = totalEnergy + TOTAL (deltaEnergy)
		i_rates = i_rates + TOTAL (((*data).rates)[range[0]: range[1], *], 1)
		err = ((*data).errors)[range[0]: range[1], *]^2.
		i_errors = i_errors + TOTAL (err, 1)

    ENDFOR

    deltaTime = ((*data).times)[1, *] - ((*data).times)[0, *]
    i_rates  = i_rates * deltaTime
    i_errors = SQRT (i_errors) * deltaTime


    ;== Divide by bin width to get counts / second-energy
    
    IF (KEYWORD_SET (perEnergy)) THEN BEGIN       

       i_rates  = i_rates  / totalEnergy
       i_errors = i_errors / totalEnergy

    ENDIF


    ;== Set object internal data members or return the results
        
    IF (ARG_PRESENT (history) OR ARG_PRESENT (errors)) THEN BEGIN
    
       history = i_rates
       errors  = i_errors
       
    ENDIF ELSE BEGIN
    
       PTR_FREE, self.integratedHist
       self.integratedHist = PTR_NEW (i_rates)

       PTR_FREE, self.integratedHistErr
       self.integratedHistErr = PTR_NEW (i_errors)

    ENDELSE
       

END    

; ----------------------------------------------------------------------------
PRO Lightcurve::integrateTimeFast, span, $
    TOTALTIME = totalTime, $
    HISTORY = history, ERRORS = errors

    data = self.Reader->data (/POINTER)

    n = N_ELEMENTS (*self.energyLookup) - 1
    combinedThresholds = [ $
        ((*data).eedges)[0, (*self.energyLookup)[0:n - 1]], $
        ((*data).eedges)[1, (*self.energyLookup)[1:n] - 1]  $
    ]
    
    COUNT_SPAN, (*data).times, span, ttidx
    tempRates = ((*data).rates)[*, ttidx]
    tempErrs  = ((*data).errors)[*, ttidx]
	;== Check for the presence of exposure data:
	haveExposure = 0
	IF (WHERE(tag_names(*data) EQ 'LIVETI') GT 0) THEN haveExposure = 1
	IF haveExposure THEN live_t = ((*data).liveti)[ttidx] $
	;== CCR #256: 03/30/2010 RDP: We need to TRANSPOSE live_t!
	ELSE live_t = TRANSPOSE(((*data).times)[1, ttidx] - $          ;/ rate_size
							((*data).times)[0, ttidx])              ; / rate_size
    
    n = SIZE (tempRates)
    i_rates  = FLTARR (n[1])
    i_errors = FLTARR (n[1])
    
    totalTime = 0.0

    espan = *self.energySpan
    COUNT_SPAN, (*data).eedges, espan, etidx
    
    numbins = N_ELEMENTS(etidx)
    lower = etidx[0]
    upper = etidx[numbins - 1]
    
    FOR i = lower, upper DO BEGIN

		i_rates[i] = i_rates[i] + TOTAL (tempRates[i, *] * live_t)
		err = (tempErrs[i, *] * live_t)^2.
		i_errors[i] = i_errors[i] + TOTAL (err)

    ENDFOR

    ;== Set object internal data members or return the results
	totalTime = TOTAL(live_t)
	history = i_rates / totalTime
	errors  = SQRT (i_errors) / totalTime
       

END    

; ----------------------------------------------------------------------------
; Integrate a lightcurve over time.  
;
; INPUTS:
;
;    SPAN (optional) : specify an time span over which to integrate, 
;                      otherwise the current time span is used
;
; KEYWORDS:
; 
;    TOTALTIME        : return the total time
;    HISTORY, ERRORS  : set these keywords to named variables to
;                       return the integrated lightcurve and errors
;
; ----------------------------------------------------------------------------
PRO Lightcurve::integrateTime, span, $
    TOTALTIME = totalTime, $
    HISTORY = history, ERRORS = errors

    data = self.Reader->data (/POINTER)

    IF (N_ELEMENTS (span) EQ 0) THEN $
       span = self->timeSpan()
       
    COUNT_SPAN, self->combinedTimes (), span, tidx
    
    ;== Something's wrong: no valid selection
    If (tidx[0] EQ -1) THEN BEGIN
        totalTime = 0.0
        RETURN
    ENDIF    
    
    range = LONARR (2, N_ELEMENTS (tidx))
    numBins = N_ELEMENTS (*self.timeLookup) - 1
    range[0, *] = (*self.timeLookup)[tidx]
    range[1, *] = (*self.timeLookup)[tidx + 1 < numBins] - 1

    n = SIZE ((*data).rates)
    i_rates   = FLTARR (n[1])
    i_errors  = FLTARR (n[1])
    totalTime = 0.0
    
    rangeSize = SIZE (range)
    numTimes = (rangeSize[0] EQ 1) ? 1 : rangeSize[2]

    ;== Background subtracted data files
    
; TODO: Handle background subtracted data files?    
;    header2 = self.BfitsReader->header (2)
;    bkgdSubtracted = SXPAR (header2, 'BCKGSUBT')
    
    ;== Loop over number of intervals, integrating each interval over energy

    FOR k = 0L, numTimes - 1L DO BEGIN

        FOR i = range[0, k], range[1, k] DO BEGIN
            
            idx = WHERE (((*data).rates)[*, i] GT 0, cnt)
;            IF (bkgdSubtracted) THEN $
;               cnt = 0

            IF (cnt GT 0) THEN BEGIN

               live_t = ((*data).rates)[idx, i] / $
                   (((*data).errors)[idx, i] * ((*data).errors)[idx, i])

               ;== Make the livetime the average
               
               dt = TOTAL (live_t) / N_ELEMENTS (idx)   

            ENDIF ELSE BEGIN

               dt = ((*data).times)[1, i] - ((*data).times)[0, i]

            ENDELSE

            totalTime = totalTime + dt

            i_rates = i_rates + ((*data).rates)[*, i] * dt
            err = ((*data).errors)[*, i] * dt
            i_errors = i_errors + err * err
            
        ENDFOR ; i

    ENDFOR ; k

    history = i_rates / totalTime
    errors  = SQRT (i_errors) / totalTime

END


; ----------------------------------------------------------------------------
; Return class data
;
; WARNING: Allows return of pointer-to-data, which violates
; encapsulation and can compromise the object internal state.
; ----------------------------------------------------------------------------
FUNCTION Lightcurve::combinedTimes, POINTER = pointer

    RETURN, (KEYWORD_SET (pointer) ? self.combinedTimes : *self.combinedTimes)
    
END

FUNCTION Lightcurve::combinedHist, POINTER = pointer

    RETURN, (KEYWORD_SET (pointer) ? self.combinedHist : *self.combinedHist)
    
END

FUNCTION Lightcurve::combinedHistErr, POINTER = pointer

    RETURN, (KEYWORD_SET (pointer)? self.combinedHistErr: *self.combinedHistErr)
    
END

FUNCTION Lightcurve::logStatus

    RETURN, self.logStatus
    
END

PRO Lightcurve::setLogStatus, status

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
PRO Lightcurve::binBySignificance, backgroundObJ, thresholds, targetSNR, $
                ERROR = error

    error = 0
    data = self.Reader->data (/POINTER)
    times = (*data).times
    
    lookup = *self.timeLookup
    lookTop = lookup[N_ELEMENTS (lookup) - 1L] + 1L
    lookup = LINDGEN (lookTop)       ;== Start out fresh
    backgroundObJ->PHA::setTimeLookup, lookup, /INITIALIZE

    self->PHA::combine, $
        *self.integratedHist, *self.integratedHistErr, $
        times, lookup, $
        history, historyErr, combTimes  ;== Count / s

    span = self->timeSpan()
    COUNT_SPAN, combTimes, span[0, *], tidx
    
    ; Check to see that we have a background model:
    bgModel = backgroundOBJ->haveModel()
    IF (NOT bgModel) THEN BEGIN
        error = 1
	RETURN
    ENDIF
    
    ; We will ask for the background rates for the full time range
    backgroundObJ->integrate, thresholds, self->PHA::energySpan(), $
                   self->PHA::energyLookup(), /ENERGY,             $
                   HISTORY = backHistory, /FULLTIME
    backErrs = FLTARR (N_ELEMENTS (backHistory))
                       
    top    = LONG (MAX (tidx))
    bottom = LONG (MIN (tidx))
    
    ;== RDP: Better than nothing, which is what we had before!
    backErrs[*] = SQRT(VARIANCE(backHistory[bottom: top]))

    index  = 0L
    done   = 0
    WHILE (NOT done) DO BEGIN
        spInd = lookup[bottom + index]
        signal = 0.0
        noise  = 0.0
        binSNR = self->PHA::calcSNR(spInd, history, historyErr, $
                 backHistory, backErrs, signal, noise)
        WHILE ((binSNR LT (0.8 * targetSNR)) AND (NOT done)) DO BEGIN
            index = index + 1L
            spInd = lookup[bottom + index]
            IF ((bottom + index + 1) GE top) THEN BEGIN 
                done = 1
            ENDIF ELSE BEGIN
                binSNR = self->PHA::calcSNR(spInd, history, historyErr, $
                         backHistory, backErrs, signal, noise)
            ENDELSE
        ENDWHILE
        
        IF ((binSNR LT targetSNR) AND (NOT done)) THEN BEGIN
            index = index + 1L
            spInd = lookup[bottom + index]
            IF ((bottom + index + 1) GE top) THEN BEGIN
                done = 1
            ENDIF ELSE BEGIN
                binSNR = self->PHA::calcSNR(spInd, history, historyErr, $
                         backHistory, backErrs, signal, noise)
                IF (binSNR GT (1.2 * targetSNR)) THEN BEGIN
                    index = index - 1L
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
            bottom = bottom + 1L
            top    = top - index
            index  = 0L
        ENDIF ELSE BEGIN
            bottom = bottom + 1L
            IF (bottom EQ top) THEN done = 1
            index  = 0L
        ENDELSE
            
    ENDWHILE

    self->setTimeLookup, lookup
    self->integrateEnergy
    self->combine

END


;------------------------------------------------------------------------------
; Rebin
;------------------------------------------------------------------------------
PRO Lightcurve::rebin, $
    TEMPORAL       = temporal,       $
    SELECTIONS     = selections,     $
    FULLRESOLUTION = fullResolution, $
    INTERVAL       = interval,       $
    NBINS          = nbins,          $
    BYHALF         = byHalf,         $
    REFINESINGLE   = refineSingle,   $
    SINGLEBIN      = singleBin,      $
    DIALOG_PARENT  = dialog_parent

    IF (KEYWORD_SET (temporal)) THEN BEGIN
    
       timeRes = DIALOG_INPUT ( $
           PROMPT = ['Enter data resolution (s).', ' ', $
                     'NOTE: Resolution must be an integer', $
                     'multiple of the independent axis data.'], $
           DIALOG_PARENT = dialog_parent, TITLE = 'Rebin')

       IF (timeRes EQ '') THEN $
          RETURN

       self->setTimeLookup, myLookup, /INITIALIZE
       self->integrateEnergy
       self->combine

       ; REBIN expects arrays of size (n_chan, n_pts)
       
       n = (SIZE (integratedHist))[1]

       ; REBIN expects arrays of size (n_chan, n_pts)
       
       n = (SIZE (self->combinedHist ()))[1]
       REBIN_DATA, $
           self->combinedTimes (), REFORM (self->combinedHist (), 1, n), $
           ERR_IN = REFORM (self->combinedHistErr (), 1, n), $
           rebinned_times, rebinned_spectra, $
           ;ERR_OUT = rebinned_spectra_err, $
           RESOLUTION = timeRes, ERROR = error, LOOKUP = myLookup
	   
       IF (NOT error) THEN BEGIN

          ;PTR_FREE, self.combinedTimes
          ;PTR_FREE, self.combinedHist         
          ;PTR_FREE, self.combinedHistErr 

          ;self.combinedTimes   = PTR_NEW (rebinned_times)
          ;self.combinedHist    = PTR_NEW (REFORM (rebinned_spectra))          
          ;self.combinedHistErr = PTR_NEW (REFORM (rebinned_spectra_err))

          ; Reset the y-axis range only
          
          ;minv = MIN (*self.combinedHist, MAX = maxv)
          ;self.range[1, *] = [minv, maxv]
	  self->setTimeLookup, myLookup
	  self->update

       ENDIF
       
       RETURN
       
    ENDIF ; temporal
    
    
    IF (KEYWORD_SET (nbins)) THEN BEGIN
    
       span   = TRANSPOSE ([[interval], [interval]])
       lookup = self->timeLookup ()
       ER = NEAREST_EDGE (self->combinedTimes (), span[0, *], idx)
	  LOOP_TOP = (idx[1] - idx[0] + 1) / nbins
	  
	  FOR LOOP = 0L, (LOOP_TOP - 1) DO BEGIN
	         lookup = [lookup[0: idx[0] + LOOP], $
	                  lookup[idx[0] + LOOP + nbins: *]]
	       ENDFOR

       self->setTimeLookup, lookup
  ;     self->integrateTime
       self->integrateEnergy
       self->combine
	       
       RETURN ; INTERVAL will also be set; we don't want to fall through
       
    ENDIF ; bin every n bins
     

    IF (KEYWORD_SET (selections)) THEN BEGIN

       span   = self->timeSpan ()
       lookup = self->timeLookup ()
         
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

           ER = NEAREST_EDGE (self->combinedTimes (), span[i, *], idx)
           lookup = [lookup[0:idx[0]], lookup[idx[1] + 1: *]]

       ENDFOR
       
       self->setTimeLookup, lookup
      
    ENDIF ; selections
    

    IF (KEYWORD_SET (fullResolution)) THEN BEGIN

       self->setTimeLookup, /INITIALIZE

    ENDIF ; full resolution
 
     
    IF (KEYWORD_SET (refineSingle)) THEN BEGIN
    
       span   = TRANSPOSE ([[singleBin], [singleBin]])
       lookup = self->timeLookup ()
       ER = NEAREST_EDGE (self->combinedTimes (), span[0, *], idx)
       j0 = lookup[idx[0]]
       j1 = lookup[idx[0] + 1]
       n = j1 - j0 - 1
	  
	  IF (n GT 0) THEN BEGIN
	         lookup = [lookup[0: idx[0]], j0 + INDGEN (n) + 1, $
	                  lookup[idx[0] + 1: *]]
	       ENDIF

       self->setTimeLookup, lookup
       
    ENDIF ; refine a single bin 
     

    IF (KEYWORD_SET (byHalf)) THEN BEGIN
    
       span   = TRANSPOSE ([[singleBin], [singleBin]])
       lookup = self->timeLookup ()
       ER = NEAREST_EDGE (self->combinedTimes (), span[0, *], idx)
       j0 = lookup[idx[0]]
       j1 = lookup[idx[0] + 1]
       n = j1 - j0 - 1
	  
	  IF (n GT 0) THEN BEGIN
	         lookup = [lookup[0: idx[0]], j0 + n / 2 + 1, $
	                  lookup[idx[0] + 1: *]]
	       ENDIF

       self->setTimeLookup, lookup
       
    ENDIF ; refine by half
     

    IF (KEYWORD_SET (interval)) THEN BEGIN

;       span   = TRANSPOSE ([[interval], [interval]])
       lookup = self->timeLookup ()
         
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

           ER = NEAREST_EDGE (self->combinedTimes (), interval, idx)
           IF idx[1] LT idx[0] THEN idx[1] = idx[0]
           lookup = [lookup[0:idx[0]], lookup[idx[1] + 1: *]]

;       ENDFOR

       self->setTimeLookup, lookup

    ENDIF ; interval
       
    self->integrateEnergy
    self->combine

END


; ----------------------------------------------------------------------------
; Object definition
; ----------------------------------------------------------------------------
PRO Lightcurve__define

    obj = { LIGHTCURVE, INHERITS PHA, $

        combinedTimes     : PTR_NEW (), $
        combinedHist      : PTR_NEW (), $
        combinedHistErr   : PTR_NEW (), $
        
        integratedHist    : PTR_NEW (), $
        integratedHistErr : PTR_NEW (), $
        
        ;== XY Axes Log status
        
        logStatus         : [0, 0]      $

    }

END
