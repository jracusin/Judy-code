; ----------------------------------------------------------------------------
;+
; NAME:
;
;     TTELightcurve (OBJECT)
;
; PURPOSE:
;
;     A decorator-type design pattern object to enhance the Lightcurve object for TTE data. 
;     Only the rebin function is replaced wholesale; some utility routines are added to 
;     support rebinning of event data. With TTELighcurve between Lightcurve and PHADisplay, 
;     the user should not be able to tell the difference, as far as interface is concerned, 
;     between TTE and regular PHA-II type data.
;
; CALLING SEQUENCE:
;
;     obj = OBJ_NEW ('TTELightcurve', Reader)
;
; INPUTS:
;
;     Reader : A reference to a TTEReader object.  Only a TTEReader will support the rebinning
;              operations herein.
;
; KEYWORDS:
;
;     NONE
;
; INHERITS:
;
;     Lightcurve
;
; DEPENDENCIES:
;
;     lightcurve__define.pro
;     pha__define.pro
;     nearest_edge.pro
;
; PUBLIC METHODS:
;     
;     rebin (FUNCTION) - Integrate the TTELightcurve over energy channels
;         Inputs: NONE, but uses current energySpan, energyLookup arrays
;                 to obtain integration intervals.  See the setEnergySpan
;                 and setEnergyLookup methods, inherited from class PHA
;        Outputs: integratedHist - the integrated TTELightcurve data
;                 integratedHistErr - errors on integratedHist
;       Keywords: TOTALENERGY - set this keyword to a named variable to return
;                 the total integrated energy
;
; MODIFICATION HISTORY:
;
;     Written, 2008 December, RDP @ UAH
;
;-
; ----------------------------------------------------------------------------


; ----------------------------------------------------------------------------
; Constructor
; ----------------------------------------------------------------------------
FUNCTION TTELightcurve::init, Reader
   
    IF (NOT self->Lightcurve::init (Reader)) THEN $
       RETURN, 0

    RETURN, 1

END


; ----------------------------------------------------------------------------
; Destructor
; ----------------------------------------------------------------------------
PRO TTELightcurve::cleanup

    self->Lightcurve::cleanup

END   


;------------------------------------------------------------------------------
; Bin by Significance to a target SNR
;------------------------------------------------------------------------------
PRO TTELightcurve::binBySignificance, backgroundObJ, thresholds, targetSNR, $
                ERROR = error

    error = 0
    data = self.Reader->data (/POINTER)
    times = (*data).times
    
    lookup = *self.timeLookup

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
	signal = 0.0
	noise  = 0.0
	delta = times[1, bottom] - times[0, bottom]
	binSNR = self->PHA::calcSNR(lookup[bottom], history, historyErr, $
			 backHistory, backErrs, signal, noise)

	countsThresh = ROUND(backHistory[bottom]) ; * delta)
	countsAccum = ROUND(targetSNR * noise) ; * delta)

	idx = INTARR(2)
	ER = NEAREST_EDGE (self->combinedTimes (), TRANSPOSE(span[0, *]), idx)
	temp = [ER[0], ER[1]]
	j0 = lookup[idx[0]]
	j1 = lookup[idx[1] + 1]

	self.Reader->accumulator, temp, MEAN(backHistory[bottom: top]), backErrs[0], targetSNR, [j0, j1 - 1]

    self->setTimeLookup, lookup, /INITIALIZE
    self->integrateEnergy
    self->combine

END


;------------------------------------------------------------------------------
; Rebin
;------------------------------------------------------------------------------
PRO TTELightcurve::rebin, $
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
                     'NOTE: Resolutions below a tenth of a ', $
                     'second may take a huge amount of time!'], $
           DIALOG_PARENT = dialog_parent, TITLE = 'Rebin')

       IF (timeRes EQ '') THEN $
          RETURN

       self->setTimeLookup, myLookup, /INITIALIZE
       self->integrateEnergy
       self->combine

       self.Reader->read, ACCUM = DOUBLE(timeRes), ERROR = error
		self->setTimeLookup, myLookup, /INITIALIZE
		self->update

       return
       
    ENDIF ; temporal
    
    
    IF (KEYWORD_SET (nbins)) THEN BEGIN
    
		span   = TRANSPOSE ([[interval], [interval]])
		lookup = self->timeLookup ()
		;data = self.Reader->data (/POINTER)
		combTimes = self->combinedTimes ()
		ER = NEAREST_EDGE (combTimes, span[0, *], idx)
		j0 = lookup[idx[0]]
		j1 = lookup[idx[0] + 1]

		LOOP_TOP = (idx[1] - idx[0] + 1) / nbins
		theTimes = [ER[0]]
		
		FOR LOOP = 1L, (LOOP_TOP - 1) DO BEGIN
			theTimes = [theTimes, combTimes[0, lookup[idx[0] + LOOP + nbins]]]
	         lookup = [lookup[0: idx[0] + LOOP], $
	                  lookup[idx[0] + LOOP + nbins: *]]
		ENDFOR
		theTimes = [theTimes, ER[1]]
		self->setTimeLookup, lookup
		self.Reader->rebinner, theTimes, [lookup[j0], lookup[j1 +loop_top - 1]]
		self->setTimeLookup, lookup, /INITIALIZE
		self->integrateEnergy
		self->combine
		self->integrateEnergy
	       
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
			j0 = lookup[idx[0]]
			j1 = lookup[idx[0] + 1]
			theTimes = [ER[0], ER[1]]
			self.Reader->rebinner, theTimes, [j0, j1 - 1]

       ENDFOR
       
	   self->setTimeLookup, lookup, /INITIALIZE
       ;self->setTimeLookup, lookup
      
    ENDIF ; selections
    

    IF (KEYWORD_SET (fullResolution)) THEN BEGIN

		self.Reader->read, /NOTIMEFILE
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
	   ENDIF  ELSE BEGIN
	   
		   targetRes = DIALOG_INPUT (PROMPT = 'Enter the desired time resolution in ms: ', $
			   TITLE = 'Refine Bin')
		   IF (targetRes EQ '') THEN RETURN
		   targetRes = targetRes / 1000.d
       
			myNumTimes = FLOOR ((ER[1] - ER[0]) / targetRes)
			theTimes = DBLARR(myNumTimes + 1)
			FOR ti = 0, myNumTimes DO theTimes[ti] = ER[0] + ti * targetRes
			theTimes = [theTimes, ER[1]]
		   self.Reader->rebinner, theTimes, [j0, j1 - 1]
		   self->setTimeLookup, lookup, /INITIALIZE
	  ENDELSE
       
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
	  ENDIF ELSE BEGIN
		   temp = [ER[0], (ER[0] + ER[1]) / 2.d, ER[1]]
		   self.Reader->rebinner, temp, [j0, j1 - 1]
		   self->setTimeLookup, lookup, /INITIALIZE
	  ENDELSE
       
    ENDIF ; refine by half
     

    IF (KEYWORD_SET (interval)) THEN BEGIN

       lookup = self->timeLookup ()

       idx = INTARR(2)
	   ER = NEAREST_EDGE (self->combinedTimes (), interval, idx)
	   temp = [ER[0], ER[1]]
       j0 = lookup[idx[0]]
       j1 = lookup[idx[0] + 1]
	   lookup = [lookup[0:idx[0]], lookup[idx[1] + 1: *]]
	   self->setTimeLookup, lookup
	   self.Reader->rebinner, temp, [lookup[j0], lookup[j0 + 1] - 1]
	   self->setTimeLookup, lookup, /INITIALIZE

    ENDIF ; interval
       
	;self->setTimeLookup, lookup
    self->integrateEnergy
    self->combine
    self->integrateEnergy

END


; ----------------------------------------------------------------------------
; Object definition
; ----------------------------------------------------------------------------
PRO TTELightcurve__define

    obj = { TTELightcurve, INHERITS LIGHTCURVE $

;        combinedTimes     : PTR_NEW (), $ 
;        combinedHist      : PTR_NEW (), $ 
;        combinedHistErr   : PTR_NEW (), $ 
;         
;        integratedHist    : PTR_NEW (), $ 
;        integratedHistErr : PTR_NEW (), $ 
;         
;        ;== XY Axes Log status 
;         
;        logStatus         : [0, 0]      $ 

    }

END
