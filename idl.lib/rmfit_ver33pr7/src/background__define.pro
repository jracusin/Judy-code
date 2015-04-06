; ----------------------------------------------------------------------------
;+
; NAME:
;     Background (OBJECT)
;
; PURPOSE:
;     Fit a background model to a lightcurve.
;
; CALLING SEQUENCE:
;     obj = OBJ_NEW ('Background', Reader)
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
;     draw_histogram.pro
;
; PUBLIC METHODS:
;     
;     read (PROCEDURE) - Read a BFITS file
;         Inputs: NONE
;        Outputs: NONE 
;       Keywords: ERROR : Set to a named variable to return 0 if the read
;                 was successful, or 1 if the read failed.
;
;     times (FUNCTION) - Return TIMES data
;         Inputs: NONE
;        Outputs: FLTARR of the start and stop time bins of the observation
;       Keywords: POINTER : Set this keyword to return a pointer to data
;                 instead of a copy.  Clients should not alter pointer data.
;
; MODIFICATION HISTORY:
;
;     Written, 1999 August, Robert.Mallozzi@msfc.nasa.gov
;     Based on FORTRAN routines by M. Briggs
;
;-
; ----------------------------------------------------------------------------



; ----------------------------------------------------------------------------
; Constructor
; ----------------------------------------------------------------------------
FUNCTION Background::init, Reader
        
    IF (NOT self->PHA::init (Reader)) THEN RETURN, 0
            
    RETURN, 1
    
END


; ----------------------------------------------------------------------------
; Destructor
; ----------------------------------------------------------------------------
PRO Background::cleanup

    STRUCT_FREE, self.fit
    STRUCT_FREE, self.integrated
    
    self->PHA::cleanup

END


; ----------------------------------------------------------------------------
;
; Calculate a background model given a background selection that is described 
; by a backSpan.  A span is an array with a column for every region of time 
; bins in the selection, and the first column is the convex hull.  For 
; example, if three background intervals are defined, backSpan = FLTARR (4, 2).
;
; KEYWORD INPUT:
;
;   order    : INT           - background polynomial order
;   channels : INTARR (2)    - energy channels over which to compute the model
;   backSpan : FLTARR (n, 2) - a span array containing the background 
;                              interval selections  
; KEYWORD OUTPUT:
;
;   totalLiveTime : scalar livetime of total background history used in fit
;
; ----------------------------------------------------------------------------
FUNCTION background::fit, $
    ORDER = order, CHANNELS = channels, BACKSPAN = backSpan, $
    TOTALLIVETIME = totalLiveTime, $
    SELECTORDER = selectOrder

           
    data = self.Reader->data (/POINTER)

    IF (N_ELEMENTS (order) NE 0) THEN BEGIN
       self.fit.order = FIX (order)
       self.fit.haveOrder = 1
    ENDIF

    IF (N_ELEMENTS (channels) NE 0) THEN BEGIN
       self.fit.channels = FIX (channels)
       self.fit.haveChannels = 1
    ENDIF
    
    IF (NOT self.fit.haveChannels) THEN BEGIN
       MESSAGE, /CONTINUE, 'Missing keyword: CHANNELS.'
       RETURN, 0
    ENDIF

    IF (NOT self.fit.haveBackSpan) THEN BEGIN
       MESSAGE, /CONTINUE, 'Missing keyword: BACKSPAN.'
       RETURN, 0
    ENDIF
    
    IF (KEYWORD_SET (selectOrder)) THEN BEGIN
       self.fit.haveOrder = 0
    ENDIF

    ;== Erase the current model
    modelSave = self.fit.haveModel
    self.fit.haveModel = 0
    
    ;== Extract time bins based on the background intervals
    
    COUNT_SPAN, (*data).times, *self.fit.backSpan, backIdx
    numBackSpectra = N_ELEMENTS (backIdx)

    IF (numBackSpectra LE 1) THEN BEGIN          

       ;== Constant background: last resort
       
       degree = 1

    ENDIF ELSE BEGIN                                

       ;== Set the degree of fit of background 
       
       maxDegree = 4
       degree = ((numBackSpectra - 2) < maxDegree) + 1

    ENDELSE

    IF (NOT self.fit.haveOrder) THEN BEGIN
    
       order = DIALOG_MENU (['CANCEL', STRTRIM (SINDGEN (degree), 2)], $
           TITLE = ['Select Order of',  'Background Polynomial'])

       IF (order EQ 'CANCEL') THEN BEGIN
           self.fit.haveModel = modelSave
           RETURN, 0
       ENDIF    

       self.fit.order = FIX (order)
       self.fit.haveOrder = 1 

    ENDIF

    t_beg = TRANSPOSE (((*data).times)[0, backIdx])
    t_end = TRANSPOSE (((*data).times)[1, backIdx])

    ;== Pass livetimes to BACK_LFIT
    
    t_live = FLTARR (numBackSpectra, /NOZERO)        

    ;== BACK_LFIT expects (time x energy) array
    
    b_y = TRANSPOSE (((*data).rates)[*, backIdx])    

    ;== Calculate the background livetimes
    
    temp_spec = ((*data).rates)[*, backIdx]
    temp_errs = ((*data).errors)[*, backIdx]
    
    num_chan = (SIZE (temp_spec))[1]
    nind = WHERE (temp_spec EQ 0., cnt)
   
    IF (cnt GT 0) THEN BEGIN 
        chan_arr = LONARR (num_chan, numBackSpectra) + 1
        chan_arr[nind] = 0
        temp_errs[nind] = 1.       ;== Just to make sure!
        noZero = FIX (TOTAL (chan_arr, 1))
        zind = WHERE (noZero EQ 0, zCnt)
        
        ;== Real problem:
        IF (zCnt GT 0) THEN BEGIN   
            ;== No counts: livetime = accumulation time
            noZero[zind] = num_chan
            FOR hh = 0, zCnt - 1 DO BEGIN
            	temp_spec[*, zind[hh]] = t_end[zind[hh]] - t_beg[zind[hh]]
            ENDFOR
        ENDIF 
        ;== Livetime: Average over energy (using channels with counts)
        t_live = TOTAL (temp_spec / (temp_errs * temp_errs), 1) / noZero
    ENDIF ELSE BEGIN
        ;== Livetime: Average over energy, using all channels
        t_live = TOTAL (temp_spec / (temp_errs * temp_errs), 1) / num_chan
        
    ENDELSE
    
    ;== Total livetime
    
    totalLiveTime = TOTAL (t_live)

    ;== MSB's polynomial fitting routine, which takes into account the finite
    ;== width of each time bin by integrating the basis functions across the 
    ;== bin.  Stores the model in BACK_FIT, each energy bin as a function of 
    ;== time.
    
    self.fit.haveModel = self->linearFit (t_beg, t_end, b_y, t_live)

    RETURN, self.fit.haveModel

END


; ----------------------------------------------------------------------------
;
; Adapted from Numerical Recipes subroutine LFIT in order do linear fitting
; to the background data.   The background data are the mean rates for the 
; various data intervals.  The "differential" fitting function is a polynomial
; of order REQ_ORDER.   The actual fitting function is the mean of the 
; differential fitting function over the data intervals.
; The subroutine LFIT has been considerably simplified by removing the option
; of fixing some of the parameters--in this version you can only choose the
; order of the polynomial.  It has also been hardcoded to this particular
; application.  Model variances are used for chisq via two fitting passes.
;
; INPUT:
; 
;     IMPORTANT! FOR NUMERICAL STABILITY THE TIMES SHOULD BE RELATIVE TO THE 
;     TRIGGER TIME.
;
; REQ_ORDER -- the requested order of the polynomial,
;           -- integer scaler.
; BEG_T -- beginging times of the data intervals,
;       -- floating vector (time interval).
; END_T -- ending times of the data intervals,
;       -- floating vector (time interval).
; RATES -- the data count rates, UNITS MUST BE COUNTS / SEC, not counts/s-keV !!
;          These units are assumed in the calculation of variances using
;          Poission statistics.
;       -- floating array (time interval, energy channel).
; LIVE_TIME -- the live time of each accumulation interval,
;           -- floating vector (time interval).
; FIRST_BACK_CHAN -- first channel for which user wants background calculated,
;                    this is an array index starting at 0,
;                 -- integer scaler.
; LAST_BACK_CHAN -- first channel for which user wants background calculated,
;                   this is an array index w. max allowed value = # of chan - 1,
;                -- integer scaler.
;                                                          
; OUTPUT:
;
; RET_BACK_CHISQ -- reduced chisq of the fit,
;                -- floating vector (energy channel).
; BACK_FIT -- the model predicted rates for each of the data intervals--in a
;             perfect world these would equal the data rates.
;          -- floating array (time interval, energy channel)
;
; BACK_ORDER -- polynomial order of the differential model,
;            -- integer scalar.
; numFitChan -- number of energy channels for which fit was done,
;              -- integer scalar.
; COEFF -- the polynomial coefficient of the differential model,
;       -- double vector (energy channel).
; COVAR -- the covariance matrix of the fits to each energy channel,
;       -- double array (coefficient, coefficient, energy channel)
; BACK_CHISQ -- reduced chisq of the fit,
;            -- floating vector (energy channel).
; BEG_BCHAN -- first channel (i.e., index) for which background model is wanted,
;           -- integer scalar.
; END_BCHAN -- last channel (i.e., index) for which background model is wanted,
;           -- integer scalar.
;
; ----------------------------------------------------------------------------
FUNCTION background::linearFit, beg_t, end_t, rates, live_time

    data = self.Reader->data (/POINTER)

    numEchan = (SIZE ((*data).rates))[1]		
    begBchan = self.fit.channels[0]
    endBchan = self.fit.channels[1]

    numTimes = N_ELEMENTS (beg_t)

    ;== Test legality of order
    
    IF (self.fit.order LT 0) OR (self.fit.order GT numTimes - 1) OR $
       (self.fit.order GT 4) THEN BEGIN
       MESSAGE, /CONTINUE, 'Illegal requested order: ' + $
           STRTRIM (self.fit.order, 2)
       RETURN, 0
    ENDIF

    IF (MIN (live_time) LE 0.0) THEN $
       MESSAGE, /CONTINUE, 'Non-positive livetime.'

    ;== Requested number of channels to fit
    
    numFitChan = endBchan - begBchan + 1   
    self.fit.chanOffset = (chanOffset = begBchan)

    ;== Test legality of channel range
    
    IF ((begBchan LT 0) OR (begBchan GT endBchan)) THEN BEGIN
       MESSAGE, /CONTINUE, 'Illegal requested channel range: ' + $
           STRTRIM (begBchan, 2) + ':' + STRTRIM (endBchan, 2) 
       RETURN, 0
    ENDIF

    ;== Setup arrays
    
    covar      = DBLARR (self.fit.order + 1, self.fit.order + 1, numEChan)
    coeff      = DBLARR (self.fit.order + 1, numEChan)
    wt         = DBLARR (self.fit.order + 1, numTimes)
    back_chisq = FLTARR (numEChan)

    ;== Get the values of the basis functions for each time interval
    
    AFUNC = self->evalBasis (beg_t, end_t)

    FOR lpass = 0, 1 DO BEGIN

        FOR kchan = begBchan, endBchan DO BEGIN

            IF (MAX (rates[*, kchan]) GT 0.0) THEN BEGIN

               ;== Have data
               
               ;== Set up weight matrix used to create covariance matrix:
               ;== How to calculate variance: 
	       ;==    1) rate * livetime = counts
	       ;==    2) Poission statistics ==> variance of counts = counts
	       ;==    3) variance of rate = variance of counts / livetime^2
	       
               IF (lpass EQ 0) THEN BEGIN

	          ;== Pass 1 ==> data variances calculated from data rates 
                  ;== livetime
                  
	          variance = rates[*, kchan] / LIVE_TIME 
                  idx = WHERE (variance GT 0.0)
                  FOR jcoeff = 0, self.fit.order DO $
                      wt[jcoeff, idx] = AFUNC (idx, jcoeff) / variance[idx]

	       ENDIF ELSE BEGIN

	          ;== Pass 2 ==> data variances calc. from model rates 
                  ;== (1st pass) and livetime
	          
                  variance = back_fit[*, kchan] / LIVE_TIME
                  idx = WHERE (VARIANCE GT 0.0)
                  IF (idx[0] NE -1) THEN BEGIN
                     FOR JCOEFF = 0, self.fit.order DO $
                       wt[jcoeff, idx] = AFUNC (idx, jcoeff) / variance[idx]
                  ENDIF ELSE BEGIN
                     MESSAGE, /CONTINUE, $
                         'SEVERE ERROR: Background model negative'
                     MESSAGE, /CONTINUE, $
                         'for all time intervals of energy index ' $
                         + STRTRIM (kchan, 2)
                     ;== Ignore weights, set to 1.0  
                     wt = DBLARR (self.fit.order + 1, numTimes) + 1.0
                  ENDELSE
               
               ENDELSE

               ;== Set up covariance matrix
	       
               covar[*, *, kchan] = wt # afunc          

               ;== Set up coefficient matrix
               
	       coeff[*, kchan] = wt # rates[*, kchan]

               ;== Get the matrix solution: need both inverse of COVAR and
               ;== inverse times COEFF
               
               IF (self.fit.order GE 1) THEN BEGIN
                  covar[*, *, kchan] = INVERT (covar[*, *, kchan])
               ENDIF ELSE BEGIN
                  covar[0, 0, kchan] = 1.0 / covar[0, 0, kchan]
               ENDELSE
               
               coeff[*, kchan] = covar[*, *, kchan] # coeff[*, kchan]

           ENDIF

        ENDFOR

        PTR_FREE, self.fit.coeff
        PTR_FREE, self.fit.covar

        self.fit.coeff  = PTR_NEW (coeff)
        self.fit.covar  = PTR_NEW (covar)

        ;== Get the model rates
        
        back_fit = self->evalModel (beg_t, end_t)

    ENDFOR ; lpass

    ;== Reduced chisq
    
    FOR kchan = begBchan, endBchan DO BEGIN
        
        IF (MAX (rates[*, kchan]) GT 0.0) THEN BEGIN
        
           variance = back_fit[*, kchan] / live_time
           dummy = (rates[*, kchan] - back_fit[*, kchan])^2
           back_chisq[kchan] = TOTAL (dummy / variance)
        
        ENDIF
    
    ENDFOR

    PTR_FREE, self.fit.chisq
    self.fit.dof   = numTimes - (self.fit.order + 1)
    self.fit.chisq = PTR_NEW (back_chisq)

    RETURN, 1

END


; ----------------------------------------------------------------------------
;
; Evaluates the basis functions of the background model.   The basis functions
; are the various polymonials (1, t, t^2 ...) averaged over the data time
; intervals.
;
; Revised 19 November 1993 by MSB: converted calculation to double precision--
; returned argument is still single precision.
;
; INPUTS
;
;    BEG_T: begining times of the intervals (which might be the data intervals),
;           floating vector (time interval).
;    END_T: ending times of the intervals (which might be the data itervals),
;           floating vector (time interval).
;
; OUTPUTS
;
;    AFUNC: the various basis func. values for each of the data time intervals,
;           floating array (time interval, coefficient).
;
; ----------------------------------------------------------------------------
FUNCTION background::evalBasis, beg_t, end_t
 
    afunc = FLTARR (N_ELEMENTS (beg_t), self.fit.order + 1, /NOZERO)

    dbeg_t = DOUBLE (beg_t)
    dend_t = DOUBLE (end_t)
    deltaT = dend_t - dbeg_t

    FOR i = 0, self.fit.order DO $
        afunc[*, i] = (dend_t^(i + 1) - dbeg_t^(i + 1)) / ((i + 1) * (deltaT))

    RETURN, afunc

END


; ----------------------------------------------------------------------------
;
; Evaluates the differential function for the requested times.
; The "differential" function is a polynomial.
; The fitting function is the mean over each interval of the polynomial.
;
; INPUT:
;
; TIMES --times to evaluate polynomial for.Intended to be relative to trig time,
;       -- floating vector ("continous" time index).
; beg_bchan -- low channel of requested fit.
; end_bchan -- high channel of requested fit.
; coeff -- fit coefficients
;
; OUTPUT:
;
; BACK_DIFF -- the polynomial values,
;           -- floating array ("continuous" time index, energy channel).
;
; ----------------------------------------------------------------------------
FUNCTION background::evalDiff, nPts, startTime, endTime, _EXTRA = extra

    data = self.Reader->data (/POINTER)

    numEchan = (SIZE ((*data).rates))[1]		
    begBchan = (self.fit.channels)[0]      
    endBchan = (self.fit.channels)[1]      

    numFitChan = endBchan - begBchan + 1

    dcoeff = DOUBLE (*self.fit.coeff)

    IF (N_PARAMS() LT 3) THEN BEGIN
   
       backTimes = self->evalTimes (_EXTRA = extra)
       
       back_diff = FLTARR (N_ELEMENTS (backTimes), numEChan)
       FOR i = begBchan, endBchan DO $
           back_diff[*, i] = POLY (backTimes, dcoeff[*, i])

    ENDIF ELSE BEGIN

       requestedTimes = CREATE_ARRAY (nPts, FLOAT (startTime), endTime)   
       back_diff = FLTARR (N_ELEMENTS (requestedTimes), numFitChan)
       FOR i = begBchan, endBchan DO $
           back_diff[*, i] = POLY (requestedTimes, dcoeff[*, i])

    ENDELSE

    RETURN, back_diff

END


; ----------------------------------------------------------------------------
;
; Evaluates the fitting funtion for each of the NUM_TIMES time intervals.
; The fitting function is the mean over each interval of a polynomial.
;
; INPUT:
;
; BEG_T -- begining times of the intervals (which might be the data intervals),
;       -- floating vector (time interval).
; END_T -- ending times of the intervals (which might be the data intervals),
;       -- floating vector (time interval).
; beg_bchan -- low channel of requested fit.
; end_bchan -- high channel of requested fit.
;
; OUTPUT: via function return
;
; BACK_FIT -- the fitting function = mean over each interval of polynomial,
;          -- floating array (time interval, energy channel).
;
; ----------------------------------------------------------------------------
FUNCTION background::evalModel, beg_t, end_t, WARN = warn

    data = self.Reader->data (/POINTER)

    numEchan = (SIZE ((*data).rates))[1]		
    begBchan = self.fit.channels[0]      
    endBchan = self.fit.channels[1]      

    numFitChan = endBchan - begBchan + 1

    back_fit = FLTARR (N_ELEMENTS (beg_t), numEChan)

    dbeg_t = DOUBLE (beg_t)
    dend_t = DOUBLE (end_t)              
    deltaT = dend_t - dbeg_t

    FOR i = begBchan, endBchan DO BEGIN
    
        FOR j = 0, self.fit.order DO BEGIN
    
            back_fit[*, i] = back_fit[*, i] + $
	    (*self.fit.coeff)[j, i] * (dend_t^(j + 1) - dbeg_t^(j + 1)) / $
                ((j + 1) * (deltaT))
    
        ENDFOR
    
    ENDFOR
 
    IF (KEYWORD_SET (warn) AND MIN (back_fit) LT 0.0) THEN $
        MESSAGE, /INFO, 'Background model contains negative value(s).'

    RETURN, back_fit

END


; ----------------------------------------------------------------------------
;
; Compute  the uncertainty in the model predicted rates for the data time
; intervals, based upon the uncertainty in the model coefficients.   See my
; (MSB) memo of 21 March 1993, "Ideas on Background Models, ...".   It is
; particularly easy for this model since it is linear in its parameters.
;
; INPUT:
;
; BEG_T -- begining times of the intervals (which might be the data intervals),
;       -- floating vector (time interval).
; END_T -- ending times of the intervals (which might be the data intervals),
;       -- floating vector (time interval).
;
; OUTPUT:
;
; MODEL_SIG -- the uncertainty in the model predicted rates, based upon the
;              uncertainties of the model parameters/coefficients in COVAR,
;           -- floating array (time interval, energy channel).
;
; ----------------------------------------------------------------------------
FUNCTION background::evalMsig, beg_t, end_t

    data = self.Reader->data (/POINTER)

    ;== Get the values of the basis functions for each time interval
    
    AFUNC = self->evalBasis (beg_t, end_t)

    numEchan = (SIZE ((*data).rates))[1]		
    begBchan = self.fit.channels[0]      
    endBchan = self.fit.channels[1]      

    numFitChan = endBchan - begBchan + 1
    numTimes   = N_ELEMENTS (beg_t)

    ;== Formal propagation of errors of fit coefficients to errors 
    ;== of model rates
    
    model_sigma = FLTARR (numTimes, numEChan)

    AFUNCT = TRANSPOSE (AFUNC)
    FOR i = begBchan, endBchan DO BEGIN
        FOR j = 0L, numTimes - 1 DO BEGIN
            MODEL_SIGMA[j, i] = $
                AFUNC[j, *] # ((*self.fit.covar)[*, *, i] # AFUNCT[*, j])
        ENDFOR
    ENDFOR
    MODEL_SIGMA = SQRT (MODEL_SIGMA)

    ;== If the fit is systematically bad, as indicated by reduced 
    ;== chisq > 1, increase the model rate errors to reflect the inadequate fit
    
    FACTOR = SQRT ( (*self.fit.chisq / self.fit.dof) > 1)
    FOR i = begBchan, endBchan DO $
        MODEL_SIGMA[*, i] = MODEL_SIGMA[*, i] * FACTOR[i]

    RETURN, MODEL_SIGMA

END


; ----------------------------------------------------------------------------
; Create the backTimes array to be 4 times the resolution of the
; times array (but no greater than ~1000 elements total)
; ----------------------------------------------------------------------------
FUNCTION background::evalTimes, FULLTIME = fulltime

    data = self.Reader->data (/POINTER)

    backTimes = TRANSPOSE ((((*data).times)[1, *] + ((*data).times)[0, *]) /2.0)
    n_back = N_ELEMENTS (backTimes)
    
    IF KEYWORD_SET (FULLTIME) THEN RETURN, backTimes

    ;== Don't make too many background bins

    IF (n_back LT 250) THEN BEGIN         
   
       ;== Sometimes we get duplicate times - eliminate these
       
       ant_ind = LONARR (n_back)
       ant_ind[INDGEN (n_back)] = 1
       bcnt = 0
       
       ;== Stupid edge condition(!):
       IF (n_back NE 1) THEN BEGIN
		   dup_ind = WHERE (FIX (((*data).times)[0, 1: *]) EQ $
		   FIX (((*data).times)[1, 0: n_back - 1]))
		   ant_ind[dup_ind] = 0
		   b_ind = WHERE (ant_ind, bcnt)

		   IF (bcnt NE 0) THEN BEGIN
			  non_dup = TRANSPOSE (((*data).times)[1, WHERE (ant_ind)])
			  backTimes = [backTimes, TRANSPOSE (((*data).times)[0, *]), non_dup]
		   ENDIF ELSE BEGIN
			  backTimes = [backTimes, TRANSPOSE (((*data).times)[0, *]), $
					  ((*data).times)[1, n_back]]
		   ENDELSE
       ENDIF ELSE BEGIN
		  non_dup = TRANSPOSE (((*data).times)[1, WHERE (ant_ind)])
		  backTimes = [backTimes, TRANSPOSE (((*data).times)[0, *]), non_dup]
       ENDELSE

       backTimes = backTimes[SORT (backTimes)]
       n_back = N_ELEMENTS (backTimes)

       ;== Make the array bigger; find midpoint between each array element
       
       backTimes = [backTimes, (backTimes[1: *] + $
	   backTimes[0: N_ELEMENTS(backTimes) - 1]) / 2.0]
       backTimes = backTimes[SORT (backTimes)]
       n_back = N_ELEMENTS (backTimes)

    ENDIF ELSE BEGIN                               

       ;== Background array too big, reduce it

       WHILE (n_back GT 2000) DO BEGIN
           even_ind = LINDGEN (n_back / 2) * 2
           even_ind = [even_ind, n_back - 1]
           backTimes = backTimes[even_ind]
           n_back = N_ELEMENTS (backTimes)
       ENDWHILE

    ENDELSE
    
    RETURN, backTimes

END


;------------------------------------------------------------------------------
; Integrate the background history over the selected time or energy bins.
; Set only one keyword (ENERGY or TIME), and pass the corresponding parameters.
;
; This method can be used in two ways:
;
;    1. By default, calling the integrate method will integrate the
;       Background over the selected time or energy range, and set the 
;       internal object data members containing the integrated results.
;
;    2. To use this method as a utility method to integrate on
;       the fly, set the keywords HISTORY or SPECTRUM, ERRORSPECTURM to 
;       named variables to return the integrated lightcurve or spectrum
;       (and errors).  If any of these keywords are set, the internal 
;       object members are not altered.
;
; KEYWORDS:
;
;    ENERGY        : integrate the background model over energy
;    TIME          : integrate the background model over time
;    PERENERGY     : return counts / time-energy (default is counts / time)    
; 
;    HISTORY       : see note above
;    SPECTRUM      : see note above
;    ERRORSPECTRUM : see note above
;
;------------------------------------------------------------------------------
PRO Background::integrate, combinedThresholds, span, lookup, $    
    TIME = time, ENERGY = energy, PERENERGY = perEnergy, $
    HISTORY = history, SPECTRUM = spectrum, ERRORSPECTRUM = errorSpectrum, $
    _EXTRA = extra
            
    IF (NOT self.fit.haveModel) THEN BEGIN
       MESSAGE, /CONTINUE, 'No background model.'
       RETURN
    ENDIF

    data = self.Reader->data (/POINTER)
    
    ;== Sum over energy
          
    IF (KEYWORD_SET (energy)) THEN BEGIN
    
       backSpectra = self->evalDiff (_EXTRA = extra)
       integratedHist = FLTARR ((SIZE (backSpectra))[1])           
       totalEnergy    = 0.0                

       ;== Standard procedure for picking bins spanned by the selection
       
       n_regions = SIZE (span) 
       IF ((n_regions[1] - 1) GT 0) THEN BEGIN

          lower = 1
          upper = n_regions[1] - 1

       ENDIF ELSE BEGIN

          ;== Only one region, use first column (convex hull)
          
          lower = (upper = 0)

       ENDELSE

       ;== Loop over regions in the selection
       numBins = (SIZE (lookup))[1] - 1
       FOR hh = lower, upper DO BEGIN

           tr = NEAREST_EDGE (combinedThresholds, (span)[hh, *], er)
           er[0] = lookup[er[0]]
           er[1] = lookup[er[1] + 1 < numBins] - 1

           ;== Integral over energy in each region spanned by selection
           
           FOR i = er[0], er[1] DO BEGIN
     	       deltaEnergy = ((*data).eedges)[1, i] - ((*data).eedges)[0, i]
    	       totalEnergy = totalEnergy + deltaEnergy
    	       integratedHist = integratedHist + backSpectra[*, i]
           ENDFOR

       ENDFOR

       ;== Divide by bin width to get counts / time-energy
       
       IF (KEYWORD_SET (perEnergy)) THEN $
          integratedHist = integratedHist / totalEnergy
       
       IF (ARG_PRESENT (history)) THEN BEGIN
       
          history = integratedHist
          
       ENDIF ELSE BEGIN
          
          PTR_FREE, self.integrated.hist
          self.integrated.hist = PTR_NEW (integratedHist)

       ENDELSE
       
    ENDIF ; integrate over energy
    
    
    ;== Sum over time
            
    IF (KEYWORD_SET (time)) THEN BEGIN
    
       COUNT_SPAN, combinedThresholds, span, timeInd
       t_beg = REFORM (combinedThresholds[0, timeInd])     
       t_end = REFORM (combinedThresholds[1, timeInd])    
       backModel = self->evalModel (t_beg, t_end)

       ;== Add up bins by time
       
       deltaTime = t_end - t_beg
       totalTime = TOTAL (deltaTime)
       backSpec = (TRANSPOSE (backModel) # deltaTime) / totalTime

       ;== Background error rates
       
       backErrs = self->evalMsig (t_beg, t_end)
       backSpecErr = SQRT (TRANSPOSE ( $
           backErrs * backErrs) # (deltaTime * deltaTime)) / totalTime
    
       ;== Need to combine the spectrum over the energy lookup table
       self->PHA::combine, backSpec, backSpecErr, $
	        (*data).eedges, *self.energyLookup, $
	        combBackSpec, combBackSpecErr, combBackThresh
	        
       deltaEnergy = combBackThresh[1, *] - combBackThresh[0, *]
       backSpec = combBackSpec * deltaEnergy
       backSpecErr = combBackSpecErr * deltaEnergy
	  
	        
       IF (ARG_PRESENT (spectrum) OR ARG_PRESENT (errorSpectrum)) THEN BEGIN

          spectrum      = backSpec
          errorSpectrum = backSpecErr

       ENDIF ELSE BEGIN
       
          PTR_FREE, self.integrated.spec
          PTR_FREE, self.integrated.specErr

          self.integrated.spec    = PTR_NEW (backSpec)   
          self.integrated.specErr = PTR_NEW (backSpecErr)

       ENDELSE

    ENDIF ; integrate over time


END


; ----------------------------------------------------------------------------
; Return fit parameters
; ----------------------------------------------------------------------------
FUNCTION Background::haveModel
    
    RETURN, self.fit.haveModel

END

FUNCTION Background::coefficients

    IF (NOT self.fit.haveModel) THEN BEGIN
       MESSAGE, /CONTINUE, 'No background model.'
       RETURN, -1
    ENDIF
    
    RETURN, *self.fit.coeff

END

FUNCTION Background::covariance

    IF (NOT self.fit.haveModel) THEN BEGIN
       MESSAGE, /CONTINUE, 'No background model.'    
       RETURN, -1
    ENDIF
       
    RETURN, *self.fit.covar

END

FUNCTION Background::chisq

    IF (NOT self.fit.haveModel) THEN BEGIN
       MESSAGE, /CONTINUE, 'No background model.'    
       RETURN, -1
    ENDIF

    RETURN, *self.fit.chisq

END

FUNCTION Background::dof

    IF (NOT self.fit.haveModel) THEN BEGIN
       MESSAGE, /CONTINUE, 'No background model.'    
       RETURN, -1
    ENDIF

    RETURN, self.fit.dof

END

FUNCTION Background::order

    IF (NOT self.fit.haveModel) THEN BEGIN
       MESSAGE, /CONTINUE, 'No background model.'    
       RETURN, -1
    ENDIF

    RETURN, self.fit.order

END

FUNCTION Background::channels

    IF (NOT self.fit.haveModel) THEN BEGIN
       MESSAGE, /CONTINUE, 'No background model.'    
       RETURN, -1
    ENDIF

    RETURN, self.fit.channels

END

FUNCTION Background::chanOffset

    IF (NOT self.fit.haveModel) THEN BEGIN
       MESSAGE, /CONTINUE, 'No background model.'    
       RETURN, -1
    ENDIF

    RETURN, self.fit.chanOffset

END

FUNCTION Background::backSpan
    
    IF (NOT PTR_VALID (self.fit.backSpan)) THEN $
       RETURN, -1
       
    RETURN, *self.fit.backSpan

END

PRO Background::setSpan, backSpan

    PTR_FREE, self.fit.backSpan 
    self.fit.backSpan = PTR_NEW (backSpan)
    self.fit.haveBackSpan = 1

END

FUNCTION Background::integrated
    
    IF (NOT self.fit.haveModel) THEN BEGIN
       MESSAGE, /CONTINUE, 'No background model.'    
       RETURN, -1
    ENDIF
       
    RETURN, self.integrated

END

PRO Background::setIntegrated, newIntegrated

    STRUCT_FREE, self.integrated

	self.integrated.hist = PTR_NEW (*newIntegrated.hist)
	self.integrated.spec    = PTR_NEW (*newIntegrated.spec)   
	self.integrated.specErr = PTR_NEW (*newIntegrated.specErr)

END

; ----------------------------------------------------------------------------
; Display chi-sq values
; ----------------------------------------------------------------------------
PRO Background::displayChisq, $
    thresholds, combinedThresholds, energySpan, energyLookup
 
    backChiSq      = self->chisq () / self->dof ()
    backChanOffset = self->chanOffset ()

    chiSqRange = [1.0e+20, -1.0e+20]
    nSpan = SIZE (energySpan)
    IF (nSpan[1] EQ 1) THEN BEGIN

       chiSqRange = [MIN (backChiSq), MAX (backChiSq)]

    ENDIF ELSE BEGIN          

       FOR i = 1, nSpan[1] - 1 DO BEGIN 

	   tr = NEAREST_EDGE (combinedThresholds, energySpan[i, *], er)
           
           er[0] = energyLookup[er[0]]
           er[1] = energyLookup[er[1] + 1] - 1
           er    = er - backChanOffset
           
           chiSqRange = [ $
               MIN ([ chiSqRange[0], backChiSq[ER[0]:ER[1]] ]), $
               MAX ([ chiSqRange[1], backChiSq[ER[0]:ER[1]] ])]

       ENDFOR

    ENDELSE  

    COUNT_SPAN, thresholds, energySpan, idx

    xData = thresholds[*, idx]       
    ;= 01/27/10 RDP: Wasn't showing the lowest channels!
    yData = (backChiSq)[idx]   ; - backChanOffset  

;    command_s = [ $
;        'PLOT, /XLOG, /YNOZERO, /NODATA, xData, yData', $
;        'DRAW_HISTORY, xData, yData' $
;    ]

    w = DIALOG_PLOT (XDATA = xData, YDATA = yData, CALL_PROC = 'DRAW_HISTORY', $
        /PS_OPTION, $
        WINTITLE = 'Background Chi-Squares', $
        XTITLE = 'Energy (keV)', YTITLE = '!7v!U2!N!X / DOF', $
        YRANGE = chiSqRange, /XLOG, /YNOZERO, /NODATA)

END


;------------------------------------------------------------------------------
; Make a standard plot of the background model.
; Call the appropriate integrator (TIME or ENERGY) before plotting!
;
; Plot the total background span with a dotted line, then highlight selected
; background intervals by overplotting them with a solid line.  This fancy
; display method forces a reintegration of the background model whenever the
; display changes.
;------------------------------------------------------------------------------
PRO Background::plot, combinedThresholds, span, lookup, $
    HISTORY = history, SPECTRUM = spectrum, _EXTRA = extra

    IF (NOT self.fit.haveModel) THEN BEGIN
       MESSAGE, /CONTINUE, 'No background model.'    
       RETURN
    ENDIF

    IF (KEYWORD_SET (history)) THEN BEGIN
    
       times = self->evalTimes ()
       hist  = *self.integrated.hist
       self->drawHistory, times, hist, LINESTYLE = 1, _EXTRA = extra
           
    ENDIF    

    IF (KEYWORD_SET (spectrum)) THEN BEGIN

       ; Spectrum is counts / s-energy
       ; Divide by threshold bin widths
       
       width = REFORM (combinedThresholds[1, *] - combinedThresholds[0, *])

		; TODO: Discontiguous channels
       self->drawHistory, $
           combinedThresholds, $
           *self.integrated.spec / width, $
           *self.integrated.specErr / width, $
            _EXTRA = extra

    ENDIF


END


;------------------------------------------------------------------------------
; Erase the current model
;------------------------------------------------------------------------------
PRO Background::erase
    
    self.fit.haveModel = 0
    self.fit.haveBackSpan = 0
    
END


; ----------------------------------------------------------------------------
; Object definition
; ----------------------------------------------------------------------------
PRO Background__define

    obj = { BACKGROUND, INHERITS PHA,  $  
        
        integrated : { BACKGROUND_INTEGRATED, $
        
            hist    : PTR_NEW (), $
            spec    : PTR_NEW (), $
            specErr : PTR_NEW ()  $
        
        }, $    
         
        fit : { BACKGROUND_FIT, $
        
            order        : 0,          $
            channels     : INTARR (2), $
            backSpan     : PTR_NEW (), $
            haveOrder    : 0,          $
            haveChannels : 0,          $
            haveBackSpan : 0,          $
            chanOffset   : 0,          $
            haveModel    : 0,          $
            coeff        : PTR_NEW (), $
            covar        : PTR_NEW (), $
            chisq        : PTR_NEW (), $
            dof          : 0           $
        
        } $    

    }

END
