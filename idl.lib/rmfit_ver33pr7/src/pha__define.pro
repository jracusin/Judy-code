; ----------------------------------------------------------------------------
;+
; NAME:
;
;     PHA (OBJECT)
;
; PURPOSE:
;
;     Base class object that encapsulates detector PHA data.
;
; CALLING SEQUENCE:
;
;     This class is not meant to be instantiated directly.  Clients should
;     override methods in this class to handle the dataset of their 
;     specific detector.  The class is inherited by the Lightcurve, Spectrum, 
;     and Background objects, which handle PHA lightcurve and energy spectrum
;     data, and background modeling.
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
; DEPENDENCIES:
;
;     axisrange.pro
;
; PUBLIC METHODS:
;     
;     combine (PROCEDURE) - Rebin an input array according to a lookup table
;         Inputs: ARR        : Input array
;                 ARR_ERR    : Input error array
;                 THRESHOLDS : Bin thresholds array (2, nBins + 1)
;                 LOOKUP     : INTEGER index array where missing indices
;                              indicate which bins are to be combined into
;                              one bin in the output
;        Outputs: OARR       : Binned array of dimension N_ELEMENTS (LOOKUP) - 1
;                 OARR_ERR   : Errors on OUTARR
;                 OTHRESHOLDS: Binned thresholds
;       Keywords: NONE
;
;     drawHistory (PROCEDURE) - Draw a curve, skipping over data gaps
;         Inputs: X  : Independent data array (2, n)
;                 Y  : Dependent data array (n)
;                 DY : Errors on Y array
;        Outputs: NONE
;       Keywords: TOLERANCE: Tolerance of a data gap 
;                 Any valid OPLOT keywords
;
;     range (FUNCTION) - Return the current data range
;         Inputs: NONE
;        Outputs: RANGE = FLTARR (2, 2)
;                 RANGE[0, *] = range of x-data
;                 RANGE[1, *] = range of y-data
;       Keywords: NONE 
;
;     setRange (PROCEDURE) - Set the current data range
;         Inputs: RANGE = FLTARR (2, 2)
;                 RANGE[0, *] = range of x-data
;                 RANGE[1, *] = range of y-data
;        Outputs: NONE
;       Keywords: NONE 
;
;     default (FUNCTION) - Return the default data range
;         Inputs: NONE
;        Outputs: RANGE = FLTARR (2, 2)
;                 RANGE[0, *] = range of x-data
;                 RANGE[1, *] = range of y-data
;       Keywords: NONE 
;
;     setdefault (PROCEDURE) - Reset the default data range
;         Inputs: NONE
;        Outputs: NONE
;       Keywords: NONE 
;
;     timeLookup (FUNCTION) - Returns the time lookup array
;         Inputs: NONE
;        Outputs: Array of time lookup indices
;       Keywords: NONE
;
;     setTimeLookup (PROCEDURE) - Set the time lookup array
;         Inputs: Array of time lookup indices
;        Outputs: NONE
;       Keywords: INITIALIZE : Set this keyword to initialize the default 
;                 value of the timeLookup array
;
;     energyLookup (FUNCTION) - Returns the energy lookup array
;         Inputs: NONE
;        Outputs: Array of energy lookup indices
;       Keywords: NONE
;
;     setEnergyLookup (PROCEDURE) - Set the energy lookup array
;         Inputs: Array of energy lookup indices
;        Outputs: NONE
;       Keywords: INITIALIZE : Set this keyword to initialize the default 
;                 value of the energyLookup array
;
;     timeSpan (FUNCTION) - Returns the selection time span array
;         Inputs: NONE
;        Outputs: Array of selected time intervals
;       Keywords: NONE
;
;     setTimeSpan (PROCEDURE) - Set the selection time span array
;         Inputs: Array of selected time intervals
;        Outputs: NONE
;       Keywords: NONE
;
;     energySpan (FUNCTION) - Returns the selection energy span array
;         Inputs: NONE
;        Outputs: Array of selected energy intervals
;       Keywords: NONE
;
;     setEnergySpan (PROCEDURE) - Set the selection energy span array
;         Inputs: Array of selected energy intervals
;        Outputs: NONE
;       Keywords: NONE
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
FUNCTION PHA::init, Reader

    ;== Verify input
        
    IF (SIZE (Reader, /TNAME) NE 'OBJREF') THEN BEGIN   
       MESSAGE, /CONTINUE, 'Input must be of class DataReader (no OBJREF).'
       RETURN, 0
    ENDIF

    IF (NOT OBJ_ISA (Reader, 'DataReader')) THEN BEGIN   
       MESSAGE, /CONTINUE, 'Input must be of class DataReader (not the expected object).'
       RETURN, 0
    ENDIF

    ;== Initialize data reader
    
    self.Reader = Reader

    ;== Initialize lookup arrays

    self->setTimeLookup,   /INITIALIZE
    self->setEnergyLookup, /INITIALIZE

    ;== Initialize selection span arrays

    self->setTimeSpan,   /INITIALIZE
    self->setEnergySpan, /INITIALIZE

    RETURN, 1

END


; ----------------------------------------------------------------------------
; Destructor
; ----------------------------------------------------------------------------
PRO PHA::cleanup
    
    PTR_FREE, self.timeLookup
    PTR_FREE, self.energyLookup

    PTR_FREE, self.timeSpan
    PTR_FREE, self.energySpan
        
END    


; ----------------------------------------------------------------------------
; Rebin an array according to a lookup table
;
; INPUTS:
;     IN_SPEC : Input array
;     IN_ERRS : Input error array
;     IN_THRESHOLDS : Bin thresholds array, (2, nBins + 1)
;     LOOKUP : Index array (INTEGER), missing integers in the sequence indicate
;       which bins are to be combined into one bin in the output.  For ease of
;       calculation, always has the last element equal to the number of bins + 1
;
; OUTPUTS:
;     OUT_SPEC : Binned array with dimension N_ELEMENTS (LOOKUP) - 1
;     OUT_ERRS : Errors on OUT_SPEC
;     OUT_THRESHOLDS : Bin thresholds of the OUT_SPEC, OUT_ERRS arrays.
;
; ----------------------------------------------------------------------------
PRO PHA::combine, $
    in_spec, in_errs, in_thresholds, lookup, $
    out_spec, out_errs, out_thresholds, LIVETIME = livetime


    n_bins = N_ELEMENTS (lookup) - 1
    dthresholds = in_thresholds[1, *] - in_thresholds[0, *]
    
    out_spec       = FLTARR (n_bins, /NOZERO) 
    out_errs       = FLTARR (n_bins, /NOZERO)
    out_thresholds = FLTARR (2, n_bins, /NOZERO)
    
    dt = FLTARR (n_bins, /NOZERO)
    
    FOR i = 0L, n_bins - 1 DO BEGIN
 
        first_ind = lookup[i]
        last_ind  = lookup[i + 1] - 1
        
        out_spec[i] = TOTAL (in_spec[first_ind:last_ind])
        
        errs = in_errs[first_ind:last_ind]
        IF KEYWORD_SET(LIVETIME) THEN errs *= livetime
        out_errs[i] = SQRT (TOTAL (errs * errs))
        dt[i] = TOTAL (dthresholds[first_ind:last_ind])
       
        ; Eliminate zero-width bins
        ;
        IF (dt[i] LE 0.0) THEN BEGIN         
            dt[i] = 1.0                 ; Arbitrary
            out_spec[i] = 0.0
            out_errs[i] = 0.0
        ENDIF
       
        out_thresholds[0, i] = in_thresholds[0, first_ind]
        out_thresholds[1, i] = in_thresholds[1, last_ind]
    
    ENDFOR
    
    out_spec = out_spec / dt
    out_errs = out_errs / dt
	IF KEYWORD_SET(LIVETIME) THEN out_errs /= livetime

END


;------------------------------------------------------------------------------
; Draw a curve, skipping over data gaps
;------------------------------------------------------------------------------
PRO PHA::drawHistory, x, y, dy, TOLERANCE = tolerance, _EXTRA = extra

    haveDY = (N_ELEMENTS (dy) NE 0)

    n = SIZE (x)
    IF (n[0] EQ 1) THEN BEGIN
   
       OPLOT, x, [y, y], _EXTRA = extra 
      
       IF (haveDY) THEN BEGIN
          xx = 0.5 * (x[0, 0] + x[1, 0])
          xx = [xx, xx]
          yy = y[0] + [-dy[0], dy[0]]
          OPLOT, xx, yy, _EXTRA = extra 
       ENDIF
   
    ENDIF ELSE BEGIN

       IF (N_ELEMENTS (tolerance) NE 0) THEN BEGIN

          gap_tol = tolerance

       ENDIF ELSE BEGIN       

          gap_tol = MAX ([5.0E-5, 0.005 * MIN (ABS (x[1, *] - x[0, *])) ])

       ENDELSE

       dx = x[0, 1:*] - x[1, 0:n[2] - 2]
       gaps = WHERE (dx GT gap_tol)

       IF (gaps[0] EQ -1) THEN BEGIN
          n_gaps = 0
          gaps = [-1, n[2] - 1]
       ENDIF ELSE BEGIN
          n_gaps = N_ELEMENTS (gaps)
          gaps = [-1, gaps, n[2] - 1]
       ENDELSE

       FOR i = 0L, n_gaps DO BEGIN
          
          good_x = [TRANSPOSE(x[0, gaps[i] + 1:gaps[i + 1]]), x[1, gaps[i + 1]]]
          good_y = y[gaps[i] + 1:gaps[i + 1]]
          
          nx = N_ELEMENTS (good_x)
          ny = N_ELEMENTS (good_y)
 
          xx = FLTARR (2 * nx)
          yy = FLTARR (2 * ny)
          
          even = 2 * LINDGEN (nx)
          xx[even] = good_x
          xx[even + 1] = good_x
          
          even = 2 * LINDGEN (ny)
          yy[even] = good_y
          yy[even + 1] = good_y
          
          k = 2 * nx - 2
          OPLOT, xx[1:k], yy, _EXTRA = extra
             
       ENDFOR
       
       ; Overplot error bars for small range
       ;
       IF (haveDY) THEN BEGIN
       
          plotOptions = TAG_NAMES(extra)
          haveYLogOption = WHERE(plotOptions EQ 'YLOG', co)
          IF (co EQ 0) THEN haveYLog = 0 ELSE $
                            haveYLog = extra.ylog
          
          x_range = 0.005 * (!X.CRANGE[1] - !X.CRANGE[0])
          dx = x[1, *] - x[0, *]
       
          FOR i = 0L, n[2] - 1 DO BEGIN
         
              IF (dx[i] GT x_range) THEN BEGIN
         
                 xx = 0.5 * (x[0, i] + x[1, i])
                 xx = [xx, xx]
; *****************************************************
                 IF (haveYLog EQ 1) THEN BEGIN
                     dy_lo = y[i] - dy[i]
                     IF (dy_lo LE 0.0) THEN dy_lo = 1E-20
                     yy = [dy_lo, y[i] + dy[i]]
                 ENDIF ELSE yy = y[i] + [-dy[i], dy[i]]
; *****************************************************
                 OPLOT, xx, yy, _EXTRA = extra 
         
              ENDIF
         
          ENDFOR

       ENDIF
    
    ENDELSE

END


; ----------------------------------------------------------------------------
; Overplot selected regions, specified by a SPAN
; ----------------------------------------------------------------------------
PRO PHA::plotSpan, binEdges, binValues, aSpan, $
    NOSHADE = noshade, _EXTRA = extra

    xr = AXISRANGE (/XAXIS)
    yr = AXISRANGE (/YAXIS)

    n_regions = SIZE (aSpan)

    IF ((n_regions[1] - 1) GT 0) THEN BEGIN

       ; Plot individual regions

       FOR jj = 1, n_regions[1] - 1 DO BEGIN

           ind = INTARR(2)
           edge = NEAREST_EDGE (binEdges, aSpan[jj, *], ind)

           IF (KEYWORD_SET (noshade)) THEN BEGIN

              IF (edge[0] NE xr[0]) THEN $
	         OPLOT, [1, 1] * edge[0], yr, _EXTRA = extra

              IF (edge(1) NE xr[1]) THEN $
	         OPLOT, [1, 1] * edge[1], yr, _EXTRA = extra
           
           ENDIF ELSE BEGIN

              numBins = ind[1] - ind[0] + 1
              indexArr = INDGEN (numBins) + ind[0]

              xArr = FLTARR (numBins * 2 + 2)           
              FOR i = 0L, numBins - 1  DO $
   	          xArr[2*i:2*i+1] = binEdges[0, indexArr[i]]
              xArr[2*i:2*i+1] = binEdges[1, indexArr[i-1]]
              xArr = [xArr, xArr[0]]

              yArr = [yr[0], FLTARR (numBins * 2 + 2)]
              FOR i = 0, numBins - 1  DO $
 	          yArr[2*i+1:2*i+2] = binValues[indexArr[i]]
              yArr[N_ELEMENTS (yArr) - 2: N_ELEMENTS (yArr) - 1] = yr[0] 

              POLYFILL, xArr, yArr, $
                  LINE_FILL = 1, SPACING = 0.2, ORIENTATION = 45, $
                  NOCLIP = 0, _EXTRA = extra

              n = N_ELEMENTS (xarr)
              PLOTS, [xarr[0], xarr[0]], $
                     [yarr[0], yarr[1]], NOCLIP = 0, _EXTRA = extra
              PLOTS, [xarr[n-2], xarr[n-2]], $
                     [yarr[n-3], yarr[n-2]], NOCLIP = 0, _EXTRA = extra
                          
           ENDELSE

       ENDFOR

    ENDIF ELSE BEGIN 
       
       ; Convex hull only (zeroth array element)
       
       edge = NEAREST_EDGE (binEdges, aSpan[0, *])

       IF (edge[0] NE xr[0]) THEN $
          OPLOT, [1, 1] * edge[0], yr, _EXTRA = extra

       IF (edge[1] NE xr[1]) THEN $
          OPLOT, [1, 1] * edge[1], yr, _EXTRA = extra
    
    ENDELSE

END


; ----------------------------------------------------------------------------
; Return Reader object data.  
;
; WARNING: Allows return of pointer-to-data, which violates
; encapsulation and can compromise the object internal state.
; ----------------------------------------------------------------------------
FUNCTION PHA::header, POINTER = pointer
       
    RETURN, KEYWORD_SET (pointer) ? $
            self.Reader->header (/POINTER) : $
            self.Reader->header ()

END

FUNCTION PHA::data, POINTER = pointer

    RETURN, KEYWORD_SET (pointer) ? $
            self.Reader->data (/POINTER) : $
            self.Reader->data ()

END

FUNCTION PHA::units, POINTER = pointer

    RETURN, KEYWORD_SET (pointer) ? $
            self.Reader->units (/POINTER) : $
            self.Reader->units ()

END


;------------------------------------------------------------------------------
; Calculate the SNR for the two cases of time and energy.
; Pass in the accumulated signal and noise to add into the total. 
;------------------------------------------------------------------------------
FUNCTION PHA::calcSNR, theIdx, obsRate, obsErr, $
                       backRate, backErrs,      $
                       signal, noise, LIVETIME = livetime

    signal    = signal + obsRate[theIdx] - backRate[theIdx] ;/ edges[theIdx]
    sigmaBack = backErrs[theIdx] ;/ edges[theIdx]
	IF KEYWORD_SET(LIVETIME) THEN sigmaBack *= SQRT(livetime)
    sigmaObs  = obsErr[theIdx]
	IF KEYWORD_SET(LIVETIME) THEN sigmaObs *= SQRT(livetime)
    noise     = SQRT (noise * noise + sigmaObs * sigmaObs + sigmaBack * sigmaBack)
    RETURN, signal / noise
        
END


; ----------------------------------------------------------------------------
; Get/set current data range
; ----------------------------------------------------------------------------
FUNCTION PHA::range        & RETURN, self.range        & END
FUNCTION PHA::default      & RETURN, self.default      & END

PRO PHA::setRange, range & self.range = range          & END
PRO PHA::setDefaultRange & self.default = self.range  & END
PRO PHA::resetDefaultRange & self.range = self.default   & END

; ----------------------------------------------------------------------------
; Get/set lookup arrays
; ----------------------------------------------------------------------------

FUNCTION PHA::timeLookup & RETURN, *self.timeLookup & END
    
PRO PHA::setTimeLookup, timeLookup, INITIALIZE = initialize

    data = self.Reader->data (/POINTER)
        
    PTR_FREE, self.timeLookup

    IF (KEYWORD_SET (initialize)) THEN BEGIN

       s = SIZE ((*data).rates)
       self.timeLookup = PTR_NEW (LINDGEN (s[2] + 1))
    
    ENDIF ELSE BEGIN

       self.timeLookup = PTR_NEW (timeLookup)
           
    ENDELSE

END

    
FUNCTION PHA::energyLookup & RETURN, *self.energyLookup & END

PRO PHA::setEnergyLookup, energyLookup, INITIALIZE = initialize
    
    data = self.Reader->data (/POINTER)

    PTR_FREE, self.energyLookup

    IF (KEYWORD_SET (initialize)) THEN BEGIN

       s = SIZE ((*data).rates)
       self.energyLookup = PTR_NEW (LINDGEN (s[1] + 1))
    
    ENDIF ELSE BEGIN

       self.energyLookup = PTR_NEW (energyLookup)
           
    ENDELSE

END


; ----------------------------------------------------------------------------
; Get/set selection span arrays
; ----------------------------------------------------------------------------

FUNCTION PHA::timeSpan & RETURN, *self.timeSpan & END

PRO PHA::setTimeSpan, timeSpan, INITIALIZE = initialize
    
    data = self.Reader->data (/POINTER)

    PTR_FREE, self.timeSpan

    IF (KEYWORD_SET (initialize)) THEN BEGIN

       self.timeSpan = PTR_NEW (FLTARR (1, 2))
       (*self.timeSpan)[0, *] = [MIN ((*data).times), MAX ((*data).times)]
    
    ENDIF ELSE BEGIN

       self.timeSpan = PTR_NEW (timeSpan)
           
    ENDELSE

END
    
FUNCTION PHA::energySpan & RETURN, *self.energySpan & END

PRO PHA::setEnergySpan, energySpan, INITIALIZE = initialize
    
    data = self.Reader->data (/POINTER)

    PTR_FREE, self.energySpan

    IF (KEYWORD_SET (initialize)) THEN BEGIN

       self.energySpan = PTR_NEW (FLTARR (1, 2))
       (*self.energySpan)[0, *] = [MIN ((*data).eedges), MAX ((*data).eedges)]

    ENDIF ELSE BEGIN

       self.energySpan = PTR_NEW (energySpan)
           
    ENDELSE

END


; ----------------------------------------------------------------------------
; Object definition
; ----------------------------------------------------------------------------
PRO PHA__define

    obj = { PHA, $

        Reader             : OBJ_NEW (),    $  ; Reference to a DataReader
        
        timeLookup         : PTR_NEW (),    $  ; Lookup into the times array
        energyLookup       : PTR_NEW (),    $  ; Lookup into the eedges array
 
        timeSpan           : PTR_NEW (),    $  ; SPAN of user time selections
        energySpan         : PTR_NEW (),    $  ; SPAN of user energy selections
        
        range              : FLTARR (2, 2), $  ; Current data ranges
        default            : FLTARR (2, 2)  $  ; Default data ranges      


    }

END
