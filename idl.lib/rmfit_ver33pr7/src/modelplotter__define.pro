; ----------------------------------------------------------------------------
; Object to display fitted spectra and fit residuals.
;
; EXAMPLE: Differential photon energy spectrum
;      xData    = energy bin edges (2 x n)
;      xDataErr = energy bin widths
;      yData    = flux
;      yDataErr = flux error
;
; Upper limits are defined by yData values which are 
; less than yDataErr values, and are determined from the
; value of upperLimitSigma
; ----------------------------------------------------------------------------



; ----------------------------------------------------------------------------
; Constructor
; ----------------------------------------------------------------------------
FUNCTION modelPlotter::init, mfitDisplay, SIGMA = sigma    

    ;== Set upper limit sigma value
                
    self.upperLimitSigma = (N_ELEMENTS (sigma) EQ 0) ? 2.0 : FLOAT (sigma) 
    
    ;== Set the display
    
    ok = self->PLOTTER::INIT (mfitDisplay)
    
    ;== Plot positions for plotting residuals
    
    self.dataPlotPos      = [0.12, 0.3, 0.97, 0.95]
    self.residualsPlotPos = [0.12, 0.1, 0.97, 0.3]
    self.noResidPlotPos   = [0.12, 0.1, 0.97, 0.95]

    ;== A single color object is used for the application
            
    self.Color = mfitDisplay.color ;OBJ_SINGLETON ('Color')
    
    ;== Various things needed for plotting
    self.plotSym    = PTR_NEW ([3, 8, 6, 4, 5, 7, 2])
    self.plotSymV   = PTR_NEW (['!13+!X', '!20B!X', '!9B!X', '!9V!X', '!4D!X', '!9X!X', '!4*!X'])

    RETURN, 1
      
END


; ----------------------------------------------------------------------------
; Destructor
; ----------------------------------------------------------------------------
PRO modelPlotter::cleanup

    PTR_FREE, self.plotSym
    PTR_FREE, self.plotSymV

    self->Plotter::cleanup
    
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
PRO modelPlotter::combine, $
    in_spec, in_errs, in_modl, in_energies, in_widths, lo, hi, lookup, $
    out_spec, out_errs, out_modl, out_energies, out_widths, lo1, hi1


    n_bins = N_ELEMENTS (lookup) - 1
    dthresholds = in_widths
    ;== Spectra already have units of keV^-1
    temp_spec = in_spec * in_widths
    temp_errs = in_errs * in_widths
    temp_modl = in_modl * in_widths
    
    in_thresholds  = TRANSPOSE ([[in_energies - in_widths / 2.], $
                                 [in_energies + in_widths / 2.]])
    
    out_spec       = FLTARR (n_bins, /NOZERO) 
    out_errs       = FLTARR (n_bins, /NOZERO)
    out_modl       = out_spec 
    out_thresholds = FLTARR (2, n_bins, /NOZERO)
    
    dt = FLTARR (n_bins, /NOZERO)
    
    FOR i = 0L, n_bins - 1 DO BEGIN
 
        first_ind = lookup[i]
        last_ind  = lookup[i + 1] - 1
        
        out_spec[i] = TOTAL (temp_spec[first_ind:last_ind])
        out_modl[i] = TOTAL (temp_modl[first_ind:last_ind])
        
        errs = temp_errs[first_ind:last_ind]
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
    out_modl = out_modl / dt
    out_energies = (out_thresholds[1, *] + out_thresholds[0, *]) / 2.
    out_widths   =  out_thresholds[1, *] - out_thresholds[0, *]
    lo1 = MAX (WHERE (lookup LE lo))
    hi1 = MIN([MAX (WHERE (lookup LE hi)) + 1, n_bins - 1])
;    hi1 = MIN (WHERE (lookup GE hi))
    
END


; ----------------------------------------------------------------------------
; Create a label of a time or energy selection interval
; ----------------------------------------------------------------------------
FUNCTION modelPlotter::label, range, UNITS = units

    ; COMPILE_OPT HIDDEN

    IF (N_ELEMENTS (units) EQ 0) THEN units = ''
    
    IF (ABS (range[0]) GT 100) THEN $
       r0 = STRTRIM (STRING (range[0], FORMAT = '(D15.2)'), 2) $
    ELSE $
       r0 = STRTRIM (STRING (range[0], FORMAT = '(D15.3)'), 2)

    IF (ABS (range[1]) GT 10000) THEN $
       r1 = STRTRIM (STRING (range[1], FORMAT = '(D15.0)'), 2) $
    ELSE $
    IF (LONG (ABS (range[1])) GT 100) THEN $
       r1 = STRTRIM (STRING (range[1], FORMAT = '(D15.2)'), 2) $
    ELSE $
       r1 = STRTRIM (STRING (range[1], FORMAT = '(D15.3)'), 2)
                  
    label = r0 + ': ' + r1 + ' ' + units
    RETURN, label              
                  
END


; ----------------------------------------------------------------------------
; Extract informational notes from an MFIT_DETECTOR structure
; ----------------------------------------------------------------------------
FUNCTION modelPlotter::getNotes, detData

; TODO Won't work for multiple detectors

    filename =  (detData.detector->dataReader())->filename (/BASE)
    notes    = ((detData.detector->dataReader())->header()).notes
  
    notes = [notes, 'File name: ' + filename]
    
    label = 'Fit interval: ' + $
            self->label (detData.timeInt, UNITS = 's') + ', ' + $
            self->label (detData.energyInt, UNITS = 'keV')
    
    notes = [notes, label]

    RETURN, notes
        
END


; ----------------------------------------------------------------------------
; Add informational notes to a Postscript page.  
;
; WARNING: This routine uses hardcoded DEVICE coordinates, and is set up 
; for the Postscript device on a Letter size page.
; ----------------------------------------------------------------------------
PRO modelPlotter::plotNotes, notes, NUFNU = nufnu

    myNotes = notes(WHERE(notes NE ' '))
    IF (N_ELEMENTS (notes) GT 40) THEN $
        myNotes = notes[0: 40]
    
    n = N_ELEMENTS (myNotes)	

	IF KEYWORD_SET(NUFNU) THEN BEGIN
    FOR i = 0, n - 1 DO BEGIN
		self.display->annotate, myNotes[i], ALIGN = 0, XPOSITION = !X.WINDOW[0] + 0.01, $
			YPOSITION = !Y.WINDOW[1] - i * 0.015 - 0.015, $
			/SMALL, _EXTRA = extra

    ENDFOR
    ENDIF ELSE BEGIN
    FOR i = 0, n - 1 DO BEGIN
		self.display->annotate, myNotes[n - 1 - i], ALIGN = 0, XPOSITION = !X.WINDOW[0] + 0.01, $
			YPOSITION = !Y.WINDOW[0] + i * 0.015 + 0.015, $
			/SMALL, _EXTRA = extra

    ENDFOR
    ENDELSE
    
END


; ----------------------------------------------------------------------------
; Plot Chi^2 confidence interval.  
; ----------------------------------------------------------------------------
PRO modelPlotter::plotChi1D, chiValues, paramValues, bestParam, paramName, $
    plotNotes, doPlotNotes, HARDCOPY = hardcopy, OPLOT = oplot, $
    _REF_EXTRA = extra

    ;theColors = self.Color->model(/NAMES)
	myDisp = self.display   ; Managed display has link to fitter:

	myStat = myDisp.fitter->getStatistic ()
	myTitles = ["Chi Squared", "-2 LOG (LIKELIHOOD)", "Castor C-STAT"]
    
    chi = FLTARR (3)
    chiLabels = STRARR (3)
    nInterest = self.numInterest  ;Really 1
    
    sigLevels = FLTARR (10, 3)
    sigLevels[0: 29] = $
             [1.00,  2.30,  3.53,  4.72,  5.89,  7.04,  8.18,  9.30, 10.42, 11.54, $
              4.00,  6.18,  8.02,  9.72, 11.31, 12.85, 14.34, 15.79, 17.21, 18.61, $
              9.00, 11.83, 14.16, 16.25, 18.20, 20.06, 21.85, 23.57, 25.26, 26.90]
    
    nineLevels = FLTARR (10, 3)
    nineLevels[0: 29] = $
            [ 2.70,  4.60,  6.25,  7.78,  9.24, 10.64, 12.02, 13.36, 14.68, 15.99, $
              6.63,  9.21, 11.34, 13.28, 15.09, 16.81, 18.48, 20.09, 21.67, 23.21, $
             10.83, 13.82, 16.27, 18.47, 20.52, 22.46, 24.32, 26.12, 27.88, 29.59]
    
    mychisq = self.FitModel->get_model_chisq()
    
    IF (self.displaySigma EQ 0) THEN BEGIN
        chi[0] = mychisq + sigLevels [nInterest, 0]
        chi[1] = mychisq + sigLevels [nInterest, 1]
        chi[2] = mychisq + sigLevels [nInterest, 2]
        chiLabels[0] = ' 1!7r!X'
        chiLabels[1] = ' 2!7r!X'
        chiLabels[2] = ' 3!7r!X'
    ENDIF ELSE BEGIN
        chi[0] = mychisq + nineLevels [nInterest, 0]
        chi[1] = mychisq + nineLevels [nInterest, 1]
        chi[2] = mychisq + nineLevels [nInterest, 2]
        chiLabels[0] = ' 90%'
        chiLabels[1] = ' 99%'
        chiLabels[2] = ' 99.9%'
    ENDELSE
    
    xRange = [MIN (paramValues), MAX (paramValues)]
    yRange = [MIN (chiValues), MAX (chiValues)]

    self->adjustRanges, xRange, yRange, 'chi1d'
    
    PLOT, paramValues, chiValues, /NODATA, /YNOZERO, XSTYLE=1, $
          XTITLE = paramName, YTITLE = myTitles[myStat], $
          COLOR = self.Color->color ('FG'), $
	      BACKGROUND = self.Color->color ('BG'), $
          POSITION = [0.12, 0.1, 0.95, 0.95], $
          XRANGE = xRange, YRANGE = yRange, $
          _EXTRA = [*self.myOptions, 'XRANGE', 'YRANGE']

    OPLOT, paramValues, chiValues, COLOR = self.Color->color ('SPEC'), _EXTRA = extra
    
    OPLOT, [bestParam, bestParam], !y.crange, lineStyle = 1, $
           COLOR = self.Color->color ('BKGD'), _EXTRA = extra
           
    FOR ii = 0, 2 DO BEGIN
        OPLOT, !X.CRANGE, [chi[ii], chi[ii]], lineStyle = 1, $
            COLOR = self.Color->color ('BKGD'), _EXTRA = extra
        IF (chi[ii] LT !y.crange[1]) THEN $
            XYOUTS, !X.CRANGE[1], chi[ii], chiLabels[ii], _EXTRA = extra
        ENDFOR
    
    IF (doPlotNotes) THEN BEGIN
        
        self->plotNotes, plotNotes

    ENDIF

    self->finishRanges, 'chi1d'
    self->annotate, COLOR = self.Color->color ('FG'), $
              BACKGROUND = self.Color->color ('BG'), $
              _EXTRA = *self.myOptions

END


; ----------------------------------------------------------------------------
; Plot 2D Chi^2 confidence interval.  
; ----------------------------------------------------------------------------
PRO modelPlotter::plotChi2D, chiValues, paramValues, bestParam, paramName, $
    plotNotes, doPlotNotes, HARDCOPY = hardcopy, OPLOT = oplot, $
    _EXTRA = extra

    ;theColors = self.Color->model(/NAMES)
    
    chi = FLTARR (3)
    chiLabels = STRARR (3)
    nInterest = self.numInterest + 1  ;Really 2
    numSteps  = N_ELEMENTS (paramValues[*, 0])
    
    sigLevels = FLTARR (10, 3)
    sigLevels[0: 29] = $
             [1.00,  2.30,  3.53,  4.72,  5.89,  7.04,  8.18,  9.30, 10.42, 11.54, $
              4.00,  6.18,  8.02,  9.72, 11.31, 12.85, 14.34, 15.79, 17.21, 18.61, $
              9.00, 11.83, 14.16, 16.25, 18.20, 20.06, 21.85, 23.57, 25.26, 26.90]
    
    nineLevels = FLTARR (10, 3)
    nineLevels[0: 29] = $
            [ 2.70,  4.60,  6.25,  7.78,  9.24, 10.64, 12.02, 13.36, 14.68, 15.99, $
              6.63,  9.21, 11.34, 13.28, 15.09, 16.81, 18.48, 20.09, 21.67, 23.21, $
             10.83, 13.82, 16.27, 18.47, 20.52, 22.46, 24.32, 26.12, 27.88, 29.59]
    
    mychisq = self.FitModel->get_model_chisq()
    
    IF (self.displaySigma EQ 0) THEN BEGIN
        chi[0] = mychisq + sigLevels [nInterest, 0]
        chi[1] = mychisq + sigLevels [nInterest, 1]
        chi[2] = mychisq + sigLevels [nInterest, 2]
        chiLabels[0] = '1!7r!X'
        chiLabels[1] = '2!7r!X'
        chiLabels[2] = '3!7r!X'
    ENDIF ELSE BEGIN
        chi[0] = mychisq + nineLevels [nInterest, 0]
        chi[1] = mychisq + nineLevels [nInterest, 1]
        chi[2] = mychisq + nineLevels [nInterest, 2]
        chiLabels[0] = '90%'
        chiLabels[1] = '99%'
        chiLabels[2] = '99.9%'
    ENDELSE

    ;== PLOT first, so we can use the FG color for the axes...
    PLOT,  paramValues[*, 0], paramValues[*, 1], /NODATA, $
          XSTYLE=1, YSTYLE=1, $
          XTITLE = paramName[0], YTITLE = paramName[1], $
          COLOR = self.Color->color ('FG'), $
	      BACKGROUND = self.Color->color ('BG'), $
          POSITION = self.noResidPlotPos, $
          _EXTRA = extra

    ;== Use MIN_CURVE_SURF to smooth out the contours if undersampled:
    IF (numSteps LT 26) THEN BEGIN
        CONTOUR, chiValues, paramValues[*, 0], paramValues[*, 1], $
            XSTYLE=1 + 4, YSTYLE=1 + 4, /NOERASE, $
            COLOR = self.Color->color ('SPEC'), $
            /FOLLOW, LEVELS = chi, C_LABELS = [1, 1, 1], $
            POSITION = self.noResidPlotPos, $
            C_ANNOTATION = chiLabels, _EXTRA = extra
    ENDIF ELSE BEGIN
        CONTOUR, chiValues, paramValues[*, 0], paramValues[*, 1], $
            XSTYLE=1 + 4, YSTYLE=1 + 4, /NOERASE, $
            COLOR = self.Color->color ('SPEC'), $
            /FOLLOW, LEVELS = chi, C_LABELS = [1, 1, 1], $
            POSITION = self.noResidPlotPos, $
            C_ANNOTATION = chiLabels, _EXTRA = extra
    ENDELSE
          
    sx = FINDGEN (16) * 2 * !PI / 16
    USERSYM, COS (sx), SIN (sx) ;, /FILL

    OPLOT, [bestParam[0], bestParam[0]], [bestParam[1], bestParam[1]], $
           PSYM = 8, COLOR = self.Color->color ('BKGD'), _EXTRA = extra
               
    IF (doPlotNotes) THEN BEGIN
        
        self->plotNotes, plotNotes

    ENDIF

    self->annotate, COLOR = self.Color->color ('FG'), $
              BACKGROUND = self.Color->color ('BG'), $
              _EXTRA = *self.myOptions

END


; ----------------------------------------------------------------------------
; Plot cumulative spectrum.  
; Set singleDet to a detector index to plot a single detector 
; (D = all detectors).
; ----------------------------------------------------------------------------
PRO modelPlotter::cumulativeSpectrum, residChoice, plotNotes, doPlotNotes, $
    HARDCOPY = hardcopy, SINGLEDET = singleDet, OPLOT = oplot, $
    XLOG = xLog, YLOG = yLog, $
    SHOWMODEL = showModel, _REF_EXTRA = extra

; TODO: Fix color model
    theColors = self.Color->model(/NAMES)
    myColors = [1]

    allDets =  N_ELEMENTS (singleDet) EQ 0       

    IF (NOT self.FitModel->haveFit ()) THEN BEGIN
       MESSAGE, /CONTINUE, 'No single model fit data available.'
       RETURN
    ENDIF
       
    model = self.FitModel->model ()

    ;== Compute net (background subtracted) count rate and errors
    
    NETCRATE, $
        model.obsCrate, model.backCrate, model.backCsig, $
        model.liveTime, model.chanWidth, netCrate, netCsig
        
    tempCRate = TOTAL(netCrate * model.chanWidth * model.liveTime, 1, /CUMULATIVE)
    ;== Only plot the global model count rate (not for each photon term)
     
    modelCntRate = REFORM (model.modelCntRate[*, 0, *])
    tempModel = TOTAL(modelCntRate * model.chanWidth * model.liveTime, 1, /CUMULATIVE)

    dataRange  = self->dataRange (model.numDet, model.fitChannels, $
        model.chanEnergy, model.chanWidth, tempCRate, YERR = netCsig, model.livetime)
    
    modelRange = self->dataRange (model.numDet, model.fitChannels, $
        model.chanEnergy, model.chanWidth, tempModel, model.livetime)
        
    yBinned = self->binnedRange(model.numDet, model.fitChannels, model.numchan, $
              model.chanEnergy, model.chanWidth, tempCRate, netCsig, tempModel)
              
    dataRange.y = yBinned

    xRange = [ $
        MIN ([dataRange.x[0, *], modelRange.x[0, *]]), $
        MAX ([dataRange.x[1, *], modelRange.x[1, *]])]

    yRange = [ $
        MIN ([dataRange.y[0, *], modelRange.y[0, *]]), $
        MAX ([dataRange.y[1, *], modelRange.y[1, *]])]
    IF ROUND(yRange[0] * 1.0E+10) EQ 0 THEN yRange[0] = MIN (dataRange.y[0, *])
        
    yRange = [nicenumber(yRange[0], /FLOOR), nicenumber(yRange[1], /CEIL)]
        
    self->adjustRanges, xRange, yRange, 'cumulative', XLOG = xLog, YLOG = yLog ;, /XLOG, /YLOG
    
    residTitle = ' '
    
    IF (residChoice EQ 1L) THEN BEGIN
       residRange  = self->dataRange (model.numDet, model.fitChannels, $
           model.chanEnergy, model.chanWidth, netCrate - modelCntRate, $
           YERR = SQRT(model.modelCRVari), $ ;netCsig, $
           model.livetime, /RESID)
    ENDIF ELSE BEGIN
       sigArr = netCsig
       sigArr[*] = 1.   
       residRange  = self->dataRange (model.numDet, model.fitChannels, $
           model.chanEnergy, model.chanWidth, (netCrate - modelCntRate) / $
           (SQRT(model.modelCRVari) > 1.E-15), YERR = sigArr, model.livetime, /RESID) ;netCsig
       residTitle = 'Sigma'
    ENDELSE
        
    yExtreme = MAX (ABS(residRange.y))
    yResidRange = [-yExtreme, yExtreme]

    detNames = self.DetectorList->names ()

    IF (residChoice NE 2L) THEN BEGIN
        FOR i = 0, model.numDet - 1 DO BEGIN
        
            idx = i
            IF (NOT allDets) THEN idx = singleDet
	        
            lo = model.fitChannels[0, idx]
            hi = model.fitChannels[1, idx]
	
            xTitle = 'Energy (keV)'
            ;yTitle = ' '
	
            ;== Set up for combining the energy bins for display purposes
            nChan = model.numchan[idx] - 1
            detData = self.DetectorList->data (detNames[idx])
            myDisp = detData.display
            eLookup = mydisp.spectrum->energyLookup()
            
            self->combine, netCrate[0: nChan, idx], SQRT(model.modelCRVari[0: nChan, idx]), $;netCsig
                modelCntRate[0: nChan, idx], model.chanEnergy[0: nChan, idx], $
                model.chanWidth[0: nChan, idx], lo, hi, eLookup, $
                out_spec, out_errs, out_modl, out_energies, out_widths, lo1, hi1
    
            IF (residChoice EQ 1L) THEN BEGIN
                residRate = out_spec[lo1:hi1] - out_modl[lo1:hi1]
                residSig  = out_errs[lo1:hi1]
            ENDIF ELSE BEGIN
                residRate = (out_spec[lo1:hi1] - out_modl[lo1:hi1]) / $
                             out_errs[lo1:hi1]
                residSig  = REPLICATE(1., hi1 - lo1 + 1)
            ENDELSE
	        
            colorInd = (idx EQ 0) ? (WHERE (theColors EQ 'SPEC'))[0] : idx
	        
            self->plotResiduals, OPLOT = (idx NE 0), $
                  out_energies[lo1:hi1], out_widths[lo1:hi1], $
                  residRate, residSig, idx, $
                  XTITLE = xTitle, YTITLE = residTitle, $
                  XRANGE = xRange, $
                  YRANGE = yResidRange, $
                  XMARGIN = [13, 3], $
                  XSTYLE = 1, XLOG = xlog, $
                  FG_COLOR = self.Color->color ('FG'), $
	              BACKGROUND = self.Color->color ('BG'), $
                  COLOR = self.Color->color (theColors[colorInd]) , $
                  _EXTRA = [*self.botOptions, 'XRANGE']
	            
            ENDFOR
        ENDIF

    FOR i = 0, model.numDet - 1 DO BEGIN

        idx = i
        IF (NOT allDets) THEN idx = singleDet
        
        lo = model.fitChannels[0, idx]
        hi = model.fitChannels[1, idx]

        xTitle = ' '
        yTitle = 'COUNTS (cumulative)'

        ;== Set up for combining the energy bins for display purposes
        nChan = model.numchan[idx] - 1
        detData = self.DetectorList->data (detNames[idx])
        myDisp = detData.display
        eLookup = mydisp.spectrum->energyLookup()
        self->combine, netCrate[0: nChan, idx], SQRT(model.modelCRVari[0: nChan, idx]), $;netCsig
            modelCntRate[0: nChan, idx], model.chanEnergy[0: nChan, idx], $
            model.chanWidth[0: nChan, idx], lo, hi, eLookup, $
            out_spec, out_errs, out_modl, out_energies, out_widths, lo1, hi1
    
        colorInd = (idx EQ 0) ? (WHERE (theColors EQ 'SPEC'))[0] : idx
		myColors = [myColors, self.Color->color (theColors[colorInd])]
		
		tempOutSpec = TOTAL(out_spec[lo1:hi1] * out_widths[lo1:hi1] * model.liveTime[lo, i], /CUMULATIVE)
		tempOutErrs = out_errs[lo1:hi1] * out_widths[lo1:hi1] * model.liveTime[lo, i]
        
        self->plotData, residChoice, OPLOT = (idx NE 0), $
            out_energies[lo1:hi1], out_widths[lo1:hi1], $
            tempOutSpec, tempOutErrs, idx, $ 
            YTITLE = yTitle, $
            XRANGE = xRange, $
            YRANGE = yRange, $
            XSTYLE = 1, YSTYLE = 1, $
            XLOG = xLog, YLOG = yLog, $
            /ARROW, XMARGIN = [13, 3], $
            FG_COLOR = self.Color->color ('FG'), $
	        BACKGROUND = self.Color->color ('BG'), $
            COLOR = self.Color->color (theColors[colorInd]), $
            _EXTRA = [(residChoice EQ 2L ? *self.myOptions: *self.topOptions), $
                      'XRANGE','YRANGE']

        ;== We don't want DRAW_HISTOGRAM to draw an extra bin!
        IF (lo1 NE hi1) THEN BEGIN
           ;== May 25, 2009 RDP: The odd-out case was when the highest channel selected was the 
           ; highest channel in the data. We don't do this very often, which is why it took so 
           ; long to find and correct.
           IF (hi EQ hi1) OR (hi EQ nChan) THEN BEGIN ;No binning!
               hi2 = hi1
           ENDIF ELSE BEGIN
               hi2 = hi1 - 1
           ENDELSE
        ENDIF ELSE BEGIN
           hi2 = hi1
        ENDELSE
		tempOutModl = TOTAL(out_modl[lo1:hi2] * out_widths[lo1:hi2] * model.liveTime[lo1, i], /CUMULATIVE)

        DRAW_HISTOGRAM, /OPLOT, out_energies[lo1:hi2], $
	        out_widths[lo1:hi2], tempOutModl, $
	        COLOR = self.Color->color ('BKGD'), THICK = 1.25, $
	        _EXTRA = *self.myOptions
	        
    ENDFOR
    
    ;Section added 6/26/03 by Y.K. to show individual model plots:    
    IF (N_ELEMENTS (SHOWMODEL) EQ 0) THEN showModel = 1 ;Keyword NOT set, default
    IF (showModel EQ 0) THEN BEGIN
        myDisp = self.display   ; Managed display has link to fitter:

        numAddTerms = myDisp.fitter->getNumAddTerms ()
        term_used = WHERE (model.term_used[1: numAddTerms] EQ 1, n_mod)
	
	IF (n_mod GT 1) THEN BEGIN  ; Number of models is not trivial:
	    
	    FOR midx = 1, n_mod DO BEGIN
	        
            mrate = REFORM (model.modelCntRate[*, midx, *])
    
            FOR idx = 0, model.numDet - 1 DO BEGIN
            
            	lo = model.fitChannels[0, idx]
            	hi = model.fitChannels[1, idx]
            
            	;== Set up for combining the energy bins for display purposes
            	nChan = model.numchan[idx] - 1
            	detData = self.DetectorList->data (detNames[idx])
            	myDisp = detData.display
            	eLookup = mydisp.spectrum->energyLookup()
            	self->combine, netCrate[0: nChan, idx], $
                    SQRT(model.modelCRVari[0: nChan, idx]), mrate[0: nChan, idx], $;netCsig
            		model.chanEnergy[0: nChan, idx], $
            		model.chanWidth[0: nChan, idx], lo, hi, eLookup, $
            		out_spec, out_errs, out_modl, out_energies, $
                    out_widths, lo1, hi1
            	
                ;== We don't want DRAW_HISTOGRAM to draw an extra bin!
                IF (lo1 NE hi1) THEN BEGIN
                   IF (hi EQ hi1) THEN BEGIN ;No binning!
                       hi2 = hi1
                   ENDIF ELSE BEGIN
                       hi2 = hi1 - 1
                   ENDELSE
                ENDIF ELSE BEGIN
                   hi2 = hi1
                ENDELSE
        	    
                good_energies = out_energies[lo1: hi2]
                good_widths   = out_widths[lo1: hi2]
                good_modl     = out_modl[lo1: hi2]
                
                nonzero = WHERE (good_modl GT 0, nzc)
                IF (nzc EQ 0) THEN BEGIN
                    good_modl = ABS(good_modl)
                    nonzero = WHERE (good_modl GT 0, nzc)
                ENDIF
                
                IF (nzc NE 0) THEN BEGIN
    		        
                    IF (nzc NE N_ELEMENTS (good_mdl)) THEN BEGIN
                    
                        good_energies = good_energies[nonzero]
                        good_widths   = good_widths[nonzero]
                        good_modl     = good_modl[nonzero]
                        
                    ENDIF
                    
		tempGoodModl = TOTAL(good_modl * good_widths * model.liveTime[lo1, idx], /CUMULATIVE)
                    
                    colorInd = (idx EQ 0) ? $
                        (WHERE (theColors EQ 'SPEC'))[0] : idx
                        
                    DRAW_HISTOGRAM, /OPLOT, good_energies, $
                    	 good_widths, tempGoodModl, $
                    	 COLOR = self.Color->color ('BKGD'), $
                    	 LINESTYLE = 2, _EXTRA = *self.myOptions
                         
                ENDIF
    		    
            ENDFOR  ; idx
        ENDFOR      ; midx
	ENDIF
    ENDIF

    IF (doPlotNotes) THEN BEGIN
        
        self->plotNotes, plotNotes, /NUFNU

    ENDIF

    self->annotate, COLOR = self.Color->color ('FG'), $
              BACKGROUND = self.Color->color ('BG'), $
              _EXTRA = *self.myOptions
    self->plotLegend, COLOR = myColors[1:*], _EXTRA = *self.myOptions  ;extra
    self->finishRanges, 'cumulative', XLOG = xLog, YLOG = yLog ;, /XLOG, /YLOG
    
END


; ----------------------------------------------------------------------------
; Plot raw spectrum.  
; Set singleDet to a detector index to plot a single detector 
; (D = all detectors).
; ----------------------------------------------------------------------------
PRO modelPlotter::rawSpectrum, residChoice, plotNotes, doPlotNotes, $
    HARDCOPY = hardcopy, SINGLEDET = singleDet, OPLOT = oplot, $
    XLOG = xLog, YLOG = yLog, $
    SHOWMODEL = showModel, _REF_EXTRA = extra

; TODO: Fix color model
    theColors = self.Color->model(/NAMES)
    myColors = [1]

    allDets =  N_ELEMENTS (singleDet) EQ 0       

    IF (NOT self.FitModel->haveFit ()) THEN BEGIN
       MESSAGE, /CONTINUE, 'No single model fit data available.'
       RETURN
    ENDIF
       
    model = self.FitModel->model ()

    ;== Compute net (background subtracted) count rate and errors
    
    NETCRATE, $
        model.obsCrate, model.backCrate, model.backCsig, $
        model.liveTime, model.chanWidth, netCrate, netCsig
        
    ;== Only plot the global model count rate (not for each photon term)
     
    modelCntRate = REFORM (model.modelCntRate[*, 0, *])

    dataRange  = self->dataRange (model.numDet, model.fitChannels, $
        model.chanEnergy, model.chanWidth, netCrate * model.chanWidth * model.liveTime, YERR = netCsig, model.livetime)
    
    modelRange = self->dataRange (model.numDet, model.fitChannels, $
        model.chanEnergy, model.chanWidth, modelCntRate * model.chanWidth * model.liveTime, model.livetime)
        
    yBinned = self->binnedRange(model.numDet, model.fitChannels, model.numchan, $
              model.chanEnergy, model.chanWidth, netCrate * model.chanWidth * model.liveTime, netCsig, modelCntRate)
              
    dataRange.y = yBinned

    xRange = [ $
        MIN ([dataRange.x[0, *], modelRange.x[0, *]]), $
        MAX ([dataRange.x[1, *], modelRange.x[1, *]])]

    yRange = [ $
        MIN ([dataRange.y[0, *], modelRange.y[0, *]]), $
        MAX ([dataRange.y[1, *], modelRange.y[1, *]])]
    IF ROUND(yRange[0] * 1.0E+10) EQ 0 THEN yRange[0] = MIN (dataRange.y[0, *])
        
    yRange = [nicenumber(yRange[0], /FLOOR), nicenumber(yRange[1], /CEIL)]
        
    self->adjustRanges, xRange, yRange, 'raw', XLOG = xLog, YLOG = yLog ;, /XLOG, /YLOG
    
    residTitle = ' '
    
    IF (residChoice EQ 1L) THEN BEGIN
       residRange  = self->dataRange (model.numDet, model.fitChannels, $
           model.chanEnergy, model.chanWidth, netCrate - modelCntRate, $
           YERR = SQRT(model.modelCRVari), $ ;netCsig, $
           model.livetime, /RESID)
    ENDIF ELSE BEGIN
       sigArr = netCsig
       sigArr[*] = 1.   
       residRange  = self->dataRange (model.numDet, model.fitChannels, $
           model.chanEnergy, model.chanWidth, (netCrate - modelCntRate) / $
           (SQRT(model.modelCRVari) > 1.E-15), YERR = sigArr, model.livetime, /RESID) ;netCsig
       residTitle = 'Sigma'
    ENDELSE
        
    yExtreme = MAX (ABS(residRange.y))
    yResidRange = [-yExtreme, yExtreme]

    detNames = self.DetectorList->names ()

    IF (residChoice NE 2L) THEN BEGIN
        FOR i = 0, model.numDet - 1 DO BEGIN
        
            idx = i
            IF (NOT allDets) THEN idx = singleDet
	        
            lo = model.fitChannels[0, idx]
            hi = model.fitChannels[1, idx]
	
            xTitle = 'Energy (keV)'
            ;yTitle = ' '
	
            ;== Set up for combining the energy bins for display purposes
            nChan = model.numchan[idx] - 1
            detData = self.DetectorList->data (detNames[idx])
            myDisp = detData.display
            eLookup = mydisp.spectrum->energyLookup()
            
            self->combine, netCrate[0: nChan, idx], SQRT(model.modelCRVari[0: nChan, idx]), $;netCsig
                modelCntRate[0: nChan, idx], model.chanEnergy[0: nChan, idx], $
                model.chanWidth[0: nChan, idx], lo, hi, eLookup, $
                out_spec, out_errs, out_modl, out_energies, out_widths, lo1, hi1
    
            IF (residChoice EQ 1L) THEN BEGIN
                residRate = out_spec[lo1:hi1] - out_modl[lo1:hi1]
                residSig  = out_errs[lo1:hi1]
            ENDIF ELSE BEGIN
                residRate = (out_spec[lo1:hi1] - out_modl[lo1:hi1]) / $
                             out_errs[lo1:hi1]
                residSig  = REPLICATE(1., hi1 - lo1 + 1)
            ENDELSE
	        
            colorInd = (idx EQ 0) ? (WHERE (theColors EQ 'SPEC'))[0] : idx
	        
            self->plotResiduals, OPLOT = (idx NE 0), $
                  out_energies[lo1:hi1], out_widths[lo1:hi1], $
                  residRate, residSig, idx, $
                  XTITLE = xTitle, YTITLE = residTitle, $
                  XRANGE = xRange, $
                  YRANGE = yResidRange, $
                  XMARGIN = [13, 3], $
                  XSTYLE = 1, XLOG = xlog, $
                  FG_COLOR = self.Color->color ('FG'), $
	              BACKGROUND = self.Color->color ('BG'), $
                  COLOR = self.Color->color (theColors[colorInd]) , $
                  _EXTRA = [*self.botOptions, 'XRANGE']
	            
            ENDFOR
        ENDIF

    FOR i = 0, model.numDet - 1 DO BEGIN

        idx = i
        IF (NOT allDets) THEN idx = singleDet
        
        lo = model.fitChannels[0, idx]
        hi = model.fitChannels[1, idx]

        xTitle = ' '
        yTitle = 'COUNTS (background subtracted)'

        ;== Set up for combining the energy bins for display purposes
        nChan = model.numchan[idx] - 1
        detData = self.DetectorList->data (detNames[idx])
        myDisp = detData.display
        eLookup = mydisp.spectrum->energyLookup()
        self->combine, netCrate[0: nChan, idx], SQRT(model.modelCRVari[0: nChan, idx]), $;netCsig
            modelCntRate[0: nChan, idx], model.chanEnergy[0: nChan, idx], $
            model.chanWidth[0: nChan, idx], lo, hi, eLookup, $
            out_spec, out_errs, out_modl, out_energies, out_widths, lo1, hi1
    
        colorInd = (idx EQ 0) ? (WHERE (theColors EQ 'SPEC'))[0] : idx
		myColors = [myColors, self.Color->color (theColors[colorInd])]
        
        self->plotData, residChoice, OPLOT = (idx NE 0), $
            out_energies[lo1:hi1], out_widths[lo1:hi1], $
            out_spec[lo1:hi1] * out_widths[lo1:hi1] * model.liveTime[lo, i], $
            out_errs[lo1:hi1] * out_widths[lo1:hi1] * model.liveTime[lo, i], idx, $ 
            YTITLE = yTitle, $
            XRANGE = xRange, $
            YRANGE = yRange, $
            XSTYLE = 1, YSTYLE = 1, $
            XLOG = xLog, YLOG = yLog, $
            /ARROW, XMARGIN = [13, 3], $
            FG_COLOR = self.Color->color ('FG'), $
	        BACKGROUND = self.Color->color ('BG'), $
            COLOR = self.Color->color (theColors[colorInd]), $
            _EXTRA = [(residChoice EQ 2L ? *self.myOptions: *self.topOptions), $
                      'XRANGE','YRANGE']

        ;== We don't want DRAW_HISTOGRAM to draw an extra bin!
        IF (lo1 NE hi1) THEN BEGIN
           ;== May 25, 2009 RDP: The odd-out case was when the highest channel selected was the 
           ; highest channel in the data. We don't do this very often, which is why it took so 
           ; long to find and correct.
           IF (hi EQ hi1) OR (hi EQ nChan) THEN BEGIN ;No binning!
               hi2 = hi1
           ENDIF ELSE BEGIN
               hi2 = hi1 - 1
           ENDELSE
        ENDIF ELSE BEGIN
           hi2 = hi1
        ENDELSE

        DRAW_HISTOGRAM, /OPLOT, out_energies[lo1:hi2], $
	        out_widths[lo1:hi2], out_modl[lo1:hi2] * out_widths[lo1:hi2] * model.liveTime[lo, i], $
	        COLOR = self.Color->color ('BKGD'), THICK = 1.25, $
	        _EXTRA = *self.myOptions
	        
    ENDFOR
    
    ;Section added 6/26/03 by Y.K. to show individual model plots:    
    IF (N_ELEMENTS (SHOWMODEL) EQ 0) THEN showModel = 1 ;Keyword NOT set, default
    IF (showModel EQ 0) THEN BEGIN
        myDisp = self.display   ; Managed display has link to fitter:

        numAddTerms = myDisp.fitter->getNumAddTerms ()
        term_used = WHERE (model.term_used[1: numAddTerms] EQ 1, n_mod)
	
	IF (n_mod GT 1) THEN BEGIN  ; Number of models is not trivial:
	    
	    FOR midx = 1, n_mod DO BEGIN
	        
            mrate = REFORM (model.modelCntRate[*, midx, *])
    
            FOR idx = 0, model.numDet - 1 DO BEGIN
            
            	lo = model.fitChannels[0, idx]
            	hi = model.fitChannels[1, idx]
            
            	;== Set up for combining the energy bins for display purposes
            	nChan = model.numchan[idx] - 1
            	detData = self.DetectorList->data (detNames[idx])
            	myDisp = detData.display
            	eLookup = mydisp.spectrum->energyLookup()
            	self->combine, netCrate[0: nChan, idx], $
                    SQRT(model.modelCRVari[0: nChan, idx]), mrate[0: nChan, idx], $;netCsig
            		model.chanEnergy[0: nChan, idx], $
            		model.chanWidth[0: nChan, idx], lo, hi, eLookup, $
            		out_spec, out_errs, out_modl, out_energies, $
                    out_widths, lo1, hi1
            	
                ;== We don't want DRAW_HISTOGRAM to draw an extra bin!
                IF (lo1 NE hi1) THEN BEGIN
                   IF (hi EQ hi1) THEN BEGIN ;No binning!
                       hi2 = hi1
                   ENDIF ELSE BEGIN
                       hi2 = hi1 - 1
                   ENDELSE
                ENDIF ELSE BEGIN
                   hi2 = hi1
                ENDELSE
        	    
                good_energies = out_energies[lo1: hi2]
                good_widths   = out_widths[lo1: hi2]
                good_modl     = out_modl[lo1: hi2]
                
                nonzero = WHERE (good_modl GT 0, nzc)
                IF (nzc EQ 0) THEN BEGIN
                    good_modl = ABS(good_modl)
                    nonzero = WHERE (good_modl GT 0, nzc)
                ENDIF
                
                IF (nzc NE 0) THEN BEGIN
    		        
                    IF (nzc NE N_ELEMENTS (good_mdl)) THEN BEGIN
                    
                        good_energies = good_energies[nonzero]
                        good_widths   = good_widths[nonzero]
                        good_modl     = good_modl[nonzero]
                        
                    ENDIF
                    
                    colorInd = (idx EQ 0) ? $
                        (WHERE (theColors EQ 'SPEC'))[0] : idx
                        
                    DRAW_HISTOGRAM, /OPLOT, good_energies, $
                    	 good_widths, good_modl * good_widths * model.liveTime[lo1, idx], $
                    	 COLOR = self.Color->color ('BKGD'), $
                    	 LINESTYLE = 2, _EXTRA = *self.myOptions
                         
                ENDIF
    		    
            ENDFOR  ; idx
        ENDFOR      ; midx
	ENDIF
    ENDIF

    IF (doPlotNotes) THEN BEGIN
        
        self->plotNotes, plotNotes

    ENDIF

    self->annotate, COLOR = self.Color->color ('FG'), $
              BACKGROUND = self.Color->color ('BG'), $
              _EXTRA = *self.myOptions
    self->plotLegend, COLOR = myColors[1:*], _EXTRA = *self.myOptions  ;extra
    self->finishRanges, 'raw', XLOG = xLog, YLOG = yLog ;, /XLOG, /YLOG
    
END


; ----------------------------------------------------------------------------
; Plot count spectrum.  
; Set singleDet to a detector index to plot a single detector 
; (D = all detectors).
; ----------------------------------------------------------------------------
PRO modelPlotter::countSpectrum, residChoice, plotNotes, doPlotNotes, $
    HARDCOPY = hardcopy, SINGLEDET = singleDet, OPLOT = oplot, $
    XLOG = xLog, YLOG = yLog, $
    SHOWMODEL = showModel, _REF_EXTRA = extra

; TODO: Fix color model
    theColors = self.Color->model(/NAMES)
    myColors = [1]

    allDets =  N_ELEMENTS (singleDet) EQ 0       

    IF (NOT self.FitModel->haveFit ()) THEN BEGIN
       MESSAGE, /CONTINUE, 'No single model fit data available.'
       RETURN
    ENDIF
       
    model = self.FitModel->model ()

    ;== Compute net (background subtracted) count rate and errors
    
    NETCRATE, $
        model.obsCrate, model.backCrate, model.backCsig, $
        model.liveTime, model.chanWidth, netCrate, netCsig
        
    ;== Only plot the global model count rate (not for each photon term)
     
    modelCntRate = REFORM (model.modelCntRate[*, 0, *])

    dataRange  = self->dataRange (model.numDet, model.fitChannels, $
        model.chanEnergy, model.chanWidth, netCrate, YERR = netCsig, model.livetime)
    
    modelRange = self->dataRange (model.numDet, model.fitChannels, $
        model.chanEnergy, model.chanWidth, modelCntRate, model.livetime)
        
    yBinned = self->binnedRange(model.numDet, model.fitChannels, model.numchan, $
              model.chanEnergy, model.chanWidth, netCrate, netCsig, modelCntRate)
              
    dataRange.y = yBinned

    xRange = [ $
        MIN ([dataRange.x[0, *], modelRange.x[0, *]]), $
        MAX ([dataRange.x[1, *], modelRange.x[1, *]])]

    yRange = [ $
        MIN ([dataRange.y[0, *], modelRange.y[0, *]]), $
        MAX ([dataRange.y[1, *], modelRange.y[1, *]])]
    IF ROUND(yRange[0] * 1.0E+10) EQ 0 THEN yRange[0] = MIN (dataRange.y[0, *])
    yRange = [nicenumber(yRange[0], /FLOOR), nicenumber(yRange[1], /CEIL)]
        
    self->adjustRanges, xRange, yRange, 'counts', XLOG = xLog, YLOG = yLog ;, /XLOG, /YLOG
    
    residTitle = ' '
    
    IF (residChoice EQ 1L) THEN BEGIN
       residRange  = self->dataRange (model.numDet, model.fitChannels, $
           model.chanEnergy, model.chanWidth, netCrate - modelCntRate, $
           YERR = SQRT(model.modelCRVari), $ ;netCsig, $
           model.livetime, /RESID)
    ENDIF ELSE BEGIN
       sigArr = netCsig
       sigArr[*] = 1.   
       residRange  = self->dataRange (model.numDet, model.fitChannels, $
           model.chanEnergy, model.chanWidth, (netCrate - modelCntRate) / $
           (SQRT(model.modelCRVari) > 1.E-15), YERR = sigArr, model.livetime, /RESID) ;netCsig
       residTitle = 'Sigma'
    ENDELSE
        
    yExtreme = MAX (ABS(residRange.y))
    yResidRange = [-yExtreme, yExtreme]

    detNames = self.DetectorList->names ()

    IF (residChoice NE 2L) THEN BEGIN
        FOR i = 0, model.numDet - 1 DO BEGIN
        
            idx = i
            IF (NOT allDets) THEN idx = singleDet
	        
            lo = model.fitChannels[0, idx]
            hi = model.fitChannels[1, idx]
	
            xTitle = 'Energy (keV)'
            ;yTitle = ' '
	
            ;== Set up for combining the energy bins for display purposes
            nChan = model.numchan[idx] - 1
            detData = self.DetectorList->data (detNames[idx])
            myDisp = detData.display
            eLookup = mydisp.spectrum->energyLookup()
            
            self->combine, netCrate[0: nChan, idx], SQRT(model.modelCRVari[0: nChan, idx]), $;netCsig
                modelCntRate[0: nChan, idx], model.chanEnergy[0: nChan, idx], $
                model.chanWidth[0: nChan, idx], lo, hi, eLookup, $
                out_spec, out_errs, out_modl, out_energies, out_widths, lo1, hi1
    
            IF (residChoice EQ 1L) THEN BEGIN
                residRate = out_spec[lo1:hi1] - out_modl[lo1:hi1]
                residSig  = out_errs[lo1:hi1]
            ENDIF ELSE BEGIN
                residRate = (out_spec[lo1:hi1] - out_modl[lo1:hi1]) / $
                             out_errs[lo1:hi1]
                residSig  = REPLICATE(1., hi1 - lo1 + 1)
            ENDELSE
	        
            colorInd = (idx EQ 0) ? (WHERE (theColors EQ 'SPEC'))[0] : idx
	        
            self->plotResiduals, OPLOT = (idx NE 0), $
                  out_energies[lo1:hi1], out_widths[lo1:hi1], $
                  residRate, residSig, idx, $
                  XTITLE = xTitle, YTITLE = residTitle, $
                  XRANGE = xRange, $
                  YRANGE = yResidRange, $
                  XMARGIN = [13, 3], $
                  XSTYLE = 1, XLOG = xlog, $
                  FG_COLOR = self.Color->color ('FG'), $
	              BACKGROUND = self.Color->color ('BG'), $
                  COLOR = self.Color->color (theColors[colorInd]) , $
                  _EXTRA = [*self.botOptions, 'XRANGE']
	            
            ENDFOR
        ENDIF

    FOR i = 0, model.numDet - 1 DO BEGIN

        idx = i
        IF (NOT allDets) THEN idx = singleDet
        
        lo = model.fitChannels[0, idx]
        hi = model.fitChannels[1, idx]

        xTitle = ' '
        yTitle = 'Rate (counts s!U-1!N keV!U-1!N)'

        ;== Set up for combining the energy bins for display purposes
        nChan = model.numchan[idx] - 1
        detData = self.DetectorList->data (detNames[idx])
        myDisp = detData.display
        eLookup = mydisp.spectrum->energyLookup()
        self->combine, netCrate[0: nChan, idx], SQRT(model.modelCRVari[0: nChan, idx]), $;netCsig
            modelCntRate[0: nChan, idx], model.chanEnergy[0: nChan, idx], $
            model.chanWidth[0: nChan, idx], lo, hi, eLookup, $
            out_spec, out_errs, out_modl, out_energies, out_widths, lo1, hi1
    
        colorInd = (idx EQ 0) ? (WHERE (theColors EQ 'SPEC'))[0] : idx
		myColors = [myColors, self.Color->color (theColors[colorInd])]
        
        self->plotData, residChoice, OPLOT = (idx NE 0), $
            out_energies[lo1:hi1], out_widths[lo1:hi1], $
            out_spec[lo1:hi1], out_errs[lo1:hi1], idx, $ 
            YTITLE = yTitle, $
            XRANGE = xRange, $
            YRANGE = yRange, $
            XSTYLE = 1, YSTYLE = 1, $
            XLOG = xLog, YLOG = yLog, $
            /ARROW, XMARGIN = [13, 3], $
            FG_COLOR = self.Color->color ('FG'), $
	        BACKGROUND = self.Color->color ('BG'), $
            COLOR = self.Color->color (theColors[colorInd]), $
            _EXTRA = [(residChoice EQ 2L ? *self.myOptions: *self.topOptions), $
                      'XRANGE','YRANGE']

        ;== We don't want DRAW_HISTOGRAM to draw an extra bin!
        IF (lo1 NE hi1) THEN BEGIN
           ;== May 25, 2009 RDP: The odd-out case was when the highest channel selected was the 
           ; highest channel in the data. We don't do this very often, which is why it took so 
           ; long to find and correct.
           IF (hi EQ hi1) OR (hi EQ nChan) THEN BEGIN ;No binning!
               hi2 = hi1
           ENDIF ELSE BEGIN
               hi2 = hi1 - 1
           ENDELSE
        ENDIF ELSE BEGIN
           hi2 = hi1
        ENDELSE

        DRAW_HISTOGRAM, /OPLOT, out_energies[lo1:hi2], $
	        out_widths[lo1:hi2], out_modl[lo1:hi2], $
	        COLOR = self.Color->color ('BKGD'), THICK = 1.25, $
	        _EXTRA = *self.myOptions
	        
    ENDFOR
    
    ;Section added 6/26/03 by Y.K. to show individual model plots:    
    IF (N_ELEMENTS (SHOWMODEL) EQ 0) THEN showModel = 1 ;Keyword NOT set, default
    IF (showModel EQ 0) THEN BEGIN
        myDisp = self.display   ; Managed display has link to fitter:

        numAddTerms = myDisp.fitter->getNumAddTerms ()
        term_used = WHERE (model.term_used[1: numAddTerms] EQ 1, n_mod)
	
	IF (n_mod GT 1) THEN BEGIN  ; Number of models is not trivial:
	    
	    FOR midx = 1, n_mod DO BEGIN
	        
            mrate = REFORM (model.modelCntRate[*, midx, *])
    
            FOR idx = 0, model.numDet - 1 DO BEGIN
            
            	lo = model.fitChannels[0, idx]
            	hi = model.fitChannels[1, idx]
            
            	;== Set up for combining the energy bins for display purposes
            	nChan = model.numchan[idx] - 1
            	detData = self.DetectorList->data (detNames[idx])
            	myDisp = detData.display
            	eLookup = mydisp.spectrum->energyLookup()
            	self->combine, netCrate[0: nChan, idx], $
                    SQRT(model.modelCRVari[0: nChan, idx]), mrate[0: nChan, idx], $;netCsig
            		model.chanEnergy[0: nChan, idx], $
            		model.chanWidth[0: nChan, idx], lo, hi, eLookup, $
            		out_spec, out_errs, out_modl, out_energies, $
                    out_widths, lo1, hi1
            	
                ;== We don't want DRAW_HISTOGRAM to draw an extra bin!
                IF (lo1 NE hi1) THEN BEGIN
                   IF (hi EQ hi1) THEN BEGIN ;No binning!
                       hi2 = hi1
                   ENDIF ELSE BEGIN
                       hi2 = hi1 - 1
                   ENDELSE
                ENDIF ELSE BEGIN
                   hi2 = hi1
                ENDELSE
        	    
                good_energies = out_energies[lo1: hi2]
                good_widths   = out_widths[lo1: hi2]
                good_modl     = out_modl[lo1: hi2]
                
                nonzero = WHERE (good_modl GT 0, nzc)
                IF (nzc EQ 0) THEN BEGIN
                    good_modl = ABS(good_modl)
                    nonzero = WHERE (good_modl GT 0, nzc)
                ENDIF
                
                IF (nzc NE 0) THEN BEGIN
    		        
                    IF (nzc NE N_ELEMENTS (good_mdl)) THEN BEGIN
                    
                        good_energies = good_energies[nonzero]
                        good_widths   = good_widths[nonzero]
                        good_modl     = good_modl[nonzero]
                        
                    ENDIF
                    
                    colorInd = (idx EQ 0) ? $
                        (WHERE (theColors EQ 'SPEC'))[0] : idx
                        
                    DRAW_HISTOGRAM, /OPLOT, good_energies, $
                    	 good_widths, good_modl, $
                    	 COLOR = self.Color->color ('BKGD'), $
                    	 LINESTYLE = 2, _EXTRA = *self.myOptions
                         
                ENDIF
    		    
            ENDFOR  ; idx
        ENDFOR      ; midx
	ENDIF
    ENDIF

    IF (doPlotNotes) THEN BEGIN
        
        self->plotNotes, plotNotes

    ENDIF

    self->annotate, COLOR = self.Color->color ('FG'), $
              BACKGROUND = self.Color->color ('BG'), $
              _EXTRA = *self.myOptions
    self->plotLegend, COLOR = myColors[1:*], _EXTRA = *self.myOptions  ;extra
    self->finishRanges, 'counts', XLOG = xLog, YLOG = yLog ;, /XLOG, /YLOG
    
END


; ----------------------------------------------------------------------------
; Plot photon spectrum.
; Set singleDet to a detector index to plot a single detector 
; (D = all detectors).
; ----------------------------------------------------------------------------
PRO modelPlotter::photonSpectrum, residChoice, plotNotes, doPlotNotes, $
    HARDCOPY = hardcopy, SINGLEDET = singleDet, OPLOT = oplot, $
	XLOG = xLog, YLOG = yLog, $
    SHOWMODEL = showModel, _REF_EXTRA = extra

    theColors = self.Color->model(/NAMES)
    myColors = [1]

    allDets =  N_ELEMENTS (singleDet) EQ 0       

    IF (NOT self.FitModel->haveFit ()) THEN BEGIN
       MESSAGE, /CONTINUE, 'No fit data available.'
       RETURN
    ENDIF
       
    model = self.FitModel->model ()

    ;== Compute net (background subtracted) count rate and errors
    
    NETCRATE, $
        model.obsCrate, model.backCrate, model.backCsig, $
        model.liveTime, model.chanWidth, netCrate, netCsig

    modelCntRate = REFORM (model.modelCntRate[*, 0, *])

    ;== Only plot the global model photon rate (not for each photon term)
     
    modelPhotRate = REFORM (model.modelPhotRate[*, 0, *])

    dataRange  = self->dataRange (model.numDet, model.fitChannels, $
        model.chanEnergy, model.chanWidth, model.photObsRate,  $
        YERR = model.photObsSig, model.livetime)
    
    modelRange = self->dataRange (model.numDet, model.fitChannels, $
        model.chanEnergy, model.chanWidth, modelPhotRate, model.livetime, /FINE)

    yBinned = self->binnedRange(model.numDet, model.fitChannels, model.numchan, $
              model.chanEnergy, model.chanWidth, model.photObsRate, model.photObsSig, $
              model.modelPhotRateByChan)
              
    dataRange.y = yBinned
    
    xRange = [ $
        MIN ([dataRange.x[0, *], modelRange.x[0, *]]), $
        MAX ([dataRange.x[1, *], modelRange.x[1, *]])]

    yRange = [ $
        MIN ([dataRange.y[0, *], modelRange.y[0, *]]), $
        MAX ([dataRange.y[1, *], modelRange.y[1, *]])]
    IF ROUND(yRange[0] * 1.0E+10) EQ 0 THEN yRange[0] = MIN (dataRange.y[0, *])

    yRange = [nicenumber(yRange[0], /FLOOR), nicenumber(yRange[1], /CEIL)]
        
    self->adjustRanges, xRange, yRange, 'photons', XLOG = xLog, YLOG = yLog ;, /XLOG, /YLOG
    
    residTitle = ' '
       
    IF (residChoice EQ 1L) THEN BEGIN
       residRange  = self->dataRange (model.numDet, model.fitChannels, $
           model.chanEnergy, model.chanWidth, model.photObsRate - model.modelPhotRateByChan, $
           YERR = model.photObsSig, model.livetime, /RESID)
    ENDIF ELSE BEGIN
       sigArr = netCsig
       sigArr[*] = 1.   
       residRange  = self->dataRange (model.numDet, model.fitChannels, $
           model.chanEnergy, model.chanWidth, (netCrate - modelCntRate) / $
           (SQRT(model.modelCRVari) > 1.E-15), YERR = sigArr, model.livetime, /RESID)
       residTitle = 'Sigma'
    ENDELSE
        
    yExtreme = MAX (ABS(residRange.y))
    yResidRange = [-yExtreme, yExtreme]
    
    detNames = self.DetectorList->names ()
    
    IF (residChoice NE 2L) THEN BEGIN
	    FOR i = 0, model.numDet - 1 DO BEGIN
	
	        idx = i
	        IF (NOT allDets) THEN idx = singleDet
	
	        lo = model.fitChannels[0, idx]
	        hi = model.fitChannels[1, idx]
	
	        xTitle = 'Energy (keV)'
	        yTitle = ' '
	
            ;== Set up for combining the energy bins for display purposes
            nChan = model.numchan[idx] - 1
            detData = self.DetectorList->data (detNames[idx])
            myDisp = detData.display
            eLookup = mydisp.spectrum->energyLookup()
    
	        IF (residChoice EQ 1L) THEN BEGIN
                self->combine, model.photObsRate[0: nChan, idx], model.photObsSig[0: nChan, idx], $
                    model.modelPhotRateByChan[0: nChan, idx], model.chanEnergy[0: nChan, idx], $
                    model.chanWidth[0: nChan, idx], lo, hi, eLookup, $
                    out_spec, out_errs, out_modl, out_energies, out_widths, lo1, hi1
                    
	            residRate = out_spec[lo1:hi1] - out_modl[lo1:hi1]
	            residSig  = out_errs[lo1:hi1]
	        ENDIF ELSE BEGIN
                self->combine, netCrate[0: nChan, idx], SQRT(model.modelCRVari[0: nChan, idx]), $;netCsig
                    modelCntRate[0: nChan, idx], model.chanEnergy[0: nChan, idx], $
                    model.chanWidth[0: nChan, idx], lo, hi, eLookup, $
                    out_spec, out_errs, out_modl, out_energies, out_widths, lo1, hi1
    
	            residRate = (out_spec[lo1:hi1] - out_modl[lo1:hi1]) / $
	                        out_errs[lo1:hi1]
	            residSig  = REPLICATE(1., hi1 - lo1 + 1)
	        ENDELSE
	        
	        colorInd = (idx EQ 0) ? (WHERE (theColors EQ 'SPEC'))[0] : idx
	        
	        self->plotResiduals, OPLOT = (idx NE 0), $
	            out_energies[lo1:hi1], out_widths[lo1:hi1], $
	            residRate, residSig, idx, $
	            XTITLE = xTitle, YTITLE = residTitle, $
	            XRANGE = xRange, $
	            YRANGE = yResidRange, $
	            XMARGIN = [13, 3], $
	            XSTYLE = 1, XLOG = xlog, $
	            FG_COLOR = self.Color->color ('FG'), $
	            BACKGROUND = self.Color->color ('BG'), $
	            COLOR = self.Color->color (theColors[colorInd]), $
	            _EXTRA = [*self.botOptions, 'XRANGE']
	
	        ENDFOR
	    ENDIF

    FOR i = 0, model.numDet - 1 DO BEGIN

        idx = i
        IF (NOT allDets) THEN idx = singleDet
        
        lo = model.fitChannels[0, idx]
        hi = model.fitChannels[1, idx]

        yTitle = 'Flux (photons cm!U-2!N s!U-1!N keV!U-1!N)'

        colorInd = (idx EQ 0) ? (WHERE (theColors EQ 'SPEC'))[0] : idx
		myColors = [myColors, self.Color->color (theColors[colorInd])]
	
        ;== Set up for combining the energy bins for display purposes
        nChan = model.numchan[idx] - 1
        detData = self.DetectorList->data (detNames[idx])
        myDisp = detData.display
        eLookup = mydisp.spectrum->energyLookup()
        self->combine, model.photObsRate[0: nChan, idx], model.photObsSig[0: nChan, idx], $
            model.photObsRate[0: nChan, idx], model.chanEnergy[0: nChan, idx], $
            model.chanWidth[0: nChan, idx], lo, hi, eLookup, $
            out_spec, out_errs, out_modl, out_energies, out_widths, lo1, hi1
        
        self->plotData, residChoice, OPLOT = (idx NE 0), $
            out_energies[lo1:hi1], out_widths[lo1:hi1], $
            out_spec[lo1:hi1], out_errs[lo1:hi1], idx, $ 
            YTITLE = yTitle, $
            XRANGE = xRange, $ 
            YRANGE = yRange, $
            XSTYLE = 1, YSTYLE = 1, $
            XLOG = xLog, YLOG = yLog, $
            /ARROW, XMARGIN = [13, 3], $
            FG_COLOR = self.Color->color ('FG'), $
	        BACKGROUND = self.Color->color ('BG'), $
            COLOR = self.Color->color (theColors[colorInd]), $
            _EXTRA = [(residChoice EQ 2L ? *self.myOptions: *self.topOptions), $
                      'XRANGE','YRANGE']

        lo = 0
        hi = model.numChan[idx] * 6
        ph_width = ALOG10(model.modelEnergy[lo + 1:hi, idx]) - $
                   ALOG10(model.modelEnergy[lo :hi - 1, idx])
        frac_width = ph_width / ALOG10(model.modelEnergy[lo :hi - 1, idx])
        ;== 6/20/03 (YK & RDP) Mask out infinite width bins
        inf_ind = WHERE (FINITE (frac_width, /INFINITY), infCo)
        IF (infCo NE 0) THEN frac_width [inf_ind] = 0.
        ll = MIN (WHERE (frac_width GT 1.25, fracCo))
        IF (fracCo NE 0) THEN hi = ll  ; Get rid of 'wide' energy bins (as with SDs)

        OPLOT, model.modelEnergy[lo :hi, idx], modelPhotRate[lo :hi, idx], $
            COLOR = self.Color->color ('BKGD'), _EXTRA = *self.myOptions

        IF (showModel EQ 0) THEN BEGIN
	
            myDisp = self.display   ; Managed display has link to fitter:

            numAddTerms = myDisp.fitter->getNumAddTerms ()
            term_used = WHERE (model.term_used[1: numAddTerms] EQ 1, n_mod)
	
    	    IF (n_mod GT 1) THEN BEGIN  ; Number of models is not trivial:
    	    
    	        FOR midx = 1, n_mod DO BEGIN
    	        
                    mrate = REFORM (model.modelPhotRate[*, midx, *])
                    
                    nonzero = WHERE (mrate GT 0, nzc)
                    IF (nzc EQ 0) THEN BEGIN
                        mrate = ABS(mrate)
                    ENDIF
                    
                    OPLOT, model.modelEnergy[lo :hi, idx], mRate[lo :hi, idx], $
                           COLOR = self.Color->color ('BKGD'), $
                           LINESTYLE = 2, $
                           _EXTRA = *self.myOptions
    			 
                ENDFOR
            ENDIF
        ENDIF
	
        IF (doPlotNotes) THEN BEGIN
        
;           detData = self.DetectorList->data (detNames[idx])
;           notes   = self->getNotes (detData)
           self->plotNotes, plotNotes

        ENDIF

    ENDFOR

    self->annotate, COLOR = self.Color->color ('FG'), $
              BACKGROUND = self.Color->color ('BG'), $
              _EXTRA = *self.myOptions
    self->plotLegend, COLOR = myColors[1:*], _EXTRA = *self.myOptions  ;extra
    self->finishRanges, 'photons', XLOG = xLog, YLOG = yLog ;, /XLOG, /YLOG

END


; ----------------------------------------------------------------------------
; Plot photon Energy spectrum.
; Set singleDet to a detector index to plot a single detector 
; (D = all detectors).
; ----------------------------------------------------------------------------
PRO modelPlotter::energySpectrum, residChoice, plotNotes, doPlotNotes, $
    HARDCOPY = hardcopy, SINGLEDET = singleDet, OPLOT = oplot, $
    XLOG = xLog, YLOG = yLog, $
    SHOWMODEL = showModel, _REF_EXTRA = extra

    theColors = self.Color->model(/NAMES)
    myColors = [1]

    allDets =  N_ELEMENTS (singleDet) EQ 0       

    IF (NOT self.FitModel->haveFit ()) THEN BEGIN
       MESSAGE, /CONTINUE, 'No fit data available.'
       RETURN
    ENDIF
       
    model = self.FitModel->model ()

    ;== Compute net (background subtracted) count rate and errors
    
    NETCRATE, $
        model.obsCrate, model.backCrate, model.backCsig, $
        model.liveTime, model.chanWidth, netCrate, netCsig

    modelCntRate = REFORM (model.modelCntRate[*, 0, *])

    ;== Only plot the global model photon rate (not for each photon term)
     
    modelPhotRate = REFORM (model.modelPhotRate[*, 0, *])

    dataRange  = self->dataRange (model.numDet, model.fitChannels, $
        model.chanEnergy, model.chanWidth, $
        model.photObsRate * model.chanEnergy,  $
        YERR = model.photObsSig * model.chanEnergy, model.livetime)
    
    modelRange = self->dataRange (model.numDet, model.fitChannels, $
        model.chanEnergy, model.chanWidth, $
        model.modelPhotRateByChan * model.chanEnergy, $
        model.livetime)

    yBinned = self->binnedRange(model.numDet, model.fitChannels, model.numchan, $
              model.chanEnergy, model.chanWidth, $
              model.photObsRate * model.chanEnergy, model.photObsSig * model.chanEnergy, $
              model.modelPhotRateByChan * model.chanEnergy)
              
    dataRange.y = yBinned
    
    xRange = [ $
        MIN ([dataRange.x[0, *], modelRange.x[0, *]]), $
        MAX ([dataRange.x[1, *], modelRange.x[1, *]])]

    yRange = [ $
        MIN ([dataRange.y[0, *], modelRange.y[0, *]]), $
        MAX ([dataRange.y[1, *], modelRange.y[1, *]])]
    IF ROUND(yRange[0] * 1.0E+10) EQ 0 THEN yRange[0] = MIN (dataRange.y[0, *])

    yRange = [nicenumber(yRange[0], /FLOOR), nicenumber(yRange[1], /CEIL)]
        
    self->adjustRanges, xRange, yRange, 'energy', XLOG = xLog, YLOG = yLog ;, /XLOG, /YLOG
    
    residTitle = ' '
       
    IF (residChoice EQ 1L) THEN BEGIN
       residRange  = self->dataRange (model.numDet, model.fitChannels, $
           model.chanEnergy, model.chanWidth, $
	   (model.photObsRate - model.modelPhotRateByChan) * model.chanEnergy, $
           YERR = model.photObsSig, model.livetime, /RESID)
    ENDIF ELSE BEGIN
       sigArr = netCsig
       sigArr[*] = 1.   
       residRange  = self->dataRange (model.numDet, model.fitChannels, $
           model.chanEnergy, model.chanWidth, (netCrate - modelCntRate) / $
           (SQRT(model.modelCRVari) > 1.E-15), YERR = sigArr, model.livetime, /RESID)
       residTitle = 'Sigma'
    ENDELSE
        
    yExtreme = MAX (ABS(residRange.y))
    yResidRange = [-yExtreme, yExtreme]
    
    detNames = self.DetectorList->names ()
    
    IF (residChoice NE 2L) THEN BEGIN
	    FOR i = 0, model.numDet - 1 DO BEGIN
	
	        idx = i
	        IF (NOT allDets) THEN idx = singleDet
	
	        lo = model.fitChannels[0, idx]
	        hi = model.fitChannels[1, idx]
	
	        xTitle = 'Energy (keV)'
	        yTitle = ' '
	
            ;== Set up for combining the energy bins for display purposes
            nChan = model.numchan[idx] - 1
            detData = self.DetectorList->data (detNames[idx])
            myDisp = detData.display
            eLookup = mydisp.spectrum->energyLookup()
    
	        IF (residChoice EQ 1L) THEN BEGIN
                self->combine, $
                    model.photObsRate[0: nChan, idx] * model.chanEnergy[0: nChan, idx], $
                    model.photObsSig[0: nChan, idx] * model.chanEnergy[0: nChan, idx], $
                    model.modelPhotRateByChan[0: nChan, idx] * model.chanEnergy[0: nChan, idx], $
                    model.chanEnergy[0: nChan, idx], $
                    model.chanWidth[0: nChan, idx], lo, hi, eLookup, $
                    out_spec, out_errs, out_modl, out_energies, out_widths, lo1, hi1
        
	            residRate = out_spec[lo1:hi1] - out_modl[lo1:hi1]
	            residSig  = out_errs[lo1:hi1]
	        ENDIF ELSE BEGIN
                self->combine, netCrate[0: nChan, idx], SQRT(model.modelCRVari[0: nChan, idx]), $;netCsig
                    modelCntRate[0: nChan, idx], model.chanEnergy[0: nChan, idx], $
                    model.chanWidth[0: nChan, idx], lo, hi, eLookup, $
                    out_spec, out_errs, out_modl, out_energies, out_widths, lo1, hi1
                    
	            residRate = (out_spec[lo1:hi1] - out_modl[lo1:hi1]) / $
	                        out_errs[lo1:hi1]
	            residSig  = REPLICATE(1., hi1 - lo1 + 1)
	        ENDELSE
	        
	        colorInd = (idx EQ 0) ? (WHERE (theColors EQ 'SPEC'))[0] : idx
	        
	        self->plotResiduals, OPLOT = (idx NE 0), $
	            out_energies[lo1:hi1], out_widths[lo1:hi1], $
	            residRate, residSig, idx, $
	            XTITLE = xTitle, YTITLE = residTitle, $
	            XRANGE = xRange, $
	            YRANGE = yResidRange, $
	            XMARGIN = [13, 3], $
	            XSTYLE = 1, XLOG = xlog, $
	            FG_COLOR = self.Color->color ('FG'), $
	            BACKGROUND = self.Color->color ('BG'), $
	            COLOR = self.Color->color (theColors[colorInd]), $
	            _EXTRA = [*self.botOptions, 'XRANGE']
	
	        ENDFOR
	    ENDIF

    yTitle = 'Energy (photon cm!U-2!N s!U-1!N)'
    FOR i = 0, model.numDet - 1 DO BEGIN

        idx = i
        IF (NOT allDets) THEN idx = singleDet
        
        lo = model.fitChannels[0, idx]
        hi = model.fitChannels[1, idx]

        ;yTitle = 'Flux (photons cm!U-2!N s!U-1!N keV!U-1!N)'

        colorInd = (idx EQ 0) ? (WHERE (theColors EQ 'SPEC'))[0] : idx
		myColors = [myColors, self.Color->color (theColors[colorInd])]
	
        ;== Set up for combining the energy bins for display purposes
        nChan = model.numchan[idx] - 1
        detData = self.DetectorList->data (detNames[idx])
        myDisp = detData.display
        eLookup = mydisp.spectrum->energyLookup()
        self->combine, $
            model.photObsRate[0: nChan, idx] * model.chanEnergy[0: nChan, idx], $
            model.photObsSig[0: nChan, idx] * model.chanEnergy[0: nChan, idx], $
            model.photObsRate[0: nChan, idx] * model.chanEnergy[0: nChan, idx], $
            model.chanEnergy[0: nChan, idx], $
            model.chanWidth[0: nChan, idx], lo, hi, eLookup, $
            out_spec, out_errs, out_modl, out_energies, out_widths, lo1, hi1
        
        self->plotData, residChoice, OPLOT = (idx NE 0), $
            out_energies[lo1:hi1], out_widths[lo1:hi1], $
            out_spec[lo1:hi1], out_errs[lo1:hi1], idx, $ 
            YTITLE = yTitle, $
            XRANGE = xRange, $ 
            YRANGE = yRange, $
            XSTYLE = 1, YSTYLE = 1, $
            XLOG = xLog, YLOG = yLog, $
            /ARROW, XMARGIN = [13, 3], $
            FG_COLOR = self.Color->color ('FG'), $
	        BACKGROUND = self.Color->color ('BG'), $
            COLOR = self.Color->color (theColors[colorInd]), $
            _EXTRA = [(residChoice EQ 2L ? *self.myOptions: *self.topOptions), $
                      'XRANGE','YRANGE']

        lo = 0
        hi = model.numChan[idx] * 6
        ph_width = ALOG10(model.modelEnergy[lo + 1:hi, idx]) - $
                   ALOG10(model.modelEnergy[lo :hi - 1, idx])
        frac_width = ph_width / ALOG10(model.modelEnergy[lo :hi - 1, idx])
        ;== 6/20/03 (YK & RDP) Mask out infinite width bins
        inf_ind = WHERE (FINITE (frac_width, /INFINITY), infCo)
        IF (infCo NE 0) THEN frac_width [inf_ind] = 0.
        ll = MIN (WHERE (frac_width GT 1.25, fracCo))
        IF (fracCo NE 0) THEN hi = ll  ; Get rid of 'wide' energy bins (as with SDs)
        OPLOT, model.modelEnergy[lo :hi, idx], $
            modelPhotRate[lo :hi, idx] * model.modelEnergy[lo :hi, idx], $
            COLOR = self.Color->color ('BKGD'), _EXTRA = *self.myOptions

        IF (showModel EQ 0) THEN BEGIN
	
            myDisp = self.display   ; Managed display has link to fitter:

            numAddTerms = myDisp.fitter->getNumAddTerms ()
            term_used = WHERE (model.term_used[1: numAddTerms] EQ 1, n_mod)
	
            IF (n_mod GT 1) THEN BEGIN  ; Number of models is not trivial:
            
                FOR midx = 1, n_mod DO BEGIN
                
        	    mrate = REFORM (model.modelPhotRate[*, midx, *])
        	    
                nonzero = WHERE (mrate GT 0, nzc)
                IF (nzc EQ 0) THEN BEGIN
                    mrate = ABS(mrate)
                ENDIF
                    
                OPLOT, model.modelEnergy[lo :hi, idx], $
        	         mRate[lo :hi, idx] * model.modelEnergy[lo :hi, idx], $
                     COLOR = self.Color->color ('BKGD'), $
        		     LINESTYLE = 2, _EXTRA = *self.myOptions
        		 
                ENDFOR
            ENDIF
        ENDIF

        IF (doPlotNotes) THEN BEGIN
        
;           detData = self.DetectorList->data (detNames[idx])
;           notes   = self->getNotes (detData)
           self->plotNotes, plotNotes

        ENDIF

    ENDFOR

    self->annotate, COLOR = self.Color->color ('FG'), $
              BACKGROUND = self.Color->color ('BG'), $
              _EXTRA = *self.myOptions
    self->plotLegend, COLOR = myColors[1:*], _EXTRA = *self.myOptions  ;extra
    self->finishRanges, 'energy', XLOG = xLog, YLOG = yLog ;, /XLOG, /YLOG

END


; ----------------------------------------------------------------------------
; Plot nuFnu spectrum.
; Set singleDet to a detector index to plot a single detector 
; (D = all detectors).
; ----------------------------------------------------------------------------
PRO modelPlotter::nuFnuSpectrum, residChoice, plotNotes, doPlotNotes, $
    HARDCOPY = hardcopy, SINGLEDET = singleDet, OPLOT = oplot, $
    XLOG = xLog, YLOG = yLog, $
    SHOWMODEL = showModel, _REF_EXTRA = extra

    theColors = self.Color->model(/NAMES)
    myColors = [1]

    allDets =  N_ELEMENTS (singleDet) EQ 0       

    IF (NOT self.FitModel->haveFit ()) THEN BEGIN
       MESSAGE, /CONTINUE, 'No fit data available.'
       RETURN
    ENDIF
       
    model = self.FitModel->model ()

    ;== Compute net (background subtracted) count rate and errors
    
    NETCRATE, $
        model.obsCrate, model.backCrate, model.backCsig, $
        model.liveTime, model.chanWidth, $
        netCrate, netCsig

    modelCntRate = REFORM (model.modelCntRate[*, 0, *])

    xTitle = ' '
    yTitle = 'nuFnu (keV cm!U-2!N s!U-1!N)'

    dataRange  = self->dataRange (model.numDet, model.fitChannels, $
        model.chanEnergy, model.chanWidth, model.nuFnuData, $
        YERR = model.nuFnuSig, model.livetime)
    
    modelRange = self->dataRange (model.numDet, model.fitChannels, $
        model.chanEnergy, model.chanWidth, model.nufnuModel, model.livetime, /FINE)

    yBinned = self->binnedRange(model.numDet, model.fitChannels, model.numchan, $
              model.chanEnergy, model.chanWidth, $
              model.nuFnuData, model.nuFnuSig, $
              model.nuFnuData)
              
    dataRange.y = yBinned
    
    xRange = [ $
        MIN ([dataRange.x[0, *], modelRange.x[0, *]]), $
        MAX ([dataRange.x[1, *], modelRange.x[1, *]])]

    yRange = [ $
        MIN ([dataRange.y[0, *], modelRange.y[0, *]]), $
        MAX ([dataRange.y[1, *], modelRange.y[1, *]])]
    IF ROUND(yRange[0] * 1.0E+10) EQ 0 THEN yRange[0] = MIN (dataRange.y[0, *])

    yRange = [nicenumber(yRange[0], /FLOOR), nicenumber(yRange[1], /CEIL)]
        
    self->adjustRanges, xRange, yRange, 'nufnu', XLOG = xLog, YLOG = yLog ;, /XLOG, /YLOG
    
    result = model.nuFnuData
    FOR idx = 0, model.numDet - 1 DO BEGIN 
        lastIdx = 6 * model.numchan[idx] - 1 
        result[0: model.numchan[idx] - 1, idx] = interpol(model.nufnuModel[0: lastIdx, idx],$
                  model.modelEnergy[0: lastIdx, idx],$
                  model.chanEnergy[0: model.numchan[idx] - 1, idx]) 
        ENDFOR
    
    residTitle = ' '
    
    IF (residChoice EQ 1L) THEN BEGIN
       residRange  = self->dataRange (model.numDet, model.fitChannels, $
           model.chanEnergy, model.chanWidth, model.nuFnuData - result, $
           YERR = model.nuFnuSig, model.livetime, /RESID)
    ENDIF ELSE BEGIN
       sigArr = netCsig
       sigArr[*] = 1.   
       residRange  = self->dataRange (model.numDet, model.fitChannels, $
           model.chanEnergy, model.chanWidth, (netCrate - modelCntRate) / $
           (SQRT(model.modelCRVari) > 1.E-15), YERR = sigArr, model.livetime, /RESID)
       residTitle = 'Sigma'
    ENDELSE
        
    yExtreme = MAX (ABS(residRange.y))
    yResidRange = [-yExtreme, yExtreme]
    
    detNames = self.DetectorList->names ()

    IF (residChoice NE 2L) THEN BEGIN
	    FOR i = 0, model.numDet - 1 DO BEGIN
	
	        idx = i
	        IF (NOT allDets) THEN idx = singleDet
	
	        lo = model.fitChannels[0, idx]
	        hi = model.fitChannels[1, idx]
	
	        xTitle = 'Energy (keV)'
	        yTitle = ' '
	
            ;== Set up for combining the energy bins for display purposes
            nChan = model.numchan[idx] - 1
            detData = self.DetectorList->data (detNames[idx])
            myDisp = detData.display
            eLookup = mydisp.spectrum->energyLookup()
    
	        IF (residChoice EQ 1L) THEN BEGIN
                self->combine, model.nuFnuData[0: nChan, idx], model.nuFnuSig[0: nChan, idx], $
                    result[0: nChan, idx], model.chanEnergy[0: nChan, idx], $
                    model.chanWidth[0: nChan, idx], lo, hi, eLookup, $
                    out_spec, out_errs, out_modl, out_energies, out_widths, lo1, hi1
        
	            residRate = out_spec[lo1:hi1] - out_modl[lo1:hi1]
	            residSig  = out_errs[lo1:hi1]
	        ENDIF ELSE BEGIN
                self->combine, netCrate[0: nChan, idx], SQRT(model.modelCRVari[0: nChan, idx]), $;netCsig
                    modelCntRate[0: nChan, idx], model.chanEnergy[0: nChan, idx], $
                    model.chanWidth[0: nChan, idx], lo, hi, eLookup, $
                    out_spec, out_errs, out_modl, out_energies, out_widths, lo1, hi1
                    
	            residRate = (out_spec[lo1:hi1] - out_modl[lo1:hi1]) / $
	                        out_errs[lo1:hi1]
	            residSig  = REPLICATE(1., hi1 - lo1 + 1)
	        ENDELSE
	        
	        colorInd = (idx EQ 0) ? (WHERE (theColors EQ 'SPEC'))[0] : idx
	        
	        self->plotResiduals, OPLOT = (idx NE 0), $
	            out_energies[lo1:hi1], out_widths[lo1:hi1], $
	            residRate, residSig, idx, $
	            XTITLE = xTitle, YTITLE = residTitle, $
	            XRANGE = xRange, $
	            YRANGE = yResidRange, $
	            XMARGIN = [13, 3], $
	            XSTYLE = 1, XLOG = xlog, $
	            FG_COLOR = self.Color->color ('FG'), $
	            BACKGROUND = self.Color->color ('BG'), $
	            COLOR = self.Color->color (theColors[colorInd]), $
	            _EXTRA = [*self.botOptions, 'XRANGE']
	
	         ENDFOR
	    ENDIF

    FOR i = 0, model.numDet - 1 DO BEGIN

        idx = i
        IF (NOT allDets) THEN idx = singleDet
        
        lo = model.fitChannels[0, idx]
        hi = model.fitChannels[1, idx]

        yTitle = 'Nu F!Dnu!N (photons keV cm!U-2!N s!U-1!N)'

        colorInd = (idx EQ 0) ? (WHERE (theColors EQ 'SPEC'))[0] : idx
		myColors = [myColors, self.Color->color (theColors[colorInd])]
        
        ;== Set up for combining the energy bins for display purposes
        nChan = model.numchan[idx] - 1
        detData = self.DetectorList->data (detNames[idx])
        myDisp = detData.display
        eLookup = mydisp.spectrum->energyLookup()
        self->combine, model.nuFnuData[0: nChan, idx], model.nuFnuSig[0: nChan, idx], $
            model.nuFnuData[0: nChan, idx], model.chanEnergy[0: nChan, idx], $
            model.chanWidth[0: nChan, idx], lo, hi, eLookup, $
            out_spec, out_errs, out_modl, out_energies, out_widths, lo1, hi1
    
        self->plotData, residChoice, OPLOT = (idx NE 0), $
            out_energies[lo1:hi1], out_widths[lo1:hi1], $
            out_spec[lo1:hi1], out_errs[lo1:hi1], idx, $ 
            YTITLE = yTitle, $
            XRANGE = xRange, $
            YRANGE = yRange, $
            XSTYLE = 1, YSTYLE = 1, $
            XLOG = xLog, YLOG = yLog, $
            /ARROW, XMARGIN = [13, 3], $
            FG_COLOR = self.Color->color ('FG'), $
	        BACKGROUND = self.Color->color ('BG'), $
            COLOR = self.Color->color (theColors[colorInd]), $
            _EXTRA = [(residChoice EQ 2L ? *self.myOptions: *self.topOptions), $
                      'XRANGE','YRANGE']

        lo = 0
        hi = model.numChan[idx] * 6
        ph_width = ALOG10(model.modelEnergy[lo + 1:hi, idx]) - $
                   ALOG10(model.modelEnergy[lo :hi - 1, idx])
        frac_width = ph_width / ALOG10(model.modelEnergy[lo :hi - 1, idx])
        
        ;== 6/20/03 (YK & RDP) Mask out infinite width bins
        inf_ind = WHERE (FINITE (frac_width, /INFINITY), infCo)
        IF (infCo NE 0) THEN frac_width [inf_ind] = 0.
        ll = MIN (WHERE (frac_width GT 1.25, fracCo))
        IF (fracCo NE 0) THEN hi = ll  ; Get rid of 'wide' energy bins (as with SDs)
        
        OPLOT, model.modelEnergy[lo :hi, idx], model.nufnuModel[lo :hi, idx], $
            COLOR = self.Color->color ('BKGD'), _EXTRA = *self.myOptions

        IF (showModel EQ 0) THEN BEGIN
	
            myDisp = self.display   ; Managed display has link to fitter:

            numAddTerms = myDisp.fitter->getNumAddTerms ()
            term_used = WHERE (model.term_used[1: numAddTerms] EQ 1, n_mod)
	
            IF (n_mod GT 1) THEN BEGIN  ; Number of models is not trivial:
            
                FOR midx = 1, n_mod DO BEGIN
                        
                    mrate = REFORM (model.modelPhotRate[*, midx, *])
        
                    nonzero = WHERE (mrate GT 0, nzc)
                    IF (nzc EQ 0) THEN BEGIN
                        mrate = ABS(mrate)
                    ENDIF
                    
                    OPLOT, model.modelEnergy[lo :hi, idx], $
                        mRate[lo :hi, idx] * (model.modelEnergy[lo :hi, idx])^2., $
                        COLOR = self.Color->color ('BKGD'), $
                        LINESTYLE = 2, _EXTRA = *self.myOptions
        		 
                ENDFOR
            ENDIF
        ENDIF

        IF (doPlotNotes) THEN BEGIN
        
;           detData = self.DetectorList->data (detNames[idx])
;           notes   = self->getNotes (detData)
           self->plotNotes, plotNotes, /NUFNU

        ENDIF

    ENDFOR

    self->annotate, COLOR = self.Color->color ('FG'), $
              BACKGROUND = self.Color->color ('BG'), $
              _EXTRA = *self.myOptions
    self->plotLegend, COLOR = myColors[1:*], /left, _EXTRA = *self.myOptions  ;extra
    self->finishRanges, 'nufnu', XLOG = xLog, YLOG = yLog ; , /XLOG, /YLOG
    
END


; ----------------------------------------------------------------------------
; Compute data extents for binned data.  Returns array of ranges of dimension:
; (2, numDet).
; ----------------------------------------------------------------------------
FUNCTION ModelPlotter::binnedRange, SINGLEDET = singleDet, $
    numDet, fitChan, numChan, chanEnergy, chanWidth, $
    netCrate, netCsig, modelCntRate

    detNames = self.DetectorList->names ()
    
    ;== Better to make sure!
    IF KEYWORD_SET(SINGLEDET) THEN numDet = 1
    
    yBinnedRange  = FLTARR(2, numDet)
    
    FOR i = 0, numDet - 1 DO BEGIN

        idx = i
        IF KEYWORD_SET(SINGLEDET) THEN idx = singleDet
        
        lo = fitChan[0, idx]
        hi = fitChan[1, idx]

        ;== Set up for combining the energy bins for display purposes
        nChan = numchan[idx] - 1
        detData = self.DetectorList->data (detNames[idx])
        myDisp = detData.display
        eLookup = mydisp.spectrum->energyLookup()
        self->combine, netCrate[0: nChan, idx], netCsig[0: nChan, idx], $
            modelCntRate[0: nChan, idx], chanEnergy[0: nChan, idx], $
            chanWidth[0: nChan, idx], lo, hi, eLookup, $
            out_spec, out_errs, out_modl, out_energies, out_widths, lo1, hi1
            
        lowData = out_spec - out_errs
        posIdx = WHERE(lowData GT 0, loco)
        IF loco EQ 0 THEN posIdx = INDGEN(N_ELEMENTS(lowData))
        yBinnedRange[0, i] = MIN(lowData[posIdx] > 1.E-6)
        yBinnedRange[1, i] = MAX(out_spec + out_errs)
        
    ENDFOR    

    RETURN, yBinnedRange   
    
END


; ----------------------------------------------------------------------------
; Compute data extents.  Returns arrays of ranges of dimension (2, numDet).
; ----------------------------------------------------------------------------
FUNCTION ModelPlotter::dataRange, $
    numDet, fitChan, x, xerr, y, mask, YERR = yerr, FINE = fine, RESID = resid
    
    IF (KEYWORD_SET(YERR)) THEN dum = 0 ELSE $
       yerr = y * 0.0

;    IF (N_PARAMS () LT 6) THEN $
;       yerr = y * 0.0
       
    factor  = KEYWORD_SET (fine) ? 6 : 1
    xRange  = FLTARR (2, numDet)
    yRange  = FLTARR (2, numDet)
    
    FOR i = 0, numDet - 1 DO BEGIN

        lo = fitChan[0, i]
        hi = fitChan[1, i]

        tmp = x[lo:hi, i] - xerr[lo:hi, i]
        idx = WHERE (tmp GT 0)
        minv = MIN (tmp[idx])

        tmp = x[lo:hi, i] + xerr[lo:hi, i]
        idx = WHERE (tmp GT 0)
        maxv = MAX (tmp[idx])
        
        xRange[0, i] = minv
        xRange[1, i] = maxv

        lo = lo * factor
        hi = hi * factor

        IF KEYWORD_SET (fine) THEN BEGIN
           tmp = y[lo:hi, i] - yerr[lo:hi, i]
        ENDIF ELSE BEGIN
           tmp_idx = WHERE (mask[*, i] NE 0.)
           tmp = y[tmp_idx, i] - yerr[tmp_idx, i]
           ENDELSE
        ;tmp_idx = WHERE (mask[*, i] NE 0.)
        ;tmp = y[tmp_idx, i] + yerr[tmp_idx, i]
        ;tmp = y[lo:hi, i] + yerr[lo:hi, i]
        IF KEYWORD_SET (resid) THEN minv = MIN (tmp) ELSE BEGIN
           idx = WHERE (tmp GT 0, cntIdx)
           IF (cntIdx EQ 0) THEN minv = MIN (tmp)  $  ; Nothing for it but to fudge...
              ELSE minv = MIN (tmp[idx])
           ENDELSE
                         
        IF KEYWORD_SET (fine) THEN BEGIN
           tmp = y[lo:hi, i] + yerr[lo:hi, i]
        ENDIF ELSE BEGIN
           tmp_idx = WHERE (mask[*, i] NE 0.)
           tmp = y[tmp_idx, i] + yerr[tmp_idx, i]
           ENDELSE
        ;tmp_idx = WHERE (mask[*, i] NE 0.)
        ;tmp = y[tmp_idx, i] + yerr[tmp_idx, i]
        ;tmp = y[lo:hi, i] + yerr[lo:hi, i]
        IF KEYWORD_SET (resid) THEN maxv = MAX (tmp) ELSE BEGIN
           idx = WHERE (tmp GT 0, cntIdx)
           IF (cntIdx EQ 0) THEN maxv = MAX (tmp)  $  ; Nothing for it but to fudge...
              ELSE maxv = MAX (tmp[idx])
           ENDELSE

        yRange[0, i] = minv
        yRange[1, i] = maxv

    ENDFOR

    RETURN, { X : xRange, Y : yRange }   

END


; ----------------------------------------------------------------------------
; Plot data
;
; EXAMPLE: Differential energy spectrum
;  xarr     = energy bin centers
;  xarr_err = energy bin widths
;  yarr     = flux
;  yarr_err = flux error
;
; Upper limits are determined by YARR values which are less than YARR_ERR.
; Set keyword ARROW to use arrows for upper limits, else use vertical bars.
;
; ----------------------------------------------------------------------------
PRO ModelPlotter::plotData, residChoice, $
    xarr, xarr_err, yarr, yarr_err, detNum, $
    OPLOT = oplot, SIGMA = sigma, ARROW = arrow, $
    FG_COLOR = fg_color, COLOR = color, _EXTRA = extra
    
    ;== Symbol to plot    
    plotSym = *self.plotSym
    detSym = plotSym[detNum MOD 7]
    IF (detSym EQ 8) THEN BEGIN
      ; Make a plot symbol
      PI = ACOS(-1.0)
      A  = FINDGEN(16)*(PI*2/16.0)
      USERSYM, COS(A), SIN(A) ;, /FILL
    ENDIF
    
    ;== If YARR(I) < YARR_ERR(I), plot point as an upper limit
                     
    IF (N_ELEMENTS (sigma) EQ 0) THEN $
       sigma = self.upperLimitSigma         

    yarr_save = yarr

    err_hi = FLTARR (N_ELEMENTS (yarr))
    err_lo = FLTARR (N_ELEMENTS (yarr))

    FOR i = 0, N_ELEMENTS (yarr) - 1 DO BEGIN

        IF (yarr[i] LT yarr_err[i]) THEN BEGIN

           ;== Sigma upper limits
           yarr[i]   = sigma * yarr_err[i]
           err_hi[i] = sigma * yarr_err[i]
           err_lo[i] = -1

        ENDIF ELSE BEGIN

           err_hi[i] = yarr[i] + yarr_err[i]
           err_lo[i] = yarr[i] - yarr_err[i]

        ENDELSE

    ENDFOR

    IF (NOT KEYWORD_SET (oplot)) THEN BEGIN


       IF (residChoice NE 2L) THEN BEGIN
           thePos = self.dataPlotPos
           xTickname = REPLICATE (STRING (32b), 30)
           xTitle = ' '

           PLOT, /XLOG, /YLOG, /NODATA, xarr, yarr, $
	           COLOR = fg_color, $
	           XTITLE = xTitle, $
	           XTICKNAME = xTickname, $
	           POSITION = thePos, /NOERASE, _EXTRA = extra

       ENDIF ELSE BEGIN
           thePos = self.noResidPlotPos
           xTitle = 'Energy (keV)'
       
           PLOT, /XLOG, /YLOG, /NODATA, xarr, yarr, $
	           XTITLE = xTitle, $
	           COLOR = fg_color, $
	           POSITION = thePos, _EXTRA = extra
	           
       ENDELSE

    ENDIF

    IF (NOT KEYWORD_SET (arrow)) THEN BEGIN
       idx = WHERE (err_lo EQ -1, cnt)
       IF (cnt NE 0) THEN err_lo[idx] = 10.0^!Y.CRANGE[0]
    ENDIF


    FOR i = 0, N_ELEMENTS (yarr) - 1 DO BEGIN
    
        IF (err_lo[i] EQ -1) THEN BEGIN
        
           c = CONVERT_COORD (xarr[i], yarr[i], /DATA, /TO_NORMAL)
           ;== Clip the arrows to the region of the plot:
           IF (xarr[i] LT 10^!x.crange[1] AND xarr[i] GT 10^!x.crange[0]   AND $
               yarr[i] LT 10^!y.crange[1] AND yarr[i] GT 10^!y.crange[0]) THEN $
               ARROW, c[0], c[1], c[0], c[1] - 0.05, /NORMAL, /SOLID, $
                   HSIZE = !D.X_SIZE / 96.0, COLOR = color
        
        ENDIF ELSE BEGIN
        
           OPLOT, [xarr[i], xarr[i]], [err_lo[i], err_hi[i]], $
               COLOR = color, _EXTRA = extra

           OPLOT, [xarr[i], xarr[i]], [yarr[i], yarr[i]], PSYM = detSym, COLOR = color


        ENDELSE
        
        OPLOT, [xarr[i] - xarr_err[i] / 2.0, xarr[i] + xarr_err[i] / 2.0 ], $
               [yarr[i], yarr[i]], COLOR = color, _EXTRA = extra

    ENDFOR

    yarr = yarr_save

END


; ----------------------------------------------------------------------------
; Plot the legend
; ----------------------------------------------------------------------------
PRO ModelPlotter::plotLegend, SYMSIZE = symsize, _EXTRA = extra

    detNames = self.DetectorList->names ()
    numDet   = self.DetectorList->count ()
    legendList = STRARR (numDet)
    myPsym = INTARR (numDet)
    plotSym = *self.plotSym
    FOR i = 0, numDet - 1 DO BEGIN
        detData = self.DetectorList->data (detNames[i])
        myDisplay = detData.display
        legendList[i] = (myDisplay).reader->getDetName()
        j = i MOD 7
        myPsym[i] = plotSym [j]
        IF (plotSym [j] EQ 8) THEN myPsym[i] = 88
        IF (plotSym [j] EQ 3) THEN myPsym[i] = 1
    ENDFOR
    
    ;== 10/13/09 RDP: Fix as per Elisabetta's bug report; make SYMSIZE the same size as PSYM.
    IF KEYWORD_SET(SYMSIZE) THEN BEGIN
    	sizeList = REPLICATE(symsize, numDet)
    
		legend, legendList, PSYM = myPsym, SYMSIZE = sizeList, BOX = 0, /RIGHT, $
              ;COLOR = self.Color->color ('FG'), $
              _EXTRA = extra
    ENDIF ELSE BEGIN
    
		legend, legendList, PSYM = myPsym, BOX = 0, /RIGHT, $
              ;COLOR = self.Color->color ('FG'), $
              _EXTRA = extra
    ENDELSE
END


; ----------------------------------------------------------------------------
; Plot residuals
; ----------------------------------------------------------------------------
PRO ModelPlotter::plotResiduals, $
    xarr, xarr_err, yarr, yarr_err, detNum, $
    OPLOT = oplot, FG_COLOR = fg_color, COLOR = color, _EXTRA = extra
    
    ;== Symbol to plot                   
    detSym = (*self.plotSym)[detNum MOD 7]
    IF (detSym EQ 8) THEN BEGIN
      ; Make a plot symbol
      PI = ACOS(-1.0)
      A  = FINDGEN(16)*(PI*2/16.0)
      USERSYM, COS(A), SIN(A)   ;, /FILL
    ENDIF
        
    err_hi = yarr + yarr_err
    err_lo = yarr - yarr_err

    yRange = [MIN (err_lo), MAX (err_hi)]
    
    IF (NOT KEYWORD_SET (oplot)) THEN BEGIN

       PLOT, /XLOG, xarr, yarr, $            ;, /NODATA
           YRANGE = yRange, YMINOR = 2,      $
           POSITION = self.residualsPlotPos, $
           XTICKLEN = 0.08, PSYM = detSym,   $
           COLOR = fg_color, _EXTRA = extra  ;, /NOERASE

    ENDIF

    FOR i = 0, N_ELEMENTS (yarr) - 1 DO BEGIN
            
        OPLOT, [xarr[i], xarr[i]], [err_lo[i], err_hi[i]], $
               COLOR = color, _EXTRA = extra

        OPLOT, [xarr[i] - xarr_err[i] / 2.0, xarr[i] + xarr_err[i] / 2.0 ], $
               [yarr[i], yarr[i]], COLOR = color, _EXTRA = extra

    ENDFOR

    OPLOT, 10.0^!X.CRANGE, [0, 0], LINESTYLE = 1, COLOR = self.Color->color ('BKGD'), $
           _EXTRA = extra
    
END


; ----------------------------------------------------------------------------
; Set the current model
; ----------------------------------------------------------------------------
PRO ModelPlotter::annotate, _EXTRA = extra

	;== Get the instrument name
	names = self.DetectorList->names()
	det = self.DetectorList->data (names[0])
	;== Get the fit interval info
	range = det.timeInt
	unit = 's'
    IF (ABS (range[0]) GT 100) THEN $
       r0 = STRTRIM (STRING (range[0], FORMAT = '(D15.2)'), 2) $
    ELSE $
       r0 = STRTRIM (STRING (range[0], FORMAT = '(D15.3)'), 2)

    IF (ABS (range[1]) GT 10000) THEN $
       r1 = STRTRIM (STRING (range[1], FORMAT = '(D15.0)'), 2) $
    ELSE $
    IF (LONG (ABS (range[1])) GT 100) THEN $
       r1 = STRTRIM (STRING (range[1], FORMAT = '(D15.2)'), 2) $
    ELSE $
       r1 = STRTRIM (STRING (range[1], FORMAT = '(D15.3)'), 2)
                  
    label = r0 + ': ' + r1 + ' ' + unit

	ts = STRCOMPRESS (label) 
	self.display->annotate, 'Times: ' + ts, YPOSITION = !Y.WINDOW[1] + 0.01, $
	    /SMALL, _EXTRA = extra

    
END


; ----------------------------------------------------------------------------
; Set the current model
; ----------------------------------------------------------------------------
PRO ModelPlotter::setFitModel, model

    self.FitModel = model

END


; ----------------------------------------------------------------------------
; Set the current photon model
; ----------------------------------------------------------------------------
PRO ModelPlotter::setPhotonModel, model

    self.PhotonModel = model
    
END


; ----------------------------------------------------------------------------
; Update the current list of detectors
; ----------------------------------------------------------------------------
PRO ModelPlotter::setDetectorList, DetectorList

    self.DetectorList = DetectorList
    
END

; ----------------------------------------------------------------------------
; For Chisq plotting: choice of displaying sigmas or percentiles
; ----------------------------------------------------------------------------
PRO ModelPlotter::setDisplaySigma, displaySigma

    self.displaySigma = displaySigma
    
END

; ----------------------------------------------------------------------------
; For Chisq plotting: choice of number of parameters of interest
; ----------------------------------------------------------------------------
PRO ModelPlotter::setNumInterest, numInterest

    self.numInterest = numInterest
    
END

; ----------------------------------------------------------------------------
; Definition of modelPlotter
; ----------------------------------------------------------------------------
PRO modelPlotter__define

    obj = { MODELPLOTTER, INHERITS PLOTTER, $
      
        Color            : OBJ_NEW (),  $   ; Color model information
        FitModel         : OBJ_NEW (),  $   ; Fit results
        PhotonModel      : OBJ_NEW (),  $   ; User photon model
        DetectorList     : OBJ_NEW (),  $   ; List of current detectors       

        plotSym          : PTR_NEW (),  $   ; Plot symbol indices for multiple detectors
        plotSymV         : PTR_NEW (),  $   ; Vector Plot symbols for multiple detectors
        upperLimitSigma  : 0.0,         $              
        displaySigma     : 0L,          $   ; Chisq plots: display sigma or percent
        numInterest      : 0L,          $   ; Chisq plots: number of params of interest
        dataPlotPos      : FLTARR(4),   $
        residualsPlotPos : FLTARR(4),   $
        noResidPlotPos   : FLTARR(4)    $

    }
    
END
