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
FUNCTION BatchPlotter::init, mfitDisplay, SIGMA = sigma    

    ;== Set upper limit sigma value
                
    self.upperLimitSigma = (N_ELEMENTS (sigma) EQ 0) ? 2.0 : FLOAT (sigma) 
    
    ;== There is no choice of plot type when we start out

    self.havePlotChoice   = 0L
    
    ;== Maximum number of histogram bins to start out
    
    self.numHistoBins  =  11L
    
    ;== Plot positions for plotting results
    
    self.defaultPlotPos   = [0.1, 0.1, 0.90, 0.95]
    
    ;== Default selection mode for plotting results
    self.selectMode       = [1L, 0L, 0L, 0L, 0L]

    ;== A single color object is used for the application
            
    self.Color = mfitDisplay.color ;OBJ_SINGLETON ('Color')
    
    ;== Set the display
    
    ok = self->PLOTTER::INIT (mfitDisplay)
             
    RETURN, 1
      
END


; ----------------------------------------------------------------------------
; Destructor
; ----------------------------------------------------------------------------
PRO BatchPlotter::cleanup

    PTR_FREE, self.readFilename
    PTR_FREE, self.labelList
    self->Plotter::cleanup
    
END


; ----------------------------------------------------------------------------
; Create a label of a time or energy selection interval
; ----------------------------------------------------------------------------
FUNCTION BatchPlotter::label, range, UNITS = units

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
FUNCTION BatchPlotter::getNotes, detData

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
; Accessor for the list of photon term parameters.  
; ----------------------------------------------------------------------------
FUNCTION BatchPlotter::getLabelList

    IF (NOT PTR_VALID (self.labelList)) THEN self->setupLabelList
    RETURN, *self.labelList
        
END


; ----------------------------------------------------------------------------
; Accessor for the list of photon term parameters.  
; ----------------------------------------------------------------------------
PRO BatchPlotter::clearLabelList

    PTR_FREE, self.labelList
        
END


; ----------------------------------------------------------------------------
; Create the list of photon term parameters.  
; ----------------------------------------------------------------------------
PRO BatchPlotter::setupLabelList

    IF (NOT self.haveBatch) THEN BEGIN    
        IF PTR_VALID (self.readFileName) THEN BEGIN  ;== Try reading a parameter file

            header2 = headfits (*self.readFileName, EXT = 2)
            throwAway = sxpar (header2, 'TTYPE*', COMMENT = labelList)
            labelList = labelList[WHERE(STRPOS(throwAway, 'PARAM') NE -1)]
            nPars = N_ELEMENTS (labelList)
            ;== Update the labelList to reflect the current fit parameters
            ;IF (PTR_VALID (self.labelList)) THEN PTR_FREE, self.labelList
            ;self.labelList = PTR_NEW (labelList)
        ENDIF
        ;RETURN                              ;== Either way, we skip the rest
    ENDIF ELSE BEGIN
    
		numTerms = self.PhotonModel->count ()
		photonModel = self.PhotonModel->model ()
	
		totalParams = 0
		FOR i = 0, numTerms - 1 DO BEGIN
			
			np  = photonModel[i]->nParams ()                 
			totalParams = totalParams + np
	
		ENDFOR   
	
		labelList = STRARR (totalParams)
	
		totalParams = 0
		FOR i = 0, numTerms - 1 DO BEGIN
			
			nam = photonModel[i]->name ()
			np  = photonModel[i]->nParams ()                 
			p   = photonModel[i]->Params ()
			
			FOR j = 0, np - 1 DO BEGIN
				
				labelList[totalParams + j] = STRTRIM(nam, 2) + ': ' + STRTRIM((p.name)[j], 2)
			ENDFOR
			totalParams = totalParams + np
	
		ENDFOR   
		;== Free the copy of the photon model
		FOR i = 0, numTerms - 1 DO OBJ_DESTROY, photonModel[i]
		
    ENDELSE
    
    labelList = [labelList, 'Photon Flux (ph/s-cm^2)']
    labelList = [labelList, 'Photon Fluence (ph/cm^2)']
    labelList = [labelList, 'Energy Flux (erg/s-cm^2)']
    labelList = [labelList, 'Energy Fluence (erg/cm^2)']
    labelList = [labelList, 'Fit Merit Function / DOF']
    labelList = [labelList, 'Degrees of Freedom']
    ;totalParams = totalParams + 6
    totalParams = N_ELEMENTS (labelList)
    
    ;== Update the labelList to reflect the current fit parameters
    IF (PTR_VALID (self.labelList)) THEN PTR_FREE, self.labelList
    self.labelList = PTR_NEW (labelList)

END


; ----------------------------------------------------------------------------
; Add informational notes to a Postscript page.  
;
; WARNING: This routine uses hardcoded DEVICE coordinates, and is set up 
; for the Postscript device on a Letter size page.
; ----------------------------------------------------------------------------
PRO BatchPlotter::plotNotes, notes

    ; notes = 'Note #' + STRTRIM (SINDGEN (60)+1, 2)

    IF (N_ELEMENTS (notes) GT 34) THEN $
       PRINT, 'WARNING: Too many notes to fit on page.'

    n = N_ELEMENTS (notes)
    start  = 0
    offset = 370
    
    xRangeData = AXISRANGE (/XAXIS)
    yRangeData = AXISRANGE (/YAXIS)
    
    ll  = CONVERT_COORD (xRangeData[0], yRangeData[0], /DATA, /TO_DEVICE)
    lr  = CONVERT_COORD (xRangeData[1], yRangeData[0], /DATA, /TO_DEVICE)
    
    
    FOR i = 0, n - 1 DO BEGIN
        
        align = 0   
        xPos  = ll[0]
        yPos  = start - (i * offset)
        
        IF (yPos LE -6000) THEN BEGIN

           align = 1        
           xPos  = lr[0]
           yPos  = yPos + 6000 + offset

        ENDIF
           
        XYOUTS, xPos, yPos, notes[i], ALIGN = align, /DEVICE

    ENDFOR
    
END


; ----------------------------------------------------------------------------
; The following procedure forwards the event structure to the class handler,
; since one cannot currently (IDL v5.2) define a class method as an
; event handler. 
; ----------------------------------------------------------------------------
PRO BatchPlotter_selectBatchFit_EVENT, event

    WIDGET_CONTROL, event.top, GET_UVALUE = state
    (*state).self->selectBatchFit_event, event

END
; ----------------------------------------------------------------------------
PRO BatchPlotter::selectBatchFit_event, event


    WIDGET_CONTROL, event.top, GET_UVALUE = state
    
    WIDGET_CONTROL, self.textID, GET_VALUE = numHisto
    
    CATCH, Error_status
    IF Error_status NE 0 THEN BEGIN
        numHisto = 11
    ENDIF
    self.numHistoBins = LONG(numHisto)
    IF self.numHistoBins EQ 0 THEN MESSAGE, 'Values must be integers!'
    
    CASE (TAG_NAMES (event, /STRUCTURE)) OF

        'WIDGET_BUTTON': BEGIN

            WIDGET_CONTROL, event.id, GET_VALUE = value
            
            CASE (value) OF
            
                'Cancel': BEGIN

                    self.cancel = 1
                    WIDGET_CONTROL, event.top, /DESTROY
                    PTR_FREE, state
                    END 
            
                'Accept': BEGIN

                    selected = WIDGET_INFO (self.termListID, /LIST_SELECT)

                    IF (selected[0] EQ -1 ) THEN BEGIN
                        message = 'Choose at least one fit parameter'
                        self.display->setStatus, message, 5, /REVERT
                        RETURN
                    ENDIF
                        
                    nSelect = N_ELEMENTS (selected)
                    IF (nSelect GT 2) THEN BEGIN
                        message = 'Choose at most two fit parameters'
                        self.display->setStatus, message, 5, /REVERT
                        RETURN
                    ENDIF
                    
                    WIDGET_CONTROL, self.chooseModeID, GET_VALUE = mode
                    self.havePlotChoice = 1L     ;== User's choice is valid
                    
                    ;== Update the mode to reflect the current plot mode
                    self.selectMode = mode
                                        
                    WIDGET_CONTROL, event.top, /DESTROY
                    PTR_FREE, state
                    END 
                  
;                'One parameter vs. another': BEGIN
;                    WIDGET_CONTROL, self.chooseModeID, GET_VALUE = mode
;                    mode[0] = 0
;                    mode[2] = 0
;                    WIDGET_CONTROL, self.chooseModeID, SET_UVALUE = mode
;                    END
      
                ELSE: 
            
            ENDCASE 
                        
            END                ;== Widget button
            
        'WIDGET_LIST': BEGIN                    ;== Trap the individual choices
             
            IF (self.firstSelected EQ 999L) THEN BEGIN 
                ;== Determine which parameter was chosen first
                self.firstSelected = event.index
                
                ;== Check that one of the single parameter modes is set
                WIDGET_CONTROL, self.chooseModeID, GET_VALUE = mode
                
                IF (mode[0] OR mode[2]) THEN BEGIN
                    self.selectMode = mode
                    self.havePlotChoice = 1L     ;== User's choice is valid

                    WIDGET_CONTROL, event.top, /DESTROY
                    PTR_FREE, state
                    
                ENDIF
            ENDIF ELSE BEGIN
                ;== First, trap accidental double-clicks
                IF ((event.index EQ self.selection) OR $
                    (event.index EQ self.firstSelected)) THEN RETURN
                    
                self.selection = event.index
                ;== Update the mode to reflect the current plot mode
                WIDGET_CONTROL, self.chooseModeID, GET_VALUE = mode
                self.selectMode = mode
                self.havePlotChoice = 1L     ;== User's choice is valid

                WIDGET_CONTROL, event.top, /DESTROY
                PTR_FREE, state

                ENDELSE
                        
            END                ;== Widget list
            
;        'WIDGET_TEXT': BEGIN                    ;== Read the choice
;        
;               WIDGET_CONTROL, self.textID, GET_VALUE = self.numHistoBins
;               
;            END
        
        ELSE: BEGIN

            ;== Handle the radio buttons; name is *not* returned in 'value'
            WIDGET_CONTROL, event.id, GET_VALUE = value

            CASE (WHERE(value NE self.selectMode))[0] OF

                0  : BEGIN
                        WIDGET_CONTROL, self.chooseModeID, GET_VALUE = mode
                        mode[1] = 0
                        mode[2] = 0
                        self.selectMode = mode
                        WIDGET_CONTROL, self.chooseModeID, SET_VALUE = mode
                    END
                1  : BEGIN
                        WIDGET_CONTROL, self.chooseModeID, GET_VALUE = mode
                        mode[0] = 0
                        mode[2] = 0
                        self.selectMode = mode
                        WIDGET_CONTROL, self.chooseModeID, SET_VALUE = mode
                    END
                2  : BEGIN
                        WIDGET_CONTROL, self.chooseModeID, GET_VALUE = mode
                        mode[0] = 0
                        mode[1] = 0
                        self.selectMode = mode
                        WIDGET_CONTROL, self.chooseModeID, SET_VALUE = mode
                    END
                ELSE:

            ENDCASE
            
        ENDELSE
                            
    ENDCASE
    
END


; ----------------------------------------------------------------------------
; Determine which type of batch fit result to plot.  
; ----------------------------------------------------------------------------
PRO BatchPlotter::selectBatchFit, $
    HARDCOPY = hardcopy, OPLOT = oplot, $
    _REF_EXTRA = extra

    IF ((NOT self.haveBatch) AND (NOT PTR_VALID (self.readFilename))) THEN RETURN
    
    self.cancel = 0
    myDisplay = self.display
    parent = myDisplay.widgetID.top
    topBase = WIDGET_BASE (TITLE = 'Batch Fit Plotter', $
              /COLUMN, /BASE_ALIGN_CENTER, /FLOATING, GROUP_LEADER = parent)

    label = WIDGET_LABEL (topBase, VALUE = 'Select one or two time')
    label = WIDGET_LABEL (topBase, VALUE = 'series of fit parameters')

    ;self->setupLabelList
    labelList = self->getLabelList() ; *self.labelList

    totalParams = N_ELEMENTS (labelList)

    self.termListID = WIDGET_LIST (topBase, VALUE = labelList, $
         YSIZE = MIN ([totalParams + 1, 10]), /MULTIPLE)
    
    choice = ['Overplot Flux history?', 'One parameter vs. another', $
              'Plot histogram (1 only)', 'Log X Axis', 'Log Y Axis']
    self.chooseModeID = CW_BGROUP (topBase, choice, /COLUMN, $
        /NONEXCLUSIVE, SET_VALUE = self.selectMode)
        
    label = WIDGET_LABEL (topBase, VALUE = 'Number of Histogram Bins:')
    self.textID = WIDGET_TEXT (topBase, $
                  VALUE = STRTRIM(STRING(self.numHistoBins), 2), $
                  /EDIT, XSIZE = 10)

    gridBase = WIDGET_BASE (topBase, /GRID, COLUMN = 2, SPACE = 20)    
    button = WIDGET_BUTTON (gridBase, VALUE = 'Accept')
    button = WIDGET_BUTTON (gridBase, VALUE = 'Cancel')

    ;== Store the object reference on the display widget
    
    state = PTR_NEW ({ self: self })
    WIDGET_CONTROL, topBase, SET_UVALUE = state
    WIDGET_CONTROL, topBase, /REALIZE
    
    ;== Start event loop
    
    self.firstSelected = 999L
    self.selection = 999L
    
    XMANAGER, OBJ_CLASS (self) + '_selectBatchFit', topBase ; , /NO_BLOCK
    
    ;== Possibly making a new kind of parameter plot; get rid of the zoom defaults
    IF (self.rangeList->inList ('paramPlot')) THEN BEGIN
        self.rangeList->delete, 'paramPlot'
    ENDIF
    
    IF NOT self.cancel THEN self->plotBatchFit, _EXTRA = extra

END


; ----------------------------------------------------------------------------
; Plot the batch fit results. One of the modes must be chosen first, else the 
; fit residual time history will be plotted.
; Set singleDet to a detector index to plot a single detector 
; (D = all detectors).
; ----------------------------------------------------------------------------
PRO BatchPlotter::plotBatchFit, $
    HARDCOPY = hardcopy, OPLOT = oplot, RESID = resid, DURATION = duration, $
    _REF_EXTRA = extra 
    
    IF ((NOT self.haveBatch) AND (NOT PTR_VALID (self.readFilename))) THEN RETURN
    
    ;self->setupLabelList
    labelList = self->getLabelList() ; *self.labelList
    myPlot = 'paramPlot'
    
    IF KEYWORD_SET (RESID) THEN BEGIN ;== Set up for residuals history
        self.havePlotChoice = 1L   
          
        ;== Default selection mode is to overplot photon flux history
        self.selectMode  = [1L, 0L, 0L, 0L, 0L]
        nLabels = N_ELEMENTS (labelList)
        self.firstSelected = LONG (nLabels - 2) ;== Fit residuals
        self.selection = 999L
        
        ;== The current plot range info is incorrect
        IF (self.rangeList->inList ('paramPlot')) THEN self.rangeList->delete, 'paramPlot'

    ENDIF

    IF KEYWORD_SET (DURATION) THEN BEGIN ;== Set up for duration calculation:
        self.havePlotChoice = 1L   
          
        ;== Override the default selection mode to overplot photon flux history
        self.selectMode  = [0L, 0L, 0L, 0L, 0L]
        nLabels = N_ELEMENTS (labelList)
        self.firstSelected = LONG (nLabels + 1) ;== Photon Fluence history over duration interval
        labelList = [labelList, labelList[nLabels - 5], labelList[nLabels - 5]]
        self.selection = 999L

        ;== We start out trying to get a background for duration calcluation
        ;self.skipDurBack = 0L

        ;== The current plot range info is incorrect
        ;IF (self.rangeList->inList ('durPlot')) THEN self.rangeList->delete, 'durPlot'
        myPlot = 'durPlot'

    ENDIF

    IF (NOT self.havePlotChoice) THEN BEGIN
        self->selectBatchFit
        RETURN
    ENDIF
    
    theColors = self.Color->model(/NAMES)

    ;== Obtain the fit parameters to plot
    paramInd = self.firstSelected
    nSelect = 1
    IF (paramInd EQ 999L) THEN BEGIN
        self.havePlotChoice = 0L
        RETURN
    ENDIF

    secondSelection = self.selection
    IF (secondSelection NE 999L) THEN BEGIN
        nSelect = 2
        paramInd = [paramInd, secondSelection]
    ENDIF
    
    mode = self.selectMode
    xLog = mode[3]
    yLog = mode[4]
    
    IF (nSelect EQ 1) THEN BEGIN

        IF (mode[2])  THEN BEGIN     ;== Plot histogram of one parameter
            result = self->getBatchParams(paramInd, times, params, perrs)
            
            ;== Trap selection of 'Fixed' parameters
            uniqElements = UNIQ (params, SORT (params))
            nUniq = N_ELEMENTS (uniqElements)
            IF (nUniq EQ 1) THEN BEGIN  ;== This parameter is fixed
                message = 'Selected Parameter Is possibly Fixed. Choose another.'
                self.display->setStatus, message, 5, /REVERT
                RETURN
            ENDIF
            
            ;== Make a first guess at the binning
            nZeroInd = WHERE (perrs GT 0.0, count)
            IF (count EQ 0) THEN BEGIN  ;== Handle differently
                nBins = MIN ([self.numHistoBins, nUniq / 2])
            ENDIF ELSE BEGIN
                nBins = MIN ([self.numHistoBins, nUniq / 2])
            ENDELSE
            
            IF xLog THEN BEGIN
                negIdx = WHERE (params[0, *] LT 0.0, negCount)
                IF (negCount GT 0) THEN $
                    xLog = 0L $
                ELSE $
                    params[0, *] = ALOG10 (params[0, *])
            ENDIF 
            BIN_DATA, params[0, *], nBins, binCent, binEdge, histBins
            IF xLog THEN BEGIN
                binCent = 10.^binCent
                binEdge = 10.^binEdge
            ENDIF
            
            xRange = [MIN (binEdge), MAX (binEdge)]
            yRange = [0.0, MAX (histBins)]
            xtitle = labelList[paramInd[0]]
            ytitle = 'Number of Occurances'
            thePos = self.defaultPlotPos
            self->adjustRanges, xRange, yRange, myPlot 
            
            PLOT, binCent, histBins, /NODATA,            $
                COLOR = self.Color->color ('FG'),        $
                BACKGROUND = self.Color->color ('BG'),   $
                XRANGE = xrange, XTITLE = xtitle,        $
                XSTYLE  = 1, XLOG = xlog,                $
                YRANGE = yRange, YTITLE = ytitle,        $
                POSITION = thePos, _EXTRA = [*self.myOptions, 'XRANGE', 'YRANGE']
                
            MAKE_HISTOGRAM, binEdge, histBins,      $
                COLOR = self.Color->color ('SPEC')
            
        ENDIF ELSE BEGIN

        IF (mode[0])  THEN BEGIN ;== User wants overplot of Photon flux history
        
            result = self->getBatchParams(paramInd, times, params, perrs, /PFLUX)
            xrange = [nicenumber (MIN (times[0, *]), /FLOOR), nicenumber (MAX (times[1, *]), /CEIL)]
            yFluxRange = [MIN (params[1, *]), MAX (params[1, *])]
            xtitle = 'Seconds since Burst Trigger'
            timeCenter = (times[1, *] + times[0, *]) / 2.
            timeWidth = (times[1, *] - times[0, *])

            IF yLog THEN BEGIN
                negIdx = WHERE (params[0, *] LT 0.0, negCount)
                IF (negCount GT 0) THEN $
                    yLog = 0L 
            ENDIF 
            yrange = [nicenumber (MIN (params[0, *] - perrs[0, *] / 2.), /FLOOR), $
                      nicenumber (MAX (params[0, *] + perrs[0, *] / 2.), /CEIL)]
            
            self->adjustRanges, xRange, yRange, myPlot 

            plot, timeCenter, params[1, *],                $
                  COLOR = self.Color->color ('FG'),        $
                  BACKGROUND = self.Color->color ('BG'),   $
                  POSITION = self.defaultPlotPos,          $
                  XTITLE = xtitle, XRANGE = xrange,        $
                  YRANGE = yFluxRange,                     $
                  YSTYLE = 16 + 4, /NODATA, _EXTRA = [*self.leftOptions, 'XRANGE'] ;, $  , 'YSTYLE'
                  ;'XSTYLE']
                  
            self->drawHistory, times, params[1, *], perrs[1, *], $
                  COLOR = self.Color->color ('BKGD'),            $
                  LINESTYLE = 1, _EXTRA = extra
                  
            AXIS, YAXIS = 1, YTITLE = labelList[paramInd[1]], YRANGE = yFluxRange, $
                  COLOR = self.Color->color ('FG'),        $
                  _EXTRA = [*self.leftOptions, 'XRANGE'], /YNOZERO  ;, 'XSTYLE', 'YSTYLE'

            ytitle = labelList[paramInd[0]]

            self->plotData, timeCenter, timeWidth, params[0, *], perrs[0, *], $
                  FG_COLOR = self.Color->color ('FG'),     $
                  COLOR = self.Color->color ('SPEC'),      $
                  XRANGE = xrange,                         $
                  YTITLE = ytitle, YRANGE = yrange,        $
                  YLOG = yLog,                             $
                  YSTYLE = 16 + 8 + 1, /NOERASE, _EXTRA = [*self.rightOptions, 'XRANGE', 'YRANGE'] ;, $  
                  ;'XSTYLE', 'YSTYLE']
       
        ENDIF ELSE BEGIN         ;== Only a single parameter time history

            result = self->getBatchParams(paramInd, times, params, perrs)
            xrange = [MIN (times[0, *]), MAX (times[1, *])]

            IF yLog THEN BEGIN
                negIdx = WHERE (params[0, *] LT 0.0, negCount)
                IF (negCount GT 0) THEN $
                    yLog = 0L 
            ENDIF 
;			IF KEYWORD_SET (DURATION) THEN BEGIN ;== Set up for duration calculation: 
;				yrange = [MIN (params - perrs / 2.), $ 
;						  MAX (params + perrs / 2.)] 
;            ENDIF ELSE BEGIN 
				yrange = [MIN (params), MAX (params)]
;            ENDELSE
            self->adjustRanges, xRange, yRange, myPlot ; xLog, yLog,
            xtitle = 'Seconds since Burst Trigger'
            ytitle = labelList[paramInd[0]]
            
            timeCenter = (times[1, *] + times[0, *]) / 2.
            timeWidth = (times[1, *] - times[0, *])
            self->plotData, timeCenter, timeWidth, params, perrs, $
                  XTITLE = xtitle, XRANGE = xrange,        $
                  YTITLE = ytitle, YRANGE = yrange,        $
                  YLOG = yLog, YSTYLE = 16,                $
                  FG_COLOR = self.Color->color ('FG'),     $
                  BACKGROUND = self.Color->color ('BG'),   $
                  COLOR = self.Color->color ('SPEC'), _EXTRA = extra

        ENDELSE
        
        ENDELSE
    ENDIF ELSE BEGIN   ;nSelect = 1

        IF (mode[1])  THEN BEGIN     ;== Plot one parameter vs the other
            result = self->getBatchParams(paramInd, times, params, perrs)
    
            IF xLog THEN BEGIN
                negIdx = WHERE (params[0, *] LT 0.0, negCount)
                IF (negCount GT 0) THEN $
                    xLog = 0L 
            ENDIF 
            xrange = [MIN (params[0, *] - perrs[0, *] / 2.), $
                      MAX (params[0, *] + perrs[0, *] / 2.)]
            
            IF yLog THEN BEGIN
                negIdx = WHERE (params[1, *] LT 0.0, negCount)
                IF (negCount GT 0) THEN $
                    yLog = 0L 
            ENDIF 
            yrange = [MIN (params[1, *] - perrs[1, *] / 2.), $
                      MAX (params[1, *] + perrs[1, *] / 2.)]
            self->adjustRanges, xRange, yRange, myPlot ; xLog, yLog,
            
            xtitle = labelList[paramInd[0]]
            ytitle = labelList[paramInd[1]]
            self->plotData, params[0, *], perrs[0, *], params[1, *], perrs[1, *], $
                      XTITLE = xtitle, XRANGE = xrange,        $
                      XLOG = xLog,                             $
                      XSTYLE = 1,                              $
                      YTITLE = ytitle, YRANGE = yrange,        $
                      YLOG = yLog, YSTYLE = 16,                $
                      FG_COLOR = self.Color->color ('FG'),     $
                      BACKGROUND = self.Color->color ('BG'),   $
                      COLOR = self.Color->color ('SPEC'), _EXTRA = extra
    
        ENDIF ELSE BEGIN             ;== Plot two time histories
                                     
			result = self->getBatchParams(paramInd, times, params, perrs)
			;xrange = [MIN (times[0, *]), MAX (times[1, *])]
			xrange = [nicenumber (MIN (times[0, *]), /FLOOR), nicenumber (MAX (times[1, *]), /CEIL)]
			yFluxRange = [MIN (params[0, *] - perrs[0, *] / 2.), $
						  MAX (params[0, *] + perrs[0, *] / 2.)]

            IF yLog THEN BEGIN
                negIdx = WHERE (params[1, *] LT 0.0, negCount)
                IF (negCount GT 0) THEN $
                    yLog = 0L 
            ENDIF 
                ;yrange = [MIN (params[1, *] - perrs[1, *] / 2.), $
        ;              MAX (params[1, *] + perrs[1, *] / 2.)]
                yrange = [nicenumber (MIN (params[1, *]), /FLOOR), $
                          nicenumber (MAX (params[1, *]), /CEIL)]
;                yrange = [nicenumber (MIN (params[1, *] - perrs[1, *] / 2.), /FLOOR), $ 
;                          nicenumber (MAX (params[1, *] + perrs[1, *] / 2.), /CEIL)] 
                self->adjustRanges, xRange, yRange, myPlot ; xLog, yLog,
            
                xtitle = 'Seconds since Burst Trigger'
                timeCenter = (times[1, *] + times[0, *]) / 2.
                timeWidth = (times[1, *] - times[0, *])
                self->plotData, timeCenter, timeWidth, params[0, *], perrs[0, *], $
                      FG_COLOR = self.Color->color ('FG'),     $
                      BACKGROUND = self.Color->color ('BG'),   $
                      COLOR = self.Color->color ('BKGD'),      $
                      XTITLE = xtitle, XRANGE = xrange,        $
                      YRANGE = yFluxRange,                     $
                      YSTYLE = 16 + 4, LINESTYLE = 1, _EXTRA = [*self.myOptions, 'XRANGE']
                  
                ytitle = labelList[paramInd[0]]
                AXIS, YAXIS = 1, YTITLE = ytitle, YRANGE = yFluxRange, $
                      _EXTRA = *self.myOptions, /YNOZERO
        
                ytitle = labelList[paramInd[1]]
                self->plotData, timeCenter, timeWidth, params[1, *], perrs[1, *], $
                      FG_COLOR = self.Color->color ('FG'),     $
                      COLOR = self.Color->color ('SPEC'),      $
                      XRANGE = xrange,                         $
                      YTITLE = ytitle, YRANGE = yrange,        $
                      YSTYLE = 16 + 8 + 1, YLOG = yLog,        $
                      /NOERASE, _EXTRA = [*self.myOptions, 'XRANGE', 'YRANGE']
                                              
        ENDELSE
    ENDELSE
    
    self->finishRanges, myPlot, XLOG = xLog, YLOG = yLog

END


;------------------------------------------------------------------------------
; Draw a curve, skipping over data gaps
;------------------------------------------------------------------------------
PRO BatchPlotter::drawHistory, x, y, dy, TOLERANCE = tolerance, _EXTRA = extra

    haveDY = (N_ELEMENTS (dy) NE 0)

;	find = WHERE (FINITE (y))
;	x = x[*,find]
;	y = y[find]
;	IF (haveDY) THEN dy = dy[find]
	
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

          gap_tol = MAX ([5.0E-5, 0.001 * MIN (ABS (x[1, *] - x[0, *])) ])

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
       
          x_range = 0.005 * (!X.CRANGE[1] - !X.CRANGE[0])
          dx = x[1, *] - x[0, *]
       
          FOR i = 0L, n[2] - 1 DO BEGIN
         
              IF (dx[i] GT x_range) THEN BEGIN
         
                 xx = 0.5 * (x[0, i] + x[1, i])
                 xx = [xx, xx]
                 yy = y[i] + [-dy[i], dy[i]]
                 OPLOT, xx, yy, _EXTRA = extra 
         
              ENDIF
         
          ENDFOR

       ENDIF
    
    ENDELSE

END


; ----------------------------------------------------------------------------
; Plot a Geoff Plot
; ----------------------------------------------------------------------------
PRO BatchPlotter::plotStack, HARDCOPY = hardcopy, _EXTRA = extra

    IF (NOT self.haveBatch) THEN BEGIN      ;== Try reading a parameter file
        IF PTR_VALID (self.readFileName) THEN BEGIN
            data1 = MRDFITS (*self.readFIleName, 1, headr1, /SILENT)
            numDets = sxpar (headr1, 'NAXIS2')
            checkSize = sxpar (headr1, 'TFIELDS')
            IF ((checkSize EQ 3) OR (checkSize EQ 6)) THEN BEGIN
                self.display->setStatus, $
                    'This parameter file has no model data.', 5, /REVERT
                RETURN      ;== Small parameter file, no model data
            ENDIF

            data2 = MRDFITS (*self.readFIleName, 2, headr2, /SILENT)
            numFit = sxpar (headr2, 'NAXIS2')
        
            ;== Try getting the Effective Area correction, if included
            mylabels = self->getlabellist()
            EffIDX = WHERE (STRPOS (mylabels, 'Eff.') NE -1, co)
            IF (co NE 0) THEN BEGIN
                ok = self->getBatchParams (EffIDX, EffTi, EffArea, EffErrs)
                IF NOT ok THEN RETURN
                effArea = TRANSPOSE (effArea)
            ENDIF ELSE BEGIN
                EffArea = FLTARR (numFit, numDets) > 1.
            ENDELSE
            
            nChan = data1.channum
            maxChan = N_ELEMENTS (data1[0].e_edges)
            photTimes = [(data2.timebin)[0, *], (data2.timebin)[1, *] ]
            
            ;== Strip out the last (overflow) channel in the data
            tempEdges = ((data1[*]).e_edges[1: maxChan - 1] + $
                         (data1[*]).e_edges[0: maxChan - 2, *]) / 2.
            
            ;== We have overlapping datasets with several detectors included;
            ;== Put everything in order of ascending energy:
            numPhtFit = reorderData (numDets, nChan, tempEdges, data1.phtModl, $
                        data1.fitChan, EffArea, phtEdges, phtArr)

            IF (numPhtFit NE numFit) THEN RETURN ;== Something is badly wrong!
            
            photEnergy = phtEdges 
            numPhotEbins = N_ELEMENTS (photEnergy)
            photSpectr = FLTARR (numFit, numPhotEbins)
            FOR i = 0, numFit - 1 DO  BEGIN
                photSpectr[i, *] = phtArr[0: numPhotEbins - 1, i]
            ENDFOR
	    
        ENDIF ELSE RETURN    ;== No Batch fit and no results file: bail
    ENDIF ELSE BEGIN

        numFit = self.BatchFitList->count ()
        batNames  = self.BatchFitList->names ()
        fit = self.BatchFitList->data (batNames[0])
        photEnergy = *fit.modelEnergy
        kk = WHERE (photEnergy, numPhotEbins)
        photEnergy = photEnergy[kk]
        photSpectr = FLTARR (numfit, N_ELEMENTS (kk))
        photModel = *fit.modelphotrate
        photSpectr[0, *] = photModel[kk]
        photTimes = FLTARR (2, numfit)
        photTimes[*, 0] = (*fit.timeint)[*,0]
	
        FOR i = 1, numfit - 1 DO BEGIN
            fit = self.BatchFitList->data (batNames[i])
            photModel = *fit.modelphotrate
            photSpectr[i, *] = photModel[kk]
            photTimes[*, i] = (*fit.timeint)[*,0]
        END
	
    ENDELSE
    	
    ; Make a color ramp and add it to the color model
    theColors = self.Color->model() ;/NAMES)
    theNames = theColors.name
    nColors = N_ELEMENTS (theNames)
    steps = (numfit LT (255 - nColors)) ? numfit : numfit - nColors
    redVector = [(thecolors.value[0,0])[*], REPLICATE(255, steps)]
    blueVector = [(thecolors.value[0,2])[*], REPLICATE(0, steps)]
    scaleFactor = FINDGEN(steps) / (steps - 1)
    beginNum = 255
    endNum = 0  		  
    greenVector = [(thecolors.value[0,1])[*], beginNum + $
                  (endNum - beginNum) * scaleFactor]
    names = [theNames, 'yel' + STRTRIM (STRING( INDGEN(steps)), 2)]
    self.color->setmodel, names, redvector, greenvector, bluevector
		
    ytitle = 'Flux (photons cm!U-2!N s!U-1!N keV!U-1!N)'
    xLog = 1
    yLog = 1
    
    stacktype = 'photstack'
    
    IF (self.stackChoice EQ 1) THEN BEGIN
        specsize = (SIZE(photSpectr))[1]
        FOR j = 0, specsize - 1 DO BEGIN
            photSpectr[j, *] = photSpectr[j, *] * photEnergy
        ENDFOR
        
        ytitle = 'Energy Flux (photons cm!U-2!N s!U-1!N)'
        stacktype = 'enrgstack'
    ENDIF
    
    IF (self.stackChoice EQ 2) THEN BEGIN
        specsize = (SIZE(photSpectr))[1]
        eSquared = photEnergy^2.
        FOR j = 0, specsize - 1 DO BEGIN
            photSpectr[j, *] = photSpectr[j, *] * eSquared
        ENDFOR
        
        ytitle = 'Nu F nu (keV photons cm!U-2!N s!U-1!N)'
        stacktype = 'nufnstack'
    ENDIF
    
    
    xr = [MIN (photEnergy), MAX (photEnergy)]
    yr = [(yLog EQ 1) ? MIN (photSpectr > 1.E-6) : MIN (photSpectr), $
          MAX (photSpectr[where(FINITE(photspectr))])]
	
    self->adjustRanges, xr, yr, stacktype ; xLog, yLog,
    
    PLOT, xr, yr, XLOG = xLog, YLOG = yLog, /NODATA, XSTYLE = 1, YSTYLE = 1, $
          BACKGROUND = self.Color->color ('BG'), COLOR = self.color->color('FG'), $
          XTITLE = 'Energy (keV)', YTITLE = ytitle, _EXTRA = extra
    
    FOR i = 0, numfit -1 DO BEGIN
        OPLOT, photEnergy, photSpectr[i, *], $
               COLOR = self.Color->color('yel' + STRTRIM (STRING(i), 2)), $
               _EXTRA = extra
    ENDFOR
    
    delTimes = photTimes[1, *] - photTimes[0, *]
    
    aveSpectr = photSpectr ## delTimes
    aveSpectr = REFORM(aveSpectr) / TOTAL(delTimes)
    OPLOT, photEnergy, aveSpectr, COLOR = self.Color->color('BKGD'), $
               LINESTYLE = 2, _EXTRA = extra

    self->finishRanges, stacktype, XLOG = xLog, YLOG = yLog
    
    ; Restore the old color model!
    self.color->setmodel, theNames, (thecolors.value[0,0])[*], $
                (thecolors.value[0,1])[*], (thecolors.value[0,2])[*]

END


; ----------------------------------------------------------------------------
; Plot Nu-F-Nu topological contour map
; ----------------------------------------------------------------------------
PRO BatchPlotter::plotTopoMap, $
    HARDCOPY = hardcopy, _EXTRA = extra

    theColors = self.Color->model(/NAMES)

    IF (NOT self.haveBatch) THEN BEGIN      ;== Try reading a parameter file
        IF PTR_VALID (self.readFileName) THEN BEGIN
            data1 = MRDFITS (*self.readFIleName, 1, headr1, /SILENT)
            numDets = sxpar (headr1, 'NAXIS2')
            checkSize = sxpar (headr1, 'TFIELDS')
            IF ((checkSize EQ 3) OR (checkSize EQ 6)) THEN BEGIN
                self.display->setStatus, $
                    'This parameter file has no model data.', 5, /REVERT
                RETURN      ;== Small parameter file, no model data
            ENDIF

            data2 = MRDFITS (*self.readFIleName, 2, headr2, /SILENT)
            numFit = sxpar (headr2, 'NAXIS2')
            times = ((data2.timebin)[1, *] + (data2.timebin)[0, *]) / 2.
        
            ;== Try getting the Effective Area correction, if included
            mylabels = self->getlabellist()
            EffIDX = WHERE (STRPOS (mylabels, 'Eff.') NE -1, co)
            IF (co NE 0) THEN BEGIN
                ok = self->getBatchParams (EffIDX, EffTi, EffArea, EffErrs)
                IF NOT ok THEN RETURN
                effArea = TRANSPOSE (effArea)
            ENDIF ELSE BEGIN
                EffArea = FLTARR (numFit, numDets) > 1.
            ENDELSE
            
            nChan = data1.channum
            maxChan = N_ELEMENTS (data1[0].e_edges)
            
            ;== Strip out the last (overflow) channel in the data
            tempEdges = ((data1[*]).e_edges[1: maxChan - 1] + $
                         (data1[*]).e_edges[0: maxChan - 2, *]) / 2.
            
            ;== We have overlapping datasets with several detectors included;
            ;== Put everything in order of ascending energy:
            numPhtFit = reorderData (numDets, nChan, tempEdges, data1.phtModl, $
                        data1.fitChan, EffArea, phtEdges, phtArr)

            IF (numPhtFit NE numFit) THEN RETURN ;== Something is badly wrong!
            
            photEnergy = phtEdges 
            numPhotEbins = N_ELEMENTS (photEnergy)
            nuFnuContours = FLTARR (numPhotEbins, numFit)
            maxNuFnu  = FLTARR (numFit)
            FOR i = 0, numFit - 1 DO  BEGIN
                nuFnuContours[*, i] = phtArr[0: numPhotEbins - 1, i] * photEnergy^2.
                tempMaxNu = MAX (nuFnuContours[*, i], maxSub)
                maxNuFnu[i] = photEnergy[maxSub]
            ENDFOR
        ENDIF ELSE RETURN    ;== No Batch fit and no results file: bail
    ENDIF ELSE BEGIN

        numFit = self.BatchFitList->count ()
        batNames  = self.BatchFitList->names ()
        fit = self.BatchFitList->data (batNames[0])
        
        times = FLTARR (numFit)
        
        photEnergy = *fit.modelEnergy
        kk = WHERE (photEnergy, numPhotEbins)
        photEnergy = photEnergy[kk]
        nuFnuContours = FLTARR (numPhotEbins, numFit)
        maxNuFnu  = FLTARR (numFit)
        
        FOR i = 0, numFit - 1 DO BEGIN
       
            fit = self.BatchFitList->data (batNames[i])
            ;== Assume time intervals all same over fitted detectors
            times[i] = ((*fit.timeInt)[1, 0] + (*fit.timeInt)[0, 0]) / 2.
            
            tempNuFnu = (*fit.modelPhotRate)[kk] * (photEnergy^2.)
            nuFnuContours[*, i] = tempNuFnu
            tempMaxNu = MAX (tempNuFnu, maxSub)
            maxNuFnu[i] = photEnergy[maxSub]
            
        ENDFOR
    ENDELSE
    
    nTime = (numFit GT 29) ? 29 : numFit            ;== Limit on number of contours
    xLog = 1
    xTitle = 'Energy (keV)'
    xRange = [photEnergy[0], photEnergy[numPhotEbins - 1]]
    yLog = 0
    yTitle = 'Seconds since burst trigger'          ;TBD: Fluence weighting
    yRange = [times[0], times[numFit - 1]]
    self->adjustRanges, xRange, yRange, 'topoPlot' ; xLog, yLog,

    CONTOUR, nuFnuContours, photEnergy, times, $
        COLOR = self.Color->color ('SPEC'),             $
        BACKGROUND = self.Color->color ('BG'),          $
        POSITION = self.defaultPlotPos,                 $
        NLEVELS = nTime, /FOLLOW, XLOG = xLog,          $
        XSTYLE = 5, XRANGE = xRange,                    $
        YSTYLE = 5, YRANGE = yRange,                    $
        /DOWNHILL,                                      $
        _EXTRA = extra 
        
    PLOT, xRange, yRange,                                $
        COLOR = self.Color->color ('FG'),                $
        /NOERASE, /NODATA, XLOG = xLog, XTITLE = xTitle, $
        POSITION = self.defaultPlotPos,                  $
        XSTYLE = 1, XRANGE = xRange,                     $
        YSTYLE = 1, YRANGE = yrange, YTITLE = ytitle,    $
        _EXTRA = extra 
        
    OPLOT, maxNuFnu, times, LINESTYLE = 2, COLOR = self.Color->color ('RED')
    
;    IF KEYWORD_SET (selection) THEN BEGIN
;       newSpan = SELECT (photEnergy, (self.myDisplay).widgetID.draw, span, $
;           HAVESELECTION = haveSelection)
;       ENDIF
    
    self->finishRanges, 'topoPlot', XLOG = xLog, YLOG = yLog
        
END


; ----------------------------------------------------------------------------
; Plot contours of the model residuals for the batch fit
; ----------------------------------------------------------------------------
PRO BatchPlotter::plotContours, $
    HARDCOPY = hardcopy, _EXTRA = extra
    
;    IF ((NOT self.haveBatch) AND (NOT PTR_VALID (self.readFilename))) THEN RETURN
;    IF (NOT self.haveBatch) THEN RETURN

    theColors = self.Color->model(/NAMES)

    IF (NOT self.haveBatch) THEN BEGIN
        IF PTR_VALID (self.readFileName) THEN BEGIN
            data1 = MRDFITS (*self.readFIleName, 1, headr1, /SILENT)
            numDets = sxpar (headr1, 'NAXIS2')
            checkSize = sxpar (headr1, 'TFIELDS')
            IF ((checkSize EQ 3) OR (checkSize EQ 6)) THEN BEGIN
                self.display->setStatus, $
                     'This parameter file has no model data.', 5, /REVERT
                RETURN      ;== Small parameter file, no model data
            ENDIF

            data2 = MRDFITS (*self.readFIleName, 2, headr2, /SILENT)
            numFit = sxpar (headr2, 'NAXIS2')
            times = ((data2.timebin)[1, *] + (data2.timebin)[0, *]) / 2.
        
            ;== Don't need the Effective Area correction for the residuals
            EffArea = FLTARR (numFit, numDets) > 1.
            
            nChan = data1.channum
            maxChan = N_ELEMENTS (data1[0].e_edges)
            
            ;== Strip out the last (overflow) channel in the data
            tempEdges = ((data1[*]).e_edges[1: maxChan - 1] + $
                         (data1[*]).e_edges[0: maxChan - 2, *]) / 2.
            
            ;== We have overlapping datasets with several detectors included;
            ;== Put everything in order of ascending energy:
            numPhtFit = reorderData (numDets, nChan, tempEdges, data1.phtModl, $
                        data1.fitChan, EffArea, phtEdges, phtModl)
            numPhtFit = reorderData (numDets, nChan, tempEdges, data1.phtCnts, $
                        data1.fitChan, EffArea, phtEdges, phtCnts)
            numPhtFit = reorderData (numDets, nChan, tempEdges, data1.phtErrs, $
                        data1.fitChan, EffArea, phtEdges, phtErrs)

            IF (numPhtFit NE numFit) THEN RETURN ;== Something is badly wrong!
            
            fullEnergy = phtEdges 
            totChan = N_ELEMENTS (fullEnergy)
            residuals = FLTARR(totChan, numFit)
            zind = WHERE ((phtErrs NE 0.0) AND FINITE(phtErrs), coZind)
            IF coZind EQ 0 THEN RETURN        ;== No hope to plot anything!
            residuals[zind] = (phtCnts[zind] - phtModl[zind]) / phtErrs[zind]
        ENDIF ELSE RETURN          ;== No Batch fit and no results file: bail
    ENDIF ELSE BEGIN
        
        numFit = self.BatchFitList->count ()
        batNames  = self.BatchFitList->names ()
        fit = self.BatchFitList->data (batNames[0])
    
        model = self.fitModel->model ()
        ;== Concatenate the energy arrays into one
        fullEnergy = model.chanEnergy[model.fitChannels[0, 0]: model.fitChannels[1, 0], 0]
        FOR j = 1, model.numDet - 1 DO BEGIN
            fullEnergy = [fullEnergy, $
                model.chanEnergy[model.fitChannels[0, j]: model.fitChannels[1, j], j]]
        ENDFOR
        totChan = N_ELEMENTS (fullEnergy)
        kk = SORT (fullEnergy)
        fullEnergy = fullEnergy[kk]
    
        residuals = FLTARR (totChan, numFit)
        times = FLTARR (numFit)
        FOR i = 0, numFit - 1 DO BEGIN
   
            fit = self.BatchFitList->data (batNames[i])
            ;== Assume time intervals all same over fitted detectors
            times[i] = ((*fit.timeInt)[1, 0] + (*fit.timeInt)[0, 0]) / 2.
        
            ;== Construct a residuals array
            tempResid = [0.0]
            FOR j = 0, model.numDet - 1 DO BEGIN
                lo = model.fitChannels[0, j]
                hi = model.fitChannels[1, j]
                diffArr = ((*fit.netCrate)[lo: hi, j] - $
                            (*fit.modelCntRate)[lo: hi, j]) ;/ $
                errArr = (*fit.netCsig)[lo: hi, j]
                sigArr = FLTARR (totChan)
                
                zind = WHERE ((errArr NE 0.0) AND FINITE(errArr), coZind)
                IF coZind NE 0 THEN BEGIN        
                    sigArr[zind] = diffArr[zind] / errArr[zind]
                ENDIF
                tempResid = [tempResid, sigArr]
            ENDFOR
            tempResid = tempResid[1: *]
            residuals[*, i] = tempResid[kk]
                
        ENDFOR
    ENDELSE

    LEV = INDGEN(7) - 3
    STY = [0,2,0,1,3,0,3]
    theColors = [self.Color->color ('WHITE'), self.Color->color ('YELLOW'), $
                 self.Color->color ('RED'), self.Color->color ('BLUE'),     $
                 self.Color->color ('GREEN'), self.Color->color ('CYAN'),   $
                 self.Color->color ('PURPLE')]
    xTitle = "Energy (keV)"
    xLog = 1
    xRange = [fullEnergy[0], fullEnergy[totChan - 1]]
    ytitle = "Seconds since burst trigger"
    yRange = [times[0], times[numFit - 1]]
    yLog = 0
    self->adjustRanges, xRange, yRange, 'residPlot' 
    
    ;Smooth the result a bit...
    ;------------- In case number of time bins is 2 ---------YK:
    IF (N_ELEMENTS (residuals[0, *]) EQ 2) THEN BEGIN
        FOR i = 0, 1 DO BEGIN
	    residuals[*, i] = SMOOTH (residuals[*, i], 3, /EDGE_TRUNCATE)
	ENDFOR
    ENDIF ELSE BEGIN
    ;----------------------------------------------------------
        residuals = SMOOTH (residuals, 3, /EDGE_TRUNCATE)
    ENDELSE
    CONTOUR, residuals, fullEnergy, times, LEVELS = lev,  $
             C_COLORS = theColors, C_LINESTYLE = sty,     $
             BACKGROUND = self.Color->color ('BG'),       $
             XSTYLE = 5, XRANGE = xRange, XLOG = xLog,    $
             YSTYLE = 5, YRANGE = yRange,                 $
             POSITION = self.defaultPlotPos, _EXTRA = extra

    PLOT, xRange, yRange,                                $
        COLOR = self.Color->color ('FG'),                $
        /NOERASE, /NODATA,                               $
        XLOG = xLog, XTITLE = xTitle, XRANGE = xRange,   $
        XSTYLE = 1, YSTYLE = 1, YTITLE = ytitle,         $
        YRANGE = yRange,                                 $
        POSITION = self.defaultPlotPos,                  $
        _EXTRA = extra
             
    self->finishRanges, 'residPlot', XLOG = xLog, YLOG = yLog
        
END


; ----------------------------------------------------------------------------
; Plot the durations plot 
; ----------------------------------------------------------------------------
PRO BatchPlotter::plotDurations, CALCULATE = calculate, $
    HARDCOPY = hardcopy, _EXTRA = extra

    IF (KEYWORD_SET(calculate)) THEN BEGIN
    	self->setDurStatus, 0
    	;== The current plot range info is incorrect
    	;IF (self.rangeList->inList ('durPlot')) THEN self.rangeList->delete, 'durPlot'
    ENDIF
    ; Display the fluence interval, so there is no mistake:
    self.durInt = self.display->get_durationInt()

	;== 1/20/10 RDP: paramInd has been adjusted upward by the "self->plotBatchFit, /DURATION" call
	paramInd = 999L
	result = self->getBatchParams(paramInd, times, params, perrs, /DURATION)
	fluInt = self.durInt

    self->plotBatchFit, /DURATION, _EXTRA = extra
    
	paramInd = self.firstSelected

	FluxDur   = params[7, *]
	pFlux     = params[0, *]
	pFerrs    = perrs[0, *]
	pFluxB    = params[6, *]
	pFlxBerrs = perrs[6, *]
	eFluence  = params[3, *]
	eFluErrs  = perrs[3, *]
	beFluence = params[8, *]
	beFluErrs = perrs[8, *]

	BATfluBackLevels = FLTARR(2)
    
    f0 = STRTRIM (STRING (fluInt[0], FORMAT = '(D15.0)'), 2) 
    f1 = STRTRIM (STRING (fluInt[1], FORMAT = '(D15.0)'), 2)
    fname = 'Fluence Interval: ' + f0 + ' - ' + f1 + ' keV'
    
	self.display->annotate, fname, YPOSITION = !Y.WINDOW[1] + 0.01, $
	  /SMALL, COLOR = self.color->color('FG'), _EXTRA = extra    
    
    IF (NOT self.haveDurBacks[0] AND NOT self.skipDurBack) THEN BEGIN
    	IF (self.haveDurFromFile) THEN BEGIN
			myIdx = WHERE (times[0,*] GE self.durInfo.durBackInt[0, 0] $
			           AND times[1,*] LT self.durInfo.durBackInt[0, 1], myCo) 

			IF (myCo GT 0) THEN BEGIN
				self.durInfo.durBackLevels[0] = TOTAL (FluxDur[0, myIdx]) / N_ELEMENTS(myIdx)
				zero_level_err = VARIANCE(FluxDur[0, myIdx]) 
				self.durInfo.fluBackLevels[0] = TOTAL (eFluence[0, myIdx]) / N_ELEMENTS(myIdx)
				BATfluBackLevels[0] = TOTAL (beFluence[0, myIdx]) / N_ELEMENTS(myIdx)
				zero_level_eerr = VARIANCE(eFluence[0, myIdx]) 
				zero_level_berr = VARIANCE(beFluence[0, myIdx]) 
				self.haveDurBacks[0] = 1L
				self.skipDurBack = 0L
				self.minTimeBin = MAX (myIdx)
			ENDIF 
    		
    	END ELSE BEGIN
			message = 'Choose the lower background interval'
			self.display->setStatus, message, 5 ;, /REVERT
			span = SELECT (times, (self.display).widgetID.draw, /SINGLEINTERVAL, $
					COLOR = self.color->color('FG'),      $ 
					BACKGROUND = self.color->color('BG'), $ 
					HAVESELECTION = haveSelection)
					
			IF (haveSelection) THEN BEGIN
				myIdx = WHERE (times[0,*] GE span[0,0] AND times[1,*] LT span[0,1], myCo) 
				IF (myCo GT 0) THEN BEGIN
					self.durInfo.durBackLevels[0] = TOTAL (FluxDur[0, myIdx]) / N_ELEMENTS(myIdx)
					zero_level_err = VARIANCE(FluxDur[0, myIdx]) 
					self.durInfo.fluBackLevels[0] = TOTAL (eFluence[0, myIdx]) / N_ELEMENTS(myIdx)
					BATfluBackLevels[0] = TOTAL (beFluence[0, myIdx]) / N_ELEMENTS(myIdx)
					zero_level_eerr = VARIANCE(eFluence[0, myIdx]) 
					zero_level_berr = VARIANCE(beFluence[0, myIdx]) 
					self.durInfo.durBackInt[0, 0] = span[0, 0]
					self.durInfo.durBackInt[0, 1] = span[0, 1]
					self.haveDurBacks[0] = 1L
					self.skipDurBack = 0L
					self.minTimeBin = MAX (myIdx)
				ENDIF 
			ENDIF ELSE BEGIN
			   self.skipDurBack = 1L
			ENDELSE
        ENDELSE
        
    ENDIF
    
    IF (self.haveDurBacks[0]) THEN BEGIN
    	myLevel = self.durInfo.durBackLevels[0]
		OPLOT, [!X.crange[0], !X.crange[1]], [myLevel, myLevel], $
			COLOR = self.Color->color ('BKGD'),                  $
			LINESTYLE = 1,                                       $
			_EXTRA = extra

    	myLimit = self.durInfo.durBackInt[0, 1]
		OPLOT, [myLimit, myLimit], [!Y.crange[0], !Y.crange[1]], $
			COLOR = self.Color->color ('BKGD'),                  $
			LINESTYLE = 1,                                       $
			_EXTRA = extra
    ENDIF

    IF (NOT self.haveDurBacks[1] AND NOT self.skipDurBack) THEN BEGIN
    	IF (self.haveDurFromFile) THEN BEGIN
			myIdx = WHERE (times[0,*] GE self.durInfo.durBackInt[1, 0] $
			           AND times[1,*] LT self.durInfo.durBackInt[1, 1], myCo) 
			IF (myCo GT 0) THEN BEGIN
				self.durInfo.durBackLevels[1] = TOTAL (FluxDur[0, myIdx]) / N_ELEMENTS(myIdx)
				total_level_err = VARIANCE(FluxDur[0, myIdx]) 
				self.durInfo.fluBackLevels[1] = TOTAL (eFluence[0, myIdx]) / N_ELEMENTS(myIdx)
				BATfluBackLevels[1] = TOTAL (beFluence[0, myIdx]) / N_ELEMENTS(myIdx)
				total_level_eerr = VARIANCE(eFluence[0, myIdx]) 
				total_level_berr = VARIANCE(beFluence[0, myIdx]) 
				self.haveDurBacks[1] = 1L
				self.skipDurBack = 0L
				self.maxTimeBin = MIN (myIdx)
			ENDIF 
    		
    	END ELSE BEGIN
			message = 'Choose the upper background interval'
			self.display->setStatus, message, 5 ;, /REVERT
			span = SELECT (times, (self.display).widgetID.draw, /SINGLEINTERVAL, $
					COLOR = self.color->color('FG'),      $ 
					BACKGROUND = self.color->color('BG'), $ 
					HAVESELECTION = haveSelection)
					
			IF (haveSelection) THEN BEGIN
				myIdx = WHERE (times[0,*] GE span[0,0] AND times[1,*] LT span[0,1], myCo) 
				IF (myCo GT 0) THEN BEGIN
					self.durInfo.durBackLevels[1] = TOTAL (FluxDur[0, myIdx]) / N_ELEMENTS(myIdx)
					total_level_err = VARIANCE(FluxDur[0, myIdx]) 
					self.durInfo.fluBackLevels[1] = TOTAL (eFluence[0, myIdx]) / N_ELEMENTS(myIdx)
					BATfluBackLevels[1] = TOTAL (beFluence[0, myIdx]) / N_ELEMENTS(myIdx)
					total_level_eerr = VARIANCE(eFluence[0, myIdx]) 
					total_level_berr = VARIANCE(beFluence[0, myIdx]) 
					self.durInfo.durBackInt[1, 0] = span[0, 0]
					self.durInfo.durBackInt[1, 1] = span[0, 1]
					self.haveDurBacks[1] = 1L
					self.skipDurBack = 0L
					self.maxTimeBin = MIN (myIdx)
				ENDIF 
			ENDIF ELSE BEGIN
			   self.skipDurBack = 1L
			ENDELSE
        ENDELSE
        
    ENDIF
    
    IF (self.haveDurBacks[1]) THEN BEGIN
    	myLevel = self.durInfo.durBackLevels[1]
		OPLOT, [!X.crange[0], !X.crange[1]], [myLevel, myLevel], $
			COLOR = self.Color->color ('BKGD'),                  $
			LINESTYLE = 1,                                       $
			_EXTRA = extra

    	myLimit = self.durInfo.durBackInt[1, 0]
		OPLOT, [myLimit, myLimit], [!Y.crange[0], !Y.crange[1]], $
			COLOR = self.Color->color ('BKGD'),                  $
			LINESTYLE = 1,                                       $
			_EXTRA = extra
    ENDIF
  
    IF (NOT self.haveDuration AND self.haveDurBacks[0] AND self.haveDurBacks[1]) THEN BEGIN
    	range = self.durInfo.durBackLevels[1] - self.durInfo.durBackLevels[0]
    	base = self.durInfo.durBackLevels[0]
    	;== Reverse the order if the user made a mistake
    	IF (range LT 0.) THEN BEGIN
    	   range = - range
    	   self.durInfo.durBackLevels[0] = self.durInfo.durBackLevels[1]
    	   self.durInfo.durBackLevels[1] = base
    	   base = self.durInfo.durBackLevels[0]
		   tempBin = self.maxTimeBin
		   self.maxTimeBin = self.minTimeBin
		   self.minTimeBin = tempBin
    	ENDIF
    	mint = self.minTimeBin
    	maxt = self.maxTimeBin
    	
    	;== The error calculation (from Valerie)
    	fs = [0.05,0.25,0.75,0.95]
        ts = FLTARR(4)
        FOR i = 0,3 DO BEGIN
			level = (self.durInfo.durationLevels[i] = base + range * fs[i])
			myDurIdx = MIN (WHERE (FluxDur[0, mint: maxt] GE level)) 
			self.durInfo.durations[i] = times[0, myDurIdx + mint]

			err_in_flux = SQRT( (1.-fs[i])^2 * zero_level_err + fs[i]^2 * total_level_err)
			levelLo = base + range * fs[i] - err_in_flux
			levelHi = base + range * fs[i] + err_in_flux
			myErrIdxLo = MIN (WHERE (FluxDur[0, mint: maxt] GE levelLo)) 
			myErrIdxHi = MIN (WHERE (FluxDur[0, mint: maxt] GE levelHi)) 
			IF (fs[i] EQ 0.05 OR fs[i] EQ 0.25) THEN err_t = times[1, myErrIdxHi + mint] - times[0, myErrIdxLo + mint]
			IF (fs[i] EQ 0.75 OR fs[i] EQ 0.95) THEN err_t = times[1, myErrIdxHi + mint] - times[0, myErrIdxLo + mint]
			ts[i] = err_t
        ENDFOR
		err_in_t90 = SQRT(ts[0]^2 + ts[3]^2)
		self.durInfo.durationErrors[0] = err_in_t90
		err_in_t50 = SQRT(ts[1]^2 + ts[2]^2)
		self.durInfo.durationErrors[1] = err_in_t50
    
    	self.durInfo.fluence = self.durInfo.fluBackLevels[1] - self.durInfo.fluBackLevels[0]
		err_in_eflux = SQRT(zero_level_eerr + total_level_eerr)
    	self.durInfo.fluenceErr = err_in_eflux ;SQRT(TOTAL(eFluErrs[0, *]^2) / N_ELEMENTS(eFluErrs[0, *]))
    	self.durInfo.fluenceBATSE = BATfluBackLevels[1] - BATfluBackLevels[0]
		err_in_beflux = SQRT(zero_level_berr + total_level_berr)
    	self.durInfo.fluenceErrBATSE = err_in_beflux ;SQRT(TOTAL(beFluErrs[0, *]^2) / N_ELEMENTS(beFluErrs[0, *]))

    	self.haveDuration = 1

    ENDIF
    
    IF (self.haveDuration) THEN BEGIN
        FOR  i = 0, 3 DO BEGIN
        
			myLevel = self.durInfo.durationLevels[i]
			OPLOT, [!X.crange[0], !X.crange[1]], [myLevel, myLevel], $
				COLOR = self.Color->color ('BKGD'),                  $
				LINESTYLE = 2,                                       $
				_EXTRA = extra
			myDurat = self.durInfo.durations[i]
			OPLOT, [myDurat, myDurat], [!Y.crange[0], !Y.crange[1]], $
				COLOR = self.Color->color ('BKGD'),                  $
				LINESTYLE = 2,                                       $
				_EXTRA = extra
				
        ENDFOR
        
        durStart = self.durInfo.durations[0]
	    c = CONVERT_COORD (durStart, self.durInfo.durationLevels[0], /DATA, /TO_NORMAL)
	    ;== Clip the arrows to the region of the plot:
	    IF (durStart LT !x.crange[1] AND durStart GT !x.crange[0]) THEN $
		   ARROW, c[0] - 0.05, c[1] + 0.05, c[0], c[1] + 0.05, /NORMAL, /SOLID ;, $
			  ; HSIZE = !D.X_SIZE  / 96.0, COLOR = color

        durStop = self.durInfo.durations[3]
	    c = CONVERT_COORD (durStop, self.durInfo.durationLevels[0], /DATA, /TO_NORMAL)
	    ;== Clip the arrows to the region of the plot:
	    IF (durStop LT !x.crange[1] AND durStop GT !x.crange[0]) THEN BEGIN
            ARROW, c[0] + 0.05, c[1] + 0.05, c[0], c[1] + 0.05, /NORMAL, /SOLID ;, $
			  ; HSIZE = !D.X_SIZE  / 96.0, COLOR = color
			t90 = STRTRIM (STRING (durStop - durStart, FORMAT = '(F15.3)'), 2)
			t90err = STRTRIM (STRING (self.durInfo.durationErrors[0] , FORMAT = '(F15.3)'), 2)
			t90Start = STRTRIM (STRING (durStart, FORMAT = '(F15.3)'), 2)
            XYOUTS, c[0] + 0.06, c[1] + 0.05, 't!D90!N = ' + t90 + ' +/- ' + t90err + ' s!C' + $
                 '   @ ' + t90Start + ' s', ALIGN = align, /NORMAL
	    END
        
        durStart = self.durInfo.durations[1]
	    c = CONVERT_COORD (durStart, self.durInfo.durationLevels[1], /DATA, /TO_NORMAL)
	    ;== Clip the arrows to the region of the plot:
	    IF (durStart LT !x.crange[1] AND durStart GT !x.crange[0]) THEN $
		   ARROW, c[0] - 0.05, c[1] + 0.05, c[0], c[1] + 0.05, /NORMAL, /SOLID ;, $
			  ; HSIZE = !D.X_SIZE  / 96.0, COLOR = color

        durStop = self.durInfo.durations[2]
	    c = CONVERT_COORD (durStop, self.durInfo.durationLevels[1], /DATA, /TO_NORMAL)
	    ;== Clip the arrows to the region of the plot:
	    IF (durStop LT !x.crange[1] AND durStop GT !x.crange[0]) THEN BEGIN
            ARROW, c[0] + 0.05, c[1] + 0.05, c[0], c[1] + 0.05, /NORMAL, /SOLID ;, $
			  ; HSIZE = !D.X_SIZE  / 96.0, COLOR = color
			t50 = STRTRIM (STRING (durStop - durStart, FORMAT = '(F15.3)'), 2)
			t50err = STRTRIM (STRING (self.durInfo.durationErrors[1] , FORMAT = '(F15.3)'), 2)
			t50Start = STRTRIM (STRING (durStart, FORMAT = '(F15.3)'), 2)
            XYOUTS, c[0] + 0.06, c[1] + 0.05, 't!D50!N = ' + t50 + ' +/- ' + t50err + ' s!C' + $
                 '   @ ' + t50Start + ' s', ALIGN = align, /NORMAL
	    END

    ENDIF
    
    ;== Need a short cut out of this place:
    IF self.haveFluxFluence OR self.haveDurFromFile THEN RETURN
    
	msg = 'Continue and Calculate Peak FLuxes and FLuences?'
	wantContinue = DIALOG_MESSAGE( msg, /QUESTION)
    
    IF (wantContinue EQ 'Yes') THEN BEGIN
    	;== Check to see whether we need to rebin and recalculate the Batch Fit:
    	mydisp = self.display
    	names = mydisp.DetectorList->names ()
    	mydet = mydisp.DetectorList->data (names[0])
    	thedisp = mydet.display
    	IF OBJ_ISA(thedisp.lightcurve, "TTELIGHTCURVE") THEN BEGIN
    		;== 03/09/10 RP (Thanks, Colleen!): EVERY timelookup is default for TTE data! Assume we have to rebin:
    		diff_tl = 2
    	ENDIF ELSE BEGIN
			default_tl = thedisp.lightCurve->timeLookup ()
			diff_tl = MAX (default_tl[1:*] - default_tl)
    	ENDELSE
    	mySpan = FLTARR(2,2)
    	myspan[*, 0] = MIN(self.durInfo.durBackInt)
    	myspan[*, 1] = MAX(self.durInfo.durBackInt)

    	IF (diff_tl NE 1) THEN BEGIN   ;== Rebin each data set:
    		ndets = mydisp.DetectorList->count ()
    		FOR dd = 0, ndets - 1 DO BEGIN
				mydet = mydisp.DetectorList->data (names[dd])
				thedisp = mydet.display
				thedisp->map
				thedisp->setTimeSpan, mySpan
				thedisp->rebin, /FULLRESOLUTION
    		ENDFOR
    		mydisp->map
    		mydisp->batchfitSpectra
    	ENDIF
		
		;== Must acquire the latest values for the calculated parameters:
		result = self->getBatchParams(paramInd, times, params, perrs, /DURATION)
    	IF (result NE 1) THEN BEGIN
			message = 'There is no valid batch fit data to plot!'
			self.display->setStatus, message, 5, /REVERT
    		RETURN
    	ENDIF
		pFlux     = params[0, *]
		pFerrs    = perrs[0, *]
		pFluxB    = params[6, *]
		pFlxBerrs = perrs[6, *]
    	;== Peak Flux / Fluence and errors calculation:
    	deltaT = times[1, *] - times[0, *]
    	p64Ind = WHERE(deltaT LT 0.120, co64, COMPLEMENT = p256Ind, NCOMPLEMENT = co256)
    	IF (co64 LT 16) THEN BEGIN
			message = 'There is no 64 ms data in the set!'
			self.display->setStatus, message, 5, /REVERT
    		RETURN
    	ENDIF

    	self.durInfo.peakFlux64 = MAX(pFlux[0,p64Ind], pFInd)
    	self.durInfo.peakFLux64Err = pFerrs[0,p64Ind[pFInd]]
    	self.durInfo.peakFLux64Beg = times[0,p64Ind[pFInd]]
    	self.durInfo.peakFlux64BATSE = MAX(pFluxB[0,p64Ind], pFBInd)
    	self.durInfo.peakFLux64BErr = pFlxBerrs[0,p64Ind[pFBInd]]
    	self.durInfo.peakFLux64BBeg = times[0,p64Ind[pFBInd]]
    	self.durInfo.peakInterval64 = times[1, p64Ind[pFInd]] - times[0, p64Ind[pFInd]]
    	
    	;== Now do the running binning on the 256 ms timescale:
    	numFlux = N_ELEMENTS(pFlux[0, p64Ind])
    	idxPad = numFlux MOD 4
    	;== Accumulate the 256 ms data, if any:
    	IF (co256 GT 0) THEN BEGIN
			tempPreFlux = TRANSPOSE(pFlux[0, p256Ind])
			tempPreFluxB = TRANSPOSE(pFluxB[0, p256Ind])
			tempPrePFerrs = TRANSPOSE(pFerrs[0, p256Ind])
			tempPrePFlxBerrs = TRANSPOSE(pFlxBerrs[0, p256Ind])
			tempPreTimes = times[*, p256Ind]
    	ENDIF
    	tempFlux = TRANSPOSE(pFlux[0, p64Ind])
    	tempFluxB = TRANSPOSE(pFluxB[0, p64Ind])
    	tempPFerrs = TRANSPOSE(pFerrs[0, p64Ind])
    	tempPFlxBerrs = TRANSPOSE(pFlxBerrs[0, p64Ind])
    	tempTimes = times[*, p64Ind]
    	FOR aa = 0, 4 - idxPad DO BEGIN
    		tempFlux = [tempFlux, 0.0]
    		tempFluxB = [tempFluxB, 0.0]
    	ENDFOR
    	tempPF = fltarr(4)
    	tempPFIdx = LONARR(4)
    	tempPFB = fltarr(4)
    	tempPFBIdx = LONARR(4)
    	FOR bb = 0, 3 DO BEGIN
    		tempBinnedPF = REFORM(tempFlux[bb: numFlux - 1 - idxPad + bb], 4, (numFlux - idxPad) / 4)
    		summedPF = TOTAL(tempBinnedPF, 1) / 4.
    		tempPF[bb] = MAX(summedPF, tempMIDX)
    		tempPFIdx[bb] = tempMIDX
    		tempBinnedPFB = REFORM(tempFluxB[bb: numFlux - 1 - idxPad + bb], 4, (numFlux - idxPad) / 4)
    		summedPFB = TOTAL(tempBinnedPFB, 1) / 4.
    		tempPFB[bb] = MAX(summedPFB, tempMIDXB)
    		tempPFBIdx[bb] = tempMIDXB
    	ENDFOR
    	self.durInfo.peakFlux256 = MAX(tempPF, pF256Idx)
    	baseIdx = tempPFIdx[pF256Idx] * 4 + pF256Idx
    	self.durInfo.peakFLux256Err =  SQRT(TOTAL(tempPFerrs[baseIdx: baseIdx + 3]^2.)) / 4.
    	self.durInfo.peakFLux256Beg = tempTimes[0, baseIdx]
    	self.durInfo.peakFlux256BATSE = MAX(tempPFB, pF256IdxB)
    	baseIdxB = tempPFBIdx[pF256IdxB] * 4 + pF256IdxB
    	self.durInfo.peakFLux256BErr = SQRT(TOTAL(tempPFlxBerrs[baseIdxB: baseIdxB + 3]^2.)) / 4.
    	self.durInfo.peakFLux256BBeg = tempTimes[0, baseIdxB]
    	self.durInfo.peakInterval256 = tempTimes[1, baseIdx + 3] - tempTimes[0, baseIdx]
    	;== Now check the rest of the data:
    	IF (co256 GT 0) THEN BEGIN
    		prePF256 = MAX(tempPreFlux, pre256Ind)
    		IF (prePF256 GT self.durInfo.peakFlux256) THEN BEGIN
    			self.durInfo.peakFlux256 = tempPreFlux[pre256Ind]
    			self.durInfo.peakFLux256Err = tempPrePFerrs[pre256Ind]
    			self.durInfo.peakFLux256Beg = tempPreTimes[0, pre256Ind]
				self.durInfo.peakInterval256 = tempPreTimes[1, pre256Ind] - tempPreTimes[0, pre256Ind]
    		ENDIF
    		prePF256B = MAX(tempPreFluxB, pre256IndB)
    		IF (prePF256B GT self.durInfo.peakFlux256BATSE) THEN BEGIN
    			self.durInfo.peakFlux256BATSE = tempPreFluxB[pre256IndB]
    			self.durInfo.peakFLux256BErr = tempPrePFlxBerrs[pre256IndB]
    			self.durInfo.peakFLux256BBeg = tempPreTimes[0, pre256IndB]
    		ENDIF
    	ENDIF

    	;== Now do the running binning on the 1024 ms timescale:
    	IF (co256 GT 0) THEN BEGIN
			tempPreFlux = TRANSPOSE(pFlux[0, p256Ind])
			tempPreFluxB = TRANSPOSE(pFluxB[0, p256Ind])
			tempPrePFerrs = TRANSPOSE(pFerrs[0, p256Ind])
			tempPrePFlxBerrs = TRANSPOSE(pFlxBerrs[0, p256Ind])
			tempPreTimes = times[*, p256Ind]
			preNumFlux = N_ELEMENTS(tempPreFlux)
			preIdxPad = preNumFlux MOD 4
			;== 256 ms data needs to be binned by 4:
			FOR aa = 0, 4 - preIdxPad DO BEGIN
				tempPreFlux = [tempPreFlux, 0.0]
				tempPreFluxB = [tempPreFluxB, 0.0]
			ENDFOR
			tempPF = fltarr(4)
			tempPFIdx = LONARR(4)
			tempPFB = fltarr(4)
			tempPFBIdx = LONARR(4)
			FOR bb = 0, 3 DO BEGIN
				tempBinnedPF = REFORM(tempPreFlux[bb + preIdxPad + 1: preNumFlux - 4 + bb], 4, (preNumFlux - preIdxPad - 4) / 4)
				summedPF = TOTAL(tempBinnedPF, 1) / 4.
				tempPF[bb] = MAX(summedPF, tempMIDX)
				tempPFIdx[bb] = tempMIDX
				tempBinnedPFB = REFORM(tempPreFluxB[bb + preIdxPad + 1: preNumFlux - 4 + bb], 4, (preNumFlux - preIdxPad - 4) / 4)
				summedPFB = TOTAL(tempBinnedPFB, 1) / 4.
				tempPFB[bb] = MAX(summedPFB, tempMIDXB)
				tempPFBIdx[bb] = tempMIDXB
			ENDFOR
			;== Now store the results for later:
			prepeakFlux256 = MAX(tempPF, pF256Idx)
			baseIdx = tempPFIdx[pF256Idx] * 4 + pF256Idx
			prepeakFLux256Err =  SQRT(TOTAL(tempPrePFerrs[baseIdx: baseIdx + 3]^2.)) / 4.
			prepeakFLux256Beg = tempPreTimes[0, baseIdx]
			prepeakFlux256BATSE = MAX(tempPFB, pF256IdxB)
			baseIdxB = tempPFBIdx[pF256IdxB] * 4 + pF256IdxB
			prepeakFLux256BErr = SQRT(TOTAL(tempPrePFlxBerrs[baseIdxB: baseIdxB + 3]^2.)) / 4.
			prepeakFLux256BBeg = tempPreTimes[0, baseIdxB]
			prepeakInterval256 = tempPreTimes[1, baseIdx + 3] - times[0, baseIdx]
    	ENDIF
    	;== DO the binning post-trigger by 16:
		tempFlux = TRANSPOSE(pFlux[0, p64Ind])
		tempFluxB = TRANSPOSE(pFluxB[0, p64Ind])
    	tempPFerrs = TRANSPOSE(pFerrs[0, p64Ind])
    	tempPFlxBerrs = TRANSPOSE(pFlxBerrs[0, p64Ind])
    	tempTimes = times[*, p64Ind]

    	idxPad = numFlux MOD 16
    	FOR aa = 0, 16 - idxPad DO BEGIN
    		tempFlux = [tempFlux, 0.0]
    		tempFluxB = [tempFluxB, 0.0]
    	ENDFOR
    	tempPF = fltarr(16)
    	tempPFIdx = LONARR(16)
    	tempPFB = fltarr(16)
    	tempPFBIdx = LONARR(16)
    	FOR bb = 0, 15 DO BEGIN
    		tempBinnedPF = REFORM(tempFlux[bb: numFlux - 1 - idxPad + bb], 16, (numFlux - idxPad) / 16)
    		summedPF = TOTAL(tempBinnedPF, 1) / 16.
    		tempPF[bb] = MAX(summedPF, tempMIDX)
    		tempPFIdx[bb] = tempMIDX
    		tempBinnedPFB = REFORM(tempFluxB[bb: numFlux - 1 - idxPad + bb], 16, (numFlux - idxPad) / 16)
    		summedPFB = TOTAL(tempBinnedPFB, 1) / 16.
    		tempPFB[bb] = MAX(summedPFB, tempMIDXB)
    		tempPFBIdx[bb] = tempMIDXB
    	ENDFOR

    	self.durInfo.peakFlux =  MAX(tempPF, pFIdx)
    	baseIdx = tempPFIdx[pFIdx] * 16 + pFIdx
    	self.durInfo.peakFLuxErr =  SQRT(TOTAL(tempPFerrs[baseIdx: baseIdx + 15]^2.)) / 16.
    	self.durInfo.peakFLuxBeg = tempTimes[0, baseIdx]
    	self.durInfo.peakFluxBATSE = MAX(tempPFB, pFIdxB)
    	baseIdxB = tempPFBIdx[pFIdxB] * 16 + pFIdxB
    	self.durInfo.peakFLuxBErr = SQRT(TOTAL(tempPFlxBerrs[baseIdxB: baseIdxB + 15]^2.)) / 16.
    	self.durInfo.peakFLuxBBeg = tempTimes[0, baseIdxB]
    	self.durInfo.peakInterval = tempTimes[1, baseIdx + 15] - tempTimes[0, baseIdx]
    	;== Now check the rest of the data:
    	IF (co256 GT 0) THEN BEGIN
    		IF (prepeakFlux256 GT self.durInfo.peakFlux) THEN BEGIN
    			self.durInfo.peakFlux = prepeakFlux256
    			self.durInfo.peakFLuxErr = prepeakFLux256Err
    			self.durInfo.peakFLuxBeg = prepeakFLux256Beg
				self.durInfo.peakInterval = prepeakInterval256
    		ENDIF
    		IF (prepeakFlux256BATSE GT self.durInfo.peakFluxBATSE) THEN BEGIN
    			self.durInfo.peakFluxBATSE = prepeakFlux256BATSE
    			self.durInfo.peakFLuxBErr = prepeakFLux256BErr
    			self.durInfo.peakFLuxBBeg = prepeakFLux256BBeg
    		ENDIF
    	ENDIF
    	
    	;== Unfortunately, the Duration calulation is invalid for this binning:
    	;self->setDurStatus, 0
    	self.haveFluxFluence = 1
    	self.display->writeFit, /FITLOG_ONLY
    ENDIF

    self.skipDurBack = 1L

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
PRO BatchPlotter::plotData, $
    xarr, xarr_err, yarr, yarr_err, $
    OPLOT = oplot, SIGMA = sigma, ARROW = arrow, $
    FG_COLOR = fg_color, COLOR = color, _EXTRA = extra
    
    yarr_save = yarr

    err_hi = FLTARR (N_ELEMENTS (yarr))
    err_lo = FLTARR (N_ELEMENTS (yarr))

    FOR i = 0, N_ELEMENTS (yarr) - 1 DO BEGIN

        err_hi[i] = yarr[i] + yarr_err[i]
        err_lo[i] = yarr[i] - yarr_err[i]

    ENDFOR

    IF (NOT KEYWORD_SET (oplot)) THEN BEGIN

           thePos = self.defaultPlotPos
       
           PLOT, /NODATA, xarr, yarr, $
               COLOR = fg_color,      $
               POSITION = thePos,     $
               _EXTRA = extra

    ENDIF

    IF (NOT KEYWORD_SET (arrow)) THEN BEGIN
       idx = WHERE (err_lo EQ -1, cnt)
       IF (cnt NE 0) THEN err_lo[idx] = !Y.CRANGE[0]
    ENDIF

    jj = WHERE (yarr_err EQ 0.0, co)
    IF (co NE 0) THEN BEGIN
        A = FINDGEN(16) * (!PI * 2 / 16.0)
        USERSYM, COS(A), SIN(A), /FILL   ; Define a circle symbol for dy = 0
        OPLOT, xarr[jj], yarr[jj], PSYM = 8, COLOR = color
    ENDIF
    
    FOR i = 0, N_ELEMENTS (yarr) - 1 DO BEGIN
    
        IF (err_lo[i] EQ -1) THEN BEGIN
        
           c = CONVERT_COORD (xarr[i], yarr[i], /DATA, /TO_NORMAL)
           ;== Clip the arrows to the region of the plot:
           IF (xarr[i] LT !x.crange[1] AND xarr[i] GT !x.crange[0]) THEN $
               ARROW, c[0], c[1], c[0], c[1] - 0.05, /NORMAL, /SOLID, $
                   HSIZE = !D.X_SIZE / 96.0, COLOR = color
        
        ENDIF ELSE BEGIN
        
           OPLOT, [xarr[i], xarr[i]], [err_lo[i], err_hi[i]], $
               COLOR = color, _EXTRA = extra

        ENDELSE
        
        OPLOT, [xarr[i] - xarr_err[i] / 2.0, xarr[i] + xarr_err[i] / 2.0 ], $
               [yarr[i], yarr[i]], COLOR = color, _EXTRA = extra

    ENDFOR
    
    yarr = yarr_save

END


; ----------------------------------------------------------------------------
; Return the batch fit parameters for a given index from a .par file
; ----------------------------------------------------------------------------
FUNCTION BatchPlotter::getParamsFromFile, paramInd, times, params, perrs, $
         PFLUX = pflux, GETALL = getall, FLUINT = fluInt, DURATION = duration 

    nIndices = N_ELEMENTS (paramInd)
            
    IF NOT PTR_VALID(self.readFIleName) THEN RETURN, 0
    
    data = MRDFITS (*self.readFIleName, 2, headr, /SILENT)
    ;== Get the duration info (if any):
    primHead = headfits(*self.readFIleName)
    nParams = N_ELEMENTS (*self.labelList) - 6
    myExtended = WHERE(TAG_NAMES(data) EQ 'DURFLNC')

    IF KEYWORD_SET (pflux) THEN BEGIN              ;== Regardless, since user wants overplot
        nIndices = 2
        paramInd = [paramInd[0], nParams]          ;== Insert proper index for Photon flux
    ENDIF ELSE IF KEYWORD_SET (getall) THEN BEGIN             ;== Client wants all of the parameters
        nIndices = nParams + 9
        paramInd = INDGEN (nIndices)               ;== Override any input
        IF (myExtended EQ -1) THEN BEGIN           ;== Old file, no extensions!
			paramInd[nIndices - 3] = nParams + nIndices - 8
			paramInd[nIndices - 2] = nParams + nIndices - 8
			paramInd[nIndices - 1] = nParams + nIndices - 6
        ENDIF
    ENDIF ELSE IF KEYWORD_SET (duration) THEN BEGIN             ;== Client wants all of the parameters
        nIndices = 9
        paramInd = INDGEN (nIndices) + nParams               ;== Override any input
        IF (myExtended EQ -1) THEN BEGIN           ;== Old file, no extensions!
			paramInd[nIndices - 3] = nParams + nIndices - 8
			paramInd[nIndices - 2] = nParams + nIndices - 8
			paramInd[nIndices - 1] = nParams + nIndices - 6
        ENDIF
        durInt = FLTARR (2)                        ;== Override any input
        lo = sxpar (primHead, 'DUR_LOW')
        durInt[0] = (lo EQ 0) ? 50.0 : lo
        hi = sxpar (primHead, 'DUR_HIGH')
        durInt[1] = (hi EQ 0) ? 300.0 : hi
        self.durInt = durInt

    ENDIF ELSE BEGIN
        IF (myExtended EQ -1) THEN BEGIN           ;== Old file, no extensions!
			paramInd[0] = nParams + 1
        ENDIF
    ENDELSE

    IF KEYWORD_SET (fluInt) THEN BEGIN             ;== Return flux/fluence integration interval
        fluInt = FLTARR (2)                        ;== Override any input
        lo = sxpar (headr, 'FLU_LOW')
        fluInt[0] = (lo EQ 0) ? 10.0 : lo
        hi = sxpar (headr, 'FLU_HIGH')
        fluInt[1] = (hi EQ 0) ? 1000.0 : hi
    ENDIF
    
    keyExist = STRPOS(primHead, 'LOBCKINT')
    keyNum = WHERE(keyExist NE -1, co)
    IF (co NE 0) THEN BEGIN
		loback = sxpar (primHead, 'LOBCKINT')
		READS, loback, lo1, lo2, FORMAT='(1x,f9.4,1x,f9.4)'
		hiback = sxpar (primHead, 'HIBCKINT')
		READS, hiback, hi1, hi2, FORMAT='(1x,f9.4,1x,f9.4)'
		self.haveDurBacks = [0,0]
		self.skipDurBack = 0L
		self.haveDuration = 0L
		self.durInfo.durBackInt = [[lo1, hi1], [lo2, hi2]]
		self.haveDurFromFile = 1L
    ENDIF
    
    times = data.timebin
    numFit = sxpar (headr, 'NAXIS2')
    params = FLTARR (nIndices, numFit)
    perrs  = params
    FOR i = 0, nIndices - 1 DO BEGIN               ;== Use tag indices here, *not* names!
        params[i, *] = data.(paramInd[i] + 1)[0, *]
        ;IF (paramInd[i] LT nParams) THEN BEGIN     ;== Some quantities have no errors
        IF ((size(data.(paramInd[i] +1)))[0] GT 1) THEN BEGIN 
            perrs[i, *] = data.(paramInd[i] + 1)[1, *]
        ENDIF
    ENDFOR
    
    RETURN, 1
    
END

; ----------------------------------------------------------------------------
; Return the batch fit parameters for a given index
; ----------------------------------------------------------------------------
FUNCTION BatchPlotter::getBatchParams, paramInd, times, params, perrs, $
         PFLUX = pflux, GETALL = getall, FLUINT = fluInt, DURATION = duration 

    nIndices = N_ELEMENTS (paramInd)

    IF self.haveBatch THEN BEGIN
        numFit = self.BatchFitList->count ()
        batNames  = self.BatchFitList->names ()
        fit = self.BatchFitList->data (batNames[0])
        nParams = N_ELEMENTS (*fit.param)
    ENDIF ELSE BEGIN
        RETURN, self->getParamsFromFile (paramInd, times, params, perrs, $
            PFLUX = pflux, GETALL = getall, FLUINT = fluInt, DURATION = duration)
    ENDELSE
        
    IF KEYWORD_SET (pflux) THEN BEGIN              ;== Regardless, since user wants overplot
        nIndices = 2
        paramInd = [paramInd[0], nParams]          ;== Insert proper index for Photon flux
    ENDIF
    
    IF KEYWORD_SET (getall) THEN BEGIN             ;== Client wants all of the parameters
        nIndices = nParams + 9
        paramInd = INDGEN (nIndices)               ;== Override any input
    ENDIF
    
    IF KEYWORD_SET (FLUINT) THEN BEGIN             ;== Return flux/fluence integration over standard interval
        nIndices = 1
        paramInd[0] = nParams ;!RM_PARAMS.fluxint  ;== Override any input
    ENDIF 
    
    IF KEYWORD_SET (DURATION) THEN BEGIN           ;== Return flux/fluence values for duration calculation
        nIndices = 9
        paramInd = INDGEN (nIndices) + nParams     ;== Override any input
    ENDIF 
        
    times = FLTARR (2, numFit)
    params = FLTARR (nIndices, numFit)
    perrs = params
    
    photEnergy = *fit.modelEnergy
    kk = WHERE (photEnergy)
    photEnergy = photEnergy[kk]
    integralPhoton = 0.0                            ;== Start the accumulator for photon fluence
    intPhotErr     = 0.0
    integralEnergy = 0.0                            ;== Start the accumulator for energy fluence
    intEnrgErr     = 0.0
    integralDur    = 0.0                            ;== Start the accumulator for energy fluence
    integralBATSE  = 0.0                            ;== Start the accumulator for energy fluence
     
    FOR i = 0, numFit - 1 DO BEGIN
   
        fit = self.BatchFitList->data (batNames[i])
        times[*, i] = (*fit.timeInt)[*, 0]          ;== Assume time intervals all same
        
        FOR j = 0, nIndices - 1 DO BEGIN               ;== Loop over the requested params
        
          IF (paramInd[j] GT nParams - 1) THEN BEGIN   ;== Handle the calculated parameters

            photModl = (*fit.modelPhotRate)[kk]
            
            CASE (paramInd[j] - nParams) OF
                0: BEGIN      ;== 'Photon Flux (ph/s-cm^2)'
                    oneTerm = fit.pFlux ;integratePhotonModel(photEnergy, photModl, fluenceInt)
                    ;nans = WHERE (FINITE (oneTerm, /NAN), coNaN)
                    ;IF (coNaN EQ 0) THEN BEGIN
                        params[j, i] = oneTerm
                        perrs[j, i] = fit.pErr ;params[j, i] * (*fit.param_uncer)[0] / $
                                      ;(*fit.param)[0] / 2.0
                    ;ENDIF ELSE BEGIN
                    ;    params[j, i] = 0.0
                    ;    perrs[j, i] = 0.0
                    ;ENDELSE
                END

                1: BEGIN      ;== 'Photon Fluence (ph/cm^2)'
                    oneTerm = fit.pFlux $;integratePhotonModel(photEnergy, photModl, fluenceInt) * $
                                          * (times[1, i] - times[0, i])
                    ;nans = WHERE (FINITE (oneTerm, /NAN), coNaN)
                    ;IF (coNaN EQ 0) THEN BEGIN
                        integralPhoton = integralPhoton + oneTerm
                        params[j, i] = integralPhoton
                        ;intPhotErr = SQRT ((oneTerm * $   ;intPhotErr^2. + 
                        ;                   (*fit.param_uncer)[0] $
                        ;                    / (*fit.param)[0] / 2.0)^2.)
                        perrs[j, i] = fit.pErr * (times[1, i] - times[0, i]) ;intPhotErr
                    ;ENDIF ELSE BEGIN
                    ;    params[j, i] = integralPhoton
                    ;    perrs[j, i] = intPhotErr
                    ;ENDELSE
                END

                2: BEGIN      ;== 'Energy Flux (erg/s-cm^2)'
                    oneTerm = fit.eFlux ;integratePhotonModel(photEnergy, photModl, $
                                   ;fluenceInt, /ENERGY_INT) * 1.602e-9
                    ;nans = WHERE (FINITE (oneTerm, /NAN), coNaN)
                    ;IF (coNaN EQ 0) THEN BEGIN
                        params[j, i] = oneTerm
                        perrs[j, i] = fit.eErr ;params[j, i] * (*fit.param_uncer)[0] / $
                                                 ;    (*fit.param)[0] / 2.0
                    ;ENDIF ELSE BEGIN
                    ;    params[j, i] = 0.0
                    ;    perrs[j, i] = 0.0
                    ;ENDELSE
                END

                3: BEGIN      ;== 'Energy Fluence (erg/cm^2)'
                    enrgTerm = fit.eFlux $;integratePhotonModel(photEnergy, photModl, $
                                   ;fluenceInt, /ENERGY_INT) * $
                                   * (times[1, i] - times[0, i]) ; * 1.602e-9
                    ;nans = WHERE (FINITE (enrgTerm, /NAN), coNaN)
                    ;IF (coNaN EQ 0) THEN BEGIN
                        integralEnergy = integralEnergy + enrgTerm
                        params[j, i] = integralEnergy
                        ;intEnrgErr = SQRT ((enrgTerm * $      ;intEnrgErr^2. + 
                        ;                   (*fit.param_uncer)[0] $
                        ;                    / (*fit.param)[0] / 2.0)^2.)
                        perrs[j, i] = fit.eErr * (times[1, i] - times[0, i]) ;intEnrgErr
                    ;ENDIF ELSE BEGIN
                    ;    params[j, i] = integralEnergy
                    ;    perrs[j, i] = intEnrgErr
                    ;ENDELSE
                END

                4: BEGIN      ;== 'Reduced Chi-squares'
                    dof = (fit.dof EQ 0) ? - fit.chisq : FLOAT (fit.dof)
                    params[j, i] = fit.chisq / dof
                    perrs[j, i] = 0.0
                END
                   
                5: BEGIN      ;TBD== 'Count History'
                    params[j, i] = fit.dof
                    perrs[j, i] = 0.0
                END

                6: BEGIN      ;== 'Photon Flux (ph/s-cm^2)' <-- BATSE energy range: 50--300 keV
                    oneTerm = fit.bFlux 
					params[j, i] = oneTerm
					perrs[j, i] = fit.bErr ;intPhotErr
                END

                7: BEGIN      ;== 'Photon Fluence (ph/cm^2)' <-- Duration energy range: User selected
                    oneTerm = fit.dFlux * (times[1, i] - times[0, i])
					integralDur = integralDur + oneTerm
					params[j, i] = integralDur
					perrs[j, i] = fit.dErr * (times[1, i] - times[0, i]) ;intPhotErr
                END

                8: BEGIN      ;== 'Energy Fluence (erg/cm^2)' <-- BATSE energy range: User selected
                    oneTerm = fit.beFlux * (times[1, i] - times[0, i])
					integralBATSE = integralBATSE + oneTerm
					params[j, i] = integralBATSE
					perrs[j, i] = fit.beErr * (times[1, i] - times[0, i]) ;intPhotErr
                END

            ENDCASE
            
        ENDIF ELSE BEGIN                            ;== Parameters of the fit model
            params[j, i] = (*fit.param)[paramInd[j]]
            perrs[j, i]  = (*fit.param_uncer)[paramInd[j]]
        ENDELSE
        
      ENDFOR
    
    ENDFOR
    
    RETURN, 1

END


; ----------------------------------------------------------------------------
; Set the fit model reference, useful for obtaining channel energies and selections
; ----------------------------------------------------------------------------
PRO BatchPlotter::setFitModel, FitModel

    self.FitModel = FitModel

END


; ----------------------------------------------------------------------------
; Set the current photon model
; ----------------------------------------------------------------------------
PRO BatchPlotter::setPhotonModel, model

    self.PhotonModel = model
    
END


; ----------------------------------------------------------------------------
; Update the current list of detectors
; ----------------------------------------------------------------------------
PRO BatchPlotter::setDetectorList, DetectorList

    self.DetectorList = DetectorList
    
END

; ----------------------------------------------------------------------------
; Set the batch fit info list, source of all plot data for this plotter
; ----------------------------------------------------------------------------
PRO BatchPlotter::setBatchFitList, BatchFitList

    self.BatchFitList = BatchFitList
    
END

;------------------------------------------------------------------------------
; Get the batch fit status
;------------------------------------------------------------------------------
FUNCTION BatchPlotter::haveBatch 

    RETURN, self.haveBatch

END

;------------------------------------------------------------------------------
; Get the duration calculation status
;------------------------------------------------------------------------------
FUNCTION BatchPlotter::haveDuration 

    RETURN, self.haveDuration

END

;------------------------------------------------------------------------------
; We have a new batch fit - clear out the old duration info:
;------------------------------------------------------------------------------
PRO BatchPlotter::setDurStatus, tf

    self.haveDuration = 0L
    self.haveFluxFluence = 0L
    self.haveDurFromFile = 0L
    self.haveDurBacks = INTARR(2)
    self.skipDurBack = 0L

END

;------------------------------------------------------------------------------
; Accessors for the Duration results
;------------------------------------------------------------------------------
FUNCTION BatchPlotter::haveDuration  & RETURN, self.haveDuration   & END
FUNCTION BatchPlotter::getDurations  & RETURN, self.durInfo        & END
FUNCTION BatchPlotter::haveFluxFluence  & RETURN, self.haveFluxFluence   & END

;------------------------------------------------------------------------------
; We have read a batch fit file, set the appropriate member data
;------------------------------------------------------------------------------
PRO BatchPlotter::setReadFilename, rf

    PTR_FREE, self.readFilename
    self.readFilename = PTR_NEW (rf)
    self.haveBatch = 0
    self.rangeList->clear
    self->setupLabelList

END

;------------------------------------------------------------------------------
; Set the Stack Plot Option Choice
;------------------------------------------------------------------------------
PRO BatchPlotter::setStackChoice, myChoice

    self.stackChoice = myChoice

END

;------------------------------------------------------------------------------
; We have a batch fit - notify the other responsible parties
;------------------------------------------------------------------------------
PRO BatchPlotter::setBatchStatus, tf

    self.haveBatch = tf

END

; ----------------------------------------------------------------------------
; Definition of BatchPlotter
; ----------------------------------------------------------------------------
PRO BatchPlotter__define

    obj = { BatchPlotter, INHERITS PLOTTER, $
      
        Color            : OBJ_NEW (), $    ; Color model information
        
        haveBatch        : 0L,         $    ; Do we have data yet?
        FitModel         : OBJ_NEW (), $    ; Fit results
        PhotonModel      : OBJ_NEW (), $    ; Photon term labels, etc.
        DetectorList     : OBJ_NEW (), $    ; List of current detectors       
        BatchFitList     : OBJ_NEW (), $    ; List of batch fit results
        
        labelList        : PTR_NEW (), $    ; Parameter label list (STR_ARR)  
        readFilename     : PTR_NEW (), $    ; Filename to read batch fit data
        selectMode       : LONARR (5), $    ; Chosen plot mode
        firstSelected    : 0L,         $    ; For multiple parameter plots, first selected
        selection        : 0L,         $    ; Parameter selection, second  
        stackChoice      : 0L,         $    ; Keep track of stack plot option
        numHistoBins     : 0L,         $    ; Current number of histogram bins
        cancel           : 0L,         $    ; User cancelled selection  

        haveDuration     : 0L,         $    ; Indicates whether a duration has been calculated
        haveFluxFluence  : 0L,         $    ; Indicates whether the flux/fluence calculation has been done
        skipDurBack      : 0L,         $    ; Indicates the background selection has been skipped
        haveDurFromFile  : 0L,         $    ; The background selection has been read from a file
        haveDurBacks     : INTARR(2),  $    ; Indicates which durations background levels exist
        durInt           : INTARR(2),  $    ; Energy interval for durations calculations

        durInfo          : { DURINFO,  $

			peakFLux         : 0.0,        $    ; Peak flux of the burst...
			peakFLuxErr      : 0.0,        $    ; Uncertainty in the Peak flux of the burst...
			peakFLuxBeg      : 0.0,        $    ; Start time of Peak Flux Interval...
			peakFluxBATSE    : 0.0,        $    ; Peak flux of the burst in the standard band...
			peakFLuxBErr     : 0.0,        $    ; Uncertainty in the Peak flux of the burst...
			peakFLuxBBeg     : 0.0,        $    ; Start time of Peak Flux Interval...
			peakInterval     : 0.0,        $    ; Interval of the peak flux
			peakFLux64       : 0.0,        $    ; Peak flux of the burst...
			peakFLux64Err    : 0.0,        $    ; Uncertainty in the Peak flux of the burst...
			peakFLux64Beg    : 0.0,        $    ; Start time of Peak Flux Interval...
			peakFlux64BATSE  : 0.0,        $    ; Peak flux of the burst in the standard band...
			peakFLux64BErr   : 0.0,        $    ; Uncertainty in the Peak flux of the burst...
			peakFLux64BBeg   : 0.0,        $    ; Start time of Peak Flux Interval...
			peakInterval64   : 0.0,        $    ; Interval of the peak flux
			peakFLux256      : 0.0,        $    ; Peak flux of the burst...
			peakFLux256Err   : 0.0,        $    ; Uncertainty in the Peak flux of the burst...
			peakFLux256Beg   : 0.0,        $    ; Start time of Peak Flux Interval...
			peakFlux256BATSE : 0.0,        $    ; Peak flux of the burst in the standard band...
			peakFLux256BErr  : 0.0,        $    ; Uncertainty in the Peak flux of the burst...
			peakFLux256BBeg  : 0.0,        $    ; Start time of Peak Flux Interval...
			peakInterval256  : 0.0,        $    ; Interval of the peak flux
			fluence          : 0.0,        $    ; Energy fluence of the burst...
			fluenceErr       : 0.0,        $    ; Uncertainty in the fluence of the burst...
			fluenceBATSE     : 0.0,        $    ; Photon fluence of the burst (BATSE)...
			fluenceErrBATSE  : 0.0,        $    ; Uncertainty in the fluence of the burst (BATSE)...
			durations        : FLTARR(4),  $    ; The T90 and T50 start and end times
			durationLevels   : FLTARR(4),  $    ; The T90 and T50 calculation levels
			durationErrors   : FLTARR(2),  $    ; The T90 and T50 calculation errors
			durBackLevels    : FLTARR(2),  $    ; The background levels for duration calculation
			fluBackLevels    : FLTARR(2),  $    ; The background levels for duration calculation
			durBackInt       : FLTARR(2,2) $    ; The background intervals for duration calculation

        }, $

        minTimeBin       : 0L,         $    ; Index of the edge of the lower duration background
        maxTimeBin       : 0L,         $    ; Index of the edge of the upper duration background
        havePlotChoice   : 0L,         $    ; We have selected the type of plot
        termListID       : 0L,         $    ; List of possible photon terms
        chooseModeID     : 0L,         $    ; Allow user to choose plotting modes
        textID           : 0L,         $    ; Allow user to determine number of bins
        upperLimitSigma  : 0.0,        $              
        defaultPlotPos   : FLTARR(4)   $

    }
    
END
