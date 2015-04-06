; ----------------------------------------------------------------------------
;+
; NAME:
;     MfitDisplay (OBJECT)
;
; PURPOSE:
;     An object for display of model fit results (spectra and residuals).
;
; CALLING SEQUENCE:
;     obj = OBJ_NEW ('MfitDisplay')
;
; INPUTS:
;     NONE
;
; KEYWORDS:
;     NONE
;
; INHERITS:
;     Display
;
; DEPENDENCIES:
;     display__define.pro
;
; METHODS:
;     
;     add (PROCEDURE) - Add data to the list
;         Inputs: KEY  : STRING name denoting the data
;                 DATA : data to add to the list (can be any IDL datatype)
;        Outputs: NONE 
;       Keywords: NONE 
;
; MODIFICATION HISTORY:
;
;     Written, 1999 December, Robert.Mallozzi@msfc.nasa.gov
;
;-
; ----------------------------------------------------------------------------



; ----------------------------------------------------------------------------
; Constructor
; ----------------------------------------------------------------------------
FUNCTION MfitDisplay::init, _EXTRA = extra
               
    title = 'Fit Display'

    IF (NOT self->Display::init (MAP = 0, TITLE = title, $
       /NO_DEFAULT_MENUS, _EXTRA = extra)) $
       THEN BEGIN

       MESSAGE, /CONTINUE, 'Display initialization failed.'
       RETURN, 0

    ENDIF

    self.Plotter = OBJ_NEW ('ModelPlotter', self)
    self.BatchPlotter = OBJ_NEW ('BatchPlotter', self)
            
    ;== Default fluence interval for flux / fluence integration (keV):
    
    self.fluenceInt = !RM_PARAMS.fluxint
    self.durationInt = !RM_PARAMS.durint
    
    ;== Set default plotting colors
    
    self->setColors, /INITIALIZE

    self->buildGUI

;    IF KEYWORD_SET (FILENAME) THEN BEGIN
;        self->readFit, Filename
;    ENDIF
;    
    RETURN, 1

END

   
; ----------------------------------------------------------------------------
; Destructor
; ----------------------------------------------------------------------------
PRO MfitDisplay::cleanup

    OBJ_DESTROY, self.Plotter
    OBJ_DESTROY, self.BatchPlotter
    OBJ_DESTROY, self.dataID
    PTR_FREE,    self.readFilename
    PTR_FREE,    self.dataMenuID
    
    IF (WIDGET_INFO (self.widgetID.top, /VALID)) THEN $
       WIDGET_CONTROL, self.widgetID.top, /DESTROY

    self->Display::cleanup

END


; ----------------------------------------------------------------------------
; Build the display widget
; ----------------------------------------------------------------------------
PRO MfitDisplay::buildGUI

    ;== Menubar

    menu = self->addMenu (VALUE = 'File')
        id = menu->add (VALUE = 'Print')
        ;id = menu->add (VALUE = 'Print Setup...')
        id = menu->add (VALUE = 'PS Configure...')
        id = menu->add (VALUE = 'No Fit Info on Plot')
        self.showPlotNotes = 0
        id = menu->add (VALUE = 'Screenshot')                                                  ; AMG
        id = menu->add (VALUE = 'Dismiss', /SEPARATOR)

    self.dataID = self->addMenu (VALUE = 'Dataset')
        id = self.dataID->add (VALUE = 'All', UVALUE = 'datamenu')

    menu = self->addMenu (VALUE = 'Options')
        id = menu->add (VALUE = 'Show rmfit Window')
        subMenu = menu->add (VALUE = 'Colors', /MENU)
            id = menu->add (subMenu, VALUE = 'Background')
            id = menu->add (subMenu, VALUE = 'Foreground')
            id = menu->add (subMenu, VALUE = 'History')
            id = menu->add (subMenu, VALUE = 'Spectrum')
            id = menu->add (subMenu, VALUE = 'Background Model')
        id = menu->add (VALUE = 'Show Fit Log', /SEPARATOR)
        id = menu->add (VALUE = 'Plot Configuration')
        id = menu->add (VALUE = 'Set Fluence Energies')
        id = menu->add (VALUE = 'Set Duration Energies')
        id = menu->add (VALUE = 'Reset Duration Calculation')
        
    menu = self->addMenu (VALUE = 'Help', /HELP)
        id = menu->add (VALUE = 'Fit Display')
        id = menu->add (VALUE = 'Spectral Fitting')
        id = menu->add (VALUE = 'About', /SEPARATOR)


    ;== Application buttons

    menu = self->addMenuButton (VALUE = 'Zoom:', /TEAROFF)
        id = menu->add (VALUE = 'Zoom')
        id = menu->add (VALUE = 'Zoom X')
        id = menu->add (VALUE = 'Zoom Y')
        id = menu->add (VALUE = 'Full Screen')

    menu = self->addMenuButton (VALUE = 'Fit Results:', /TEAROFF)
        id = menu->add (VALUE = 'Dump Results to Log')
        id = menu->add (VALUE = 'Write Results to File')
        id = menu->add (VALUE = 'Write Fit Params Only')
        id = menu->add (VALUE = 'Read Fit Results File')
        ;id = menu->add (VALUE = 'Construct Simulation')
	button = self->addButton ('Redo Last Fit')
    menu = self->addMenuButton (VALUE = 'Spectral Fitting:', /TEAROFF)
        ;id = menu->add (VALUE = 'Redo Last Fit')
        id = menu->add (VALUE = 'Synthesize Burst')
        id = menu->add (VALUE = 'Error Interval')
        id = menu->add (VALUE = 'ChiSQ 1D Plot')
        id = menu->add (VALUE = 'ChiSQ 2D Plot')
        id = menu->add (VALUE = 'Fit One Spectrum')
        id = menu->add (VALUE = 'Fit One Interval')
        id = menu->add (VALUE = 'Fit Selections')
        id = menu->add (VALUE = 'Batch Fit Selections')

	menu = self->addMenuButton (VALUE = 'Fit Display Options:', /TEAROFF)
	    id = menu->add (VALUE = 'Cumulative')
	    id = menu->add (VALUE = 'Raw Counts')
	    id = menu->add (VALUE = 'Counts Spectrum')
	    id = menu->add (VALUE = 'Photon Spectrum')
	    id = menu->add (VALUE = 'Energy Spectrum')
	    id = menu->add (VALUE = 'Nu Fnu Spectrum')

    specBaseID = WIDGET_BASE (self.widgetID.buttonBase, $
                 COLUMN = 1, MAP = 0L)    ;/GRID, == NOTE BENE: All buttons 
                                          ; would be as high as the CW_BGROUP!
	logChoice = ['X Log', 'Y Log']
	self.logChoiceID = CW_BGROUP (specBaseID, logChoice, /ROW, $
            /NONEXCLUSIVE, SET_VALUE = [1,1])
        
    label = WIDGET_LABEL (specBaseID, VALUE = 'Model Display Options:')
	modChoice = ['Show', 'Hide']
;	self.showModel = CW_BGROUP (specBaseID, modChoice, /ROW, $
;            /EXCLUSIVE, SET_VALUE = 1)
	self.showModel = self->addButtonGroup (specBaseID, modChoice, /ROW, $
            /EXCLUSIVE, SET_VALUE = 1)
	label = WIDGET_LABEL (specBaseID, VALUE = 'Residual Display Options:')
	choice = ['Sigma Residuals', 'Counts Residuals', 'No Residuals']
	self.residChoiceID = CW_BGROUP (specBaseID, choice, /COLUMN, $
		/EXCLUSIVE, SET_VALUE = 0)
	self.specBaseID = specBaseID
    
    
    gridBaseID = WIDGET_BASE (self.widgetID.buttonBase, $
                 /GRID, COLUMN = 1, MAP = 0L)    
        label = WIDGET_LABEL (gridBaseID, VALUE = 'Batch Plot Options:')
        button = self->addButton (gridBaseID, 'Fit Parameters...')
        ;button = self->addButton (gridBaseID, 'Burst Duration')
        button = self->addButton (gridBaseID, 'Residuals Contours')
        button = self->addButton (gridBaseID, 'NuFnu Contours')
    stackmenu = self->addMenuButton (PARENT = gridBaseID, VALUE = 'Stack Spectra:')
        id = stackmenu->add (VALUE = 'Stack Photon Spectra')
        id = stackmenu->add (VALUE = 'Stack Energy Spectra')
        id = stackmenu->add (VALUE = 'Stack NuFnu Spectra')
    self.gridBaseID = gridBaseID

    ;== Add a text message in the status bar  
   
    self->setStatus, 'Display initialized' ;, 5
             
END


; ----------------------------------------------------------------------------
; Override the widget event handler
; ----------------------------------------------------------------------------
PRO MfitDisplay::eventHandler, event

    self->setWindow
    
    WIDGET_CONTROL, self.residChoiceID, GET_VALUE = residChoice
    
    CASE (TAG_NAMES (event, /STRUCTURE)) OF

        'WIDGET_KILL_REQUEST': BEGIN

            ;== Dismiss, not kill

            WIDGET_CONTROL, event.top, MAP = 0 
            END

        'WIDGET_BUTTON': BEGIN

            WIDGET_CONTROL, event.id, GET_VALUE = value

            CASE (value) OF

                'Dismiss'           : self->map, 0

                'Print'             : self->Display::eventHandler, event
                'Print Setup...'    : self->Display::eventHandler, event
                'PS Configure...'   : self->Display::eventHandler, event
                'Print Fit Info on Plot' : BEGIN
                    self.showPlotNotes = 0
                    WIDGET_CONTROL, event.id, SET_VALUE = 'No Fit Info on Plot'
                    self->Plot, _EXTRA = *self.PlotOptions
                    END
                
                'No Fit Info on Plot' : BEGIN
                    self.showPlotNotes = 1
                    WIDGET_CONTROL, event.id, SET_VALUE = 'Print Fit Info on Plot'
                    self->Plot, _EXTRA = *self.PlotOptions
                    END
                
                'Screenshot'         : BEGIN                                ; AMG
																			;
					 file=DIALOG_PICKFILE(DEFAULT_EXTENSION='png',  $       ;
										  PATH=!RM_PARAMS.lastPath, $       ;
										  FILE='grbfit.png',        $       ;
										  /WRITE)                           ;
					 IF file EQ '' THEN RETURN                              ;
					 image=TVRD(/TRUE)                                      ;
					 TVLCT,r,g,b,/GET                                       ;
					 WRITE_PNG, file, image, r, g, b                        ;
																			;
                                       END                                  ;
                'Show Fit Log'       : self.logger->show
                'Show rmfit Window'  : WIDGET_CONTROL, (self.myrmfit).topID, /show
                
                ;---------------
                ; COLORS
                ;---------------  
                    
                'Background'         : self->setColors, value
                'Foreground'         : self->setColors, value
                'History'            : self->setColors, value
                'Spectrum'           : self->setColors, value
                'Background Model'   : self->setColors, value
                'Plot Configuration' : BEGIN
                    self->setPlotOptions
                    self->plot, _EXTRA = *self.PlotOptions
                    END
                'Set Duration Energies'  : self->set_durationInt
                'Set Fluence Energies'  : self->set_fluenceInt

                'Zoom'              : self->zoom
                'Zoom X'            : self->zoom, /X
                'Zoom Y'            : self->zoom, /Y
                'Full Screen'       : self->zoom, /FULLSCREEN 

                ; Fitting
                'Redo Last Fit'     : self->refitSpectra, /SELECT_MODEL, /REFIT
                'Synthesize Burst'  : self->synthesize
                'Error Interval'    : self->doChiSq, /INTERVAL
                'ChiSQ 1D Plot'     : self->doChiSq, /CHI1D
                'ChiSQ 2D Plot'     : self->doChiSq, /CHI2D
                'Fit One Spectrum'  : self->refitSpectra, /SELECT_MODEL, /SINGLEBIN
                'Fit Selections'    : self->refitSpectra, /SELECT_MODEL, /SELECTIONS
                'Fit One Interval'  : self->refitSpectra, /SELECT_MODEL, /SINGLEINTERVAL
                'Batch Fit Selections' : self->batchfitSpectra
                'Cumulative'        : self->plot, _EXTRA = *self.PlotOptions, /CUMULATIVE
                'Raw Counts'        : self->plot, _EXTRA = *self.PlotOptions, /RAW
                'Counts Spectrum'   : self->plot, _EXTRA = *self.PlotOptions, /COUNTS
                'Photon Spectrum'   : self->plot, _EXTRA = *self.PlotOptions, /PHOTONS
                'Energy Spectrum'   : self->plot, _EXTRA = *self.PlotOptions, /ENERGY
                'Nu Fnu Spectrum'   : self->plot, _EXTRA = *self.PlotOptions, /NUFNU

                'Show'              : self->plot, _EXTRA = *self.PlotOptions
                'Hide'              : self->plot, _EXTRA = *self.PlotOptions
                                
                'Sigma Residuals'   : self->plot, _EXTRA = *self.PlotOptions
                'Counts Residuals'  : self->plot, _EXTRA = *self.PlotOptions
                'No Residuals'      : self->plot, _EXTRA = *self.PlotOptions

                'Dump Results to Log'   : self->dumpFit ;, /CLEAR
                'Write Results to File' : self->writeFit, /FULL
                'Write Fit Params Only' : self->writeFit
                'Read Fit Results File' : self->readFit ;, _EXTRA = *self.PlotOptions
                    ;self->plot, _EXTRA = *self.PlotOptions, /BATCH, /RESID
                    ;(self.BatchPlotter)->setBatchStatus, 1
                
                'Fit Parameters...' : BEGIN
                    self.plotType = 'batch'
                    self.BatchPlotter->selectBatchFit, _EXTRA = *self.PlotOptions
                    END
                    
                'Burst Duration' : BEGIN
                    self.plotType = 'duration'
                    self.BatchPlotter->plotDurations, _EXTRA = *self.PlotOptions
                    END

                'Reset Duration Calculation' : BEGIN
                    self.plotType = 'duration'
                    self.BatchPlotter->plotDurations, /CALCULATE, _EXTRA = *self.PlotOptions
                    END

                'Residuals Contours': BEGIN
                    self.haveDefaultRange = 0
                    self->plot, _EXTRA = *self.PlotOptions, /SIGMA
                    END
                    
                'NuFnu Contours': BEGIN
                    self.haveDefaultRange = 0
                    self->plot, _EXTRA = *self.PlotOptions, /TOPOMAP
                    END
                    
                'Stack Photon Spectra': BEGIN
                    batchPlotter = self.batchPlotter
                    batchPlotter->setStackChoice, 0
                    self->plot, _EXTRA = *self.PlotOptions, /STACK
                    END

                'Stack Energy Spectra': BEGIN
                    batchPlotter = self.batchPlotter
                    batchPlotter->setStackChoice, 1
                    self->plot, _EXTRA = *self.PlotOptions, /STACK
                    END

                'Stack NuFnu Spectra': BEGIN
                    batchPlotter = self.batchPlotter
                    batchPlotter->setStackChoice, 2
                    self->plot, _EXTRA = *self.PlotOptions, /STACK
                    END

                'Fit Display'       : XDISPLAYFILE, !MFIT.HELP_PATH + 'fitplotter.hlp',    DONE = 'Done'
                'Spectral Fitting'  : XDISPLAYFILE, !MFIT.HELP_PATH + 'spectralfit.hlp',    DONE = 'Done'
                                        
                ELSE : BEGIN
                
                    WIDGET_CONTROL, event.id, GET_UVALUE = uvalue
                    IF (N_ELEMENTS (uvalue) EQ 0) THEN RETURN
                    
                    IF (uvalue EQ 'datamenu') THEN BEGIN
                    
                       IF NOT OBJ_VALID (self.DetectorList) THEN RETURN
                       names  = ['All',  self.DetectorList->names()]
                       idx = WHERE (names EQ value, cnt)
                       IF (cnt EQ 0) THEN BEGIN
                           MESSAGE, 'Failed to find requested dataset.'
                           ENDIF

                       self->setStatus, names[idx]
                       
                       IF (value EQ 'All') THEN BEGIN
                           idx = WHERE (names[*] NE value, cnt)
                       ENDIF   
                       
                       ;== Unusual error where there are no datasets at all:
                       IF (self.DetectorList->count () EQ 0) THEN cnt = 0
                       ;== Show the selected displays
                       FOR k = 0, cnt - 1 DO BEGIN
                           myDet = self.DetectorList->data (names[idx[k]])
                           myDisp = myDet.display
                           myDisp->map
                       ENDFOR
                       ;self->plot, _EXTRA = *self.PlotOptions

                    ENDIF
                    END
                                                     
            ENDCASE
            
              
            END ; WIDGET_BUTTON events
        
        'WIDGET_TEXT_CH': self->keyBindings, event.ch
        
        ELSE: BEGIN

            ;== Handle the radio buttons; name is *not* returned in 'value'
            WIDGET_CONTROL, event.id, GET_VALUE = value

			IF (event.id EQ self.logChoiceID) THEN self->plot, _EXTRA = *self.PlotOptions 
			IF (event.id EQ self.showModel) THEN self->plot, _EXTRA = *self.PlotOptions 
			IF (event.id EQ self.residChoiceID) THEN self->plot, _EXTRA = *self.PlotOptions      ;'No Residuals'     
			;		ELSE: 
	 
			;		ENDCASE 
            ;    ENDIF
            
            END

;self->setStatus, 'Event not handled.' , 2
                       
    ENDCASE
      
END

            
; ----------------------------------------------------------------------------
; Handle keyboard events in the drawing area
; ----------------------------------------------------------------------------
PRO MfitDisplay::keyBindings, char
    
    ; COMPILE_OPT HIDDEN

    CASE (STRING (char)) OF

         'd' : WIDGET_CONTROL, self.widgetID.top, MAP = 0
         
         'z' : BEGIN
                   self->disable
				   self->zoom
				   self->enable
               END
         
         'r' : self->Plot, _EXTRA = *self.PlotOptions
         
         'f' : self->zoom, /FULLSCREEN

         'x' : BEGIN
                   self->disable
				   self->zoom, /X
				   self->enable
               END

         'y' : BEGIN
                   self->disable
				   self->zoom, /Y
				   self->enable
               END

         'c' : self->plot, _EXTRA = *self.PlotOptions, /COUNTS
         
         'p' : self->plot, _EXTRA = *self.PlotOptions, /PHOTONS
         
         'e' : self->plot, _EXTRA = *self.PlotOptions, /ENERGY
         
         'n' : self->plot, _EXTRA = *self.PlotOptions, /NUFNU
         
         ELSE: 
         
    ENDCASE
         
END


; ----------------------------------------------------------------------------
; Set up to do the chisquare mapping, using the current data set
; ----------------------------------------------------------------------------
PRO MfitDisplay::doChiSq, CHI1D = chi1d, CHI2D = chi2d, INTERVAL = interval, _EXTRA = extra

    ;== Number of detectors currently loaded
    IF NOT OBJ_VALID (self.DetectorList) THEN RETURN
    numDet = self.DetectorList->count ()
    names  = self.DetectorList->names ()

    IF (numDet LT 1) THEN BEGIN
       self->setStatus, 'No detector data available.', 5, /REVERT
       RETURN
    ENDIF
        
    ;== These things can happen...
    IF NOT OBJ_VALID (self.FitModel) THEN RETURN
    IF NOT self.FitModel->haveFit () THEN RETURN
    
    ans = 'Yes'
    IF ((KEYWORD_SET (CHI1D) AND self.fitter->haveChi1D ()) OR $
        (KEYWORD_SET (CHI2D) AND self.fitter->haveChi2D ())) THEN BEGIN
       msg = 'Redo Chisq calculation with new parameters or ranges?'
       ans = DIALOG_MESSAGE( msg, /QUESTION)
    ENDIF
    
    
    IF (ans EQ 'Yes') THEN BEGIN
    	
    	IF KEYWORD_SET(INTERVAL) THEN CHI1D = 1
    	
    	(self.PhotonModel)->selectParams, $
                   self.fitModel->get_term_used(), self.fitModel->get_param(), $
                   self.fitModel->get_param_uncer(), theSelection, theRanges, $
                   displaySigma, numInterest, $
                   CHI1D = chi1d, CHI2D = chi2d, INTERVAL = interval
                     
        ;== Now check to see that something has been selected
        selected = WHERE (theSelection, gotCount)
        IF (gotCount EQ 0 ) THEN BEGIN
            message = 'Choose at least one parameter'
            self->setStatus, message, 5, /REVERT
            RETURN
        ENDIF
            
        IF (KEYWORD_SET (chi1d) AND (gotCount GT 1) AND NOT KEYWORD_SET(INTERVAL)) THEN BEGIN
            message = 'Choose at most one parameter'
            self->setStatus, message, 5, /REVERT
            RETURN
        ENDIF
                    
        IF (gotCount GT 2) AND NOT KEYWORD_SET(INTERVAL) THEN BEGIN
            message = 'Choose at most two parameters'
            self->setStatus, message, 5, /REVERT
            RETURN
        ENDIF
                    
        IF KEYWORD_SET (CHI1D) THEN numSteps = 51 $
                               ELSE numSteps = 21 ; For now!
                               
        self.fitter->fitChiSq, theSelection, theRanges, numSteps, $
                displaySigma, numInterest, $
                CHI1D = chi1D, CHI2D = chi2d, INTERVAL = interval, _EXTRA = extra
                
    ENDIF ELSE BEGIN

        (self.PhotonModel)->selectParams, $
                   self.fitModel->get_term_used(), self.fitModel->get_param(), $
                   self.fitModel->get_param_uncer(), theSelection, theRanges, $
                   displaySigma, numInterest, $
                   CHI1D = chi1d, CHI2D = chi2d, /REPLOT
                   
    ENDELSE
    
    ;== Store useful plotting parameters for chisq plots
    self.plotter->setDisplaySigma, displaySigma
    self.plotter->setNumInterest, numInterest
        
    IF NOT KEYWORD_SET(INTERVAL) THEN BEGIN
         self->plot, CHI1D = chi1D, CHI2D = chi2d, _EXTRA = *self.PlotOptions  
         self->map
    ENDIF
           
END

; ----------------------------------------------------------------------------
; Refit the current data set
; ----------------------------------------------------------------------------
PRO MfitDisplay::refitSpectra, _EXTRA = extra

    ;== Number of detectors currently loaded
    IF NOT OBJ_VALID (self.DetectorList) THEN RETURN
    numDet = self.DetectorList->count ()
    names  = self.DetectorList->names ()

    IF (numDet LT 1) THEN BEGIN
       self->setStatus, 'No detector data available.', 5, /REVERT
       RETURN
    ENDIF
    
    ;== These things can happen...
    IF NOT OBJ_VALID (self.FitModel) THEN RETURN
    IF NOT self.FitModel->haveFit () THEN RETURN
    
    self.fitter->fitModel, ENERGYINTERVAL = self.fluenceInt, _EXTRA = extra
    self->plotModel, _EXTRA = *self.PlotOptions  
           
END

; ----------------------------------------------------------------------------
; Create a synthetic burst from the current model
; ----------------------------------------------------------------------------
PRO MfitDisplay::synthesize, _EXTRA = extra

    ;== Number of detectors currently loaded
    IF NOT OBJ_VALID (self.DetectorList) THEN RETURN
    numDet = self.DetectorList->count ()
    names  = self.DetectorList->names ()

    IF (numDet LT 1) THEN BEGIN
       self->setStatus, 'No detector data available.', 5, /REVERT
       RETURN
    ENDIF
    
    ;== These things can happen...
    IF NOT OBJ_VALID (self.FitModel) THEN RETURN
    IF NOT self.FitModel->haveFit () THEN RETURN
    
    ;== Set the model; found in returned photModel
    self.fitter->fitTemplate, numChan, fitChan, liveTime, chanEnergy, $
                 chanWidth, photModel, status
    IF (status NE 0) THEN BEGIN
	    ;WIDGET_CONTROL, self.gridBaseID, MAP = 0L
        RETURN
        ENDIF
    
    ;== For now, we only synthesize one detector's worth
    use_det = LONARR (numDet)
    IF numDet GT 1 THEN BEGIN
        use_det = DIALOG_CHECKLIST (names, INITIAL = use_det, $
              TITLE = 'Select One Dataset to Synthesize', /EXCLUSIVE) 
        IF TOTAL (use_det) EQ 0 THEN RETURN
    ENDIF ELSE BEGIN
        ;== Assume the user just wants to use the one dataset!
        use_det[0] = 1
    ENDELSE
    
    midx = WHERE(use_det)
    lo = (fitChan[0, midx])[0]
    hi = (fitChan[1, midx])[0]
    
    obsCounts = photmodel[*, 0, midx] * chanWidth[*, midx]
    mySeed = FLOAT(FIX(ABS(randomn(cake)*10000)))
          
    ;== Parameters for the synthesized data:
    result = DIALOG_INPUT (NFIELDS = 4, $
           PROMPT = ['Background Livetime (s): ', $
                     'Source Livetime (s):     ', $
                     'Number of Spectra (>0):  ', $
                     'Random Number Seed (>0): '], $
           INITIAL = [100., 1., 100., mySeed], $
           TITLE = 'Input Synthetic Burst Parameters: ')
          
    IF ((SIZE (result))[0] EQ 0) THEN RETURN ; User cancelled!
    
    ;== Set up to suggest a file name and ask user about it:
    det = self.DetectorList->data (names[midx])
    reader = det.detector->dataReader ()
    path = reader->filename (/PATH)
    root = reader->filename (/ROOT)
       
    f = root + '_' + STRTRIM(STRING(LONG(result[3])), 2) + '.pha' 

    out_file = DIALOG_PICKFILE ( $
              TITLE = 'Select a Filename for Writing Results', $
              FILE = f, FILTER = '*.pha', $
              PATH = !RM_PARAMS.lastPath, GET_PATH = lastPath, $
              DIALOG_PARENT = self.widgetID.top, $
              /WRITE, /OVERWRITE_PROMPT)           ; <---Changed for Mac!
	
    IF (out_file EQ '') THEN RETURN  ; Cancel
    !RM_PARAMS.lastPath = lastPath
          
    ; User may have slipped up! Filename is only a path:
    IF ((STRPOS(out_file, '/', /REVERSE_SEARCH) + 1) EQ STRLEN(out_file)) THEN BEGIN
        out_file = out_file + f               ;TBD: ask again?
        self->setStatus, 'Writing file: ' + out_file, 10, /REVERT
	ENDIF
	; If the user accidently puts a space in front of the filename...
	retpos = STRPOS(out_file, STRING(10B))
	IF (retpos NE 0) THEN BEGIN
	   out_file = STRMID(out_file, retpos + 1)
	ENDIF

    ;== 5/20/09 RDP: Coerce the results to be floats before implicit conversion from STRARR[i]:
    backLive = 0.
	backLive = result[0]
	obsLive = 0.
	obsLive = result[1]
    numSpectra = FIX(result[2])
    seed = LONG(result[3])
	;== Obtain the background from the model:
	myDisplay = det.display
	myObsTime = (liveTime[lo, midx])[0]
	myBack = myDisplay.background->evalModel(0.0, myObsTime)
	myBack = TRANSPOSE(myBack) * backLive ;myObsTime
	synthBack = myBack
	
    ;== Main task: create the synthetic burst
    synthRates = FLTARR(numChan[midx], numSpectra + 2) 
    synthErrs = FLTARR(numChan[midx], numSpectra + 2)
    synthTimes = FLTARR(2, numSpectra + 2)
    synthTimes[0, 0] = -backLive
    synthTimes[1, 0] = 0.0
    synthTimes[0, 1: numSpectra] = FINDGEN(numSpectra) * obsLive
    synthTimes[1, 1: numSpectra] = (FINDGEN(numSpectra) + 1) * obsLive
    synthTimes[0, numSpectra + 1] = synthTimes[1, numSpectra]
    synthTimes[1, numSpectra + 1] = synthTimes[1, numSpectra] + backLive
    
    ;== IDL's RANDOMN makes a vector, but only for one value of the POISSON mean:
    FOR jj = lo, hi DO BEGIN
    	;== 01/07/09 RDP: Implement CCR# 122, where the backgrounds are random for each 
    	; spectrum. To do this, we first store the high-resolution background rate:
    	IF (myBack[jj] NE 0) THEN $ 
		;synthRates[jj, 0] = RANDOMN(seed, 1L, $
		;	POISSON = myBack[jj], /DOUBLE)
		synthRates[jj, 0] = myBack[jj]
			
		IF ((obsCounts[jj] NE 0) OR (myBack[jj] NE 0)) THEN $
		synthRates[jj, 1: numSpectra] = RANDOMN(seed, numSpectra, $
			POISSON = (obsCounts[jj] + $
			myBack[jj] / backLive) * obsLive, /DOUBLE)
		
		IF (myBack[jj] NE 0) THEN $
		;synthRates[jj, numSpectra + 1] = RANDOMN(seed, 1L, $
		;	POISSON = myBack[jj], /DOUBLE)
		synthRates[jj, numSpectra + 1] = myBack[jj]
            
    ENDFOR
    
    ;== Determine what type of file to create BFITS or GBM or ???
    IF OBJ_ISA(reader, 'BFITSReader') THEN BEGIN
    
		;== Prepare the data for writing into a BFITS file:
		posIdx = WHERE(synthRates GT 0.)
		synthErrs[posIdx] = SQRT(synthRates[posIdx])
		synthRates[*, 0] = synthRates[*, 0] / backLive
		synthErrs[*, 0] = synthErrs[*, 0] / backLive
		synthRates[*, 1: numSpectra] = synthRates[*, 1: numSpectra] / obsLive
		synthErrs[*, 1: numSpectra] = synthErrs[*, 1: numSpectra] / obsLive
		synthRates[*, numSpectra + 1] = synthRates[*, numSpectra + 1] / backLive
		synthErrs[*, numSpectra + 1] = synthErrs[*, numSpectra + 1] / backLive

    	synth_bfits, reader->filename (), out_file, numSpectra + 2, $
                 synthTimes, synthRates, synthErrs
                 
    ENDIF ELSE IF (OBJ_ISA(reader, "PHAReader") OR OBJ_ISA(reader, "PHAIIReader") $
        OR OBJ_ISA(reader, "TTEReader")) THEN BEGIN
        
		;== Prepare the data for writing into a PHA type II file:
		;== Populate the livetime array:
		livetime = FLTARR(numSpectra + 2)
		livetime[0] = backLive
		livetime[1: numSpectra] = obsLive
		livetime[numSpectra + 1] = backLive

		allheaders  = reader->header ()   
		header      = allheaders.ext0
		;= Get the energy info:
		data        = reader->data ()
		eThresholds = (data).eedges
		
		;= File CREATOR info:
		vers = 'rmfit ' + !RM_PARAMS.VERSION
		
		myChan = numChan[midx]

		WritePHAIIFile, out_file, header, myChan[0], numSpectra + 2, $
				synthTimes, eThresholds, synthRates, livetime, vers, $
				SEED = LONG(result[3])

		;self->setStatus, 'Not currently handled! ', 5, /REVERT
        RETURN
    ENDIF
                 
    self->setStatus, 'Wrote file: ' + out_file, 5, /REVERT
              
END

; ----------------------------------------------------------------------------
; Fit the current data selection as batch fit
; ----------------------------------------------------------------------------
PRO MfitDisplay::batchfitSpectra, _EXTRA = extra

    ;== These things can happen...
    IF NOT OBJ_VALID (self.DetectorList) THEN RETURN
    ;IF NOT OBJ_VALID (self.FitModel) THEN RETURN
    ;IF NOT self.FitModel->haveFit () THEN RETURN
    
    ;== Number of detectors currently loaded
    numDet = self.DetectorList->count ()
    names  = self.DetectorList->names ()

    IF (numDet LT 1) THEN BEGIN
       self->setStatus, 'No detector data available.', 5, /REVERT
       RETURN
    ENDIF
    
    self.fitter->fitBatch, status, _EXTRA = extra
    IF (status NE 0) THEN BEGIN
	    WIDGET_CONTROL, self.gridBaseID, MAP = 0L
        RETURN
        ENDIF
    
    ;== Set up for dumping the fit results:
    ;We are going to dump the info before plotting
    self->setFitModel,     self.Fitter->getFitModel ()
    self->setPhotonModel,  self.Fitter->getPhotonModel ()
    self->setDetectorList, self.Fitter->getDetectorList ()
    self->setTermInfo,     self.Fitter->getTermInfo ()
    self->setBatchFitList, self.Fitter->getBatchFitList ()
    self.batchPlotter->setFitModel,     self.FitModel
    self.batchPlotter->setPhotonModel,  self.PhotonModel
    self.batchPlotter->setDetectorList, self.DetectorList
    self.batchPlotter->setBatchFitList, (self.fitter).batchFitList
    self.batchPlotter->setupLabelList        
;    self->dumpFit
    self.batchPlotter->clearRanges  ; Since we have a new batch fit!
    self->map
    self->plot, _EXTRA = extra, /BATCH, /RESID   
;    self->dumpFit
          
END

; ----------------------------------------------------------------------------
; Set the current fit model
; ----------------------------------------------------------------------------
PRO MfitDisplay::setFitModel, model, UPDATE = update

    self.FitModel = model

    IF KEYWORD_SET (update) THEN BEGIN
        self->plot, _EXTRA = *self.PlotOptions, /COUNTS
    ENDIF
    
END


; ----------------------------------------------------------------------------
; Set the current photon model
; ----------------------------------------------------------------------------
PRO MfitDisplay::setPhotonModel, model

    self.PhotonModel = model
    
END


; ----------------------------------------------------------------------------
; Set the fit logger
; ----------------------------------------------------------------------------
PRO MfitDisplay::setLogger, logger

    self.logger = logger
    
END


; ----------------------------------------------------------------------------
; Set the fitter (MFIT) for easy reference
; ----------------------------------------------------------------------------
PRO MfitDisplay::setFitter, fitter

    self.fitter = fitter
    
END


; ----------------------------------------------------------------------------
; Update the current list of detectors
; ----------------------------------------------------------------------------
PRO MfitDisplay::setDetectorList, DetectorList

    self.DetectorList = DetectorList
    
END


; ----------------------------------------------------------------------------
; Set the term info structure, so we can obtain the model and parameter names
; ----------------------------------------------------------------------------
PRO MfitDisplay::setTermInfo, TermInfo

    self.TermInfo = TermInfo
    
END


; ----------------------------------------------------------------------------
; Set the batch fit info list, source of all plot data for this plotter
; ----------------------------------------------------------------------------
PRO MfitDisplay::setBatchFitList, BatchFitList

    self.BatchFitList = BatchFitList
    
END

; ----------------------------------------------------------------------------
; We want to list all the relevant fit data out to the fit log
; ----------------------------------------------------------------------------
PRO MfitDisplay::dumpFit, CLEAR = clear

    ;== Number of detectors currently loaded
;    IF OBJ_VALID (self.DetectorList) THEN BEGIN
;        numDet = self.DetectorList->count ()
;        names  = self.DetectorList->names ()
;    ENDIF ELSE BEGIN
;        IF PTR_VALID ((self.BatchPlotter).readFileName) THEN BEGIN
;            data = MRDFITS (*(self.BatchPlotter).readFileName, 1, headr, /SILENT)
;            numDet = (SIZE (data))[1]
;        ENDIF ELSE numDet = 0
;    ENDELSE
    
    ; Switch on batch fit, to dump batch fit results
    IF (self.plotType EQ 'batch' OR self.plotType EQ 'topomap' OR $
        self.plotType EQ 'stack' OR self.plotType EQ 'sigma') THEN BEGIN
	
        reading = 0         ; We are not reading the batch fit info from a file
        IF OBJ_VALID (self.DetectorList) THEN BEGIN
            numDet = self.DetectorList->count ()
            names  = self.DetectorList->names ()
        ENDIF ELSE $
        IF PTR_VALID ((self.BatchPlotter).readFileName) THEN BEGIN
            data = MRDFITS (*(self.BatchPlotter).readFileName, 1, headr, /SILENT)
            numDet = (SIZE (data))[1]
            reading = 1
        ENDIF ELSE numDet = 0
	
        batch = 1
    ENDIF ELSE BEGIN   ; Single detector fit

        ;== Number of detectors currently loaded
        IF OBJ_VALID (self.DetectorList) THEN BEGIN
            numDet = self.DetectorList->count ()
            names  = self.DetectorList->names ()
		ENDIF
        batch = 0
    ENDELSE

    IF (numDet LT 1) THEN BEGIN
       MESSAGE, /CONTINUE, 'No detector data available.'
       RETURN
    ENDIF
               
;    reading = 0         ; We are reading the batch fit info from a file
;    IF (NOT (self.BatchPlotter).haveBatch) THEN BEGIN
;        IF PTR_VALID ((self.BatchPlotter).readFileName) THEN BEGIN
;            ;batch = 1
;            reading = 1
;        ENDIF ELSE BEGIN
;            batch = 0
;            numFit = 1
;        ENDELSE
;       ;TBD: fit = ?
;    ENDIF 
    
    ;== Bring the log window to the front
    self.logger->show
    IF KEYWORD_SET (CLEAR) THEN self.logger->clear
    self.logger->append, '  '
    
    IF batch THEN BEGIN
        BatchPlotter = self.BatchPlotter
        msg = [' ', STRING (REPLICATE (BYTE ('-'), 100))]
        self.logger->append, msg

        ;== Get the parameters
        paramInd = INDGEN (2) ;Dummy argument to getBatchParams using /getall
        fluInt = self->get_fluenceInt()   ;FLTARR (2)
        success = BatchPlotter->getBatchParams (paramInd, times, params, perrs, $
            /getall) ;2/15/10 RDP as per VC bug report: , FLUINT = fluInt)
        IF (NOT success) THEN RETURN ;TBD: better error handling
        
        ;== Be paraniod: the model may have changed...
        batchPlotter->setupLabelList
        labelList = BatchPlotter->getLabelList ()
        nLabels = N_ELEMENTS (labelList) - 1
        
        self.logger->append, 'Batch Fit Results'
        self.logger->append, '  '
        
        IF NOT reading THEN BEGIN
            ;== Getting very paranoid:
            IF NOT OBJ_VALID (self.fitModel) THEN RETURN
            model = self.fitModel->model()
            use_det = model.useDet
            IF (numdet NE N_ELEMENTS(use_det)) THEN $ ;This can happen...
                use_det = LONARR (numDet) + 1         
	    
            FOR i = 0, numDet - 1 DO BEGIN   
                det = self.DetectorList->data (names[i])
                ts = STRCOMPRESS (STRING (det.display->label (/TIME))) 
        
                es = STRCOMPRESS (STRING (det.display->label (/ENERGY)))
    
                cs = STRCOMPRESS (STRING (det.fitChan, $
                    FORMAT = '(I, ": ", I)'))
    
                ;use_det = model.useDet
            
                detStatus = (use_det[i] EQ 1) ? ' INCLUDED' : ' OMITTED'
                msg = [ $
                '==> Dataset      : #' + STRTRIM (i, 2) + detStatus, $
                '==> Data file    : ' + names[i], $
                '==> Response file: ' + det.respName, $
                '==> Fit interval : ' + ts + ', ' + es + ', channels ' + cs, $
                ' ']
                self.logger->append, msg
            ENDFOR
        ENDIF ELSE BEGIN  ;self.fitModel is valid. 
            FOR i = 0, numDet - 1 DO BEGIN   
                detStatus = (data[i]).detstat
                datafile = (data[i]).datafile
                fitInfo = (data[i]).fit_int
                msg = [ $
                '==> Dataset      : #' + STRTRIM (i, 2) + detStatus, $
                '==> Data file    : ' + datafile, $
                '==> Fit interval : ' + fitInfo, $
                ' ']
                self.logger->append, msg
            ENDFOR
        ENDELSE
	
        self.logger->append, STRING (nLabels + 1) + ' columns are labeled:'
        self.logger->append, labelList[0: nLabels - 1]
        
        ;== Number of degrees of freedom should be handled differently:
        self.logger->append, '==> ' + STRTRIM (labelList[nLabels]) + ': ' + $
                     STRTRIM (STRING (FIX (params[nLabels, 0])), 2)
        ;energyInt = !RM_PARAMS.fluxint == Report the Fluence integration interval
        IF (fluInt[0] NE 0.0) THEN BEGIN
            self.logger->append, '==> Fluence interval: ' + STRING (fluInt[0]) + ': ' + STRING (fluInt[1]) + ' keV'
        ENDIF
        self.logger->append, '  '
        self.logger->append, 'Start and Stop times:'
        timesStr = string (times[0,*]) + string (times[1,*])
        self.logger->append, timesStr
        self.logger->append, '  '
        self.logger->append, 'Batch Fit Parameters:'
        paramStr = STRING (params[0, *])
        errorStr = STRING (perrs[0, *])
        
        ;ChiSquares and Degrees of Freedom have no errors
        FOR i = 1, nLabels - 1 DO BEGIN
            paramStr = paramStr + STRING (params[i, *])
            errorStr = errorStr + STRING (perrs[i, *])
        ENDFOR
        
        self.logger->append, paramStr
        self.logger->append, '  '
        self.logger->append, 'Batch Fit Uncertainties:'
        self.logger->append, errorStr

    ENDIF ELSE BEGIN ; Display the individual fit results
        
        ;== Make sure we have the correct plot displayed!
        IF ((self.plotType EQ 'chi1d') OR (self.plotType EQ 'chi2d')) THEN RETURN
        
        detNames = self.DetectorList->names ()

        self.logger->append, self.fitter->getFitModelInfo ()
        
        ;== Switch info depending upon the plot type:
        IF (self.plotType EQ 'counts') OR (self.plotType EQ 'raw') OR (self.plotType EQ 'cumulative') THEN BEGIN
            title1 = ' Chan         E range         Observ.Rate  Back.Rate  Model.Rate  Model.Sigma   deduced     Sigma'
            title2 = '               (keV)                       (counts / s-keV-det)                obs cnts'
            form1  = '(I4, 2(1X, G10.5), 1X, 4(G11.4, 1X), F11.2, 1X, F11.3)'
            
        ENDIF ELSE $
        IF (self.plotType EQ 'photons') THEN BEGIN 
            title1 = ' Chan         E range       Photon.Rate   Model.Rate   Model.Sigma     Sigma'
            title2 = '               (keV)                 (photons / s-keV-cm^2)'
            form1  = '(I4, 2(1X, G10.5), 1X, 3(G11.4, 2X), F11.3)'
            
        ENDIF ELSE $
        IF (self.plotType EQ 'energy') THEN BEGIN 
            title1 = ' Chan         E range        EFlux.Rate   Model.Rate   Model.Sigma     Sigma'
            title2 = '               (keV)               (photons keV / s-keV-cm^2)'
            form1  = '(I4, 2(1X, G10.5), 1X, 3(G11.4, 2X), F11.3)'
            
        ENDIF ELSE $
        IF (self.plotType EQ 'nufnu') THEN BEGIN 
            title1 = ' Chan         E range         nuFnu.Rate      nuFnu.Sigma     Sigma'
            title2 = '               (keV)              (keV^2 / s-keV-cm^2)'
            form1  = '(I4, 2(1X, G10.5), 1X, 2(G11.4, 3X), F11.3)'
            
        ENDIF   
        
        ;== Use accessors to get at the data
        numChan    = self.fitModel->get_numChan ()
        fitChan    = self.fitModel->get_fitChan ()
        chanEnergy = self.fitModel->get_chanenergy ()
        chanWidth  = self.fitModel->get_chanwidth ()
        obsCRate   = self.fitModel->get_obsCrate ()
        backCRate  = self.fitModel->get_backCrate ()
        backCSig   = self.fitModel->get_backCSig ()
        liveTime   = self.fitModel->get_liveTime ()
        modelRate  = self.fitModel->get_model_cnt_rate ()
        modelVari  = self.fitModel->get_model_cr_vari ()
        photRate   = self.fitModel->get_phot_obs_rate ()
        photSig    = self.fitModel->get_phot_obs_sig ()
        photModel  = self.fitModel->get_model_phot_rate_bychan ()
        nufnuRate  = self.fitModel->get_nu_f_nu_data ()
        nufnuSig   = self.fitModel->get_nu_f_nu_sig ()

        FOR i = 0, numDet - 1 DO BEGIN  
            detData = self.DetectorList->data (detNames[i])
            myDisplay = detData.display
            legendList = (myDisplay).reader->getDetName()
            
            idx = INDGEN (numChan[i])
            idx = idx [fitChan[0, i]: fitChan[1, i]]
            chiSq = (obsCRate[*, i] - backCRate[*, i] - modelRate[*, 0, i]) / $
                     SQRT (modelVari[*, i])
            
            msg = '  '
            self.logger->append, msg
            msg = 'Detector ' + STRTRIM (STRING (i), 2) + ': ' + legendList
            self.logger->append, msg
            msg = 'Livetime:   ' + STRTRIM (STRING (liveTime[fitChan[0, i], i]), 2)
            self.logger->append, msg
            msg = 'Total ChiSq: ' + STRTRIM (STRING (TOTAL (chiSq[idx]^2.)), 2)
            self.logger->append, msg
            ;title1 = ' Chan         E range         Observ.Rate  Back.Rate  Model.Rate  Model.Sigma   deduced     Sigma'
            self.logger->append, title1
            ;title2 = '               (keV)                       (counts / s-keV-det)                obs cnts'
            self.logger->append, title2

            eLo = chanEnergy[*, i] - chanWidth[*, i] / 2.0
            eHi = chanEnergy[*, i] + chanWidth[*, i] / 2.0
            
            msgArr = ['  ']

            FOR j = fitChan[0, i], fitChan[1, i] DO BEGIN
                IF (self.plotType EQ 'counts') OR (self.plotType EQ 'raw') OR (self.plotType EQ 'cumulative') THEN BEGIN
                    msg = STRING (j, eLo[j], eHi[j], obsCRate[j, i], backCRate[j, i], $
                      modelRate[j, 0, i], SQRT (modelVari[j, i]), $
                      obsCRate[j, i] * chanWidth[j, i] * ABS (liveTime[j, i]), chiSq[j], $
                      FORMAT = form1)

                ENDIF ELSE $  
                IF (self.plotType EQ 'photons') THEN BEGIN 
                    msg = STRING (j, eLo[j], eHi[j], photRate[j, i], $
                      photModel[j, i], photSig[j, i], $
                      chiSq[j], FORMAT = form1)

                ENDIF ELSE $  
                IF (self.plotType EQ 'energy') THEN BEGIN 
                    enrg = (eLo[j] + eHi[j]) / 2.0
                    msg = STRING (j, eLo[j], eHi[j], photRate[j, i] * enrg, $
                      photModel[j, i] * enrg, photSig[j, i] * enrg, $
                      chiSq[j], FORMAT = form1)

                ENDIF ELSE $  
                IF (self.plotType EQ 'nufnu') THEN BEGIN 
                    msg = STRING (j, eLo[j], eHi[j], nufnuRate[j, i], $
                      nufnuSig[j, i], chiSq[j], FORMAT = form1) 

                ENDIF
                             
                msgArr = [msgArr, msg]
            ENDFOR
            
            self.logger->append, msgArr[1:*]

        ENDFOR 

    ENDELSE
    
END

pro MfitDisplay::writeHeader, myHDR, CLEAR = clear

   ;== Bring the log window to the front
    self.logger->show
    IF KEYWORD_SET (CLEAR) THEN self.logger->clear
    self.logger->append, '  '
    msg = [' ', STRING (REPLICATE (BYTE ('-'), 100))]
    self.logger->append, msg
    FOR j = 0, N_ELEMENTS(myHDR) - 1 DO BEGIN
		IF STRTRIM(myHDR[j], 2) NE '' THEN self.logger->append, myHDR[j]
    ENDFOR
    
END

;+-----------------------------------------------------------------------------
; Procedure to write a FITS file of WINGSPAN spectra fit parameters,
; optionally including the photon model and errors. The primary header
; is derived from the BFITS input files; the first extension table consists
; of data, and (optionally) sigma residuals and
; the photon model from each detector; the second table 
; contains the actual fit times, parameters and errors.
;
;INPUTS:
;	NONE 
;
;KEYWORDS:
;    FULL - Set to indicate that all the fit results are to be written, 
;           including the photon model, data and errors.
;
;------------------------------------------------------------------------------
pro MfitDisplay::writeFit, FULL = full, FITLOG_ONLY = fitlog_only

    batch = 1
	; Assume there is no duration calulation to begin with
	haveDur = 0
    IF (NOT (self.BatchPlotter).haveBatch) THEN batch = 0
    
    ;== Sigh... 
    IF (NOT batch AND NOT OBJ_VALID (self.FitModel)) THEN BEGIN
        self->setStatus, "Don't try this again, you weasel!", 5, /REVERT  ;'
        RETURN
    ENDIF

    ;== Number of detectors currently loaded
    IF OBJ_VALID (self.DetectorList) THEN BEGIN
        numDet = self.DetectorList->count ()
        names  = self.DetectorList->names ()
    ENDIF ;ELSE BEGIN
    
    IF (numDet LT 1) THEN BEGIN
       MESSAGE, /CONTINUE, 'No detector data available.'
       RETURN
    ENDIF
    
;    IF batch THEN BEGIN
;        BatchPlotter = self.BatchPlotter
;        numFit = BatchPlotter.BatchFitList->count ()
;        batNames  = BatchPlotter.BatchFitList->names ()
;    ENDIF ELSE BEGIN  
;        numFit = 1
;    ENDELSE
    
    ;*************************************************************************
    ;Automatically recognises single fit or batch fit, depending on which plot
    ;is currently on the screen and saves parameters to the *.par file -- YK
    
    ; For single fit:
    IF (self.plotType EQ 'counts' OR self.plotType EQ 'photons' OR $
        self.plotType EQ 'energy' OR self.plotType EQ 'nufnu') THEN BEGIN
	
        batch = 0
        numfit = 1
    ENDIF ELSE BEGIN
        ; For batch fit:
        IF NOT batch THEN BEGIN     ;Something's really wrong!
            self->setStatus, "Don't try this again, you weasel!", 5, /REVERT  ;'
            RETURN
        ENDIF
        BatchPlotter = self.BatchPlotter
        numfit = BatchPlotter.BatchFitList->count ()
        batNames = BatchPlotter.BatchFitList->names ()
        haveDur = BatchPlotter->haveFluxFluence()
        IF haveDur THEN BEGIN

        	durInfo = BatchPlotter->getDurations()
        	peakFLux       = durInfo.peakFLux
			peakFLuxErr    = durInfo.peakFLuxErr
			peakFLuxBeg    = durInfo.peakFLuxBeg
        	peakFluxBATSE  = durInfo.peakFluxBATSE
			peakFLuxBErr   = durInfo.peakFLuxBErr
			peakFLuxBBeg   = durInfo.peakFLuxBBeg
        	peakInterval   = durInfo.peakInterval
        	peakFLux64     = durInfo.peakFLux64
			peakFLux64Err  = durInfo.peakFLux64Err
			peakFLux64Beg  = durInfo.peakFLux64Beg
        	peakFLux64BATSE= durInfo.peakFLux64BATSE
			peakFLux64BErr = durInfo.peakFLux64BErr
			peakFLux64BBeg = durInfo.peakFLux64BBeg
        	peakInterval64 = durInfo.peakInterval64
        	peakFLux256     = durInfo.peakFLux256
			peakFLux256Err  = durInfo.peakFLux256Err
			peakFLux256Beg  = durInfo.peakFLux256Beg
        	peakFLux256BATSE= durInfo.peakFLux256BATSE
			peakFLux256BErr = durInfo.peakFLux256BErr
			peakFLux256BBeg = durInfo.peakFLux256BBeg
        	peakInterval256 = durInfo.peakInterval256
			durations      = durInfo.durations
			durationErrors = durInfo.durationErrors
			durationLevels = durInfo.durationLevels
			durBackLevels  = durInfo.durBackLevels
			durBackInt     = durInfo.durBackInt
			fluence        = durInfo.fluence
			fluenceErr     = durInfo.fluenceErr
			fluenceBATSE   = durInfo.fluenceBATSE
			fluenceErrBAT  = durInfo.fluenceErrBATSE
        ENDIF
    ENDELSE
    
    ;*************************************************************************
	    
    ;== Construct a candidate name for the batch fit FITS file
    det = self.DetectorList->data (names[0])
    reader = det.detector->dataReader ()
    path = reader->filename (/PATH)
    root = reader->filename (/ROOT)
	base = reader->filename (/BASENAME)
       
    f = root + '.par' ;path + 
    IF (STRPOS(base, 'glg') NE -1) THEN BEGIN ; We're using a GBM file, construct the correct name:
		fileStem = haveDur ? 'glg_bcat_all' : 'glg_scat_all'
		f = fileStem + STRMID(base, 19, 14, /REVERSE)
		result = file_search(path + f + '*', count=fcnt)
		f += string(fcnt,format='(i02)') + '.fit'
		myTCATfile = 'glg_tcat_all' + STRMID(base, 19, 14, /REVERSE) + '*.fit'
		tresult = file_search(path + myTCATfile, count=tcnt)
		IF (tcnt NE 0) THEN BEGIN
			TCAT_Header = HEADFITS(tresult[tcnt - 1]) ;path + myTCATfile)
		ENDIF
    ENDIF

    headers = reader->header ()     ;== We intend to alter this, so no /POINTER
    header = headers.ext0
    h2 = headers.ext2

    myMeritFunc = self.Fitter->getStatistic ()
	CASE (myMeritFunc) OF
	   0:     myStatStr = "CHISQ"
	   1:     myStatStr = "-2 LOG (LIKELIHOOD)"
	   2:     myStatStr = "Castor C-STAT"
	   ELSE : myStatStr = "CHISQ"
	ENDCASE

    ;== Get the data first; don't create a file just to drop it!
    ;== Loop over the parameters array to add columns to the table
    IF batch THEN BEGIN
        ;== These things can happen!
        IF OBJ_VALID(BatchPlotter) THEN BEGIN
            fit = BatchPlotter.BatchFitList->data (batNames[0])
            labelList = BatchPlotter->getLabelList ()
            nParams = N_ELEMENTS (*fit.param)

            ;== Get the parameters
            paramInd = INDGEN (2) ;Dummy argument to getBatchParams using /getall
            fluInt = self->get_fluenceInt()   ;!RM_PARAMS.fluxint
			durInt = self->get_durationInt() ;!RM_PARAMS.fluxint
            success = BatchPlotter->getBatchParams (paramInd, times, params, perrs, $
                /getall)
            IF (NOT success) THEN RETURN ;TBD: better error handling
        ENDIF ELSE RETURN
    
    ENDIF ELSE BEGIN  ; Single fit, not batch fit
    	times = det.timeInt
        fluInt = self.fluenceInt ;!RM_PARAMS.fluxint
    	
    	numTerms = self.PhotonModel->count ()
    	photonModel = self.PhotonModel->model ()
    
    	totParams = 0
    	FOR i = 0, numTerms - 1 DO BEGIN
            np  = photonModel[i]->nParams ()                 
            totParams = totParams + np
    	ENDFOR   
    	
    	; Make room for flux & fluence:
    	labelList = STRARR (totParams + 6)
    	params = FLTARR (totParams + 9)
    	perrs  = FLTARR (totParams + 9)
    
    	;== Get the parameters
    	model = self.FitModel->Model()
    
    	termArr = INDGEN (N_ELEMENTS (model.term_used))
    	termInd = termArr [WHERE (model.term_used)]
    
    	nParams = 0
    	FOR i = 0, numTerms - 1 DO BEGIN
    	
    		nam = photonModel[i]->name ()
    		np  = photonModel[i]->nParams ()                 
    		p   = photonModel[i]->Params ()
    
    		FOR j = 0, np - 1 DO BEGIN
    		labelList[nParams + j] = STRTRIM(nam, 2) + ': ' + STRTRIM((p.name)[j], 2)
    		ENDFOR
    	
    		params[nParams: nParams + np - 1] = model.param[termInd[i + 1] - 1, 0: np - 1]
    		perrs[nParams: nParams + np - 1] = model.param_uncer[termInd[i + 1] - 1, 0: np - 1]
    		nParams = nParams + np
    	
    	ENDFOR   
    	
    	;== Free the copy of the photon model
    	FOR i = 0, numTerms - 1 DO $
    		OBJ_DESTROY, photonModel[i]
    	
    	;== Set up the valid energy interval:
    	;myFitChan = model.fitChannels
    	;FOR ii = 0, numDet - 1 DO BEGIN
    	;	myFitChan[0, ii] = MIN (WHERE (model.modelenergy[*, ii]))
    	;	myFitChan[1, ii] = MAX (WHERE (model.modelenergy[*, ii]))
    	;ENDFOR
    	;myNumAddTerms = self.fitter->getNumAddTerms ()
    	;success = reorderData (model.numDet, model.numChan, model.modelenergy, $
    	;			 REFORM (model.modelphotrate[*, 0, *]), myFitChan, $
    	;			 model.param[myNumAddTerms, *], fullEnergy, fullModel)  ;, /WIDE
    	
    	;== Use the livetime for FLuences, as there could be gaps in the selection:
    	mylive = model.livetime[WHERE(model.livetime)]
    	totLive = TOTAL(mylive) / N_ELEMENTS(mylive)
;PRINT, (times[1, 0] - times[0, 0]), totlive

    	;== 'Photon Flux (ph/s-cm^2)'
    	labelList[totParams + 0] = 'Photon Flux (ph/s-cm^2)'
    	pFlux = self.fitter->getPFlux() ;integratePhotonModel(fullEnergy, fullModel, fluInt)
    	pFErr = self.fitter->getPErr() 
    	params[totParams + 0] = pFlux
    	perrs[totParams + 0]  = pFErr;pFlux * model.param_uncer[termInd[1] - 1, 0] / $
    						;model.param[termInd[1] - 1, 0] / 2.0
    	labelList[totParams + 1] = 'Photon Fluence (ph/cm^2)'
    	params[totParams + 1] = pFlux * totLive ;(times[1, 0] - times[0, 0])
    	perrs[totParams + 1]  = pFErr * totLive ;(times[1, 0] - times[0, 0]) ; perrs[totParams + 0]
    
    	;== 'Energy Flux (erg/s-cm^2)'
    	labelList[totParams + 2] = 'Energy Flux (erg/s-cm^2)'
    	eFlux = self.fitter->getEFlux() ;integratePhotonModel(fullEnergy, fullModel, $
    			   ;fluInt, /ENERGY_INT) * 1.602e-9 ; erg / keV
    	pEErr = self.fitter->getEErr() 
    	params[totParams + 2] = eFlux
    	perrs[totParams + 2]  = pEErr ;* model.param_uncer[termInd[1] - 1, 0] / $
    						;model.param[termInd[1] - 1, 0] / 2.0
    	labelList[totParams + 3] = 'Energy Fluence (erg/cm^2)'
    	params[totParams + 3] = eFlux * totLive ;(times[1, 0] - times[0, 0])
    	perrs[totParams + 3]  = pEErr * totLive ;perrs[totParams + 2] * (times[1, 0] - times[0, 0])
    
    	labelList[totParams + 4] = 'Reduced ' + myStatStr
    	IF (model.dof NE 0) THEN BEGIN
    		params[totParams + 4] = model.chisq / model.dof
    	ENDIF ELSE BEGIN
    	    params[totParams + 4] = - model.chisq
    	ENDELSE
            labelList[totParams + 5] = 'Degrees of Freedom'
    	params[totParams + 5]    = model.dof
    	
    	;== 'Photon Flux (ph/s-cm^2) BATSE energy (50-300)'
    	pBFlux = self.fitter->getBFlux() 
    	pBErr = self.fitter->getBErr() 
    	params[totParams + 6] = pBFlux
    	perrs[totParams + 6]  = pBErr
    	
    	;== 'Photon Fluence (ph-cm^2) for durations (user)'
    	pBFlux = self.fitter->getBFlux() 
    	pBErr = self.fitter->getBErr() 
    	params[totParams + 7] = pBFlux * totLive
    	perrs[totParams + 7]  = pBErr * totLive
    	
    	;== ''Energy Fluence (erg-cm^2) BATSE energy (50-300)''
    	pBEFlux = self.fitter->getBEFlux() 
    	pBEErr = self.fitter->getBEErr() 
    	params[totParams + 8] = pBEFlux * totLive
    	perrs[totParams + 8]  = pBEErr * totLive

    ENDELSE
    
    ;== Create the new PRIMARY header
    fxhmake, hdr, /extend, /date, /initialize

    get_date, dte, /timetag
    sxaddpar, hdr, 'DATE', dte, /SAVECOM
    fxaddpar, hdr, 'FILETYPE', 'SPECTRAL FITS', 'Unique FITS file type name'
	vers = 'rmfit ' + !RM_PARAMS.VERSION
    fxaddpar, hdr, 'CREATOR', vers, 'Software/version creating file'
    fxaddpar, hdr, 'ORIGIN', 'GIOC', 'Name of organization'
    fxaddpar, hdr, 'TELESCOP', 'GLAST', 'Name of mission'
    fxaddpar, hdr, 'INSTRUME', 'GBM', 'Name of instrument'
    fxaddpar, hdr, 'OBSERVER', 'Meegan', 'Name of instrument PI'
    fxaddpar, hdr, 'MJDREFI', 51910., 'MJD date of reference epoch, int part'
    sxaddpar, hdr, 'MJDREFF', 7.428703703703703D-4, 'MJD date of reference epoch, frac part',f='(e21.15)'
    fxaddpar, hdr, 'TIMESYS', 'TT', 'Time system'
    fxaddpar, hdr, 'TIMEUNIT', 's', 'Time unit used in TSTART, TSTOP and TRIGTIME'
    theobject = sxpar(header, 'OBJECT')
    fxaddpar, hdr, 'OBJECT', theobject
    ww = STRPOS(header, 'BATSE_TR')
    wnum = WHERE(ww NE -1, co)
	errorRad = -1
    IF (co NE 0) THEN BEGIN
		old_trig = sxpar(header, 'BATSE_TR')
		fxaddpar, hdr, 'BATSE_TR', old_trig, 'BATSE Trigger number (if applicable)'
		TJD = LONG (SXPAR (h2, 'BASETIME'))
		dateObs = TJD
		dateEnd = TJD
		fxaddpar, hdr, 'TRIG-DAY', TJD, 'Truncated Julian Date of event trigger'
		relTrigSec = sxpar(header, 'TRIG-TIM')
		fxaddpar, hdr, 'TRIG-TIM', relTrigSec, 'Seconds of day for event trigger'
		trigSec = relTrigSec + TJD * 24 * 60 * 60.
		tstart = trigSec + times[0, 0]
		tstop  = trigSec + times[1, numfit - 1]
		objectRA = sxpar(header, 'OBJCTRA')
		objectDEC = sxpar(header, 'OBJCTDEC')
	ENDIF ELSE BEGIN
		trigSec = sxpar(header, 'TRIGTIME')
		objectRA = sxpar(header, 'RA_OBJ')
		objectDEC = sxpar(header, 'DEC_OBJ')
		;dateObs = sxpar(header, 'DATE-OBS')
		;dateEnd = sxpar(header, 'DATE-END')
		tstart = trigSec + times[0, 0] ;sxpar(header, 'TSTART')
		tstop  = trigSec + times[1, numfit - 1] ;sxpar(header, 'TSTOP')
		dateObs = FITSTime(tstart)
		dateEnd = FITSTime(tstop)
		
	ENDELSE
	fxaddpar, hdr, 'DATE-OBS', dateObs, 'Date of start of observation'
	fxaddpar, hdr, 'DATE-END', dateEnd, 'Date of start of observation'
	fxaddpar, hdr, 'TSTART', tstart, 'Observation start time, relative to MJDREF',f='(f19.7)'
	fxaddpar, hdr, 'TSTOP', tstop, 'Observation end time, relative to MJDREF',f='(f19.7)'
	fxaddpar, hdr, 'TRIGTIME', trigSec, 'Trigger time, relative to MJDREF',f='(f19.7)'
    fxaddpar, hdr, 'RADECSYS', 'FK5', 'Stellar reference frame'
    fxaddpar, hdr, 'EQUINOX', 2000., 'Equinox for RA and Dec'
    fxaddpar, hdr, 'RA_OBJ', objectRA, 'J2000 RA of source, degrees'
    fxaddpar, hdr, 'DEC_OBJ', objectDEC, 'J2000 DEC of source, degrees'
    fxaddpar, hdr, 'CLASS', 'GRB', 'Classification of trigger'
    fxaddpar, hdr, 'RELIABLT', 1.0, 'Reliability of classification (0. -- 1.)'

	IF (N_ELEMENTS(TCAT_Header) GT 1) THEN BEGIN ;== We have the trigger info: add it in!
		myPar = SXPAR(TCAT_Header, 'CLASS', COMMENT = myComm)
		fxaddpar, hdr, 'CLASS', myPar, myComm
		myPar = SXPAR(TCAT_Header, 'RELIABLT', COMMENT = myComm)
		fxaddpar, hdr, 'RELIABLT', myPar, myComm
		myPar = SXPAR(TCAT_Header, 'TRIGSCAL', COMMENT = myComm)
		fxaddpar, hdr, 'TRIGSCAL' , myPar, myComm
		myPar = SXPAR(TCAT_Header, 'TRIG_ALG', COMMENT = myComm)
		fxaddpar, hdr, 'TRIG_ALG' , myPar, myComm
		myPar = SXPAR(TCAT_Header, 'CHAN_LO', COMMENT = myComm)
		fxaddpar, hdr, 'CHAN_LO' , myPar, myComm
		myPar = SXPAR(TCAT_Header, 'CHAN_HI', COMMENT = myComm)
		fxaddpar, hdr, 'CHAN_HI' , myPar, myComm
		myPar = SXPAR(TCAT_Header, 'ADC_LO', COMMENT = myComm)
		fxaddpar, hdr, 'ADC_LO' , myPar, myComm
		myPar = SXPAR(TCAT_Header, 'ADC_HI', COMMENT = myComm)
		fxaddpar, hdr, 'ADC_HI' , myPar, myComm
		myPar = SXPAR(TCAT_Header, 'DET_MASK', COMMENT = myComm)
		fxaddpar, hdr, 'DET_MASK' , myPar, myComm
		objectRA = SXPAR(TCAT_Header, 'RA_OBJ', COMMENT = myComm)
		fxaddpar, hdr, 'RA_OBJ' , objectRA, myComm
		objectDEC = SXPAR(TCAT_Header, 'DEC_OBJ', COMMENT = myComm)
		fxaddpar, hdr, 'DEC_OBJ' , objectDEC, myComm
		errorRad = SXPAR(TCAT_Header, 'ERR_RAD', COMMENT = errComm)
		fxaddpar, hdr, 'ERR_RAD' , errorRad, errComm
	ENDIF

    IF haveDur THEN BEGIN
        fluIntString = STRCOMPRESS (STRING (fluInt, FORMAT = '(I, "-", I)'))
		fxaddpar, hdr, 'FLU'     , fluence, '[erg/cm^2] ' + fluIntString + ' keV fluence'
		fxaddpar, hdr, 'FLU_ERR' , fluenceErr, '[erg/cm^2] Uncertainty on fluence'
		fxaddpar, hdr, 'FLUB'    , fluenceBATSE, '[erg/cm^2] 50-300 keV fluence'
		fxaddpar, hdr, 'FLUB_ERR', fluenceErrBAT, '[erg/cm^2] Uncertainty on fluence'
		fxaddpar, hdr, 'FLU_LOW' , fluInt[0], '[keV] Lower limit of flux/fluence integration'
		fxaddpar, hdr, 'FLU_HIGH', fluInt[1], '[keV] Upper limit of flux/fluence integration'
		fxaddpar, hdr, 'DUR_LOW' , durInt[0], '[keV] Lower limit of duration integration'
		fxaddpar, hdr, 'DUR_HIGH', durInt[1], '[keV] Upper limit of duration integration'
		fxaddpar, hdr, 'PFLX_INT', peakInterval, '[s] Time interval for peak flux'
		fxaddpar, hdr, 'PFLX'    , peakFLux, '[ph/(s cm^2)] ' + fluIntString + ' keV peak flux'
		fxaddpar, hdr, 'PFLX_ERR', peakFLuxErr, '[ph/(s cm^2)] Uncertainty on peak flux'
		fxaddpar, hdr, 'PFLX_BEG', peakFLuxBeg, '[s] Start time for peak flux interval'
		fxaddpar, hdr, 'PFLXB'   , peakFluxBATSE, '[ph/(s cm^2)] 50-300 keV peak flux'
		fxaddpar, hdr, 'PFLXBERR', peakFLuxBErr, '[ph/(s cm^2)] Uncertainty on peak flux'
		fxaddpar, hdr, 'PFLXBBEG', peakFLuxBBeg, '[s] Start time for peak flux interval (BATSE)'
		fxaddpar, hdr, 'PF64_INT', peakInterval64, '[s] Time interval for peak flux'
		fxaddpar, hdr, 'PF64'    , peakFLux64, '[ph/(s cm^2)] ' + fluIntString + ' keV peak flux'
		fxaddpar, hdr, 'PF64_ERR', peakFLux64Err, '[ph/(s cm^2)] Uncertainty on peak flux'
		fxaddpar, hdr, 'PF64_BEG', peakFLux64Beg, '[s] Start time for peak flux interval'
		fxaddpar, hdr, 'PF64B'   , peakFLux64BATSE, '[ph/(s cm^2)] 50-300 keV peak flux'
		fxaddpar, hdr, 'PF64BERR', peakFLux64BErr, '[ph/(s cm^2)] Uncertainty on peak flux'
		fxaddpar, hdr, 'PF64BBEG', peakFLux64BBeg, '[s] Start time for peak flux interval (BATSE)'
		fxaddpar, hdr, 'PF256INT', peakInterval256, '[s] Time interval for peak flux'
		fxaddpar, hdr, 'PF256'   , peakFLux256, '[ph/(s cm^2)] ' + fluIntString + ' keV peak flux'
		fxaddpar, hdr, 'PF256ERR', peakFLux256Err, '[ph/(s cm^2)] Uncertainty on peak flux'
		fxaddpar, hdr, 'PF256BEG', peakFLux256Beg, '[s] Start time for peak flux interval'
		fxaddpar, hdr, 'PF256B'  , peakFLux256BATSE, '[ph/(s cm^2)] 50-300 keV peak flux'
		fxaddpar, hdr, 'PF256BER', peakFLux256BErr, '[ph/(s cm^2)] Uncertainty on peak flux'
		fxaddpar, hdr, 'PF256BBG', peakFLux256BBeg, '[s] Start time for peak flux interval (BATSE)'
		fxaddpar, hdr, 'T90'     , durations[3] - durations[0], '[s] T90 duration'
		fxaddpar, hdr, 'T90_ERR' , durationErrors[0], '[s] Uncertainty on T90'
		fxaddpar, hdr, 'T90START', durations[0], '[s] Start of t90 interval'
		fxaddpar, hdr, 'T50'     , durations[2] - durations[1], '[s] T50 duration'
		fxaddpar, hdr, 'T50_ERR' , durationErrors[1], '[s] Uncertainty on T50'
		fxaddpar, hdr, 'T50START', durations[1], '[s] Start of t50 interval'
		loBackInt = '(' + STRTRIM(STRING(durBackInt[0,0], FORMAT = '(D10.2)'), 2) + $
		            ', ' + STRTRIM(STRING(durBackInt[0,1], FORMAT = '(D10.2)'), 2) + ')'
		fxaddpar, hdr, 'LOBCKINT', loBackInt, '[s] Lower background selection'
		hiBackInt = '(' + STRTRIM(STRING(durBackInt[1,0], FORMAT = '(D10.2)'), 2) + $
		            ', ' + STRTRIM(STRING(durBackInt[1,1], FORMAT = '(D10.2)'), 2) + ')'
		fxaddpar, hdr, 'HIBCKINT', hiBackInt, '[s] Upper background selection'
    ENDIF
	
	IF KEYWORD_SET(fitlog_only) THEN BEGIN
		;== Show what we have written:
		self->writeHeader, hdr
		RETURN
	ENDIF

    out_file = DIALOG_PICKFILE ( $
              TITLE = 'Select a Filename for Writing Results', $
              FILE = f, FILTER = '*.fit', $
              PATH = !RM_PARAMS.lastPath, GET_PATH = lastPath, $
              DIALOG_PARENT = self.widgetID.top, $
              /WRITE, /OVERWRITE_PROMPT)              ; <---Changed for Mac!
	
    IF (out_file EQ '') THEN RETURN  ; Cancel
    !RM_PARAMS.lastPath = lastPath
          
    ; User may have slipped up! Filename is only a path:
    IF ((STRPOS(out_file, '/', /REVERSE_SEARCH) + 1) EQ STRLEN(out_file)) THEN BEGIN
        out_file = out_file + f               ;TBD: ask again?
        self->setStatus, 'Writing file: ' + out_file, 10, /REVERT
	ENDIF
	; If the user accidently puts a space in front of the filename...
	retpos = STRPOS(out_file, STRING(10B))
	IF (retpos NE 0) THEN BEGIN
	   out_file = STRMID(out_file, retpos + 1)
	ENDIF
       
    pathEnd = STRPOS(out_file, '/', /REVERSE_SEARCH)
    IF (pathEnd NE -1) THEN BEGIN
    	shortName = STRMID(out_file, pathEnd + 1)
    ENDIF ELSE BEGIN
    	shortName = out_file
    ENDELSE
    fxaddpar, hdr, 'FILENAME', shortName, 'Name of FITS file'
   ; fxaddpar, hdr, 'MNEMONIC', 'RMFIT ' + !RM_PARAMS.VERSION, 'Program creating this file'
    fxaddpar, hdr, 'COMMENT', 'This file consists of time-sequenced spectral fit parameters'
	fits_add_checksum, hdr
    fxwrite, out_file, hdr
	
    ;== Energy thresholds table: one row for each detector used...
    fxbhmake, ch, numDet
    fitchannels = LONARR (2)

    dummy = STRING (REPLICATE (32B, 20))
    comment = 'Instrument name for this detector'
    fxbaddcol, id, ch, dummy, 'INSTRMNT', comment
    comment = 'Detector number; if one of several available'
    fxbaddcol, id, ch, dummy, 'DETECTOR', comment
    comment = 'Data type used for this analysis'
    fxbaddcol, id, ch, dummy, 'DATATYPE', comment
    comment = 'Was this detector INCLUDED or OMITTED'
    fxbaddcol, id, ch, dummy, 'DETSTAT', comment
    dummy2 = STRING (REPLICATE (32B, 60))
    comment = 'File name for this dataset'
    fxbaddcol, id, ch, dummy2, 'DATAFILE', comment
    comment = 'Fit intervals'
    fxbaddcol, id, ch, dummy2, 'FIT_INT', comment

    IF KEYWORD_SET (full) THEN BEGIN
        ;== Find out how many channels the maximum array size will be
        numChan = LONARR (numDet)
        FOR i = 0, numDet - 1 DO BEGIN   
            numChan[i] = (self.DetectorList->data (names[i])).numChan
        ENDFOR
        maxChan = MAX (numChan) + 1
        
        comment = 'Total number of energy channels for this detector'
        fxbaddcol, id, ch, maxChan, 'CHANNUM', comment
        comment = 'Channels selected in fitting this detector'
        fxbaddcol, id, ch, fitchannels, 'FITCHAN', comment
        thresholds = FLTARR (maxChan)
        comment = 'Energy edges for each selected detector'
        fxbaddcol, id, ch, thresholds, 'E_EDGES', comment, /variable
        fxaddpar, ch, 'TUNIT9', 'keV', after='TFORM9'
        
        ;== Determine the maximum array size for use with VARIABLE keyword
        dumArray = FLTARR (maxChan, numFit)
        comment = 'Array of photon counts data'
        fxbaddcol, id, ch, dumArray, 'PHTCNTS', comment, /variable
        fxaddpar, ch, 'TUNIT10', 'Photon cm^-2 s^-1 keV^-1', after='TFORM10'
        comment = 'Array of photon model data'
        fxbaddcol, id, ch, dumArray, 'PHTMODL', comment, /variable
        fxaddpar, ch, 'TUNIT11', 'Photon cm^-2 s^-1 keV^-1', after='TFORM11'
        comment = 'Array of errors in photon counts data'
        fxbaddcol, id, ch, dumArray, 'PHTERRS', comment, /variable
        fxaddpar, ch, 'TUNIT12', 'Photon cm^-2 s^-1 keV^-1', after='TFORM12'
    ENDIF
 	    
    ;== Unique name for this FITS extension
    sxaddpar, ch, 'DATE', dte, /SAVECOM
    fxaddpar, ch, 'EXTNAME', 'DETECTOR DATA', 'Name of this binary table extension'
    fxaddpar, ch, 'ORIGIN', 'GIOC', 'Name of organization'
    fxaddpar, ch, 'TELESCOP', 'GLAST', 'Name of mission'
    fxaddpar, ch, 'INSTRUME', 'GBM', 'Name of instrument'
    fxaddpar, ch, 'OBSERVER', 'Meegan', 'Name of instrument PI'
    fxaddpar, ch, 'MJDREFI', 51910., 'MJD date of reference epoch, int part'
    sxaddpar, ch, 'MJDREFF', 7.428703703703703D-4, 'MJD date of reference epoch, frac part',f='(e21.15)'
    fxaddpar, ch, 'TIMESYS', 'TT', 'Time system'
    fxaddpar, ch, 'TIMEUNIT', 's', 'Time unit used in TSTART, TSTOP and TRIGTIME'
    fxaddpar, ch, 'OBJECT', theobject
    ;fxaddpar, ch, 'BATSE_TR', old_trig, 'BATSE Trigger number (if applicable)'
    ;fxaddpar, ch, 'TRIG-DAY', TJD, 'Truncated Julian Date of event trigger'
    ;fxaddpar, ch, 'TRIG-TIM', trigSec, 'Seconds of day for event trigger'
	fxaddpar, ch, 'DATE-OBS', dateObs, 'Date of start of observation'
	fxaddpar, ch, 'DATE-END', dateEnd, 'Date of start of observation'
	fxaddpar, ch, 'TSTART', tstart, 'Observation start time, relative to MJDREF',f='(f19.7)'
	fxaddpar, ch, 'TSTOP', tstop, 'Observation end time, relative to MJDREF',f='(f19.7)'
    fxaddpar, ch, 'TRIGTIME', trigSec, 'Trigger time (s) relative to MJDREF',f='(f19.7)'
    fxaddpar, ch, 'RADECSYS', 'FK5', 'Stellar reference frame'
    fxaddpar, ch, 'EQUINOX', 2000., 'Equinox for RA and Dec'
    fxaddpar, ch, 'RA_OBJ', objectRA, 'J2000 RA of source, degrees'
    fxaddpar, ch, 'DEC_OBJ', objectDEC, 'J2000 DEC of source, degrees'
    IF (errorRad NE -1) THEN $
		fxaddpar, ch, 'ERR_RAD' , errorRad, 'Calculated Location Error Radius'
	fits_add_checksum, ch
   
    fxbcreate, unit_out, out_file, ch

	;== Get the status of the detector
	model = self.fitModel->model()
	use_det = model.useDet

	IF (numdet NE N_ELEMENTS(use_det)) THEN $ ;This can happen...
		use_det = LONARR (numDet) + 1         

    FOR i = 0, numDet - 1 DO BEGIN
   
        outer = i + 1
        
        ;== Get the instrument name
        det = self.DetectorList->data (names[i])
        reader = det.detector->dataReader ()
        headers = reader->header ()
        prim = headers.ext0
        theInstrument = sxpar(prim, 'INSTRUME')
        fxbwrite, unit_out, theInstrument, 1, outer 
        
        ;== Get the detector name and number (TBD: not instrument agnostic!)
        dh = headers.ext2
        detName = STRTRIM (sxpar(dh, 'DET_MODE'), 2)
        IF (detName EQ '0') THEN BEGIN
			detName = STRTRIM (sxpar(prim, 'DETNAM'), 2)
        ENDIF ELSE BEGIN
			detsel = STRTRIM (sxpar(dh, 'DSELECT'), 2)
			detnum = 7 - STRPOS (detsel, 'Y')
			detName = detName + STRTRIM (STRING (detnum), 2)
        ENDELSE
        
        fxbwrite, unit_out, detName, 2, outer
        
        ;== Get the datatype(s) used
        dataType = STRTRIM (sxpar(prim, 'DATATYPE'), 2)
        IF (dataType EQ '0') THEN BEGIN
        	;== For backward compatibility:
			dataType = STRTRIM (sxpar(dh, 'DATATYPE'), 2)
        ENDIF        
        fxbwrite, unit_out, dataType, 3, outer
        
;        ;== Get the status of the detector 
;        model = self.fitModel->model() 
;        use_det = model.useDet 
;            IF (numdet NE N_ELEMENTS(use_det)) THEN $ ;This can happen... 
;                use_det = LONARR (numDet) + 1          
        detStatus = (use_det[i] EQ 1) ? 'INCLUDED' : 'OMITTED'
        fxbwrite, unit_out, detStatus, 4, outer
        
        ;== Write out the name of the data file
		pathEnd = STRPOS(names[i], '/', /REVERSE_SEARCH)
		IF (pathEnd NE -1) THEN BEGIN
			shortDetName = STRMID(names[i], pathEnd + 1)
		ENDIF ELSE BEGIN
			shortDetName = names[i]
		ENDELSE
        fxbwrite, unit_out, shortDetName, 5, outer
        
        ;== Get the fit interval info
        ts = STRCOMPRESS (STRING (det.display->label (/TIME))) 
        es = STRCOMPRESS (STRING (det.display->label (/ENERGY)))
        cs = STRCOMPRESS (STRING (det.fitChan, FORMAT = '(I, ": ", I)'))
        fit_int = ts + ', ' + es + ', channels ' + cs
        fxbwrite, unit_out, fit_int, 6, outer
        
        IF KEYWORD_SET (full) THEN BEGIN
            fxbwrite, unit_out, numChan[i], 7, outer
            
            ;== Get the channel range of energies used for fitting
            ;(FITS channels are 1-indexed!)
            fitChan = det.fitChan
            fxbwrite, unit_out, fitChan, 8, outer
            
            ;== Get the energy bin edges for the given detector
            resp = *(self.DetectorList->data (names[i])).resp        
            chanEnergy = resp.chan_energy 
            chanWidth = resp.chan_width
            thresholds = FLTARR (numChan[i] + 1)
            thresholds[0: numChan[i] - 1] = chanEnergy - chanWidth / 2.
            thresholds[numChan[i]] = chanEnergy[numChan[i] - 1] + $
                                     chanWidth[numChan[i] - 1] / 2.
            fxbwrite, unit_out, thresholds, 9, outer
    
            ;== Get the photon 'counts', model and errors
            photCounts = FLTARR (numChan[i], numFit)
            photModel  = photCounts
            photErrors = photCounts
            
            FOR j = 0, numFit - 1 DO BEGIN
                IF batch THEN BEGIN
                    fit = self.BatchFitList->data (batNames[j])
                    ; NB: The fit structure data are misnamed! They are actually 
                    ;     the *photon* count rate, model and sigmas.
                    photCounts[*, j] = (*fit.netCrate)[0: numChan[i] - 1, i]
                    photModel[*, j]  = (*fit.modelCntRate)[0: numChan[i] - 1, i]
                    photErrors[*, j] = (*fit.netCsig)[0: numChan[i] - 1, i]
                ENDIF ELSE BEGIN
                    fit = self.FitModel
                    
                    tempChan = fit->Get_NumChan () - 1
                    temp = fit->GET_PHOT_OBS_RATE ()
    ;*************************************************************************
    ; Changed the dimension of temp below from [0:tempChan[i]] to 
    ; [0:tempChan[i], i] to take into account the case with more than 
    ; one detector. -- Y.K.
    ;*************************************************************************
                    photCounts[*, j] = temp[0: tempChan[i], i]
                    temp = fit->GET_MODEL_PHOT_RATE_BYCHAN ()
                    photModel[*, j]  = temp[0: tempChan[i], i]
                    temp = fit->GET_PHOT_OBS_SIG ()
                    photErrors[*, j] = temp[0: tempChan[i], i]
                ENDELSE
                ; NB: The fit structure data are misnamed! They are actually 
                ;     the *photon* count rate, model and sigmas.
            ENDFOR
	        
            photCounts = REFORM (photCounts, numChan[i] * numFit)
            fxbwrite, unit_out, photCounts, 10, outer
            photModel = REFORM (photModel, numChan[i] * numFit)
            fxbwrite, unit_out, photModel,  11, outer
            photErrors = REFORM (photErrors, numChan[i] * numFit)
            fxbwrite, unit_out, photErrors, 12, outer
        ENDIF

    ENDFOR

    fxbfinish, unit_out

    ; Fit parameters table: one column for each parameter, 
    ;one row for each time bin.
    fxbhmake, paramh, numFit

    twoFloats = FLTARR (2)
    comment = 'Start and stop times relative to trigger'
    fxbaddcol, id, paramh, twoFloats, 'TIMEBIN', comment
    
    ;== Columns for the fitmodel data
    FOR k = 0, nParams - 1 DO BEGIN
        comment = labelList[k]
        fxbaddcol, id, paramh, twoFloats, 'PARAM' + STRTRIM (k, 2), comment
    ENDFOR
    
    ;== Add the derived quantities
    comment = 'Photon Flux (ph/s-cm^2) std energy (8-1000)'
    fxbaddcol, id, paramh, twoFloats, 'PHTFLUX', comment
    comment = 'Photon Fluence (ph/cm^2) std energy (8-1000)'
    fxbaddcol, id, paramh, twoFloats, 'PHTFLNC', comment
    comment = 'Energy Flux (erg/s-cm^2) std energy (8-1000)'
    fxbaddcol, id, paramh, twoFloats, 'NRGFLUX', comment
    comment = 'Energy Fluence (erg/cm^2) std energy (8-1000)'
    fxbaddcol, id, paramh, twoFloats, 'NRGFLNC', comment
    comment = 'Reduced Chi-squares'
    fxbaddcol, id, paramh, 0.0, 'REDCHSQ', comment
    comment = 'Degrees of Freedom'
    fxbaddcol, id, paramh, 0, 'CHSQDOF', comment
    comment = 'Photon Flux (ph/s-cm^2) BATSE energy (50-300)'
    fxbaddcol, id, paramh, twoFloats, 'PHTFLUXB', comment
    comment = 'Photon Fluence (ph-cm^2) for durations (user)'
    fxbaddcol, id, paramh, twoFloats, 'DURFLNC', comment
    comment = 'Energy Fluence (erg-cm^2) BATSE energy (50-300)'
    fxbaddcol, id, paramh, twoFloats, 'NRGFLNCB', comment
    
    
    sxaddpar, paramh, 'DATE', dte, /SAVECOM
    fxaddpar, paramh, 'ORIGIN', 'GIOC', 'Name of organization'
    fxaddpar, paramh, 'TELESCOP', 'GLAST', 'Name of mission'
    fxaddpar, paramh, 'INSTRUME', 'GBM', 'Name of instrument'
    fxaddpar, paramh, 'OBSERVER', 'Meegan', 'Name of instrument PI'
    fxaddpar, paramh, 'MJDREFI', 51910., 'MJD date of reference epoch, int part'
    sxaddpar, paramh, 'MJDREFF', 7.428703703703703D-4, 'MJD date of reference epoch, frac part',f='(e21.15)'
    fxaddpar, paramh, 'TIMESYS', 'TT', 'Time system'
    fxaddpar, paramh, 'TIMEUNIT', 's', 'Time unit used in TSTART, TSTOP and TRIGTIME'
    fxaddpar, paramh, 'OBJECT', theobject
    ;fxaddpar, paramh, 'BATSE_TR', old_trig, 'BATSE Trigger number (if applicable)'
    ;fxaddpar, paramh, 'TRIG-DAY', TJD, 'Truncated Julian Date of event trigger'
    ;fxaddpar, paramh, 'TRIG-TIM', trigSec, 'Seconds of day for event trigger'
	fxaddpar, paramh, 'DATE-OBS', dateObs, 'Date of start of observation'
	fxaddpar, paramh, 'DATE-END', dateEnd, 'Date of start of observation'
	fxaddpar, paramh, 'TSTART', tstart, 'Observation start time, relative to MJDREF',f='(f19.7)'
	fxaddpar, paramh, 'TSTOP', tstop, 'Observation end time, relative to MJDREF',f='(f19.7)'
    fxaddpar, paramh, 'TRIGTIME', trigSec, 'Trigger time (s) relative to MJDREF',f='(f19.7)'
    fxaddpar, paramh, 'RADECSYS', 'FK5', 'Stellar reference frame'
    fxaddpar, paramh, 'EQUINOX', 2000., 'Equinox for RA and Dec'
    fxaddpar, paramh, 'RA_OBJ', objectRA, 'J2000 RA of source, degrees'
    fxaddpar, paramh, 'DEC_OBJ', objectDEC, 'J2000 DEC of source, degrees'
    IF (errorRad NE -1) THEN $
		fxaddpar, paramh, 'ERR_RAD' , errorRad, 'Calculated Location Error Radius'
    fxaddpar, paramh, 'FLU_LOW', fluInt[0], 'Lower limit of flux/fluence integration (keV)'
    fxaddpar, paramh, 'FLU_HIGH', fluInt[1], 'Upper limit of flux/fluence integration (keV)'
    fxaddpar, paramh, 'STATISTC', myStatStr, 'Indicates merit function used for fitting'
    fxaddpar, paramh, 'EXTNAME', 'FIT PARAMS', 'Name of this binary table extension'
	fits_add_checksum, paramh
 
    fxbcreate, unit_out, out_file, paramh
    
    FOR inner = 1, numFit DO BEGIN
        fxbwrite, unit_out, times[*, inner - 1], 1, inner
        FOR k = 1, nParams + 4 DO BEGIN
            fxbwrite, unit_out, [params[k - 1, inner - 1], $
                                perrs[k - 1, inner - 1]], k + 1, inner
        ENDFOR
;        FOR k = nParams + 1, nParams + 5 DO BEGIN
;            fxbwrite, unit_out, params[k - 1, inner - 1], k + 1, inner
;        ENDFOR
        fxbwrite, unit_out, params[nParams + 4, inner - 1], nParams + 6, inner
        fxbwrite, unit_out, FIX (params[nParams + 5, inner - 1]), nParams + 7, inner
		fxbwrite, unit_out, [params[nParams + 6, inner - 1], $
                                perrs[nParams + 6, inner - 1]], nParams + 8, inner
		fxbwrite, unit_out, [params[nParams + 7, inner - 1], $
                                perrs[nParams + 7, inner - 1]], nParams + 9, inner
		fxbwrite, unit_out, [params[nParams + 8, inner - 1], $
                                perrs[nParams + 8, inner - 1]], nParams + 10, inner
    ENDFOR

    fxbfinish, unit_out
    
    ;== This next may fail, but it is better than nothing:
    SPAWN, 'fchecksum update+ datasum+ ' + out_file

END


; ----------------------------------------------------------------------------
; Parse a spectral fit FITS file. 
; We would like to know if the file has only one or more spectral fits 
; ----------------------------------------------------------------------------
FUNCTION MfitDisplay::parseFITS, filename
    
    numfit = 0
    
    data1 = MRDFITS(filename, 1, hdr1)
    result = sxpar(hdr1, 'EXTNAME')
    ok = (result EQ 'DETECTOR DATA')
    IF NOT ok THEN BEGIN
        data1 = MRDFITS(filename, 2, hdr1)
	result = sxpar(hdr1, 'EXTNAME')
	ok = (result EQ 'DETECTOR DATA')
	IF NOT ok THEN BEGIN
	    self->setStatus, 'Wrong file type!', 10, /REVERT
	    RETURN, -1
	ENDIF
    ENDIF
    
    ; The limited version of the FITS file does not have the following:
    result = sxpar(hdr1, 'TFORM7')
    IF (result NE STRING(0)) THEN BEGIN 
        ; Fit data are stored the second extension
        headr2 = HEADFITS(filename, EXT = 2)
        numFit = sxpar (headr2, 'NAXIS2')
    ENDIF ;We have found a .par file that has no photon data...
    
    RETURN, numfit

END


; ----------------------------------------------------------------------------
; Read a spectral single fit FITS file. 
; ----------------------------------------------------------------------------
FUNCTION MfitDisplay::setFitInfoFromFile

	IF PTR_VALID (self.readFileName) THEN BEGIN
		data1 = MRDFITS (*self.readFIleName, 1, headr1, /SILENT)
		numDets = sxpar (headr1, 'NAXIS2')
		checkSize = sxpar (headr1, 'TFIELDS')
		IF (checkSize EQ 3) THEN BEGIN
			self.display->setStatus, $
                'This parameter file has no model data.', 5, /REVERT
			RETURN, -1      ;== Small parameter file, no model data
		ENDIF

		data2 = MRDFITS (*self.readFIleName, 2, headr2, /SILENT)
		numFit = sxpar (headr2, 'NAXIS2')
		times = ((data2.timebin)[1, *] + (data2.timebin)[0, *]) / 2.
	
		;== Try getting the Effective Area correction, if included
;		mylabels = self->getlabellist()
;		EffIDX = WHERE (STRPOS (mylabels, 'Eff.') NE -1, co)
;		IF (co NE 0) THEN BEGIN
;			ok = self->getBatchParams (EffIDX, EffTi, EffArea, EffErrs)
;			IF NOT ok THEN RETURN, -1
;			effArea = TRANSPOSE (effArea)
;		ENDIF ELSE BEGIN
;			EffArea = FLTARR (numFit, numDets) > 1.
;		ENDELSE
		
		nChan = data1.channum
		maxChan = N_ELEMENTS (data1[0].e_edges)
		
		;== Strip out the last (overflow) channel in the data
		tempEdges = ((data1[*]).e_edges[1: maxChan - 1] + $
					 (data1[*]).e_edges[0: maxChan - 2, *]) / 2.
		
;		== We have overlapping datasets with several detectors included;
;		== Put everything in order of ascending energy:
;		numPhtFit = reorderData (numDets, nChan, tempEdges, data1.phtModl, $
;					data1.fitChan, EffArea, phtEdges, phtArr)
;
;		IF (numPhtFit NE numFit) THEN RETURN, -1 ;== Something is badly wrong!
;		
;		photEnergy = phtEdges 
;		numPhotEbins = N_ELEMENTS (photEnergy)
;		nuFnuContours = FLTARR (numPhotEbins, numFit)
;		maxNuFnu  = FLTARR (numFit)
;		FOR i = 0, numFit - 1 DO  BEGIN
;			nuFnuContours[*, i] = phtArr[0: numPhotEbins - 1, i] * photEnergy^2.
;			tempMaxNu = MAX (nuFnuContours[*, i], maxSub)
;			maxNuFnu[i] = photEnergy[maxSub]
;		ENDFOR
	ENDIF ELSE RETURN, -1    ;== No fit and no results file: bail
    
    ;== Success!
    RETURN, 0

END


; ----------------------------------------------------------------------------
; Read a spectral fit FITS file. Since MRDFITS is so efficient, all this does is
;set the filename parameter of the BatchPlotter object. The reading is done
;later, when the data are needed for display. 
; ----------------------------------------------------------------------------
PRO MfitDisplay::readFit, FILENAME = filename, _EXTRA = extra
    
    IF KEYWORD_SET (FILENAME) THEN BEGIN
        out_file = filename
    ENDIF ELSE BEGIN
        out_file = DIALOG_PICKFILE ( $
              TITLE = 'Select a spectral fit filename', $
              FILTER = ['*.fit', '.par'], $ ;== the last is not used for GBM
              PATH = !RM_PARAMS.lastPath, GET_PATH = lastPath, $
              DIALOG_PARENT = self.widgetID.top, $
              /READ)                                ; <---Changed for Mac!
        IF (out_file EQ '') THEN RETURN             ; User Cancelled!
        ; User may have slipped up! Filename is only a path:
        IF ((STRPOS(out_file, '/', /REVERSE_SEARCH) + 1) EQ STRLEN(out_file)) $
            THEN RETURN
    	; If the user accidently puts a space in front of the filename...
    	retpos = STRPOS(out_file, STRING(10B))
    	IF (retpos NE 0) THEN BEGIN
    	   out_file = STRMID(out_file, retpos + 1)
    	ENDIF
        !RM_PARAMS.lastPath = lastPath
    ENDELSE
    
    ; See how many fits in the file:
    numfits = self->parseFITS (out_file)
    IF (numfits EQ -1) THEN RETURN             ;Wrong type of file
    
    IF (numfits EQ 1) THEN BEGIN              ;Plot the single fit
        self->setReadFilename, out_file
    
        self->setStatus, out_file
        result = self->setFitInfoFromFile()

        IF result LT 0 THEN RETURN
        
        self->map
        self->plot, _EXTRA = extra, /PHOTON
	;[*self.PlotOptions, 'XRANGE', 'YRANGE']
    ENDIF ELSE BEGIN
        self.BatchPlotter->setReadFilename, out_file
    
        ;== We're plotting new data; clear out the old plot range info
        self.batchPlotter->clearRanges
        self->map
        self->setStatus, out_file
        self->plot, /BATCH, /RESID, _EXTRA = *self.PlotOptions   
    ENDELSE

END


; ----------------------------------------------------------------------------
; Set the readFilename instance data to a filename. 
; ----------------------------------------------------------------------------
PRO MfitDisplay::setReadFilename, filename

    ;(self.BatchPlotter).readFilename = PTR_NEW ()
    PTR_FREE, self.readFilename
    self.readFilename = PTR_NEW (filename)
       
END


; ----------------------------------------------------------------------------
; Set the readFilename instance data to a null filename. 
; ----------------------------------------------------------------------------
PRO MfitDisplay::clearFit

    ;(self.BatchPlotter).readFilename = PTR_NEW ()
    self.readFilename = PTR_NEW ()
       
END


;------------------------------------------------------------------------------
; Plot the user model.  Count spectrum is plotted by default.
;------------------------------------------------------------------------------
PRO MfitDisplay::plotModel, BATCHPLOT = batchPlot, _EXTRA = extra

    IF (NOT (self.Fitter).FitModel->haveFit ()) THEN BEGIN
       ;ans = self.Error->alert ('Model fit not available.')
       self->setDetectorList, self.Fitter->getDetectorList ()
       self->map
       self->readFit, _EXTRA = extra
       ;== We're plotting new data; clear out the old plot range info
       ;self.batchPlotter->clearRanges
       ;self->plot, _EXTRA = extra, /BATCH, /RESID
       RETURN
    END
 
    self->setFitModel,     self.Fitter->getFitModel ()
    self->setPhotonModel,  self.Fitter->getPhotonModel ()
    self->setDetectorList, self.Fitter->getDetectorList ()
    self->map

    IF KEYWORD_SET (batchPlot) THEN BEGIN
        self->setTermInfo, self.Fitter->getTermInfo ()
        self->setBatchFitList, self.Fitter->getBatchFitList ()
        ;== We're plotting new data; clear out the old plot range info
        self.batchPlotter->clearRanges
        self->plot, _EXTRA = extra, /BATCH, /RESID
    ENDIF ELSE BEGIN
        ;== We're plotting new data; clear out the old plot range info
        self.plotter->clearRanges
        self->plot, _EXTRA = extra, /COUNTS
        names  = self.DetectorList->names ()
        self->setStatus, names[0]
    ENDELSE

END


; ----------------------------------------------------------------------------
; Override the plot method
; ----------------------------------------------------------------------------
PRO MfitDisplay::plot, $
    COUNTS = counts, RAW = raw, PHOTONS = photons, NUFNU = nufnu, ENERGY = energy, $
    CUMULATIVE = cumulative, CHI1D = chi1d, CHI2D = chi2d, DURATION = duration, $
    BATCH = batch, TOPOMAP = topoMap, SIGMA = sigma, STACK = stack, $
    HARDCOPY = hardcopy, _EXTRA = extra    

    plotType_save = self.plotType
    IF (KEYWORD_SET (cumulative)) THEN self.plotType = 'cumulative'
    IF (KEYWORD_SET (raw))     THEN self.plotType = 'raw'
    IF (KEYWORD_SET (counts))  THEN self.plotType = 'counts'
    IF (KEYWORD_SET (photons)) THEN self.plotType = 'photons'
    IF (KEYWORD_SET (energy))  THEN self.plotType = 'energy'
    IF (KEYWORD_SET (nufnu))   THEN self.plotType = 'nufnu'
    IF (KEYWORD_SET (chi1d))   THEN self.plotType = 'chi1d'
    IF (KEYWORD_SET (chi2d))   THEN self.plotType = 'chi2d'
    IF (KEYWORD_SET (batch))   THEN self.plotType = 'batch'
    IF (KEYWORD_SET (duration)) THEN self.plotType = 'duration'
    IF (KEYWORD_SET (topoMap)) THEN self.plotType = 'topoMap'
    IF (KEYWORD_SET (stack))   THEN self.plotType = 'stack'
    IF (KEYWORD_SET (sigma))   THEN self.plotType = 'sigma'
    ;== Different data for batch output
    IF (KEYWORD_SET (batch) OR KEYWORD_SET (topoMap) OR $
        KEYWORD_SET (sigma) OR KEYWORD_SET (duration)) THEN BEGIN  
        self.batchPlotter->setFitModel,     self.FitModel
	    self.batchPlotter->setPhotonModel,  self.PhotonModel
	    self.batchPlotter->setDetectorList, self.Fitter->getDetectorList()
	    self.batchPlotter->setBatchFitList, self.BatchFitList
	    WIDGET_CONTROL, self.gridBaseID, MAP = 1L
    ENDIF 
    
;    IF (self.batchPlotter->haveBatch ()) THEN BEGIN
;        WIDGET_CONTROL, self.gridBaseID, MAP = 1L
;    ENDIF ELSE WIDGET_CONTROL, self.gridBaseID, MAP = 0L
    
    IF (KEYWORD_SET (counts) OR KEYWORD_SET (photons) OR $
        KEYWORD_SET (energy) OR $
        KEYWORD_SET (nufnu) OR KEYWORD_SET (chi1d) OR $
        KEYWORD_SET (chi2d)) THEN BEGIN  ;== Different data for batch output
        ;== Client may call this after reading in batch fit file, 
        ;so we have to fail well...
        IF (NOT OBJ_VALID (self.FitModel)) THEN BEGIN
            self->setStatus, 'No single model fit data available.', 5, /REVERT
            self.plotType = plotType_save
            RETURN
        ENDIF

        self.Plotter->setFitModel,     self.FitModel
        self.Plotter->setPhotonModel,  self.PhotonModel
        self.Plotter->setDetectorList, self.DetectorList
        
	WIDGET_CONTROL, self.specBaseID, MAP = 1L
	;WIDGET_CONTROL, self.gridBaseID, MAP = 0L

    ENDIF

    WIDGET_CONTROL, self.residChoiceID, GET_VALUE = residChoice
    WIDGET_CONTROL, self.showModel, GET_VALUE = modChoice
    WIDGET_CONTROL, self.logChoiceID, GET_VALUE = logChoice
       
    CASE (self.plotType) OF
   
        'cumulative' : self.Plotter->cumulativeSpectrum, residChoice, $
                        (self.fitter)->getFitModelInfo (), $
                        self.showPlotNotes, SHOWMODEL = modChoice, $
                        XLOG = logChoice[0], YLOG = logChoice[1], $
                        HARDCOPY = hardcopy, _EXTRA = extra 
        'raw'      : self.Plotter->rawSpectrum, residChoice, $
                        (self.fitter)->getFitModelInfo (), $
                        self.showPlotNotes, SHOWMODEL = modChoice, $
                        XLOG = logChoice[0], YLOG = logChoice[1], $
                        HARDCOPY = hardcopy, _EXTRA = extra 
        'counts'   : self.Plotter->countSpectrum, residChoice, $
                        (self.fitter)->getFitModelInfo (), $
                        self.showPlotNotes, SHOWMODEL = modChoice, $
                        XLOG = logChoice[0], YLOG = logChoice[1], $
                        HARDCOPY = hardcopy, _EXTRA = extra 
        'photons'  : self.Plotter->photonSpectrum, residChoice, $
                        (self.fitter)->getFitModelInfo (), $
                        self.showPlotNotes, SHOWMODEL = modChoice, $
                        XLOG = logChoice[0], YLOG = logChoice[1], $
                        HARDCOPY = hardcopy, _EXTRA = extra 
        'energy'   : self.Plotter->energySpectrum, residChoice, $
                        (self.fitter)->getFitModelInfo (), $
                        self.showPlotNotes, SHOWMODEL = modChoice, $
                        XLOG = logChoice[0], YLOG = logChoice[1], $
                        HARDCOPY = hardcopy, _EXTRA = extra 
        'nufnu'    : self.Plotter->nufnuSpectrum, residChoice, $
                        (self.fitter)->getFitModelInfo (), $
                        self.showPlotNotes, SHOWMODEL = modChoice, $
                        XLOG = logChoice[0], YLOG = logChoice[1], $
                        HARDCOPY = hardcopy, _EXTRA = extra 
        'chi1d'    : BEGIN
        
             IF (NOT self.fitter->haveChi1D ()) THEN BEGIN
                 self->setStatus, 'No Chisq mapping data available.', 5, /REVERT
                 self.plotType = plotType_save
                 RETURN
             ENDIF
             self.Plotter->plotChi1D, *(self.fitter).chi1DVals, $
                        *(self.fitter).chi1DParm, (self.fitter).bestChi1DParam, $
                        (self.fitter).param1DName, $
                        (self.fitter)->getFitModelInfo (), $
                        self.showPlotNotes, $
                        HARDCOPY = hardcopy, _EXTRA = extra 
             END
             
        'chi2d'    : BEGIN
        
             IF (NOT self.fitter->haveChi2D ()) THEN BEGIN
                 self->setStatus, 'No Chisq mapping data available.', 5, /REVERT
                 self.plotType = plotType_save
                 RETURN
             ENDIF
             self.Plotter->plotChi2D, *(self.fitter).chi1DVals, $
                  *(self.fitter).chi1DParm, $
                  [(self.fitter).bestChi1DParam, (self.fitter).bestChi2DParam], $
                  [(self.fitter).param1DName, (self.fitter).param2DName], $
                   (self.fitter)->getFitModelInfo (), $
                    self.showPlotNotes, $
                  HARDCOPY = hardcopy, _EXTRA = extra 
             END
                          
        'batch'    : self.batchPlotter->plotBatchFit, $
                        HARDCOPY = hardcopy, _EXTRA = extra 
        'topoMap'  : self.batchPlotter->plotTopoMap, $
                        HARDCOPY = hardcopy, _EXTRA = extra 
        'stack'    : self.batchPlotter->plotStack, $
                        HARDCOPY = hardcopy, _EXTRA = extra 
        'sigma'    : self.batchPlotter->plotContours, $
                        HARDCOPY = hardcopy, _EXTRA = extra 
        'duration' : self.batchPlotter->plotDurations, $
                        HARDCOPY = hardcopy, _EXTRA = extra 
    
        ELSE      : MESSAGE, /CONTINUE, 'Unknown plot type: ' + $
                    STRING (self.plotType)
    
    ENDCASE

    ;== Construct a name for the fit FITS file
    ;TODO: Boy, is THIS a kludge!
;    names  = self.DetectorList->names ()
;    det = self.DetectorList->data (names[0])
;    myDet = det.detector
;    reader=mydet->datareader()
;    notes  = (*reader.header).notes
;
;    ;== Add a text message in the status bar  
;    self->setStatus, names[0]

END


; ----------------------------------------------------------------------------
; Override the Display map member function
; ----------------------------------------------------------------------------
PRO MfitDisplay::map, doMap

self->updateDataMenu
self->Display::map, doMap

END


; ----------------------------------------------------------------------------
; Update the Dataset menu
; ----------------------------------------------------------------------------
PRO MfitDisplay::updateDataMenu

;== Remove the old menu elements, if any
IF PTR_VALID (self.dataMenuID) THEN BEGIN

    menuNames = self.dataID->value ()
    numDataMenu = N_ELEMENTS (menuNames)
   
    FOR j = 2, numDataMenu - 1 DO BEGIN
        self.dataID->remove, menuNames[j]
    ENDFOR
ENDIF

;== Add any new datasets to the menu
IF OBJ_VALID (self.DetectorList) THEN BEGIN
    detnames = self.DetectorList->names()
    detnum = self.DetectorList->count ()
    ;== Free the pointer regardless, as we will be doing PTR_NEW on it:
    PTR_FREE, self.dataMenuID
    IF (detNum GT 0) THEN BEGIN
        dataMenuID = LONARR (detnum)
        FOR i = 0, detnum - 1 DO BEGIN
            dataMenuID[i] = self.dataID->add (VALUE = detnames[i], UVALUE = 'datamenu')
        ENDFOR
        self.dataMenuID = PTR_NEW (dataMenuID)  
    ENDIF ELSE self.dataMenuID = PTR_NEW ()
ENDIF
    
END


; ----------------------------------------------------------------------------
; Accessors for fluence interval
; ----------------------------------------------------------------------------
Pro MfitDisplay::set_durationInt

    result = DIALOG_INPUT (NFIELDS = 2, $
           PROMPT = ['Lower bound (keV):', 'Upper bound (keV):'], $
           INITIAL = [self.durationInt[0], self.durationInt[1]], $
           TITLE = 'Set duration integration interval')
          
    IF ((SIZE (result))[0] EQ 0) THEN RETURN ; User cancelled!
    
    mydurationInt = result (SORT (FLOAT (result)))
    
    ;== No guarantee the user entered a valid number!
    IF ((mydurationInt[0] EQ 0.0) OR (mydurationInt[1] EQ 0.0)) THEN RETURN
    
    self.durationInt = mydurationInt
    !RM_PARAMS.durint = mydurationInt

END

Pro MfitDisplay::set_fluenceInt

    result = DIALOG_INPUT (NFIELDS = 2, $
           PROMPT = ['Lower bound (keV):', 'Upper bound (keV):'], $
           INITIAL = [self.fluenceInt[0], self.fluenceInt[1]], $
           TITLE = 'Set flux integration interval')
          
    IF ((SIZE (result))[0] EQ 0) THEN RETURN ; User cancelled!
    
    myfluenceInt = result (SORT (FLOAT (result)))
    
    ;== No guarantee the user entered a valid number!
    IF ((myfluenceInt[0] EQ 0.0) OR (myfluenceInt[1] EQ 0.0)) THEN RETURN
    
    self.fluenceInt = myfluenceInt
    !RM_PARAMS.fluxint = myfluenceInt

END

FUNCTION MfitDisplay::get_fluenceInt

    RETURN, self.fluenceInt

END

FUNCTION MfitDisplay::get_durationInt

    RETURN, self.durationInt

END

; ----------------------------------------------------------------------------
; MfitDisplay
; ----------------------------------------------------------------------------
PRO MfitDisplay__define

    obj = { MFITDISPLAY, INHERITS DISPLAY,   $
        
        plotType      : '',          $   ; Current plot type      
        residChoiceID : 0L,          $   ; Button for choice of residual plot
        specBaseID    : 0L,          $   ; Grid base for spectral fit options
        gridBaseID    : 0L,          $   ; Grid base for batch plot options
        showPlotNotes : 0L,          $   ; Display fit info on printed plots
        showModel     : 0L,          $   ; Display individual models on plots
        logChoiceID   : 0L,          $   ; Log/Lin choice for plots
        fluenceInt    : FLTARR(2),   $   ; The energy interval for peak flux / fluence integration 
        durationInt   : FLTARR(2),   $   ; The energy interval for duration integration
        dataMenuID    : PTR_NEW (),  $   ; Data menu members' IDs
        readFilename  : PTR_NEW (),  $   ; Filename to read fit data
        dataID        : OBJ_NEW (),  $   ; Data menu ID
        Plotter       : OBJ_NEW (),  $   ; Model plotter        
        BatchPlotter  : OBJ_NEW (),  $   ; Batch Fit plotter        

        FitModel      : OBJ_NEW (),  $   ; Fit results
        PhotonModel   : OBJ_NEW (),  $   ; User photon model
        Logger        : OBJ_NEW (),  $   ; The fit logger
        Fitter        : OBJ_NEW (),  $   ; The fit engine (MFIT)
        DetectorList  : OBJ_NEW (),  $   ; List of current detectors       
        TermInfo      : OBJ_NEW (),  $   ; Photon term info
        BatchFitList  : OBJ_NEW ()   $   ; List of batch fit results

    }

END
