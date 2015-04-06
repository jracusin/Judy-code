; ----------------------------------------------------------------------------
;+
; NAME:
;
;     SpectrumDisplay (OBJECT)
;
; PURPOSE:
;
;     An object for interactive manipulation of PHA SPECTRUM data.
;
; CALLING SEQUENCE:
;
;     obj = OBJ_NEW ('spectrumDisplay', filename)
;
; INPUTS:
;
;     filename : A STRING containing a FITS filename
;
; KEYWORDS:
;
;     MAP : Set this keyword to map the widget the display.
;           If MAP = 0, the widget will not be mapped.  See the MAP method.
;           If the keyword is omitted, the default = 1.
;
;     LOOKUP : Set this keyword to 0 to omit trying to read a lookup file 
;           automatically when the object is instantiated, or to 1
;           to read a lookup file.  If the keyword is omitted, the 
;           default = 1.
;
; INHERITS:
;
;     Display, PHA, Spectrum
;
; DEPENDENCIES:
;
;
; METHODS:     
;
; MODIFICATION HISTORY:
;
;     Many changes 1999-2002 by RDP. 
;
;     Written, 1999 Fall, Robert.Mallozzi@msfc.nasa.gov
;
;-
; ----------------------------------------------------------------------------



; ----------------------------------------------------------------------------
; Constructor
; ----------------------------------------------------------------------------
FUNCTION spectrumDisplay::init, filename, MAP = map, LOOKUP = lookup, _EXTRA = extra
        
    ;== Verify input
        
    IF (N_ELEMENTS (filename) EQ 0) THEN BEGIN   
       MESSAGE, /CONTINUE, 'Incorrect number of arguments.'
       RETURN, 0
    ENDIF

    IF (NOT FILE_EXISTS (filename)) THEN BEGIN   
       MESSAGE, /CONTINUE, 'File not found: ' + STRING (filename)
       RETURN, 0
    ENDIF

    IF (N_ELEMENTS (lookup) EQ 0) THEN lookup = 1
    IF (N_ELEMENTS (map) EQ 0)    THEN map = 1
        
    ;== Create a new detector
        
    self.Detector = OBJ_NEW ('Detector', filename, _EXTRA = extra)

    ;== Extract some object references from the Detector for convenience
    self.haveBackground = 0
    
    IF (OBJ_VALID (self.Detector)) THEN BEGIN
        
        self.Reader     = self.Detector->dataReader ()    
        self.Response   = self.Detector->Response ()
        self.Fitter     = self.Detector->Fitter ()
        result = self->spectrum::init(self.Reader)
        self.spectrum = self 
        IF NOT result THEN BEGIN
            MESSAGE, /CONTINUE, 'Spectrum object initialization failed.'
            RETURN, 0
        ENDIF
        ;== 07/11/07 RDP: Added code to automatically read in associated RSP files:
        respFileName = ''
		respFileName = STRLOWCASE(STRTRIM(self.Reader->getRMFFile(), 2))
		IF (respFileName NE '' AND respFileName NE 'none') THEN BEGIN
			mypath = self.Reader->filename (/PATH)
			self.Response->setFilename, mypath + respFileName
			self.Response->read, DATAREADER = self.Reader, ERROR = error
			IF error THEN self.Response->setFilename, ''
		ENDIF
		
		;== Check to automatically read in saved background file:
		backFileName = STRLOWCASE(STRTRIM(self.Reader->getBackFile(/NONINTERACTIVE), 2))
 		IF (backFileName NE '' AND backFileName NE 'none') THEN BEGIN
			mypath = self.Reader->filename (/PATH)
			self.BackReader = OBJ_NEW("phareader", mypath + backFileName)
			;== User may have moved the file!
			IF OBJ_VALID(self.BackReader) THEN BEGIN
				backData = (self.BackReader)->data()
				backSpan = [transpose(backData.times),transpose(backData.times)]
			
				self.background = OBJ_NEW("background", self.BackReader)
				self.background->setspan, backSpan
				
				self->fitBackground, ERROR = error
				self.haveBackground = 1
				;IF error THEN self.Response->setFilename, ''
			ENDIF
		ENDIF
   
    ENDIF ELSE BEGIN

        MESSAGE, /CONTINUE, 'You have not chosen a PHA FITS file.'
        RETURN, 0
    
    ENDELSE
    
    ;== Create the display object
    
    IF (NOT self->Display::init (MAP = 0, /NO_DEFAULT_MENUS, $
        TITLE = self.Reader->filename (/BASENAME), _EXTRA = extra)) THEN BEGIN
       
        MESSAGE, /CONTINUE, 'Display initialization failed.'
        RETURN, 0
     
    ENDIF    

    ;== Set default plotting colors
    
    self->setColors, /INITIALIZE

    ;== Build the interface
    
    self->buildGUI, MAP = map

    ;== Try to read a lookup file
    
    IF (lookup) THEN BEGIN
        
       self->readLookup, /AUTO, /SILENT, FILENAME = f, ERROR = error, _EXTRA = extra
       IF (NOT error) THEN BEGIN
;          self->integrateAndCombine
          self->setStatus, 'Read lookup file: ' + f ;, 5, /REVERT
       ENDIF ELSE BEGIN        ;== No lookup file, must set plot ranges to the default
          self->resetDefaultRange
          self->setLogStatus, [1, 1]
       ENDELSE

    ENDIF 
    
    ;== Register the display with the fitter
    
    self.fitter->registerDetector, self
    
    ;== Start clean
    
    self.dirty = 0
    self.backdirty = 0
    self.RMFdirty = 0

    ;== Plot spectrum:
        
    self->plot, _EXTRA = *self.PlotOptions
    self->showSpan

    RETURN, 1

END

   
; ----------------------------------------------------------------------------
; Destructor
; ----------------------------------------------------------------------------
PRO spectrumDisplay::cleanup
    
    ;== Unregister the display with the fitter
    Detector = self->detector()
    Reader   = Detector->dataReader ()
    filename = Reader->filename (/BASENAME)
    
    ;== Update the associated FITS file:
    IF self.dirty THEN BEGIN
        d = DIALOG_MESSAGE (/QUESTION, 'Save changed lookup parameters for dataset: ' +$
            STRTRIM (filename, 2) + '?')
        IF (d EQ 'Yes') THEN self->lookup, /WRITE
    ENDIF
    
    IF self.RMFdirty THEN BEGIN
        d = DIALOG_MESSAGE (/QUESTION, 'Save new response file into dataset: ' +$
            STRTRIM (filename, 2) + '?')
        IF (d EQ 'Yes') THEN BEGIN
            rmfFile = self.Response->filename (/BASE)
            self->setRespName, rmfFile, /RESP
        ENDIF ;self->lookup, /WRITE
    ENDIF
    
    IF self.backdirty THEN BEGIN
        d = DIALOG_MESSAGE (/QUESTION, 'Save new background file into dataset: ' +$
            STRTRIM (filename, 2) + '?')
        IF (d EQ 'Yes') THEN BEGIN
            backFile = self.BackReader->filename (/BASE)
            self->setRespName, backFile, /BACK
        ENDIF ;self->lookup, /WRITE
    ENDIF
    
    ;== Save the color info
    
    !RM_PARAMS.colors = self.colors
    
    self.fitter->deleteDetector, Reader->filename()
    self.Fitter->haveFit, 0
    self.Fitter->haveBatch, 0

    OBJ_DESTROY, self.Detector
    OBJ_DESTROY, self.BackReader   
    OBJ_DESTROY, self.Background   
    self->Display::cleanup
    self->Spectrum::cleanup

   ; STRUCT_FREE, self.integrated

END


; ----------------------------------------------------------------------------
; Build the display widget
; ----------------------------------------------------------------------------
PRO spectrumDisplay::buildGUI, MAP = map

    ; COMPILE_OPT HIDDEN

    map = KEYWORD_SET (map)
         
    ;== Menubar
    
    menu = self->addMenu (VALUE = 'File')
        id = menu->add (VALUE = 'Print')
        ;id = menu->add (VALUE = 'Print Setup...')
        id = menu->add (VALUE = 'PS Configure...')
        id = menu->add (VALUE = 'Screenshot')                                                      ; AMG

        subMenu = menu->add (VALUE = 'Headers', /MENU)
            id = menu->add (subMenu, VALUE = 'Primary     (EXT 0)')
            id = menu->add (subMenu, VALUE = 'Data Table  (EXT 1)')
            id = menu->add (subMenu, VALUE = 'Notes')
 
        subMenu = menu->add (VALUE = 'Lookup', /MENU)
            id = menu->add (subMenu, VALUE = 'Save Lookup')
            id = menu->add (subMenu, VALUE = 'Read Lookup')
            id = menu->add (subMenu, VALUE = 'File Content')
            id = menu->add (subMenu, VALUE = 'Erase Current')
 
        id = menu->add (VALUE = 'Dismiss', /SEPARATOR)
        
    menu = self->addMenu (VALUE = 'Misc')
        id = menu->add (VALUE = 'Show Current Selections', /SEPARATOR)
        id = menu->add (VALUE = 'Show Keyboard Bindings')
        id = menu->add (VALUE = 'Refresh Plot')
   
    menu = self->addMenu (VALUE = 'Options')
        id = menu->add (VALUE = 'Show rmfit Window')
        ;id = menu->add (VALUE = 'Display RMF')
        subMenu = menu->add (VALUE = 'Colors', /MENU)
            id = menu->add (subMenu, VALUE = 'Background')
            id = menu->add (subMenu, VALUE = 'Foreground')
            id = menu->add (subMenu, VALUE = 'History')
            id = menu->add (subMenu, VALUE = 'Spectrum')
            id = menu->add (subMenu, VALUE = 'Background Model')
        id = menu->add (VALUE = 'Plot Configuration')
        id = menu->add (VALUE = 'Display Default DRM')
        id = menu->add (VALUE = 'Reset Response Filename')
        id = menu->add (VALUE = 'Reset Background Filename')
;
;        ; RMK
;        id = menu->add (VALUE = 'Export ASCII Data')
;        id = menu->add (VALUE = 'Export DRM Data')

    menu = self->addMenu (VALUE = 'Help', /HELP)
        id = menu->add (VALUE = 'Full Help')
        mainMenu = menu->add (VALUE = 'Main Menu Items', /Menu)
              id = menu->add (mainMenu, VALUE = 'File Menu')
              id = menu->add (mainMenu, VALUE = 'Options Menu')
        otherMenu = menu->add (VALUE = 'Other Menu Items', /Menu)
              id = menu->add (otherMenu, VALUE = 'Zoom Items')
              id = menu->add (otherMenu, VALUE = 'Rebin Items')
              id = menu->add (otherMenu, VALUE = 'Selection Items')
              id = menu->add (otherMenu, VALUE = 'Background Fitting')
        id = menu->add (VALUE = 'Spectral Fitting')
        id = menu->add (VALUE = 'About', /SEPARATOR)


    ;== Application buttons
     
    menu = self->addMenuButton (VALUE = 'Zoom:', /TEAROFF)
        id = menu->add (VALUE = 'Zoom')
        id = menu->add (VALUE = 'X Zoom')
        id = menu->add (VALUE = 'Y Zoom')
        id = menu->add (VALUE = 'Zoom In: Selection')
        id = menu->add (VALUE = 'Zoom Out: Full Range')
        id = menu->add (VALUE = 'Full Screen')

    menu = self->addMenuButton (VALUE = 'Rebin:', /TEAROFF)
        id = menu->add (VALUE = 'Signal to Noise')
        id = menu->add (VALUE = 'Full Resolution')
        subMenu = menu->add (VALUE = 'Refine Bins', /MENU)
            id = menu->add (subMenu, VALUE = 'Refine by Half')
            id = menu->add (subMenu, VALUE = 'Refine Single Bin')
        subMenu = menu->add (VALUE = 'Combine Bins', /MENU)
            id = menu->add (subMenu, VALUE = 'Source Intervals')
            id = menu->add (subMenu, VALUE = 'Single Bin')
            id = menu->add (subMenu, VALUE = 'Combine by...')

    id = self->addButton ('Select Background')
    menu = self->addButton ('Select Source')

    menu = self->addMenuButton (VALUE = 'Adjust Source:', /TEAROFF)
        id = menu->add (VALUE = '< Shift Selection')
        id = menu->add (VALUE = '> Shift Selection')
        id = menu->add (VALUE = '< Left Selection')
        id = menu->add (VALUE = '> Left Selection')
        id = menu->add (VALUE = '< Right Selection')
        id = menu->add (VALUE = '> Right Selection')
    
    menu = self->addMenuButton (VALUE = 'Spectral Fitting:', /TEAROFF)
        id = menu->add (VALUE = 'Fit Selections')
        id = menu->add (VALUE = 'Fit Plotter')

;    id = self->addButton ('Fit Plotter')

	logChoice = ['X Log', 'Y Log']
	self.logChoiceID = CW_BGROUP (self.widgetID.buttonBase, logChoice, /ROW, $
            /NONEXCLUSIVE, SET_VALUE = [1,1])
               
    header = self.Reader->header (/POINTER)
    notes  = (*header).notes
    nNotes = N_ELEMENTS (notes)
    FOR i = 0, nNotes - 1 DO $
        id = self->addLabel (notes[i], /ALIGN_LEFT)

    ;== Add a text message in the status bar
    
    self->setStatus, 'Display initialized' ;, 5
     
    ;== Now that the custom widget is build, map it to the screen
       
    IF (map) THEN self->map
        
END


; ----------------------------------------------------------------------------
; Override the widget event handler
; ----------------------------------------------------------------------------
PRO SpectrumDisplay::eventHandler, event

    ; COMPILE_OPT HIDDEN

    ;== Set the drawing area as the IDL current window
    
    self->setWindow
    
    CASE (TAG_NAMES (event, /STRUCTURE)) OF

        'WIDGET_KILL_REQUEST': BEGIN

            ;== Dismiss, not kill

            WIDGET_CONTROL, event.top, MAP = 0 
            END

        'WIDGET_BUTTON': BEGIN

            WIDGET_CONTROL, event.id, GET_VALUE = value

            CASE (value) OF
                                
                ;---------------
                ; MENUBAR EVENTS
                ;---------------  

                'Dismiss'             : WIDGET_CONTROL, event.top, MAP = 0
                'Quit'                : self->Display::eventHandler, event
                'Print'               : self->Display::eventHandler, event
                'Print Setup...'      : self->Display::eventHandler, event
                'PS Configure...'     : self->Display::eventHandler, event
                'Screenshot': BEGIN                                                            ; AMG
                                                                                               ;
                             file=DIALOG_PICKFILE(DEFAULT_EXTENSION='png',  $                  ;
                                                  PATH=!RM_PARAMS.lastPath, $                  ;
                                                  FILE='lightcurve.png',    $                  ;
                                                  /WRITE)                                      ;
                             IF file EQ '' THEN RETURN                                         ;
                             image=TVRD(/TRUE)                                                 ;
                             TVLCT,r,g,b,/GET                                                  ;
                             WRITE_PNG, file, image, r, g, b                                   ;
                                                                                               ;
                           END                                                                 ;

                ;'Spacecraft Location' : self->showCGRO

                'Primary     (EXT 0)' : self.Reader->showHeader, 0
                'Data Table  (EXT 1)' : self.Reader->showHeader, 1
                'Notes'               : self.Reader->showNotes
                 
                'Read Lookup'         : self->lookup, /READ
                'Save Lookup'         : self->lookup, /WRITE
                'File Content'        : self->showLookupFile
                'Erase Current'       : self->lookup, /ERASE
                                                                    
                'Show rmfit Window'  : WIDGET_CONTROL, (self.myrmfit).topID, map = 1;/show
                'Display RMF'        : (self.myrmfit)->load, FILENAME = self.Reader->getRMFFile()
                'Show Current Selections' : self->showSpan
                'Show Keyboard Bindings'  : self->showKeyBindings
                'Refresh Plot'            : self->plot, _EXTRA = *self.PlotOptions
                'Reset Response Filename' : self->setRespName, rmfFile, /RESP, /RESET
                'Reset Background Filename' : self->setRespName, rmfFile, /BACK, /RESET
                'Clear Default DRM' : BEGIN
					rmfFile = ''
					self->setRespName, rmfFile
					self.Response->setFilename, rmfFile
					self.Reader->setRMFFile, rmfFile
					self.RMFdirty = 0
                    END
                'Display Default DRM' : BEGIN
					rmfFile = self.Response->filename ()
					IF (rmfFile NE '' AND rmfFile NE 'none') THEN BEGIN
						self.myrmfit->load, FILENAME = rmfFile
					ENDIF ELSE self->setStatus, 'No default DRM found in file!', 10, /REVERT
                    END

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

                ;---------------
                ; SELECTION EVENTS
                ;---------------  

                'Select Source' : BEGIN
                    self->disable
                    self->setStatus, $
                        'Select source interval(s).  ' + $
                        'Use margins for special functions.' 
                    self->selectSource,/INTERACTIVE
                    self->showSpan
                    self->enable
                    END
                   
                '< Shift Selection'  : BEGIN
                    self->adjustSource, /LEFT,  /EXPAND
                    self->adjustSource, /RIGHT, /CONTRACT
                    self->showSpan
                    END
                     
                '> Shift Selection'  : BEGIN
                    self->adjustSource, /RIGHT,  /EXPAND
                    self->adjustSource, /LEFT, /CONTRACT
                    self->showSpan
                    END
                     
                '< Left Selection'  : BEGIN
                    self->adjustSource, /LEFT,  /EXPAND
                    self->showSpan
                    END
                     
                '> Left Selection'  : BEGIN
                    self->adjustSource, /LEFT,  /CONTRACT
                    self->showSpan
                    END
                     
                '< Right Selection' : BEGIN
                    self->adjustSource, /RIGHT, /CONTRACT
                    self->showSpan
                    END
                     
                '> Right Selection' : BEGIN
                    self->adjustSource, /RIGHT, /EXPAND
                    self->showSpan
                    END

                'Select Background' : self->addBackground, /RENEW

                ;---------------
                ; ZOOM EVENTS
                ;---------------  

                'Zoom' : BEGIN
                    self->disable
                    self->setStatus, $
                       'Select zoom region.  ' + $
                       'Use margins for special functions.' 
                    self->zoom            
                    ;self->clearStatus
                    self->showSpan
                    self->enable
                    END

                'X Zoom' : BEGIN
                    self->disable
                    self->setStatus, $
                       'Select zoom interval.  ' + $
                       'Use margins for special functions.' 
                    self->zoom, /X
                    ;self->clearStatus
                    self->showSpan
                    self->enable
                    END

                'Y Zoom' : BEGIN
                    self->disable
                    self->setStatus, $
                       'Select zoom interval.  ' + $
                       'Use margins for special functions.' 
                    self->zoom, /Y
                    ;self->clearStatus
                    self->showSpan
                    self->enable
                    END

                'Zoom Out: Full Range' : self->zoom, /DEFAULTRANGE            
                'Zoom In: Selection'   : self->zoom, /SELECTION
                'Full Screen'          : self->zoom, /FULLSCREEN

                ;---------------
                ; REBIN EVENTS
                ;---------------  

                'Signal to Noise'   : self->rebin, /BINBYSNR
                'Full Resolution'   : self->rebin, /FULLRESOLUTION
                'Source Intervals'  : self->rebin, /SELECTIONS                
                'Single Bin'        : BEGIN
										  self->disable
										  self->rebin, /SINGLEINTERVAL
										  self->enable
									  END
                'Combine by...'     : BEGIN
										  self->disable
										  self->rebin, /USERINTERVAL
										  self->enable
									  END
                'Refine by Half'    : BEGIN
										  self->disable
										  self->rebin, /BYHALF
										  self->enable
									  END
                'Refine Single Bin' : BEGIN
										  self->disable
										  self->rebin, /REFINESINGLE
										  self->enable
									  END
                
                ;---------------
                ; FIT SPECTRA
                ;---------------  

                ;== Fit the model
        
                'Fit Selections'   : BEGIN
                        self.Fitter->fitModel, ENERGYINTERVAL = !RM_PARAMS.fluxint, $
                                    /SELECT_MODEL, /SELECTIONS, FITPLOT = *self.PlotOptions
                        END
                
                ;---------------
                ; PLOT SPECTRA
                ;---------------  

                ; Count spectrum plotted by default.  Other spectrum types
                ; can be selected directly from the MfitDisplay object window.
                
                'Fit Plotter' : BEGIN
                        (self.Fitter).display->plotModel, _EXTRA = *self.PlotOptions
                        ;self.Fitter->plotModel, _EXTRA = *self.PlotOptions
                        END

                ;---------------
                ; INFO
                ;---------------  
                
                'Full Help'          : self->info, /HELP
                'File Menu'          : XDISPLAYFILE, !MFIT.HELP_PATH + 'file.hlp',    DONE = 'Done'
                'Options Menu'       : XDISPLAYFILE, !MFIT.HELP_PATH + 'options.hlp', DONE = 'Done'
                'Spectral Fitting'   : XDISPLAYFILE, !MFIT.HELP_PATH + 'specfit.hlp', DONE = 'Done'
                'Zoom Items'         : XDISPLAYFILE, !MFIT.HELP_PATH + 'zoom.hlp',    DONE = 'Done'
                'Rebin Items'        : XDISPLAYFILE, !MFIT.HELP_PATH + 'rebin.hlp',   DONE = 'Done'
                'Selection Items'    : XDISPLAYFILE, !MFIT.HELP_PATH + 'select.hlp',  DONE = 'Done'
                'Background Fitting' : XDISPLAYFILE, !MFIT.HELP_PATH + 'backfit.hlp', DONE = 'Done'
                'About'              : self->info, /ABOUT                    

                ELSE: ;self->setStatus, 'Event not handled: ' + value, 2

            ENDCASE
            
            END ; WIDGET_BUTTON events
        

        'WIDGET_TEXT_CH': self->keyBindings, event.ch

        ELSE: BEGIN

            ;== Handle the radio buttons; name is *not* returned in 'value'
            WIDGET_CONTROL, event.id, GET_VALUE = value

            self->changeLog, value
            
            END;self->setStatus, 'Event not handled.' , 2
                       
    ENDCASE

    self->enable      
END

 
; ----------------------------------------------------------------------------
; Handle keyboard events in the drawing area
; ----------------------------------------------------------------------------
PRO SpectrumDisplay::keyBindings, char
    
    ; COMPILE_OPT HIDDEN

    setXRange = 0
    setYRange = 0

    CASE (STRING (char)) OF

         'd' : WIDGET_CONTROL, self.widgetID.top, MAP = 0
         
         'i' : BEGIN     ;== interactive selection
                    self->disable
                    self->setStatus, $
                        'Select source interval(s).  ' + $
                        'Use margins for special functions.' 
                    self->selectSource,/INTERACTIVE
                    self->showSpan
                    self->enable
                    END

         'z' : BEGIN
                   self->disable
				   self->zoom
				   self->enable
               END
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
         'g' : self->zoom, /SELECTION
         
         'r' : self->Plot, _EXTRA = *self.PlotOptions
         
         'f' : self->zoom, /FULLSCREEN
         
         ',' : BEGIN ; zoom in by steps: x-axis
             self->Plot, XSTYLE = 1;, _EXTRA = *self.PlotOptions
             xr = SCALEAXIS (/XAXIS, -0.05) 
             yr = AXISRANGE (/YAXIS)
             setXRange = 1
             END

         '.' : BEGIN ; zoom out by steps: x-axis
             self->Plot, XSTYLE = 1;, _EXTRA = *self.PlotOptions
             xr = SCALEAXIS (/XAXIS, 0.05) 
             yr = AXISRANGE (/YAXIS)
             setXRange = 1
             END

         'k' : BEGIN ; pan left: x-axis
             self->Plot, XSTYLE = 1;, _EXTRA = *self.PlotOptions
             xr = PANAXIS (/XAXIS, 0.03) 
             yr = AXISRANGE (/YAXIS)
             setXRange = 1
             END

         'l' : BEGIN ; pan right: x-axis
             self->Plot, XSTYLE = 1;, _EXTRA = *self.PlotOptions
             xr = PANAXIS (/XAXIS, -0.03) 
             yr = AXISRANGE (/YAXIS)
             setXRange = 1
             END

         '<' : BEGIN ; zoom in by steps: y-axis
             self->Plot, YSTYLE = 1;, _EXTRA = *self.PlotOptions
             yr = SCALEAXIS (/YAXIS, -0.05) 
             xr = AXISRANGE (/XAXIS)
             setYRange = 1
             END

         '>' : BEGIN ; zoom out by steps: y-axis
             self->Plot, YSTYLE = 1;, _EXTRA = *self.PlotOptions
             yr = SCALEAXIS (/YAXIS, 0.05) 
             xr = AXISRANGE (/XAXIS)
             setYRange = 1
             END

         'K' : BEGIN ; pan left: y-axis
             self->Plot, YSTYLE = 1;, _EXTRA = *self.PlotOptions
             yr = PANAXIS (/YAXIS, 0.03) 
             xr = AXISRANGE (/XAXIS)
             setYRange = 1
             END

         'L' : BEGIN ; pan right: y-axis
             self->Plot, YSTYLE = 1;, _EXTRA = *self.PlotOptions
             yr = PANAXIS (/YAXIS, -0.03) 
             xr = AXISRANGE (/XAXIS)
             setYRange = 1
             END

         '9' : BEGIN ; < Left Selection
             self->adjustSource, /LEFT,  /EXPAND
             self->showSpan
             END

         '(' : BEGIN ; > Left Selection
             self->adjustSource, /LEFT,  /CONTRACT
             self->showSpan
             END

         ')' : BEGIN ; < Right Selection
             self->adjustSource, /RIGHT, /CONTRACT
             self->showSpan
             END

         '0' : BEGIN ; > Right Selection
             self->adjustSource, /RIGHT, /EXPAND
             self->showSpan
             END

         ELSE: 
         
    ENDCASE
         
    IF ((setXRange) OR (setYRange)) THEN BEGIN
    
       range = TRANSPOSE ([[xr], [yr]])
       
       self->setRange, range
           
       self->plot, $
           XRANGE = xr, XSTYLE = setXRange, $
           YRANGE = yr, YSTYLE = setYRange;, $
          ; _EXTRA = *self.PlotOptions
           
    ENDIF
    
END


; ----------------------------------------------------------------------------
; Display available key bindings
; ----------------------------------------------------------------------------
PRO SpectrumDisplay::showKeyBindings

    ; COMPILE_OPT HIDDEN

    txt = [ $
    
        'd : Dismiss (unmap) the display widget', $         
        'z : Enter interactive zoom mode', $      
        'x : Zoom mode restricted to the x-axis', $   
        'y : Zoom mode restricted to the y-axis', $   
        'g : Go (and zoom in) to selected data', $
        'f : Fullscreen display of the current plot', $
        ' ', $
        ', : Zoom in by steps  : x-axis', $
        '. : Zoom out by steps : x-axis', $
        '< : Zoom in by steps  : y-axis', $
        '> : Zoom out by steps : y-axis', $
        ' ', $
        'k : Pan left  : x-axis', $
        'l : Pan right : x-axis', $
        'K : Pan left  : y-axis', $
        'L : Pan right : y-axis', $
        ' ', $
        '9 : Expand the left selection boundry',   $
        '0 : Expand the right selection boundry',  $
        '( : Contract the left selection boundry', $
        ') : Contract the right selection boundry' $
    ]        

    base = WIDGET_BASE (/COLUMN, /BASE_ALIGN_CENTER, $
        GROUP_LEADER = self.widgetID.top, $
        TITLE = 'Keyboard Bindings')
    
    b = WIDGET_BASE (base, /COLUMN, /FRAME, XPAD = 10, YPAD = 10)
    FOR i = 0, N_ELEMENTS (txt) - 1 DO $
        label = WIDGET_LABEL (b, VALUE = txt[i], /ALIGN_LEFT)
        
    button = WIDGET_BUTTON (base, VALUE = 'Dismiss')
    
    WIDGET_CONTROL, base, /REALIZE

    XMANAGER, '_unused', base, /NO_BLOCK, EVENT_HANDLER = 'DIALOG_DISMISS'
   
END


; ----------------------------------------------------------------------------
; Create a label of the current span
; ----------------------------------------------------------------------------
FUNCTION spectrumDisplay::label, TIME = time, ENERGY = energy

    ; COMPILE_OPT HIDDEN

    units = self.Reader->units (/POINTER)
            
    IF (KEYWORD_SET (time)) THEN BEGIN
       
       myData = self.Reader->data (/POINTER)
       range = (*myData).times
       range = REFORM (range)
       
       unit = (*units).times
       
               
    ENDIF 
    
    IF (KEYWORD_SET (energy)) THEN BEGIN
     
       range = NEAREST_EDGE (self->combinedThresholds (), $
           (self->energySpan ())[0, *])
       range = REFORM (range)
       
       unit = (*units).eedges
       
    ENDIF 
    
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
    RETURN, label              
                  
END


;------------------------------------------------------------------------------
; Interactively select source intervals from the plotted event
;------------------------------------------------------------------------------
PRO SpectrumDisplay::selectSource, $
    INTERACTIVE = interactive, SIGNAL_TO_NOISE = signalToNoise, $
    HAVESELECTION = haveSelection

    ; COMPILE_OPT HIDDEN

	thresholds = self->combinedThresholds ()
	span = self->energySpan ()
    
    ;== Select source interval(s) remotely - haveSelection should be a span
    IF (KEYWORD_SET (haveSelection)) THEN BEGIN

       self->plot, _EXTRA = *self.PlotOptions
       self->setEnergySpan, haveSelection
;       self->integrateAndCombine

    ENDIF ; remotely

    ;== Select source interval(s) interactively
    IF (KEYWORD_SET (interactive)) THEN BEGIN

       self->plot, _EXTRA = *self.PlotOptions      ;, XSTYLE = 1, YSTYLE = 1
       newSpan = SELECT (thresholds, self.widgetID.draw, span, $
                COLOR = self.color->color('FG'),      $ 
                BACKGROUND = self.color->color('BG'), $ 
                HAVESELECTION = gotSelection)

       IF (NOT gotSelection) THEN BEGIN
           self->setEnergySpan, span, /INITIALIZE
           RETURN
       ENDIF
       
       self->setEnergySpan, newSpan
;       self->integrateAndCombine

    ENDIF ; interactive
    
    self.dirty = 1   ;== Selections have changed
    self->plot, _EXTRA = *self.PlotOptions         ;, XSTYLE = 1, YSTYLE = 1

END 


;------------------------------------------------------------------------------
; Interactively select source intervals from the plotted event
;------------------------------------------------------------------------------
PRO SpectrumDisplay::adjustSource, $
    LEFT = left, RIGHT = right, EXPAND = expand, CONTRACT = contract 

    ; COMPILE_OPT HIDDEN

	data = self->combinedThresholds ()
	span = self->energySpan ()

    nSelections = (SIZE (span))[1] - 1           
    s = SIZE (data)
    
    FOR i = 1, nSelections DO BEGIN
    
        edges = NEAREST_EDGE (data, span[i, *], idx) 

        IF (KEYWORD_SET (right)) THEN BEGIN

           sign = KEYWORD_SET (expand) ? +1 : -1

           idx[1] = idx[1] + sign
           IF (idx[1] LT idx[0]) THEN idx[1] = idx[0]
           IF (idx[1] GE s[2])   THEN idx[1] = s[2] - sign

           e = data[*, idx[1]]
           m = (e[1] - e[0]) / 2.0 + e[0]
           span[i, 1] = m

        ENDIF

        IF (KEYWORD_SET (left)) THEN BEGIN

           sign = KEYWORD_SET (expand) ? -1 : +1

           idx[0] = idx[0] + sign
           IF (idx[0] GT idx[1]) THEN idx[0] = idx[1]
           IF (idx[0] LT 0)      THEN idx[0] = 0

           e = data[*, idx[0]]
           m = (e[1] - e[0]) / 2.0 + e[0]
           span[i, 0] = m

        ENDIF

    ENDFOR
    
    maxv = MAX (span, MIN = minv)
    span[0, *] = [minv, maxv]
    
    ;== Resolve overlapping selections

; TODO: resolve selections - this is not exactly the correct behavior
    
    newSpan = span
    FOR i = 1, nSelections DO BEGIN
        
        coords = FLTARR (2, 2)
        coords[0, *] = span[i, *]
        newSpan = RESOLVE_SELECTIONS (data, coords, newSpan)
        
    ENDFOR
        
    self->setEnergySpan, newSpan
;    self->integrateAndCombine

    self.dirty = 1   ;== Selections have changed
    self->plot, _EXTRA = *self.PlotOptions

END


;------------------------------------------------------------------------------
; Fit background model
;------------------------------------------------------------------------------
PRO spectrumDisplay::addBackground, RENEW = renew, ERROR = error, $
    CHANNELS = channels, BACKSPAN = backSpan
    
    ;== Get the background file name:    
    IF KEYWORD_SET(RENEW) THEN BEGIN
    	OBJ_DESTROY, self.BackReader
    	OBJ_DESTROY, self.background
		backFileName = self.Reader->getBackFile(/RENEW)
    ENDIF ELSE backFileName = self.Reader->getBackFile()
                          
	IF (backFileName EQ '' OR backFileName EQ 'none') THEN BEGIN
		RETURN ; No such file exists!
	ENDIF
	
	self.BackReader = OBJ_NEW("phareader", backFileName)
	backData = (self.BackReader)->data()
	backSpan = [transpose(backData.times),transpose(backData.times)]

	self.background = OBJ_NEW("background", self.BackReader)
    self.background->setspan, backSpan
    
    self->fitBackground
    self.BackDirty = 1        ;== Background has changed
    
    self->plot, _EXTRA = *self.PlotOptions
    
END


;------------------------------------------------------------------------------
; Fit background model
;------------------------------------------------------------------------------
PRO spectrumDisplay::fitBackground, ERROR = error, $
    CHANNELS = channels, BACKSPAN = backSpan
    
    ; COMPILE_OPT HIDDEN

    ;== Get user-selected energy channels
    
    combinedThresholds = self.Spectrum->combinedThresholds ()
    data = self.Reader->data (/POINTER)
    energySpan = self.Spectrum->energySpan ()
    e = NEAREST_EDGE (combinedThresholds, energySpan[0, *], channels) 
    energyLookup = self.Spectrum->energyLookup ()
    self.background->setEnergyLookup, energyLookup, INITIALIZE = initialize
    numBins = (SIZE (energyLookup))[1] - 1
    channels = [energyLookup[channels[0]], energyLookup[channels[1] + 1 < numBins] - 1]

    ;== Fit background model

    success = self.Background->fit (ORDER = 0, CHANNELS = channels)    
    
    IF (NOT success) THEN BEGIN
       self->setStatus, 'Background fit failed or canceled.'
       RETURN
    ENDIF
    
    ;== Integrate the background model
    
    self->integrateBackground
    self.haveBackground = 1   ;== Background is valid for plotting

END


;------------------------------------------------------------------------------
; Integrate the background model
;------------------------------------------------------------------------------
PRO spectrumDisplay::integrateBackground

    ; COMPILE_OPT HIDDEN

    ;== Integrate background over time
    
	self.Background->integrate, $
	   self.Spectrum->combinedThresholds (), $
	   self.Spectrum->energySpan (), $
	   self.Spectrum->energyLookup (), /TIME

END


; ----------------------------------------------------------------------------
; Update the status bar with the current spans
; ----------------------------------------------------------------------------
PRO spectrumDisplay::showSpan

    ; COMPILE_OPT HIDDEN

    tLabel = self->label (/TIME)
    eLabel = self->label (/ENERGY)
    
    label = tLabel + ', ' + eLabel
    self->setStatus, label
    
END


; ----------------------------------------------------------------------------
; Read the response
; ----------------------------------------------------------------------------
FUNCTION spectrumDisplay::readRMF

    ;== Read DRM
        
    done = 0
    prevResp = self.Response->Filename()

    IF (prevResp EQ '') THEN BEGIN   ; We don't have a DRM for this detector yet:
	    WHILE (NOT done) DO BEGIN
	        
;             header = self.Reader->header ()   
;             f = SXPAR (header.ext1, 'RESPFILE')
             f = self.Reader->getRMFFile ()
             ;== The filename can still be blank:
             IF (f eq '') THEN BEGIN
                 cnt = 0
             ENDIF ELSE BEGIN
                 c = FINDFILE (!rm_params.lastpath+f, COUNT = cnt)
             ENDELSE
             
             IF (CNT NE 0) THEN BEGIN
                self.Response->setFilename, !rm_params.lastpath+f,  ERROR = error ;/INTERACTIVE, $
                    ;/MUST_EXIST, FILTER = ['*.rmf','*.rsp'], DIALOG_PARENT = self.widgetID.top, $
                    ;TITLE = 'Select Response File for Reading',
             ENDIF ELSE BEGIN
                 path = self.Reader->filename (/PATH)
                 root = self.Reader->filename (/ROOT)
    			
                 f = path + root + '.rsp'
                 ; 12/22/09 RDp CCR#223: Added filter for RSP type II files:
                 self.Response->setFilename, f, /INTERACTIVE, $
                    /MUST_EXIST, FILTER = ['*.rsp','*.rsp2','*.rmf','*.RSP','*.RMF'], DIALOG_PARENT = self.widgetID.top, $
                    TITLE = 'Select Response File for Dataset "' + root + '.pha"', $
                    PATH = path, ERROR = error 
                 !RM_PARAMS.lastpath = self.Response->filename (/PATH)

				self.RMFdirty = 1
             ENDELSE

             IF (error) THEN BEGIN
                 cancel = 1
                 RETURN, 0    
             ENDIF

             self.Response->read, DATAREADER = self.Reader, ERROR = error
             
             IF (error) THEN BEGIN
                 self->setStatus, $
                    'Failed to read response file: ' + $
                    self.Response->filename (), 5, /REVERT
             ENDIF ELSE BEGIN
                self->showSpan
                done = 1
             ENDELSE
 	           
         ENDWHILE
    ENDIF
    
    resp = self.Response->response ()
    
    RETURN, resp
    
END


; ----------------------------------------------------------------------------
; Handle XY Log axes change events
; ----------------------------------------------------------------------------
PRO spectrumDisplay::changeLog, myValue

    ; COMPILE_OPT HIDDEN

    self->setLogStatus, myValue

    self->plot, _EXTRA = *self.PlotOptions
    
END


;------------------------------------------------------------------------------
; Fit photon model
;------------------------------------------------------------------------------
PRO spectrumDisplay::getSpectra, cancel, BATCH = batch, $
    SELECTIONS = selections

    ; COMPILE_OPT HIDDEN
    
    cancel = 0
    
    ;== Return pointers for efficiency.  Do NOT alter these data!

    data           = self.Reader->data (/POINTER)   
    combTimes      = (*data).times
    combThresholds = self->combinedThresholds (/POINTER)
    
        
    IF (KEYWORD_SET (selections)) THEN $
       span = combTimes
    
    ;== Read DRM
    resp = self->readRMF ()
    
    self->integrateTime, span, $       ;HISTORY = history, 
        SPECTRUM = spectrum, ERRORS = errors, LIVETIME = totalTime

    ;== Get user-selected energy channels and time range
    
    energySpan = self->energySpan ()
    eInt = NEAREST_EDGE (*combThresholds, energySpan[0, *], fitChannels) 
    eLookup = self->energyLookup()
    numBins = N_ELEMENTS (eLookup) - 1

    fitChannels[0] = eLookup[fitChannels[0]]
    fitChannels[1] = eLookup[fitChannels[1] + 1 < numBins] - 1
    tInt = TRANSPOSE (combTimes) 

    ;== Energy bin widths
    width = resp.chan_width ;REFORM (((*data).eedges)[1, *] - ((*data).eedges)[0, *])

    ;== Convert to counts / s-energy
    
    obsCrate = spectrum / width   
        
    ;== Integrate the background model over the selected time span
    ;using the full energy resolution
    
    lookupSave = eLookup     ;self->energyLookup()
    self->setEnergyLookup, lookupSave, /INITIALIZE

    n = SIZE (((*data).rates))
    backCrate = FLTARR (n[1])
    backCsig  = FLTARR (n[1])
    
    ;== Compute the livetime
    COUNT_SPAN, *combThresholds, energySpan, eIdx
    liveTime = FLTARR (N_ELEMENTS (obsCrate))
    nebin = N_ELEMENTS (eIdx)

    IF (self.haveBackground) THEN BEGIN
		;self->integrateBackgroundSelections, span, backCrate, backCsig, error
		;backCrate = TRANSPOSE(self.Background->evalModel (span[0], span[1]) )
		;backCsig = TRANSPOSE(self.Background->evalMSig (span[0], span[1]) )
		myback = self.background
		myBakData = myback.reader->data()
		backCrate = myBakData.rates
		backCsig  = myBakData.errors

        FOR i = 0, nebin - 1 DO BEGIN
            eLow = elookup[eidx[i]]
            eHigh = elookup[eidx[i] + 1] - 1
            myLive = (errors[eLow] EQ 0.0) ? totalTime : spectrum[eLow] / (errors[eLow])^2
            liveTime[eLow: eHigh] = myLive
        ENDFOR
    ENDIF ELSE BEGIN
		;== Implement the kludge that accepts no background model
		;TODO: Obtain the background model from a PHA file!!!
		backCsig = errors ;/ width
		FOR i = 0, nebin - 1 DO BEGIN
			liveTime[elookup[eidx[i]]: elookup[eidx[i] + 1] - 1] = 9.99E26
		ENDFOR
    ENDELSE

    self->setEnergyLookup, lookupSave   ;== Restore previous lookup

    ;== Add this detector to the Mfit list
     
    filename = self.Reader->filename ()
    IF (self.Fitter->inList (filename)) THEN BEGIN ;== We have to delete the old data first:

       self.Fitter->deleteDetector, filename          

    ENDIF    
    
    self.Fitter->addDetector, $          
           self, tInt, eInt, fitChannels, obsCrate, $
           backCrate / resp.chan_width, backCsig / resp.chan_width, liveTime

END


; ----------------------------------------------------------------------------
; Integrate a Spectrum over time.
; OVERRIDING the version in SPECTRUM!
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
PRO SpectrumDisplay::integrateTime, span, $
    SPECTRUM = spectrum, ERRORS = errors, LIVETIME = livetime

    data = self.Reader->data (/POINTER)
    spectra_t     = ((*data).rates)
    spectra_t_err = ((*data).errors)
    
    totalTime = ((*data).times)[1]

    ;== Set object internal data members or return the results
        
    IF (ARG_PRESENT (spectrum) OR ARG_PRESENT (errors) OR $
       ARG_PRESENT (liveTime)) THEN BEGIN

       spectrum = spectra_t 
       errors   = spectra_t_err
       liveTime = totalTime
       
    ENDIF ELSE BEGIN
       
       PTR_FREE, self.integratedSpec
       PTR_FREE, self.integratedSpecErr

       self.integratedSpec    = PTR_NEW (spectra_t)
       self.integratedSpecErr = PTR_NEW (spectra_t_err)

       self.liveTime = totalTime
   
    ENDELSE
     
END    


;------------------------------------------------------------------------------
; Rebin 
;------------------------------------------------------------------------------
PRO SpectrumDisplay::rebin, $
    SELECTIONS     = selections, $
    FULLRESOLUTION = fullResolution, $
    SINGLEINTERVAL = singleInterval, $
    HAVEINTERVAL   = haveInterval,   $
    USERINTERVAL   = userInterval,   $
    BYHALF         = byHalf,         $
    REFINESINGLE   = refineSingle,   $
    BINBYSNR       = binBySNR

    ; COMPILE_OPT HIDDEN

	data  = self->combinedThresholds ()

    IF (KEYWORD_SET (binBySNR)) THEN BEGIN
       targetSNR = 0.
       targetSNR = DIALOG_INPUT (PROMPT = 'Enter the desired SNR per bin: ', $
           TITLE = 'Combine Bins', DIALOG_PARENT = self.widgetID.top)
       IF (targetSNR EQ '') THEN RETURN
       ;targetSNR = FIX (targetSNR)
      
    ENDIF

    IF (KEYWORD_SET (userInterval)) THEN BEGIN

       nBins = DIALOG_INPUT (PROMPT = 'Enter number of bins to combine', $
           TITLE = 'Combine Bins', DIALOG_PARENT = self.widgetID.top)
       IF (nBins EQ '') THEN RETURN
       nBins = FIX (nBins)
      
    ENDIF
    
    IF (KEYWORD_SET (userInterval) OR KEYWORD_SET (singleInterval)) THEN BEGIN
    
       self->plot, _EXTRA = *self.PlotOptions
       haveSelection = 1
       WHILE (haveSelection) DO BEGIN
           newSpan = SELECT (data, self.widgetID.draw, $
                COLOR = self.color->color('FG'),      $ 
                BACKGROUND = self.color->color('BG'), $ 
                HAVESELECTION = haveSelection, /SINGLEINTERVAL)
           IF (haveSelection) THEN BEGIN    
               interval = REFORM (newSpan[0, *])
               IF (KEYWORD_SET (userInterval)) THEN BEGIN 
                   self->spectrum::rebin, INTERVAL = interval, NBINS = nBins
               ENDIF ELSE BEGIN
                   self->spectrum::rebin, INTERVAL = interval
               ENDELSE
				IF (self.haveBackground) THEN BEGIN
					self->fitBackground
					;self->integrateBackground
				ENDIF
               self->plot, _EXTRA = *self.PlotOptions 
           ENDIF
       END 
             
    ENDIF
        
    IF (KEYWORD_SET (byHalf) OR KEYWORD_SET (refineSingle)) THEN BEGIN
    
        ;== Modified June 19, 2003 RDP: We want to have multiple selections:
        haveSelection = 0
       
        REPEAT BEGIN
    	    self->plot, _EXTRA = *self.PlotOptions
    	    newSpan = SELECT (data, self.widgetID.draw, /SINGLEBIN, $
                COLOR = self.color->color('FG'),      $ 
                BACKGROUND = self.color->color('BG'), $ 
            	HAVESELECTION = haveSelection, /IMMEDIATE)
    
    	    IF (haveSelection) THEN BEGIN    
        	    singleBin = REFORM (newSpan[0, *])
        
        	    ; New stuff:
        	    CASE (1) OF
                	KEYWORD_SET (byHalf)         : self->spectrum::rebin, SINGLEBIN = singleBin, /BYHALF
                	KEYWORD_SET (refineSingle)   : self->spectrum::rebin, SINGLEBIN = singleBin, /REFINESINGLE
        	    END  
    	    ENDIF
    
    		lookup = self->energyLookup()
    		self->setEnergyLookup, lookup
    
			IF (self.haveBackground) THEN BEGIN
				self->fitBackground
				;self->integrateBackground
			ENDIF
    	    self->plot, _EXTRA = *self.PlotOptions
        ENDREP UNTIL (NOT haveSelection)
        self.dirty = 1   ;== Binning has changed
        RETURN
    ENDIF
        
    CASE (1) OF
    
        KEYWORD_SET (selections)     : self->spectrum::rebin, /SELECTIONS
        KEYWORD_SET (fullResolution) : self->spectrum::rebin, /FULLRESOLUTION
        KEYWORD_SET (haveInterval)   : self->spectrum::rebin, INTERVAL = haveInterval
        KEYWORD_SET (binBySNR)       : BEGIN
	          myDat = self.backreader->data()
	          thresholds = myDat.times
              self.spectrum->binBySignificance, self.Background, thresholds, targetSNR, $
	          ERROR = error
			  IF error THEN BEGIN
				 self->setStatus, 'No background model exists!', 10, /REVERT
			     RETURN
	          ENDIF
           END
                    
        ELSE: 

    ENDCASE

	lookup = self->energyLookup()
	self->setEnergyLookup, lookup
    IF (self.haveBackground) THEN BEGIN
        self->fitBackground
        ;self->integrateBackground
    ENDIF

    self.dirty = 1   ;== Binning has changed
    self->plot, _EXTRA = *self.PlotOptions

END


;------------------------------------------------------------------------------
; Read/write lookup file
;------------------------------------------------------------------------------
PRO spectrumDisplay::lookup, READ = read, WRITE = write, ERASE = erase

    ; COMPILE_OPT HIDDEN
                
    IF (KEYWORD_SET (read)) THEN BEGIN
                
       self->readLookup, ERROR = error, FILENAME = f
       IF (NOT error) THEN BEGIN
          self->setStatus, 'Read lookup file: ' + f ;, 5, /REVERT
          self->plot, _EXTRA = *self.PlotOptions
          self->showSpan
       ENDIF ELSE BEGIN
          self->setStatus, 'Failed to read lookup file: ' + f, 5, /REVERT
       ENDELSE
       
    ENDIF 

    IF (KEYWORD_SET (write)) THEN BEGIN
                
       ok = self->writeLookup (FILENAME = f)
       IF (ok) THEN BEGIN
          self->setStatus, 'Wrote lookup file: ' + f, 5, /REVERT
       ENDIF ELSE BEGIN
          IF (f NE '') THEN $
             self->setStatus, 'Failed to write lookup file: ' + f, 5, /REVERT
       ENDELSE
       
    ENDIF   

    self.dirty = 0   ;== Selections have been read or written, start clean again
    
    IF (KEYWORD_SET (erase)) THEN BEGIN

       self->setTimeLookup,   /INITIALIZE
       self->setEnergyLookup, /INITIALIZE
       self->setTimeSpan,     /INITIALIZE
       self->setEnergySpan,   /INITIALIZE

       self->rebin, /FULLRESOLUTION
       self->update, /FULL
       self->combine, /FULL_RESET
       self.haveDefaultRange   = 0 

       self.dirty = 1   ;== Selections have changed (obviously!)
       self.myLookup = ''
       self->plot, _EXTRA = *self.PlotOptions
       self->showSpan

    END
    
END


; ----------------------------------------------------------------------------
; Display the current lookup file
; ----------------------------------------------------------------------------
PRO SpectrumDisplay::showLookupFile

    ; COMPILE_OPT HIDDEN

    path = self.Reader->filename (/PATH)
    root = self.Reader->filename (/ROOT)
       
    filename = path + root + '.lu'

    c = FINDFILE (filename, COUNT = cnt)
    IF (cnt EQ 0) THEN BEGIN
       MESSAGE, /CONTINUE, 'File not found: ' + filename
       RETURN
    ENDIF

    OPENR, FL, filename, ERROR = error, /GET_LUN
    IF (error NE 0) THEN BEGIN
       MESSAGE, /CONTINUE, 'File open failed: ' + filename
       RETURN
    ENDIF

    txt = ['']
    
    WHILE (NOT EOF (FL)) DO BEGIN
       line = ''
       READF, FL, line
       txt = [txt, line]
    ENDWHILE
    
    base = WIDGET_BASE (/COLUMN, /BASE_ALIGN_CENTER, $
        XSIZE = 500, YSIZE = 540, GROUP_LEADER = self.widgetID.top, $
        TITLE = filename)
    
    b = WIDGET_BASE (base, /COLUMN, /FRAME, /SCROLL, SPACE = 0, $
        X_SCROLL_SIZE = 480, Y_SCROLL_SIZE = 480, XPAD = 10, YPAD = 10)
    FOR i = 0, N_ELEMENTS (txt) - 1 DO $
        label = WIDGET_LABEL (b, VALUE = txt[i], /ALIGN_LEFT)
        
    button = WIDGET_BUTTON (base, VALUE = 'Dismiss')
    
    WIDGET_CONTROL, base, /REALIZE

    XMANAGER, '_unused', base, /NO_BLOCK, EVENT_HANDLER = 'DIALOG_DISMISS'
   
END

 
;------------------------------------------------------------------------------
; Read lookup file
;------------------------------------------------------------------------------
PRO SpectrumDisplay::readLookup, FILENAME = filename, $
    AUTO = auto, SILENT = silent, LUFILE = luFile, ERROR = error

    ; COMPILE_OPT HIDDEN

    error = 0
    
    data = self.Reader->data (/POINTER)
    ;TBD: make this better!
    energySpan   = self->energySpan ()
    energyLookup = self->energyLookup ()

    timeSpan   = self->timeSpan ()
    timeLookup = self->timeLookup ()
    
    IF KEYWORD_SET(LUFILE) THEN BEGIN 
    	IF luFile NE '' THEN filename = luFile
    ENDIF

    IF (N_ELEMENTS (filename) EQ 0) THEN BEGIN    

       path = self.Reader->filename (/PATH)
       root = self.Reader->filename (/ROOT)
       
       f = path + root + '.lu'

       IF (KEYWORD_SET (auto)) THEN BEGIN
       
          filename = f
       
       ENDIF ELSE BEGIN
          
          filename = DIALOG_PICKFILE ( $
              TITLE = 'Select a Lookup File for Reading', $
              PATH = !RM_PARAMS.lastPath, GET_PATH = lastPath, $
              /MUST_EXIST, FILE = f, FILTER = '*.lu', $
              DIALOG_PARENT = self.widgetID.top)
          IF (filename EQ '') THEN RETURN
          ; User may have slipped up! Filename is only a path:
          IF ((STRPOS(filename, '/', /REVERSE_SEARCH) + 1) EQ STRLEN(filename)) $
              THEN RETURN
          ; If the user accidently puts a space in front of the filename...
          retpos = STRPOS(filename, STRING(10B))
          IF (retpos NE 0) THEN BEGIN
             filename = STRMID(filename, retpos + 1)
          ENDIF
          !RM_PARAMS.lastPath = lastPath
       
       ENDELSE
                  
    ENDIF

    c = FINDFILE (filename, COUNT = cnt)
    IF (cnt EQ 0) THEN BEGIN
       IF (NOT KEYWORD_SET (silent)) THEN $
          MESSAGE, /CONTINUE, 'File not found: ' + filename
       GOTO, BAD_TABLE
    ENDIF

    N = SIZE ((*data).rates)  
    default_el = LINDGEN (N[1] + 1)
    default_tl = LINDGEN (N[2] + 1)


    OPENR, FL, filename, ERROR = error, /GET_LUN
    IF (error NE 0) THEN BEGIN
       MESSAGE, /CONTINUE, 'File open failed: ' + filename
       GOTO, BAD_TABLE
    ENDIF
    
    n = 0
    READF, FL, n
    
    table = (n EQ 0) ? INTARR (1) : INTARR (n)
    READF, FL, table
    
    nt = SIZE ((*data).eedges)
    IF (n EQ 0) THEN BEGIN
       n = nt[2] + 1
       table = INDGEN (n)
    ENDIF
    table = table[SORT (table)]

    DIFF = MIN (table[1:*] - table)

    IF (table[0] NE 0) THEN BEGIN

       MESSAGE, /CONTINUE, 'Energy table does not begin with channel zero.'
       GOTO, BAD_TABLE

    ENDIF $
    ELSE IF (table[n - 1] NE nt[2]) THEN BEGIN

       MESSAGE, /CONTINUE, 'Energy table does not end with last ' + $
           'threshold in the FITS file.'
       PRINT, TABLE
       GOTO, BAD_TABLE

    ENDIF $
    ELSE IF (diff EQ 0) THEN BEGIN

       MESSAGE, /CONTINUE, 'Energy table contains a channel more than once.'
       GOTO, BAD_TABLE

    ENDIF $
    ELSE default_el = table

    READF, FL, n
    IF (n NE 0) THEN BEGIN
       table = FLTARR (n)
       READF, FL, table
       energySpan = FLTARR (n/2, 2)
       energySpan[0: (n/2) - 1, 0] = table[0: (n/2) - 1]
       energySpan[0: (n/2) - 1, 1] = table[(n/2): *]
    ENDIF

    energyLookup = default_el

    READF, FL, n
    table = (n EQ 0) ? INTARR (1) : INTARR (n)
    READF, FL, table
    
    nt = 1 ;Only one time in the file!
    IF (n EQ 0) THEN BEGIN
       n = nt + 1
       table = INDGEN (n)
    ENDIF
    table = table[SORT (table)]

    diff = MIN (Table[1:*] - table)

    IF (table[0] NE 0) THEN BEGIN

       MESSAGE, /CONTINUE, 'Time table does not begin with channel zero.'
       GOTO, BAD_TABLE

    ENDIF $
    ELSE IF (table[n - 1] NE nt) THEN BEGIN

       MESSAGE, /CONTINUE, 'Time table does not end with last ' + $
           'threshold in the FITS file.'
       GOTO, BAD_TABLE

    ENDIF $
    ELSE IF (diff EQ 0) THEN BEGIN

       MESSAGE, /CONTINUE, 'Time table contains a channel more than once.'
       GOTO, BAD_TABLE

    ENDIF $
    ELSE default_tl = table

    timeLookup = default_tl

    READF, FL, n
    IF (n NE 0) THEN BEGIN
       table = FLTARR (n)
       READF, FL, table
       timeSpan = FLTARR (n/2, 2)
       timeSpan[0: (n/2) - 1, 0] = table[0: (n/2) - 1]
       timeSpan[0: (n/2) - 1, 1] = table[(n/2): *]
    ENDIF


    READF, FL, n

    fitBackground = 0 
    IF (N NE 0) THEN BEGIN   ;== We should not get to here!

       fitBackground = 1
        
       table = FLTARR (n)
       READF, FL, table
       backSpan = FLTARR (n/2 + 1, 2)

       backSpan[1: (n/2), 0] = table[0: (n/2) - 1]
       backSpan[1: (n/2), 1] = table[(n/2): *]

       backSpan[0, 0] = backSpan[1, 0]
       backSpan[0, 1] = backSpan[n/2, 1]

    ENDIF


    ;== Read axis types

    IF (NOT EOF(FL)) THEN BEGIN

       str = ''

       READF, FL, str    ; "STACKED SPECTRA" ???
       
       READF, FL, str & e_scale = STRTRIM(str, 2) 
       IF (e_scale EQ 'LOG') THEN xlog = 1 ELSE xlog = 0
       READF, FL, str & r_scale = STRTRIM(str, 2)
       IF (r_scale EQ 'LOG') THEN ylog = 1 ELSE ylog = 0
       self.spectrum->setLogStatus, [xlog, ylog]
    
    ENDIF
    
    ;== Read in the plot ranges
    
    IF (NOT EOF (FL)) THEN BEGIN      
       
       range = FLTARR(2)
       historyRange  = FLTARR (2, 2)
       spectrumRange = FLTARR (2, 2)

       READF, FL, range & historyRange[0, *]  = range
       READF, FL, range & historyRange[1, *]  = range
       READF, FL, range & spectrumRange[0, *] = range
       READF, FL, range & spectrumRange[1, *] = range

    ENDIF

    ;== Read in the background model order
    
    order = -1
    IF (NOT EOF (FL)) THEN $       
       READF, FL, order

    CLOSE, fl
    FREE_LUN, FL

    IF (NOT error) THEN BEGIN

       self->setTimeSpan, timeSpan
       self->setEnergySpan, energySpan
       self->setTimeLookup, timeLookup
       self->setEnergyLookup, energyLookup

       ;== Update the defaults
       self->update, spectrumRange

       IF (self.haveBackground) THEN BEGIN
          ;self.Background->setSpan, backSpan
          self->fitBackground
       ENDIF
       
       ;== We have gotten this far, save my lookup filename
       myLUfile = OBJ_NEW ('File', filename)
       basename = myLUfile->get (/BASENAME)
       OBJ_DESTROY, myLUfile
       self.myLookup = basename

       RETURN
       
    ENDIF 

BAD_TABLE:

    error = 1
    
END


;------------------------------------------------------------------------------
; Save lookup file
;------------------------------------------------------------------------------
FUNCTION spectrumDisplay::writeLookup, AUTO = auto, FILENAME = filename
    
    ; COMPILE_OPT HIDDEN

    IF (N_ELEMENTS (filename) EQ 0) THEN BEGIN

       path = self.Reader->filename (/PATH)
       root = self.Reader->filename (/ROOT)
       
       f = path + root + '.lu'

       IF (KEYWORD_SET (auto)) THEN BEGIN
       
          filename = f
       
       ENDIF ELSE BEGIN
          
          filename = DIALOG_PICKFILE ( $
              TITLE = 'Select a Lookup File for Writing', $
              FILE = f, FILTER = '*.lu', $
              PATH = !RM_PARAMS.lastPath, GET_PATH = lastPath, $
              DIALOG_PARENT = self.widgetID.top, $
              /WRITE)                 ; <---Changed for Mac!
          IF (filename EQ '') THEN RETURN, 0
          ; User may have slipped up! Filename is only a path:
          IF ((STRPOS(filename, '/', /REVERSE_SEARCH) + 1) EQ STRLEN(filename)) $
              THEN RETURN, 0
          ; If the user accidently puts a space in front of the filename...
          retpos = STRPOS(filename, STRING(10B))
          IF (retpos NE 0) THEN BEGIN
             filename = STRMID(filename, retpos + 1)
          ENDIF
          !RM_PARAMS.lastPath = lastPath
       
       ENDELSE

    ENDIF

    IF (NOT OVERWRITE (filename, PARENT = self.widgetID.top)) THEN $
       RETURN, 0

    default_el = self->energyLookup ()
    diff_el = MAX (default_el[1:*] - default_el)
    
    default_tl = self->timeLookup ()
    diff_tl = MAX (default_tl[1:*] - default_tl)
    
    energySpan = self->energySpan ()
    n_espan = N_ELEMENTS (energySpan)
    e_span = FLTARR (n_espan)
    e_span[0:(n_espan / 2) - 1] = energySpan[*, 0]
    e_span[(n_espan / 2): *]    = energySpan[*, 1]

    timeSpan = self->timeSpan ()
    n_tspan = N_ELEMENTS (timeSpan)
    t_span = FLTARR (n_tspan)
    t_span[0:(n_tspan / 2) - 1] = timeSpan[*, 0]
    t_span[(n_tspan / 2): *]    = timeSpan[*, 1]

    n_bspan = 0

    
    OPENW, FL, filename, ERROR = status, /GET_LUN

    IF (status EQ 0) THEN BEGIN

       IF (diff_el EQ 1) THEN BEGIN
          PRINTF, FL, 0
          PRINTF, FL, default_el[0]
       ENDIF ELSE BEGIN
          PRINTF, FL, N_ELEMENTS (default_el)
          PRINTF, FL, default_el
       ENDELSE
       
       PRINTF, FL, n_espan
       IF (n_espan NE 0) THEN $
          PRINTF, FL, e_span
       IF (diff_tl EQ 1) THEN BEGIN
          PRINTF, FL, 0
          PRINTF, FL, default_tl[0]
       ENDIF ELSE BEGIN
          PRINTF, FL, N_ELEMENTS (default_tl)
          PRINTF, FL, default_tl
       ENDELSE
       
       PRINTF, FL, n_tspan
       IF (n_tspan NE 0) THEN $
          PRINTF, FL, t_span

; TODO: background 
       
       PRINTF, FL, n_bspan
       IF (n_bspan NE 0) THEN $
          PRINTF, FL, b_span

       PRINTF, FL, 'STACKED SPECTRA'
       
       logValues = self->logStatus()
       PRINTF, FL, (logValues[0] EQ 1) ? 'LOG' : 'LIN'
       PRINTF, FL, (logValues[1] EQ 1) ? 'LOG' : 'LIN'
       
       histRange = FLTARR(2, 2)
       specRange = self->range ()
       
       PRINTF, FL, histRange[0, *]
       PRINTF, FL, histRange[1, *]
       PRINTF, FL, specRange[0, *]
       PRINTF, FL, specRange[1, *]

       CLOSE, fl
       FREE_LUN, FL

    ENDIF
    
    ;== We have gotten this far, save my lookup filename
    myLUfile = OBJ_NEW ('File', filename)
    basename = myLUfile->get (/BASENAME)
    OBJ_DESTROY, myLUfile
    self.myLookup = basename
    
    RETURN, (status EQ 0)

END


; ----------------------------------------------------------------------------
; Override the plot method.
; ----------------------------------------------------------------------------
PRO SpectrumDisplay::plot, HARDCOPY = hardcopy, _EXTRA = extra
    
    ; COMPILE_OPT HIDDEN

    hardcopy = KEYWORD_SET (hardcopy)
    
    ;== Plot to offscreen pixmap    
    
    IF (NOT hardcopy) THEN BEGIN
       win = !D.WINDOW
       self->setPixWindow
    ENDIF

	self->Spectrum::plot, $ 
		FGCOLOR = self.colors.fg, $
		BGCOLOR = self.colors.bg, $
		DRAWCOLOR = self.colors.spec, $
		_EXTRA = extra

	IF (self.haveBackground) THEN BEGIN

	   self.Background->plot, /SPECTRUM, $
		   self.Spectrum->combinedThresholds (), $
		   self.Spectrum->energySpan (), $
		   self.Spectrum->energyLookup (), $
		   COLOR = self.color->color('BKGD'), $ ;self.colors.bkgd, $
		   LINESTYLE = 1, $
		   _EXTRA = extra

	ENDIF

	label = self->label (/TIME)
	self->annotate, label, COLOR = self.color->color('FG'), _EXTRA = extra 
	fname = self.Reader->filename (/BASENAME)
	luname = self.myLookup
	IF (luname NE '') THEN fname = fname + '; ' + STRTRIM(luname, 2)
	self->annotate, fname, YPOSITION = !Y.WINDOW[1] + 0.01, $
        /SMALL, COLOR = self.color->color('FG'), _EXTRA = extra

    self->plotSpan, /NOSHADE, _EXTRA = extra  ;$ COLOR = self.colors.hist

    ;== Copy pixmap to screen

    IF (NOT hardcopy) THEN BEGIN
       WSET, win
       self->copyPixWindow
    ENDIF
        
END


;------------------------------------------------------------------------------
; Zoom the plotted event
;------------------------------------------------------------------------------
PRO SpectrumDisplay::zoom, XZOOM = xZoom, YZOOM = yZoom, $
    DEFAULTRANGE = defaultRange, SELECTION = selection, $
    FULLSCREEN = fullscreen

    ; COMPILE_OPT HIDDEN

    IF (KEYWORD_SET (fullscreen)) THEN BEGIN
       self->fullscreen
       RETURN
    ENDIF   
    
    ;== Need to set all the !plot info, especially ![xy].crange for AXISRANGE
    self->plot, _EXTRA = *self.PlotOptions

	 ;== First pass: set default axis ranges
	 IF (NOT self.haveDefaultRange) THEN BEGIN
		self.haveDefaultRange   = 1  
		self.defaultRange = self->default ()
		self->setRange, self->default ()
	 ENDIF
	 
	 self.currentRange = self->range ()

	 IF (KEYWORD_SET (defaultRange)) THEN BEGIN
	 
		self->resetDefaultRange
		self->plot, _EXTRA = *self.PlotOptions                    
		
	 ENDIF ELSE $
	 IF (KEYWORD_SET (selection)) THEN BEGIN
			
		range  = self->range ()
		xRange = REFORM ((self->energySpan ())[0, *])
		
		idx = INTARR (2)
		thresholds = self->combinedThresholds ()
		range[0, *] = NEAREST_EDGE (thresholds, xRange, idx)
						
		spec    = self->combinedSpec ()
		specErr = self->combinedSpecErr ()
		
		lo = spec[idx[0]:idx[1]] - specErr[idx[0]:idx[1]]
		hi = spec[idx[0]:idx[1]] + specErr[idx[0]:idx[1]]
						
		yRange = [ MIN (lo[WHERE (lo GT 0)]), MAX (hi[WHERE (hi GT 0)])]
		range[1, *] = yRange
						 
		self->setRange, range
		self->plot, _EXTRA = *self.PlotOptions                    

	 ENDIF ELSE BEGIN   
		
		self->DISPLAY::zoom, self.widgetID.draw, $
			X = KEYWORD_SET (xZoom), $
			Y = KEYWORD_SET (yZoom)

		xRange = AXISRANGE (/XAXIS)
		yRange = AXISRANGE (/YAXIS)
		range  = TRANSPOSE ([[xRange], [yRange]])
		self->setRange, range

	 ENDELSE 
    
    self.dirty = 1   ;== Views have changed

END
; ----------------------------------------------------------------------------
FUNCTION SpectrumDisplay::detector & RETURN, self.Detector & END


; ----------------------------------------------------------------------------
; Set the Response Matrix filename in the file associated with a PHA Reader. 
; ----------------------------------------------------------------------------
PRO SpectrumDisplay::setRespName, theFile, BACK = back, RESP = resp, RESET = reset
		
	myPHAFile = self.Reader->filename ()
	;== Open the SPECTRUM extension and add the response filename to the header
	extNo = 2
	myHDR = HEADFITS(myPHAFile, EXTEN = extNo)
    ;== Verify FITS filetype
    filetype = SXPAR (myHDR, 'EXTNAME')
    IF (STRUPCASE (filetype) NE 'SPECTRUM') THEN BEGIN
        extNo = 1
    	myHDR = HEADFITS(myPHAFile, EXTEN = extNo)
    ENDIF
    IF KEYWORD_SET(RESET) THEN theFile = 'none'
    IF KEYWORD_SET(BACK) THEN SXADDPAR, myHDR, 'BACKFILE', theFile
    IF KEYWORD_SET(RESP) THEN SXADDPAR, myHDR, 'RESPFILE', theFile
	fits_add_checksum, myHDR
	MODFITS, myPHAFile, 0, myHDR, EXTEN = extNo

END


; ----------------------------------------------------------------------------
; Define the subclass, inheriting Display and PHA
; ----------------------------------------------------------------------------
PRO spectrumDisplay__define

    obj = { SPECTRUMDISPLAY, INHERITS DISPLAY, INHERITS SPECTRUM, $
         
        ;== Detector object reference
        
        Detector   : OBJ_NEW (), $
        Response   : OBJ_NEW (), $
        Fitter     : OBJ_NEW (), $
        Spectrum   : OBJ_NEW (), $
        BackReader : OBJ_NEW (), $
        Background : OBJ_NEW (), $
        haveBackground :  0, $

        ;== Lookup filename
	
        myLookup   : '', $
        
        ;== Maintain a record of whether the lookup has changed
        
        dirty     :  0, $
        RMFdirty  :  0, $
        BackDirty :  0, $
        
        ;== Keep track of the log-linear axes choice for plotting:
        
        logChoiceID  : 0L $

    }

END
