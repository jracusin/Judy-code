; ----------------------------------------------------------------------------
;+
; NAME:
;
;     PhaDisplay (OBJECT)
;
; PURPOSE:
;
;     An object for interactive manipulation of PHA data.
;
; CALLING SEQUENCE:
;
;     obj = OBJ_NEW ('PhaDisplay', filename)
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
;     Display
;
; DEPENDENCIES:
;
;     display__define.pro
;     lightcurve__define.pro
;     spectrum__define.pro
;     background__define.pro
;
; METHODS:     
;
; MODIFICATION HISTORY:
;
;     12/17/08 RDP: Rethought out the RSP ingest code; should now be more rational:
;     we always ask the first time a response is needed, even if we have one with 
;     the correct name. 
;     Added a test for TTEReader in init, so we can insert the TTE decorator class 
;     for the lightcurve (TTELightcurve), to make arbitrary rebinning of TTE work.
;
;     07/11/07 RDP: Added code to automatically read in RSP files associated with 
;     PHA and PHAII files (filename in RESPFILE keyword).
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
FUNCTION PhaDisplay::init, filename, MAP = map, LOOKUP = lookup, _EXTRA = extra
        
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
    
    IF (OBJ_VALID (self.Detector)) THEN BEGIN
        
        self.Reader     = self.Detector->dataReader ()    
        IF (OBJ_ISA(self.Reader, "TTEReader")) THEN BEGIN
			self.Lightcurve = OBJ_NEW ('TTELightcurve', self.Reader)
        ENDIF ELSE BEGIN
			self.Lightcurve = OBJ_NEW ('Lightcurve', self.Reader)
        ENDELSE
        self.Spectrum   = OBJ_NEW ('Spectrum',   self.Reader)    
        self.Background = OBJ_NEW ('Background', self.Reader)
        self.Response   = self.Detector->Response ()
        self.Fitter     = self.Detector->Fitter ()
    
        ;== 07/11/07 RDP: Added code to automatically read in associated RSP files:
;        respFileName = '' 
;        IF (OBJ_ISA(self.Reader, "PHAReader") OR OBJ_ISA(self.Reader, "PHAIIReader") $ 
;         OR OBJ_ISA(self.Reader, "TTEReader")) THEN BEGIN 
;            respFileName = STRLOWCASE(STRTRIM(self.Reader->getRMFFile(), 2)) 
;			IF (respFileName NE '' AND respFileName NE 'none') THEN BEGIN 
;				mypath = self.Reader->filename (/PATH) 
;				self.Response->setFilename, mypath + respFileName 
;				self.Response->read, self.Reader, ERROR = error 
;				IF error THEN self.Response->setFilename, '' 
;			ENDIF 
;        ENDIF 
        
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

    ;== Set default domain
     
    self.domain = 'TIME'

    ;== Try to read a lookup file
    
    IF (lookup) THEN BEGIN
        
       self->readLookup, /AUTO, /SILENT, FILENAME = f, ERROR = error, _EXTRA = extra
       IF (NOT error) THEN BEGIN
          self->integrateAndCombine
          self->setStatus, 'Read lookup file: ' + f ;, 5, /REVERT
       ENDIF ELSE BEGIN        ;== No lookup file, must set plot ranges to the default
          self.lightCurve->resetDefaultRange
          self.spectrum->resetDefaultRange
          self.spectrum->setLogStatus, [1, 1]
       ENDELSE

    ENDIF 
    
    ;== Register the display with the fitter
    
    self.fitter->registerDetector, self
    
    ;== Start clean
    
    self.dirty = 0
    
    self.rndseed = 0L
    header = self.Reader->header (/POINTER)   
	prim_hdr = (*header).ext0
	ww = STRPOS(prim_hdr, 'RND_SEED')
	wnum = WHERE(ww NE -1, co)
	IF (co NE 0) THEN BEGIN 
		self.rndseed = fxpar(prim_hdr, 'RND_SEED')
		;== If we are simulating, the error bars go bad:
		self.Reader->setPoisson, 0
	ENDIF

    ;== Plot lightcurve
        
    self->plot, _EXTRA = *self.PlotOptions
    self->showSpan
    
    RESTORE, !MFIT.script_file                                                                   ; AMG
    IF (script) AND (luFile[0] NE '') THEN self->lookup, /READ                                   ;
    IF (script EQ 1) AND (phaNum EQ N_ELEMENTS(lcFile)) THEN BEGIN                               ;
      phanum=0                                                                                   ;
      SAVE, file=!MFIT.script_file, script, lcFile, rspFile, luFile, phaNum, model, vals, $      ;
                 statistic, eInterval, numfits, iter, scriptVerbose, table                       ;
      IF N_ELEMENTS(eInterval) EQ 2 THEN energyInterval=eInterval ELSE $                         ;
                                         energyInterval=!RM_PARAMS.fluxint                       ;
      self.Fitter->fitModel, ENERGYINTERVAL = energyInterval, $                                  ;
                                    /SELECT_MODEL, /SELECTIONS, FITPLOT = *self.PlotOptions      ;
    ENDIF                                                                                        ;
    
    ;self.BackSelect=OBJ_NEW('BACKSELECT')
    
    RETURN, 1 

END

; ----------------------------------------------------------------------------
; Two routines to handle event ID strings. (Added 4/16/09)
; ----------------------------------------------------------------------------

PRO PhaDisplay::setEventID, filename, EVENTAG=evstr
    IF N_ELEMENTS(evstr) EQ 0 THEN BEGIN
    	names = STREGEX(filename,'.*_.*_(.*)_v..*$',/SUBEXPR,/EXTRACT)
		self.eventID = names[1]
	ENDIF ELSE self.eventID = evstr 
END

FUNCTION PhaDisplay::getEventID 
	RETURN, self.eventID 
END

   
; ----------------------------------------------------------------------------
; Destructor
; ----------------------------------------------------------------------------
PRO PhaDisplay::cleanup
    
    Detector = self->detector()
    Reader   = Detector->dataReader ()
    filename = Reader->filename (/BASENAME)
    
    IF self.dirty THEN BEGIN
        d = DIALOG_MESSAGE (/QUESTION, 'Save changed lookup parameters for dataset: ' +$
            STRTRIM (filename, 2) + '?')
        IF (d EQ 'Yes') THEN self->lookup, /WRITE
    ENDIF
    
    IF (NOT OBJ_ISA(Reader, 'BFITSReader') AND self.RMFdirty) THEN BEGIN
        d = DIALOG_MESSAGE (/QUESTION, 'Save new response file into dataset: ' +$
            STRTRIM (filename, 2) + '?')
        IF (d EQ 'Yes') THEN BEGIN
            rmfFile = self.Response->filename (/BASE)
            self->setRespName, rmfFile
        ENDIF ;self->lookup, /WRITE
    ENDIF
    
    ;== Save the color info
    
    !RM_PARAMS.colors = self.colors
    
    ;== Unregister the display with the fitter
    
    self.fitter->deleteDetector, Reader->filename()
    self.Fitter->haveFit, 0
    self.Fitter->haveBatch, 0

    OBJ_DESTROY, self.Detector
    OBJ_DESTROY, self.Lightcurve   
    OBJ_DESTROY, self.Spectrum     
    OBJ_DESTROY, self.Background   
           
    self->Display::cleanup

END


; ----------------------------------------------------------------------------
; Build the display widget
; ----------------------------------------------------------------------------
PRO PhaDisplay::buildGUI, MAP = map

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
            id = menu->add (subMenu, VALUE = 'Calibration (EXT 1)')
            id = menu->add (subMenu, VALUE = 'Data Table  (EXT 2)')
            id = menu->add (subMenu, VALUE = 'Notes')
 
        subMenu = menu->add (VALUE = 'Lookup', /MENU)
            id = menu->add (subMenu, VALUE = 'Save Lookup')
            id = menu->add (subMenu, VALUE = 'Read Lookup')
            id = menu->add (subMenu, VALUE = 'File Content')
            id = menu->add (subMenu, VALUE = 'Erase Current')
 
        id = menu->add (VALUE = 'Dismiss', /SEPARATOR)
        
    menu = self->addMenu (VALUE = 'Misc')
        id = menu->add (VALUE = 'Show Current Selections')
        id = menu->add (VALUE = 'Show Keyboard Bindings')
        id = menu->add (VALUE = 'Refresh Plot')
        id = menu->add (VALUE = 'Selection -> PHA', /SEPARATOR)
        id = menu->add (VALUE = 'Background -> PHA')
        id = menu->add (VALUE = 'Export IDL Data')
        ; RMK
        id = menu->add (VALUE = 'Export ASCII Data')
        id = menu->add (VALUE = 'Export DRM Data')
		IF OBJ_ISA(self.Reader, "TTEReader") THEN BEGIN
			id = menu->add (VALUE = 'Import Background')
		ENDIF
        ;id = menu->add (VALUE = 'Spacecraft Location')
   
    menu = self->addMenu (VALUE = 'Options')
        id = menu->add (VALUE = 'Show rmfit Window')
        subMenu = menu->add (VALUE = 'Colors', /MENU)
            id = menu->add (subMenu, VALUE = 'Background')
            id = menu->add (subMenu, VALUE = 'Foreground')
            id = menu->add (subMenu, VALUE = 'History')
            id = menu->add (subMenu, VALUE = 'Spectrum')
            id = menu->add (subMenu, VALUE = 'Background Model')
        id = menu->add (VALUE = 'Plot Configuration')
        id = menu->add (VALUE = 'Display Default DRM')
        id = menu->add (VALUE = 'Clear Default DRM')

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
     
    id = self->addButton ('Toggle')

    menu = self->addMenuButton (VALUE = 'Zoom:', /TEAROFF)
        id = menu->add (VALUE = 'Zoom')
        id = menu->add (VALUE = 'X Zoom')
        id = menu->add (VALUE = 'Y Zoom')
        id = menu->add (VALUE = 'Zoom In: Selection')
        id = menu->add (VALUE = 'Zoom Out: Full Range')
        id = menu->add (VALUE = 'Full Screen')

    menu = self->addMenuButton (VALUE = 'Rebin:', /TEAROFF)
        id = menu->add (VALUE = 'Temporal Resolution')
        id = menu->add (VALUE = 'Signal to Noise')
        id = menu->add (VALUE = 'Full Resolution')
        subMenu = menu->add (VALUE = 'Refine Bins', /MENU)
            id = menu->add (subMenu, VALUE = 'Refine by Half')
            id = menu->add (subMenu, VALUE = 'Refine Single Bin')
        subMenu = menu->add (VALUE = 'Combine Bins', /MENU)
            id = menu->add (subMenu, VALUE = 'Source Intervals')
            id = menu->add (subMenu, VALUE = 'Single Bin')
            id = menu->add (subMenu, VALUE = 'Combine by...')

    id = self->addButton ('Fit Background')

    menu = self->addMenuButton (VALUE = 'Select Source:', /TEAROFF)
        id = menu->add (VALUE = 'Source Interactive')
        id = menu->add (VALUE = 'Source by Signal to Noise')

;    id = self->addButton ('Select Source')

    menu = self->addMenuButton (VALUE = 'Adjust Source:', /TEAROFF)
        id = menu->add (VALUE = '< Shift Selection')
        id = menu->add (VALUE = '> Shift Selection')
        id = menu->add (VALUE = '< Left Selection')
        id = menu->add (VALUE = '> Left Selection')
        id = menu->add (VALUE = '< Right Selection')
        id = menu->add (VALUE = '> Right Selection')
    
;    id = self->addLabel ('Spectral Fitting:')
    
    menu = self->addMenuButton (VALUE = 'Spectral Fitting:', /TEAROFF)
        id = menu->add (VALUE = 'Fit One Spectrum')
        id = menu->add (VALUE = 'Fit One Interval')
        id = menu->add (VALUE = 'Fit Selections')
        id = menu->add (VALUE = 'Batch Fit Selections')
        id = menu->add (VALUE = 'Fit Plotter')

;    id = self->addButton ('Fit Plotter')

	logChoice = ['X Log', 'Y Log']
	self.logChoiceID = CW_BGROUP (self.widgetID.buttonBase, logChoice, /ROW, $
            /NONEXCLUSIVE, SET_VALUE = [0,0])
               
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
PRO PhaDisplay::eventHandler, event

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
                'Calibration (EXT 1)' : self.Reader->showHeader, 1
                'Data Table  (EXT 2)' : self.Reader->showHeader, 2
                'Notes'               : self.Reader->showNotes
                 
                'Selection -> PHA'    : self->makePHAFile, /SELECTION
                'Background -> PHA'   : self->makePHAFile, /BACKGROUND
                'Export IDL Data'     : self->export
                ;RMK
                'Export ASCII Data'   : self->exportasc
                ;RMK
                'Export DRM Data'     : self->exportdrm  
                'Import Background'   : self->importBack

                'Read Lookup'         : self->lookup, /READ
                'Save Lookup'         : BEGIN
                	self->lookup, /WRITE
					IF (OBJ_ISA(self.Reader, "TTEReader")) THEN BEGIN
						path = self.Reader->filename (/PATH)
						self.Reader->writeTiFile, LOOKUPFILENAME = path + self.myLookup
					ENDIF
                	END
                'File Content'        : self->showLookupFile
                'Erase Current'       : self->lookup, /ERASE
                                                                    
                'Show rmfit Window'  : WIDGET_CONTROL, (self.myrmfit).topID, map = 1;/show
                
                'Show Current Selections' : self->showSpan
                'Show Keyboard Bindings'  : self->showKeyBindings
                'Refresh Plot'            : self->plot, _EXTRA = *self.PlotOptions

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
                ; SELECTION EVENTS
                ;---------------  

                'Source Interactive' : BEGIN
                    self->disable
                    self->setStatus, $
                        'Select source interval(s).  ' + $
                        'Use margins for special functions.' 
                    self->selectSource,/INTERACTIVE
                    self->showSpan
                    self->enable
                    END
                   
                'Source by Signal to Noise' : BEGIN
                    self->disable
                    self->setStatus, $
                        'Select a fluence interval.  ' + $
                        'Use margins for special functions.' 
                    self->selectSource, /SIGNAL_TO_NOISE
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

                'Fit Background' : BEGIN
                    self->disable
                    self->setStatus, $
                       'Select background interval(s).  ' + $
                       'Use margins for special functions.'
;                    self.Lightcurve->GET_LC, xtot, ytot                                           ; AMG    
;                    self.BackSelect->SIGNAL, *xtot, *ytot, self.Background                        ; AMG
                    self->fitBackground, /INTERACTIVE;
                    self->clearStatus
                    self->plot, _EXTRA = *self.PlotOptions
                    self->showSpan
                    self->enable
                    END

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

                'Temporal Resolution' : BEGIN
                    IF (STRUPCASE (self.domain) NE 'TIME') THEN $
                       self->togglePlot
                    self.Lightcurve->rebin, /TEMPORAL
                    self->plot, _EXTRA = *self.PlotOptions
                    END
                     
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
                
                'Toggle' : self->togglePlot

                ;---------------
                ; FIT SPECTRA
                ;---------------  

                ;== Fit the model
        
                'Fit One Spectrum' : BEGIN
                        self.Fitter->fitModel, ENERGYINTERVAL = !RM_PARAMS.fluxint, $
                                    /SELECT_MODEL, /SINGLEBIN, FITPLOT = *self.PlotOptions
                        END
                'Fit Selections'   : BEGIN
                        self.Fitter->fitModel, ENERGYINTERVAL = !RM_PARAMS.fluxint, $
                                    /SELECT_MODEL, /SELECTIONS, FITPLOT = *self.PlotOptions
                        END
                'Fit One Interval' : BEGIN
                        self.Fitter->fitModel, ENERGYINTERVAL = !RM_PARAMS.fluxint, $
                                    /SELECT_MODEL, /SINGLEINTERVAL, FITPLOT = *self.PlotOptions
                        END
                'Batch Fit Selections' : BEGIN
                        self.Fitter->fitBatch, status, BATCHPLOT = *self.PlotOptions
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


;------------------------------------------------------------------------------
; Create an XSPEC-compatible PHA file
;------------------------------------------------------------------------------
PRO PhaDisplay::makePHAFile, SELECTION = selection, BACKGROUND = background


    ;== Return pointers for efficiency.  Do NOT alter these data!

    allheaders     = self.Reader->header ()   
    header = allheaders.ext0
    combTimes      = self.Lightcurve->combinedTimes ()
    combThresholds = self.Spectrum->combinedThresholds ()
    
    
    ;== Integrate the user-selected span:  
    span = self.lightCurve->timeSpan ()
       
    self.Lightcurve->integrateTime, span, $
        HISTORY = history, ERRORS = errors, TOTALTIME = totalTime ;<- livetime
        
    ;== Something wrong happened: no selection
    IF (totalTime EQ 0.0) THEN BEGIN
           cancel = 1
           RETURN    
    ENDIF        

    ;== Integrate the background model over the selected time span
    ;using the full energy resolution
    
    lookupSave = self.Spectrum->energyLookup()
    self->setEnergyLookup, lookupSave, /INITIALIZE
    eLookup = self.Spectrum->energyLookup()
    numBins = N_ELEMENTS (history)
    
    fitChannels = INTARR(2)
    fitChannels[0] = eLookup[0]
    fitChannels[1] = eLookup[numBins] - 1
    tInt = NEAREST_EDGE (combTimes, span[0, *],tind) 
    
    self->setEnergyLookup, lookupSave   ;== Restore previous lookup

    IF (self.Background->haveModel ()) THEN BEGIN
        brates = self.Background->evalModel (combTimes[0,tind[0]], combTimes[1,tind[1]])
        brates = transpose(brates)
        brerrs = self.Background->evalMsig (combTimes[0,tind[0]], combTimes[1,tind[1]])
        brerrs = transpose(brerrs)
    ENDIF ELSE BEGIN
        brates = FLTARR(numbins)
        brerrs = FLTARR(numbins)
    ENDELSE

   obsCRate = FLTARR(N_ELEMENTS(history))
   jind = WHERE (brates)
   
   IF (jind[0] EQ -1) THEN BEGIN
       fit = DIALOG_MESSAGE (/QUESTION, $
           ['No background model exists for the file: ', $
           self.Reader->filename (/BASENAME), $
           'Continue?'], $
           DIALOG_PARENT = self.widgetID.top)
       IF (fit EQ 'No') THEN BEGIN
          error = 1
          RETURN
       ENDIF ELSE BEGIN
          jind = INDGEN(N_ELEMENTS(history))
       ENDELSE
   ENDIF

   obsCRate[jind] = history[jind] - brates[jind]
   obsCSig = errors
   
    ; Save file name
    ;
    IF KEYWORD_SET(BACKGROUND) THEN BEGIN
        fileExt = '.bak'
        myFilt = '*.bak'
    ENDIF ELSE BEGIN
        fileExt = '.pha'
        myFilt = '*.pha'
    ENDELSE
    
    file = self.Reader->filename(/ROOT) + fileExt
    out_file = DIALOG_PICKFILE ( $
              TITLE = 'Select a Filename for Writing Results', $
              FILE = file, FILTER = myFilt, $
              PATH = !RM_PARAMS.lastPath, GET_PATH = lastPath, $
              DIALOG_PARENT = self.widgetID.top, $
              /WRITE, /OVERWRITE_PROMPT)            ; <---Changed for Mac!
    
    IF (out_file EQ '') THEN RETURN  ; Cancel
    !RM_PARAMS.lastPath = lastPath
          
    ; User may have slipped up! Filename is only a path:
    IF ((STRPOS(out_file, '/', /REVERSE_SEARCH) + 1) EQ STRLEN(out_file)) THEN BEGIN
        out_file = out_file + file               ;TBD: ask again?
    ENDIF
    ; If the user accidently puts a space in front of the filename...
    retpos = STRPOS(out_file, STRING(10B))
    IF (retpos NE 0) THEN BEGIN
       out_file = STRMID(out_file, retpos + 1)
    ENDIF
	
	;= Get the energy info:
    data        = self.Reader->data ()
    eThresholds = (data).eedges
    
    ;= File CREATOR info:
	vers = 'rmfit ' + !RM_PARAMS.VERSION
    
    ;== Write out the background file:
    IF KEYWORD_SET(BACKGROUND) THEN BEGIN
        IF (self.Background->haveModel ()) THEN BEGIN
            obsCRate = brates
            obsCSig  = brerrs
			WritePHAFile, out_file, header, numbins, FitChannels, totalTime, tint, $
				eThresholds, obsCRate, obsCSig, vers, /BACKGROUND
        ENDIF ELSE BEGIN
            error = 1
            RETURN
        ENDELSE
    ENDIF ELSE BEGIN
		WritePHAFile, out_file, header, numbins, FitChannels, totalTime, tint, $
			eThresholds, obsCRate, obsCSig, vers
    ENDELSE
	

END


;------------------------------------------------------------------------------
; Export IDL data
;------------------------------------------------------------------------------
PRO PhaDisplay::export

    ; COMPILE_OPT HIDDEN

    ;items = 4
    ;input = REPLICATE ({ name: '', data: PTR_NEW() }, items) 
     
    data = self.Reader->data (/POINTER)

    times  = (*data).times
    rates  = (*data).rates
    errors = (*data).errors
    eedges = (*data).eedges
    ;--------------Binbin  added
        IF (self.Background->haveModel ()) THEN BEGIN
        brates = self.Background->evalModel (times[0,*], times[1,*])
        brates = transpose(brates)
        brerrs = self.Background->evalMsig (times[0,*], times[1,*])
        brerrs = transpose(brerrs)
    ENDIF ELSE BEGIN
        brates = rates * 0.0
        brerrs = rates * 0.0
    ENDELSE
    ;--------------Binbin added
    
    ; Save file name
    ;
    file = self.Reader->filename(/ROOT) + '_data.sav'
    out_file = DIALOG_PICKFILE ( $
              TITLE = 'Select a Filename for Writing Results', $
              FILE = file, FILTER = '*.sav', $
              PATH = !RM_PARAMS.lastPath, GET_PATH = lastPath, $
              DIALOG_PARENT = self.widgetID.top, $
              /WRITE, /OVERWRITE_PROMPT)            ; <---Changed for Mac!
	
    IF (out_file EQ '') THEN RETURN  ; Cancel
    !RM_PARAMS.lastPath = lastPath
          
    ; User may have slipped up! Filename is only a path:
    IF ((STRPOS(out_file, '/', /REVERSE_SEARCH) + 1) EQ STRLEN(out_file)) THEN BEGIN
        out_file = out_file + file               ;TBD: ask again?
	ENDIF
	; If the user accidently puts a space in front of the filename...
	retpos = STRPOS(out_file, STRING(10B))
	IF (retpos NE 0) THEN BEGIN
	   out_file = STRMID(out_file, retpos + 1)
	ENDIF

    self->setStatus, 'Writing file: ' + out_file, 10, /REVERT
       
;        ------ Binbin Zhang modified
    SAVE, FILENAME=out_file, times, rates, errors, eedges,brates,brerrs
;        ------ Binbin Zhang    
END


;------------------------------------------------------------------------------
; RMK--Export ASCII data
;------------------------------------------------------------------------------
PRO PhaDisplay::exportasc

    ; COMPILE_OPT HIDDEN

    data = self.Reader->data (/POINTER)

    times  = (*data).times
    ntime  = n_elements(times[0,*])
    rates  = (*data).rates
    errors = (*data).errors
    eedges = (*data).eedges
    nchan  = n_elements(eedges[0,*])

    IF (self.Background->haveModel ()) THEN BEGIN
        brates = self.Background->evalModel (times[0,*], times[1,*])
        brates = transpose(brates)
        brerrs = self.Background->evalMsig (times[0,*], times[1,*])
        brerrs = transpose(brerrs)
    ENDIF ELSE BEGIN
        brates = rates * 0.0
	brerrs = rates * 0.0
    ENDELSE

    ; Ascii file name
    ;
    file = self.Reader->filename(/ROOT) + '_ascii.dat'
    out_file = DIALOG_PICKFILE ( $
              TITLE = 'Select a Filename for Writing Results', $
              FILE = file, FILTER = '*.dat', $
              PATH = !RM_PARAMS.lastPath, GET_PATH = lastPath, $
              DIALOG_PARENT = self.widgetID.top, $
              /WRITE, /OVERWRITE_PROMPT)            ; <---Changed for Mac!
	
    IF (out_file EQ '') THEN RETURN  ; Cancel
    !RM_PARAMS.lastPath = lastPath
          
    ; User may have slipped up! Filename is only a path:
    IF ((STRPOS(out_file, '/', /REVERSE_SEARCH) + 1) EQ STRLEN(out_file)) THEN BEGIN
        out_file = out_file + file               ;TBD: ask again?
        self->setStatus, 'Writing file: ' + out_file, 10, /REVERT
	ENDIF
	; If the user accidently puts a space in front of the filename...
	retpos = STRPOS(out_file, STRING(10B))
	IF (retpos NE 0) THEN BEGIN
	   out_file = STRMID(out_file, retpos + 1)
	ENDIF
       
    OPENW, lun, out_file, /GET_LUN

    ; Write data
    ;
    header = self.Reader->header (/POINTER)
    notes  = (*header).notes
    nnotes = N_ELEMENTS(notes)
    PRINTF, lun, format='(";",70("-"))'
    FOR i=0L,nnotes-1 DO PRINTF, lun, '; ', notes[i]
    PRINTF, lun, format='(";",70("-"))'
    PRINTF, lun, '; Number of energy channels, time bins'
    PRINTF, lun, nchan, ntime
    PRINTF, lun, '; Energy channel definitions Start, End energies in keV'
    FOR i=0L,nchan-1 DO PRINTF, lun, eedges(*,i), format='(2f12.3)'
    PRINTF, lun, '; Tstart(s), Tend(s), RateCh1(ct/s), ErrRateCh1(ct/s), ' $
		 + 'BgdCh1(ct/s), ErrBgdCh1(ct/s), ' $
		 + 'RateCh2(ct/s), ErrRateCh2(ct/s), ' $
		 + 'BgdCh2(ct/s), ErrBgdCh2(ct/s), ...'
    tmp = fltarr(4*nchan,ntime)
    FOR i=0L,nchan-1 DO BEGIN
        tmp[4*i+0,*] = rates[i,*]
        tmp[4*i+1,*] = errors[i,*]
        tmp[4*i+2,*] = brates[i,*]
        tmp[4*i+3,*] = brerrs[i,*]
    ENDFOR

    FOR i=0L, ntime-1L DO BEGIN
        PRINTF, lun, times[*,i], tmp[*,i], $
          FORMAT='(2f15.6,2x,999(e12.4))'
    ENDFOR

    ;PRINT, 'Data written to file ', file
    self->setStatus, 'Data written to file ' + file, 10, /REVERT

    ; Close
    ;   
    CLOSE, lun
    FREE_LUN, lun
    
END


;------------------------------------------------------------------------------
; RDP-- Import backround model from another file
;------------------------------------------------------------------------------
PRO PhaDisplay::importBack

    myDetList = self.Fitter->getDetectorList()
    detnames  = myDetList->names ()
    detnum = myDetList->count ()
    bak_filelist = STRARR(detnum)
    FOR i = 0, detnum - 1 DO BEGIN
		delimpos = STRPOS(detnames[i], '/', /REVERSE_SEARCH)
		bak_filename = detnames[i]
		IF (delimpos NE 0) THEN BEGIN
			bak_filename = STRMID(bak_filename, delimpos + 1, STRLEN(detnames[i]))
		ENDIF
		bak_filelist[i] = bak_filename
	ENDFOR
	
	initSel = INTARR(detnum)
	bakSelect = dialog_checklist(bak_filelist, INITIAL = initSel, $
	            TITLE = 'Import Background from Dataset:')
	bakInd = WHERE(bakSelect, bcnt)
	IF bcnt EQ 0 THEN RETURN
	
	det = myDetList->data (detnames[bakInd])
	myDisp = det.display

	myBack = myDisp.background
	IF myBack->haveModel() EQ 0 THEN BEGIN
          self->setStatus, 'Background model is required for this function.', 10, /REVERT
		RETURN
	ENDIF
	
	newESpan = myDisp.lightcurve->energySpan()
	newELu = myDisp.lightcurve->energyLookup()
	self->setEnergyLookup, newELu
	self->setEnergySpan, newESpan
	OBJ_DESTROY, self.background
	self.background = myBack
    self->integrateAndCombine
	self.lightcurve->update
	self.spectrum->update
	self->plot
	;== It will usually be the case that we don't want to fit the data we drew the model from:
	self.Fitter->deleteDetector,detnames[bakInd]

END


;------------------------------------------------------------------------------
; RMK--Export DRM data as IDL save file
;------------------------------------------------------------------------------
PRO PhaDisplay::exportdrm

    ;== Read DRM
        
    done = 0
    prevResp = self.Response->Filename()

    IF (prevResp EQ '') THEN BEGIN   ; We don't have a DRM for this detector yet:

	    WHILE (NOT done) DO BEGIN
	        
             path = self.Reader->filename (/PATH)
             root = self.Reader->filename (/ROOT)
			
             f = path + root + '.drm'
             c = FINDFILE (f, COUNT = cnt)
             IF (CNT NE 0) THEN BEGIN
                self.Response->setFilename, f, $;/INTERACTIVE, $
                    /MUST_EXIST, FILTER = '*.rsp', DIALOG_PARENT = self.widgetID.top, $
                    TITLE = 'Select Response File for Reading', ERROR = error 
             ENDIF ELSE BEGIN
                myPath = !RM_PARAMS.lastpath
                self.Response->setFilename, /INTERACTIVE, $
                    /MUST_EXIST, FILTER = '*.rsp', DIALOG_PARENT = self.widgetID.top, $
                    TITLE = 'Select Response File for Dataset "' + root + '.fits"', $
                    PATH = myPath, ERROR = error 
                !RM_PARAMS.lastpath = self.Response->filename (/PATH)
             ENDELSE

             IF (error) THEN BEGIN
                 cancel = 1
                 RETURN    
             ENDIF

             self.Response->read, DATAREADER = self.Reader, ERROR = error
             IF (error) THEN BEGIN

                 self->setStatus, $
                    'Failed to read response file: ' + self.Response->filename (), 5
	
             ENDIF ELSE BEGIN
	           
                self->clearStatus 
                done = 1
	
             ENDELSE
	           
         ENDWHILE
    ENDIF

    resp = self.Response->response ()

    ; Save file name
    ;
    file = self.Reader->filename(/ROOT) + '_drm.sav'
    out_file = DIALOG_PICKFILE ( $
              TITLE = 'Select a Filename for Writing Results', $
              FILE = file, FILTER = '*.sav', $
              PATH = !RM_PARAMS.lastPath, GET_PATH = lastPath, $
              DIALOG_PARENT = self.widgetID.top, $
              /WRITE, /OVERWRITE_PROMPT)           ; <---Changed for Mac!
	
    IF (out_file EQ '') THEN RETURN  ; Cancel
    !RM_PARAMS.lastPath = lastPath
          
    ; User may have slipped up! Filename is only a path:
    IF ((STRPOS(out_file, '/', /REVERSE_SEARCH) + 1) EQ STRLEN(out_file)) THEN BEGIN
        out_file = out_file + file               ;TBD: ask again?
	ENDIF
	; If the user accidently puts a space in front of the filename...
	retpos = STRPOS(out_file, STRING(10B))
	IF (retpos NE 0) THEN BEGIN
	   out_file = STRMID(out_file, retpos + 1)
	ENDIF
	
    self->setStatus, 'Writing file: ' + out_file, 10, /REVERT       

    SAVE, FILENAME=out_file, resp
    
END


; ----------------------------------------------------------------------------
; Help and About information
; ----------------------------------------------------------------------------
PRO PhaDisplay::info, HELP = help, ABOUT = about
    
    ; COMPILE_OPT HIDDEN

    IF (KEYWORD_SET (help)) THEN BEGIN
        XDISPLAYFILE, !MFIT.HELP_PATH + 'data.hlp', DONE = 'Done'
    ENDIF

    IF (KEYWORD_SET (about)) THEN BEGIN

;       text = [ $
;
;           'RMFIT',                                      $
;           'A Lightcurve and Spectral Analysis Tool',    $ 
;           '   ',                                        $
;           'Robert S. Mallozzi',                         $
;           'Robert D. Preece',                           $
;           'Michael S. Briggs',                          $
;           '   ',                                        $
;           'UNIVERSITY OF ALABAMA HUNTSVILLE',           $
;           '   ',                                        $
;           'Copyright (C) 2000 Robert S. Mallozzi',      $
;           'Portions (C) 2000 Robert D. Preece',         $
;           'This program is free software; you can',     $
;           'redistribute it and/or modify it under the', $
;           'terms of the GNU General Public License as', $
;           'published by the Free Software Foundation.', $
;           '   '                                         $         
;           ]

       @banner_text.pro

       d = DIALOG_MESSAGE (/INFO, DIALOG_PARENT = self.widgetID.top, $
           TITLE = 'About', text)
    
    ENDIF


END

 
; ----------------------------------------------------------------------------
; Handle keyboard events in the drawing area
; ----------------------------------------------------------------------------
PRO PhaDisplay::keyBindings, char
    
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
         
         't' : self->togglePlot
         
         'r' : self->Plot, _EXTRA = *self.PlotOptions
         
         'f' : self->zoom, /FULLSCREEN
         
         'h' : IF (STRUPCASE (self.domain) EQ 'ENERGY') THEN self->togglePlot
         's' : IF (STRUPCASE (self.domain) EQ 'TIME')   THEN self->togglePlot

         ',' : BEGIN ; zoom in by steps: x-axis
             self->Plot, XSTYLE = 1, _EXTRA = *self.PlotOptions
             xr = SCALEAXIS (/XAXIS, -0.05) 
             yr = AXISRANGE (/YAXIS)
             setXRange = 1
             END

         '.' : BEGIN ; zoom out by steps: x-axis
             self->Plot, XSTYLE = 1, _EXTRA = *self.PlotOptions
             xr = SCALEAXIS (/XAXIS, 0.05) 
             yr = AXISRANGE (/YAXIS)
             setXRange = 1
             END

         'k' : BEGIN ; pan left: x-axis
             self->Plot, XSTYLE = 1, _EXTRA = *self.PlotOptions
             xr = PANAXIS (/XAXIS, 0.03) 
             yr = AXISRANGE (/YAXIS)
             setXRange = 1
             END

         'l' : BEGIN ; pan right: x-axis
             self->Plot, XSTYLE = 1, _EXTRA = *self.PlotOptions
             xr = PANAXIS (/XAXIS, -0.03) 
             yr = AXISRANGE (/YAXIS)
             setXRange = 1
             END

         '<' : BEGIN ; zoom in by steps: y-axis
             self->Plot, YSTYLE = 1, _EXTRA = *self.PlotOptions
             yr = SCALEAXIS (/YAXIS, -0.05) 
             xr = AXISRANGE (/XAXIS)
             setYRange = 1
             END

         '>' : BEGIN ; zoom out by steps: y-axis
             self->Plot, YSTYLE = 1, _EXTRA = *self.PlotOptions
             yr = SCALEAXIS (/YAXIS, 0.05) 
             xr = AXISRANGE (/XAXIS)
             setYRange = 1
             END

         'K' : BEGIN ; pan left: y-axis
             self->Plot, YSTYLE = 1, _EXTRA = *self.PlotOptions
             yr = PANAXIS (/YAXIS, 0.03) 
             xr = AXISRANGE (/XAXIS)
             setYRange = 1
             END

         'L' : BEGIN ; pan right: y-axis
             self->Plot, YSTYLE = 1, _EXTRA = *self.PlotOptions
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
       
       CASE (STRUPCASE (self.domain)) OF

           'TIME'   : self.Lightcurve->setRange, range
           'ENERGY' : self.Spectrum->setRange, range
           
           ELSE: MESSAGE, 'Internal error: unknown domain: ' + self.domain

       ENDCASE        
       
       ;self->adjustRanges, xr, yr, self.domain  ;, /XLOG, /YLOG
 
       self->plot, $
           XRANGE = xr, XSTYLE = setXRange, $
           YRANGE = yr, YSTYLE = setYRange, $
           _EXTRA = *self.PlotOptions
           
       ;self->finishRanges, self.domain, XLOG = (self.domain EQ 'ENERGY'), /YLOG

    ENDIF
    
END


; ----------------------------------------------------------------------------
; Display available key bindings
; ----------------------------------------------------------------------------
PRO PhaDisplay::showKeyBindings

    ; COMPILE_OPT HIDDEN

    txt = [ $
    
        'd : Dismiss (unmap) the display widget', $         
        'z : Enter interactive zoom mode', $      
        'x : Zoom mode restricted to the x-axis', $   
        'y : Zoom mode restricted to the y-axis', $   
        'g : Go (and zoom in) to selected data', $
        'f : Fullscreen display of the current plot', $
        'i : Enter interactive source selection mode', $
        ' ', $
        't : Toggle lightcurve/spectrum', $         
        'h : Toggle to lightcurve display', $
        's : Toggle to spectrum display', $
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
; Display the current lookup file
; ----------------------------------------------------------------------------
PRO PhaDisplay::showLookupFile

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
    FOR i = 0L, N_ELEMENTS (txt) - 1L DO $
        label = WIDGET_LABEL (b, VALUE = txt[i], /ALIGN_LEFT)
        
    button = WIDGET_BUTTON (base, VALUE = 'Dismiss')
    
    WIDGET_CONTROL, base, /REALIZE

    XMANAGER, '_unused', base, /NO_BLOCK, EVENT_HANDLER = 'DIALOG_DISMISS'
   
END

 
; ----------------------------------------------------------------------------
; Create a label of the current span
; ----------------------------------------------------------------------------
FUNCTION PhaDisplay::label, TIME = time, ENERGY = energy

    ; COMPILE_OPT HIDDEN

    units = self.Reader->units (/POINTER)
            
    IF (KEYWORD_SET (time)) THEN BEGIN
     
       range = NEAREST_EDGE (self.Lightcurve->combinedTimes (), $
           (self.LightCurve->timeSpan ())[0, *])
       range = REFORM (range)
       
       unit = (*units).times
       
               
    ENDIF 
    
    IF (KEYWORD_SET (energy)) THEN BEGIN
     
       range = NEAREST_EDGE (self.Spectrum->combinedThresholds (), $
           (self.Spectrum->energySpan ())[0, *])
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


; ----------------------------------------------------------------------------
; Update the status bar with the current spans
; ----------------------------------------------------------------------------
PRO PhaDisplay::showSpan

    ; COMPILE_OPT HIDDEN

    tLabel = self->label (/TIME)
    eLabel = self->label (/ENERGY)
    
    label = tLabel + ', ' + eLabel
    self->setStatus, label
    
END


; ----------------------------------------------------------------------------
; Handle XY Log axes change events
; ----------------------------------------------------------------------------
PRO PhaDisplay::changeLog, myValue

    ; COMPILE_OPT HIDDEN

    CASE (STRUPCASE (self.domain)) OF

        'TIME'  : BEGIN
            
            IF (myValue[0] EQ 1) THEN BEGIN
            
               fit = DIALOG_MESSAGE (/QUESTION, $
                   ['Log X Axes not recommended for Time Histories, are you sure?'], $
                   DIALOG_PARENT = self.widgetID.top)
               IF (fit EQ 'No') THEN BEGIN
                  error = 1
                  WIDGET_CONTROL, self.logChoiceID, SET_VALUE = (self.lightcurve)->logStatus()
                  RETURN
                  ENDIF
                  
              ENDIF
              self.lightcurve->setLogStatus, myValue
        
            END
            
        'ENERGY' : BEGIN

              self.spectrum->setLogStatus, myValue

            END

        ELSE: MESSAGE, 'Internal error: unknown domain: ' + self.domain

    ENDCASE        
    
    self->plot, _EXTRA = *self.PlotOptions
    
END


; ----------------------------------------------------------------------------
; Override the plot method.
; ----------------------------------------------------------------------------
PRO PhaDisplay::plot, HARDCOPY = hardcopy, _EXTRA = extra
    
    ; COMPILE_OPT HIDDEN

    hardcopy = KEYWORD_SET (hardcopy)
    
    ;== Plot to offscreen pixmap    
    
    IF (NOT hardcopy) THEN BEGIN
       win = !D.WINDOW
       self->setPixWindow
    ENDIF
     
    CASE (STRUPCASE (self.domain)) OF

        'TIME'  : BEGIN
        
            self.Lightcurve->plot, $
                FGCOLOR = self.color->color('FG'), $ ;self.colors.fg, $
                BGCOLOR = self.color->color('BG'), $ ;self.colors.bg, $
                DRAWCOLOR = self.color->color('HIST'), $ ;self.colors.hist, $
                _EXTRA = extra

            IF (self.Background->haveModel ()) THEN BEGIN

               self.Background->plot, /HISTORY, $
                   self.Lightcurve->combinedTimes (), $
                   self.Lightcurve->timeSpan (), $
                   self.Lightcurve->timeLookup (), $
                   COLOR = self.color->color('BKGD'), $ ;self.colors.bkgd, $
                   LINESTYLE = 1, $
                   _EXTRA = extra

            ENDIF

            label = self->label (/ENERGY)
            self->annotate, label, COLOR = self.color->color('FG'), _EXTRA = extra 
            fname = self.Reader->filename (/BASENAME)
            luname = self.myLookup
            IF (luname NE '') THEN fname = fname + '; ' + STRTRIM(luname, 2)
            self->annotate, fname, YPOSITION = !Y.WINDOW[1] + 0.01, $
              /SMALL, COLOR = self.color->color('FG'), _EXTRA = extra

            END
            
        'ENERGY' : BEGIN

            self.Spectrum->plot, $ 
                FGCOLOR = self.color->color('FG'), $ ;self.colors.fg, $
                BGCOLOR = self.color->color('BG'), $ ;self.colors.bg, $
                DRAWCOLOR = self.color->color('SPEC'), $ ;self.colors.spec, $
                _EXTRA = extra

            IF (self.Background->haveModel ()) THEN BEGIN
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

            END

        ELSE: MESSAGE, 'Internal error: unknown domain: ' + self.domain

    ENDCASE        

    self->plotSpan, _EXTRA = extra

    ;== Copy pixmap to screen

    IF (NOT hardcopy) THEN BEGIN
       WSET, win
       self->copyPixWindow
    ENDIF
        
END


;------------------------------------------------------------------------------
; Read lookup file
;------------------------------------------------------------------------------
PRO PhaDisplay::readLookup, FILENAME = filename, FINESTAT = fineStat, $
    AUTO = auto, SILENT = silent, LUFILE = luFile, ERROR = error

    ; COMPILE_OPT HIDDEN

    error = 0
    
    data = self.Reader->data (/POINTER)
    ;TBD: make this better!
    energySpan   = self.lightCurve->energySpan ()
    energyLookup = self.lightCurve->energyLookup ()

    timeSpan   = self.lightCurve->timeSpan ()
    timeLookup = self.lightCurve->timeLookup ()
    
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
              /MUST_EXIST, FILE = f, FILTER = '*.lu', $
              PATH = !RM_PARAMS.lastPath, GET_PATH = lastPath, $
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
    table = (n EQ 0) ? LONARR (1) : LONARR (n)
    READF, FL, table
    
    nt = SIZE ((*data).times)
    IF (n EQ 0) THEN BEGIN
       n = nt[2] + 1
       table = INDGEN (n)
    ENDIF
    table = table[SORT (table)]

    diff = MIN (Table[1:*] - table)

    IF (table[0] NE 0) THEN BEGIN

       MESSAGE, /CONTINUE, 'Time table does not begin with channel zero.'
       GOTO, BAD_TABLE

    ENDIF $
    ELSE IF (table[n - 1] NE nt[2]) THEN BEGIN

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
    IF (N NE 0) THEN BEGIN

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
       ;WIDGET_CONTROL, self.logChoiceID, SET_VALUE = [xlog, ylog]
    
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

    ;== Read in the trigdat fine time status
    
    fineRead = 0

    IF (NOT EOF (FL)) THEN BEGIN       
       READF, FL, fineRead
       fineStat = fineRead
    ENDIF

    CLOSE, fl
    FREE_LUN, FL

    IF (NOT error) THEN BEGIN

       self->setTimeSpan, timeSpan
       self->setEnergySpan, energySpan
       self->setTimeLookup, timeLookup
       self->setEnergyLookup, energyLookup
	
		;== Look for the associated ".ti" file:
		IF OBJ_ISA(self.Reader, "TTEReader") THEN BEGIN
			dotpos = STRPOS(filename, '.', /REVERSE_SEARCH)
			IF (dotpos NE 0) THEN BEGIN
				ti_filename = STRMID(filename, 0, dotpos) + '.ti'
			ENDIF
			c = FINDFILE (ti_filename, COUNT = cnt)
			IF (cnt NE 0) THEN BEGIN
			   self.Reader->read, TIME_FILE = ti_filename, ERROR = error
			   self->setTimeLookup, timeLookup, /INITIALIZE
			   self.lightcurve->update
			ENDIF
		ENDIF

       ;== Update the defaults
       self.Lightcurve->update, historyRange
       ;self.Lightcurve->setDefaultRange
       self.Spectrum->update, spectrumRange
       ;self.Spectrum->setDefaultRange

       IF (fitBackground) THEN BEGIN
       
          self.Background->setSpan, backSpan
          self->fitBackground, ORDER = order

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
; Display CGRO location at trigger time
;------------------------------------------------------------------------------
;PRO PhaDisplay::showCGRO

    ; COMPILE_OPT HIDDEN

;    header = self.Reader->header (/POINTER)
    
;    TJD = FIX (SXPAR ((*header).ext2, 'BASETIME'))
    
;    CGRO_LOCATION, TJD, (*header).ext0

;END


;------------------------------------------------------------------------------
; Adjust user-selected intervals.  This procedure is passed to TRACK_MOUSE
; via the CALL_METHOD keyword - do not add parameters.
;------------------------------------------------------------------------------
PRO PhaDisplay::snap, coordinates, _EXTRA = extra

    ; COMPILE_OPT HIDDEN

    xr = coordinates[0, *]
    yr = coordinates[1, *]
 
    yspan = AXISRANGE (/YAXIS)
    
    ;== Erase
    
    OPLOT, [1, 1] * xr[0], yspan, COLOR = !P.BACKGROUND, LINESTYLE = 0
    OPLOT, [1, 1] * xr[1], yspan, COLOR = !P.BACKGROUND, LINESTYLE = 0
   
    ;== Snap to outer bin edges
    
    CASE (STRUPCASE (self.domain)) OF
    
        'TIME': $
            edges = NEAREST_EDGE (self.Lightcurve->combinedTimes (), xr)
                
        'ENERGY':   $
            edges = NEAREST_EDGE (self.Spectrum->combinedThresholds (), xr)
    
        ELSE: MESSAGE, 'Internal error: unknown domain: ' + self.domain
	     
    ENDCASE
    
    OPLOT, [1, 1] * edges[0], yspan, LINESTYLE = 0, _EXTRA = extra
    OPLOT, [1, 1] * edges[1], yspan, LINESTYLE = 0, _EXTRA = extra

END


;------------------------------------------------------------------------------
; Toggle the current plot
;------------------------------------------------------------------------------
PRO PhaDisplay::togglePlot
 
    ; COMPILE_OPT HIDDEN

    CASE (STRUPCASE (self.domain)) OF

        'TIME'   : BEGIN
            self.domain = 'ENERGY'
            WIDGET_CONTROL, self.logChoiceID, SET_VALUE = (self.spectrum)->logStatus()
            END
            
        'ENERGY' : BEGIN
            self.domain = 'TIME'
            WIDGET_CONTROL, self.logChoiceID, SET_VALUE = (self.lightcurve)->logStatus()
            END

        ELSE: MESSAGE, 'Internal error: unknown domain: ' + self.domain

    ENDCASE        
    
    self.haveDefaultRange = 0
    self->plot, _EXTRA = *self.PlotOptions

END


;------------------------------------------------------------------------------
; Toggle the current plot to the requested domain
;------------------------------------------------------------------------------
PRO PhaDisplay::setPlotDomain, domain
 
    ; COMPILE_OPT HIDDEN

    self.domain = domain
    self.haveDefaultRange = 0
    self->plot, _EXTRA = *self.PlotOptions

END

;------------------------------------------------------------------------------
; Plot the current span
;------------------------------------------------------------------------------
PRO PhaDisplay::plotSpan, _EXTRA = extra

    ; COMPILE_OPT HIDDEN

    CASE (STRUPCASE (self.domain)) OF

        'TIME': self.Lightcurve->plotSpan, $
                COLOR = self.colors.hist, _EXTRA = extra
        
        'ENERGY': self.Spectrum->plotSpan, /NOSHADE, $
                  COLOR = self.colors.spec, _EXTRA = extra

        ELSE: MESSAGE, 'Internal error: unknown domain: ' + self.domain

    ENDCASE        

END


;------------------------------------------------------------------------------
; Integrate and combine the history or spectrum
;------------------------------------------------------------------------------
PRO PhaDisplay::integrateAndCombine

    ; COMPILE_OPT HIDDEN

    CASE (STRUPCASE (self.domain)) OF

        'TIME'   : BEGIN
            self.Spectrum->integrateTime
            self.Spectrum->combine
            END
                   
        'ENERGY' : BEGIN
            self.Lightcurve->integrateEnergy
            self.Lightcurve->combine
            END
            
         ELSE    : MESSAGE, 'Internal error: unknown domain: ' + self.domain

    ENDCASE
        
END


;------------------------------------------------------------------------------
; Interactively select source intervals from the plotted event
;------------------------------------------------------------------------------
PRO PhaDisplay::selectSource, $
    INTERACTIVE = interactive, SIGNAL_TO_NOISE = signalToNoise, $
    HAVESELECTION = haveSelection

    ; COMPILE_OPT HIDDEN

    domain = STRUPCASE (self.domain)

    CASE (domain) OF

        'TIME'  : BEGIN
            thresholds = self.Lightcurve->combinedTimes ()
            span = self.lightCurve->timeSpan ()
            END

        'ENERGY' : BEGIN

	    IF (KEYWORD_SET (signalToNoise)) THEN BEGIN
		
		 w = DIALOG_MENU ('Dismiss', TITLE = [ $
		       'This function is not yet', $
		       'implemented for the energy spectrum'])
	        RETURN     
		
	    ENDIF

            thresholds = self.Spectrum->combinedThresholds ()
            span = self.spectrum->energySpan ()
            END

         ELSE: MESSAGE, 'Internal error: unknown domain: ' + self.domain

    ENDCASE
    
    ;== Select source interval(s) remotely - haveSelection should be a span
    IF (KEYWORD_SET (haveSelection)) THEN BEGIN

       self->plot, _EXTRA = *self.PlotOptions
       self->setSpan, haveSelection
       self->integrateAndCombine

    ENDIF ; remotely

    ;== Select source interval(s) interactively
    IF (KEYWORD_SET (interactive)) THEN BEGIN

       self->plot, _EXTRA = *self.PlotOptions      ;, XSTYLE = 1, YSTYLE = 1
       newSpan = SELECT (thresholds, self.widgetID.draw, span, $
                COLOR = self.color->color('FG'),      $ 
                BACKGROUND = self.color->color('BG'), $ 
                HAVESELECTION = gotSelection)

       IF (NOT gotSelection) THEN BEGIN
           self->setSpan, span, /INITIALIZE
           RETURN
       ENDIF
       
       self->setSpan, newSpan
       self->integrateAndCombine

    ENDIF ; interactive
    
    ;== Choose interval(s) based on S/N

; TODO S/N selection not working for SPECTRUM
    
    IF (KEYWORD_SET (signalToNoise)) THEN BEGIN
    
       ;== Background model required

       IF (NOT self.Background->haveModel ()) THEN BEGIN
          self->setStatus, 'Background model required for this function.', 10, /REVERT
          RETURN
       ENDIF

       ;== Select interval to evaluate

       combinedTimes = self.Lightcurve->combinedTimes ()

       self->plot, _EXTRA = *self.PlotOptions            ;, XSTYLE = 1, YSTYLE = 1
       span = SELECT (thresholds, self.widgetID.draw, $
                COLOR = self.color->color('FG'),      $ 
                BACKGROUND = self.color->color('BG'), $ 
                /SINGLEINTERVAL, HAVESELECTION = haveSelection)

       IF (NOT haveSelection) THEN RETURN

       ;== Evaluate the background model, and integrate over 
       ;== selected energies or times

       COUNT_SPAN, combinedTimes, span, selectInd
       t_beg = REFORM (combinedTimes[0, selectInd])     
       t_end = REFORM (combinedTimes[1, selectInd])    
       ;backModel = self.Background->evalModel (t_beg, t_end)

       CASE (domain) OF
       
           'TIME': BEGIN
;               results = self.Background->integrated()
               self.Background->integrate, /ENERGY,      $ 
                   self.Spectrum->combinedThresholds (), $ 
                   self.Spectrum->energySpan (),         $ 
                   self.Spectrum->energyLookup (),       $ 
                   HIST = tempBackground, /FULLTIME 
               data = self.Reader->data (/POINTER)
               delta_t = (*data).times[1, *] - (*data).times[0, *]
               tempBackground *= delta_t
               self.lightcurve->PHA::combine, tempBackground, SQRT(tempBackground), $
                   (*data).times, *(self.lightcurve).timelookup, $
                   background, temperrs, tempti
               rates = self.Lightcurve->combinedHist ()
;               background = *results.hist
               END    

           'ENERGY': BEGIN
               self.Background->integrate, /TIME,        $
                   self.Lightcurve->combinedTimes (),    $
                   self.Lightcurve->timeSpan (),         $
                   self.Lightcurve->timeLookup (),       $
                   SPEC = background, /FULLTIME
               rates = self.Spectrum->combinedSpec ()
               END
               
       ENDCASE
       
       begThres = REFORM (thresholds[0, selectInd])
       endThres = REFORM (thresholds[1, selectInd])

       rates = rates[selectInd]
       bkgd  = background[selectInd]

       res          = endThres - begThres
       counts       = rates * res
       bkgdCounts   = bkgd  * res
       noise        = SQRT (bkgd * res)
       sourceCounts = counts - bkgdCounts
       sig2noise    = sourceCounts / noise

       ;== Get sigma level

RETRY:
       
       sigma = DIALOG_INPUT (TITLE = 'Sigma Level', $
           PROMPT = 'Enter sigma level above background', INITIAL = '5.5')
       IF (sigma EQ '') THEN RETURN
       sigma = FLOAT (sigma)

       idx  = WHERE (sig2noise GE sigma, cnt)

       ;== Loop through the IDX indices, merging intervals that are contiguous

       intIdx = REDUCE_INTERVALS (idx)
       IF (intIdx[0] EQ -1) THEN BEGIN
          self->setStatus, 'No intervals found.', 5, /REVERT
          GOTO, RETRY
       ENDIF
          
       ;== Set the span 

       begThres = REFORM (begThres[intIdx[0, *]] + (res[intIdx[0, *]] / 2.0))
       endThres = REFORM (endThres[intIdx[1, *]] - (res[intIdx[1, *]] / 2.0))

       nInt = N_ELEMENTS (begThres)
       span = FLTARR (nInt + 1, 2)
       span[0, 0] = MIN (begThres)
       span[0, 1] = MAX (endThres)

       FOR i = 1, nInt DO BEGIN
           span[i, 0] = begThres[i - 1]
           span[i, 1] = endThres[i - 1]
       ENDFOR

       self->setSpan, span
       self->integrateAndCombine
 
    ENDIF ; signalToNoise
    
    self.dirty = 1   ;== Selections have changed
    self->plot, _EXTRA = *self.PlotOptions         ;, XSTYLE = 1, YSTYLE = 1

    IF (self.Background->haveModel ()) THEN $
       self->fitBackground

END 


;------------------------------------------------------------------------------
; Interactively select source intervals from the plotted event
;------------------------------------------------------------------------------
PRO PhaDisplay::adjustSource, $
    LEFT = left, RIGHT = right, EXPAND = expand, CONTRACT = contract 

    ; COMPILE_OPT HIDDEN

    CASE (STRUPCASE (self.domain)) OF

        'TIME'  : BEGIN
            data = self.Lightcurve->combinedTimes ()
            span = self.Lightcurve->timeSpan ()
            END
            
        'ENERGY' : BEGIN
            data = self.Spectrum->combinedThresholds ()
            span = self.Spectrum->energySpan ()
            END
            
         ELSE: MESSAGE, 'Internal error: unknown domain: ' + self.domain

    ENDCASE

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
        
    self->setSpan, newSpan
    self->integrateAndCombine

    IF (self.Background->haveModel ()) THEN $
       self->fitBackground

    self.dirty = 1   ;== Selections have changed
    self->plot, _EXTRA = *self.PlotOptions

END


;------------------------------------------------------------------------------
; Set the current span
;------------------------------------------------------------------------------
PRO PhaDisplay::setSpan, span, _EXTRA = extra

    ; COMPILE_OPT HIDDEN

    CASE (STRUPCASE (self.domain)) OF

        'TIME': BEGIN
            self->setTimeSpan, span, _EXTRA = extra
            END 
            
        'ENERGY': BEGIN
            self->setEnergySpan, span, _EXTRA = extra
            END
            
        ELSE: MESSAGE, 'Internal error: unknown domain: ' + self.domain

    ENDCASE        

END


;------------------------------------------------------------------------------
; Fit background model
;------------------------------------------------------------------------------
PRO PhaDisplay::fitBackground, ERROR = error, $
    ORDER = order, CHANNELS = channels, BACKSPAN = backSpan, $
    INTERACTIVE = interactive
    
    ; COMPILE_OPT HIDDEN

    IF (KEYWORD_SET (interactive)) THEN BEGIN
    
       self.domain = 'TIME'

       self->plot, _EXTRA = *self.PlotOptions           
       self->plotBackSpan
       self->selectBackground, HAVESELECTION = haveSelection         
       self->plotBackSpan, /ERASE

     ;  IF (haveSelection EQ 0) THEN RETURN
           
    ENDIF
;    self->plotBackSpan, /ERASE
    ;== Get user-selected energy channels
    
    combinedThresholds = self.Spectrum->combinedThresholds ()
    data = self.Reader->data (/POINTER)
    energySpan = self.lightCurve->energySpan ()
    e = NEAREST_EDGE (combinedThresholds, energySpan[0, *], channels) 
    energyLookup = self.lightCurve->energyLookup ()
    numBins = (SIZE (energyLookup))[1] - 1
    channels = [energyLookup[channels[0]], energyLookup[channels[1] + 1 < numBins] - 1]

    ;== Fit background model

    IF (N_ELEMENTS (order) NE 0) THEN BEGIN   

       success = self.Background->fit (ORDER = order, CHANNELS = channels)    

    ENDIF ELSE BEGIN

       IF (KEYWORD_SET (interactive) OR NOT self.Background->haveModel ()) $
          THEN BEGIN
          
          success = self.Background->fit (/SELECTORDER, CHANNELS = channels)    

       ENDIF ELSE BEGIN
          
           success = self.Background->fit (ORDER = self.Background->order (), $
               CHANNELS = channels)    
       
       ENDELSE
       
    ENDELSE
    
    IF (NOT success) THEN BEGIN
       self->setStatus, 'Background fit failed or canceled.'
       RETURN
    ENDIF
    
    ;== Display chi-sq values
       
    IF (KEYWORD_SET (interactive)) THEN BEGIN
    
       data = self.Reader->data (/POINTER)

       self.Background->displayChisq, (*data).eEdges, $
           self.Spectrum->combinedThresholds (), $
           self.Spectrum->energySpan (), $
           self.Spectrum->energyLookup ()

    ENDIF
    
    ;== Integrate the background model
    
    self->integrateBackground, /ENERGY
    self->integrateBackground, /TIME
    self.dirty = 1   ;== Selections have changed

END


;------------------------------------------------------------------------------
; Interactively select background intervals from the plotted event
;------------------------------------------------------------------------------
PRO PhaDisplay::selectBackground, HAVESELECTION = haveSelection

    ; COMPILE_OPT HIDDEN

    IF (self.Background->haveModel ()) THEN BEGIN
    
       backSpan = self.Background->backSpan ()
       
       span = SELECT (self.Lightcurve->combinedTimes (), $
                self.widgetID.draw, backSpan, $
                COLOR = self.color->color('FG'),      $ 
                BACKGROUND = self.color->color('BG'), $ 
                LINESTYLE = 2, HAVESELECTION = haveSelection)

    ENDIF ELSE BEGIN
        
       span = SELECT (self.Lightcurve->combinedTimes (), $
                self.widgetID.draw, $
                COLOR = self.color->color('FG'),      $ 
                BACKGROUND = self.color->color('BG'), $ 
                LINESTYLE = 2, HAVESELECTION = haveSelection)
    
    ENDELSE 

    IF (haveSelection) THEN BEGIN
       self.Background->setSpan, span
    ENDIF ELSE BEGIN
       self.Background->erase
    ENDELSE
       
END 


;------------------------------------------------------------------------------
; Plot background interval selections
;------------------------------------------------------------------------------
PRO PhaDisplay::plotBackSpan, ERASE = erase

    ; COMPILE_OPT HIDDEN

    IF (self.Background->haveModel ()) THEN BEGIN
       
       backSpan = self.Background->backSpan ()
       self.Lightcurve->plotSpan, SPAN = backSpan, /NOSHADE, LINESTYLE = 1, $
           ERASE = erase, _EXTRA = extra       
    
    ENDIF
    
END
   

;------------------------------------------------------------------------------
; Integrate the background model
;------------------------------------------------------------------------------
PRO PhaDisplay::integrateBackground, TIME = time, ENERGY = energy

    ; COMPILE_OPT HIDDEN

    ;== Integrate background over time
    
    IF (KEYWORD_SET (time)) THEN $
       self.Background->integrate, $
           self.Lightcurve->combinedTimes (), $
           self.lightCurve->timeSpan (), $
           self.lightCurve->timeLookup (), /TIME

    ;== Integrate background over energy
    
    IF (KEYWORD_SET (energy)) THEN $	
       self.Background->integrate, $
           self.Spectrum->combinedThresholds (), $
           self.Spectrum->energySpan (), $
           self.Spectrum->energyLookup (), /ENERGY

END


;------------------------------------------------------------------------------
; Integrate the background model over the current user time selections
;------------------------------------------------------------------------------
PRO PhaDisplay::integrateBackgroundSelections, span, backCrate, backCsig, error, $
                BATCH = batch

    ; COMPILE_OPT HIDDEN
    
    error = 0       ;== No error
    
    ;== Don't want to put up dialog during batch fitting!
    IF NOT KEYWORD_SET(batch) THEN batch = 0

    ;== Return pointers for efficiency.  Do NOT alter these data!
    
    data           = self.Reader->data (/POINTER)
    combTimes      = self.Lightcurve->combinedTimes (/POINTER)
    combThresholds = (*data).eedges

    IF (self.Background->haveModel ()) THEN BEGIN

       ;== Energy bin widths

       width = REFORM (combThresholds[1, *] - combThresholds[0, *])

       COUNT_SPAN, *combTimes, span, tidx
       numTimes = N_ELEMENTS (tidx)

       timeLookup = self.Lightcurve->timeLookup ()

       range = INTARR (2, numTimes)
       range[0, *] = timeLookup[tidx]
       range[1, *] = timeLookup[tidx + 1] - 1

       n = SIZE ((*data).rates)
       T_BACK_SIG = FLTARR (n[1])
       T_BACK_RATE = FLTARR (n[1])
       TOT_BACK_TIME = 0.0

       FOR i = 0, numTimes - 1 DO BEGIN

           temp_span = FLTARR(1, 2)
           temp_span[0, 0] = ((*data).times)[0, range[0, i]] + 1.0E-6
           temp_span[0, 1] = ((*data).times)[1, range[1, i]] - 1.0E-6
           COUNT_SPAN, ((*data).times), temp_span, temp_ind

           num_ind = N_ELEMENTS (temp_ind)

           t_beg = ((*data).times)[0, temp_ind]
           t_end = ((*data).times)[1, temp_ind]

           back_specs = self.Background->evalModel (T_BEG, T_END)

           FOR j = 0, num_ind - 1 DO BEGIN

               temp_bcrate = TRANSPOSE (back_specs[j, *]) 

               ;== Calculate observed livetime

               temp_onesp = ((*data).rates)[*, temp_ind[j]]
               temp_onebs = ((*data).errors)[*, temp_ind[j]]

               nbind = WHERE (temp_onebs, nbcount)
               IF (nbcount GT 0) THEN BEGIN

                  blive_t = temp_onesp[nbind] / $
                      (temp_onebs[nbind] * temp_onebs[nbind])
                  dbt = TOTAL (blive_t) / N_ELEMENTS (nbind)

               ENDIF ELSE BEGIN

                  dbt = ((*data).times)[1, temp_ind[j]] - $
                        ((*data).times)[0, temp_ind[j]]

               ENDELSE

               ;== Weight by observed Livetime

               t_back_rate = t_back_rate + temp_bcrate * dbt
               tot_back_time = tot_back_time + dbt

           ENDFOR

       ENDFOR

       backCrate = t_back_rate / width / tot_back_time 

       back_err_specs = self.Background->evalMSig ( $
           ((*data).times)[0, range[0, 0]], $
           ((*data).times)[1, range[1, numTimes - 1]])

       temp_bsig = TRANSPOSE (back_err_specs[0, *]) 

       backCsig = temp_bsig / width

    ENDIF ELSE BEGIN

       IF NOT batch THEN BEGIN
           fit = DIALOG_MESSAGE (/QUESTION, $
               ['No background model exists for the file: ', $
               self.Reader->filename (/BASENAME), $
               'Continue with fitting?'], $
               DIALOG_PARENT = self.widgetID.top)
           IF (fit EQ 'No') THEN BEGIN
              error = 1
              RETURN
           ENDIF
       ENDIF
       
       n = SIZE (((*data).rates))
       backCrate = FLTARR (n[1])
       backCsig  = FLTARR (n[1])

    ENDELSE

END


;------------------------------------------------------------------------------
; Fit photon model
;------------------------------------------------------------------------------
PRO PhaDisplay::getSpectra, cancel, SELECTBYINDEX = selectByIndex, BATCH = batch, $
    SINGLEBIN = singleBin, SELECTIONS = selections, SINGLEINTERVAL = singleInterval

    ; COMPILE_OPT HIDDEN
    
    cancel = 0
    ;== Return pointers for efficiency.  Do NOT alter these data!

    header         = self.Reader->header (/POINTER)   
    data           = self.Reader->data (/POINTER)   
    combTimes      = self.Lightcurve->combinedTimes (/POINTER)
    combThresholds = self.Spectrum->combinedThresholds (/POINTER)
    
    ;== Integrate the user-selected span.  This could be the selection span,
    ;== or interactive picking of intervals.
    
    IF (N_ELEMENTS (selectByIndex) NE 0) THEN BEGIN    
       ;== Select only the ith bin in span (batch processing only)
       
       selectedSpan = self.lightCurve->timeSpan ()
       COUNT_SPAN, *combTimes, selectedSpan, tidx
       numTimes = N_ELEMENTS (tidx)
       IF (selectByIndex GE numTimes) THEN BEGIN
           MESSAGE, 'Internal error: selection index is too large! '
           cancel = 1
           RETURN
       ENDIF
       span = FLTARR (2, 2)
       span[*] = TOTAL((*combTimes)[*, tidx[selectByIndex]]) / 2.

    ENDIF
    
    IF (KEYWORD_SET (selections)) THEN $
       span = self.lightCurve->timeSpan ()
    
    IF (KEYWORD_SET (singleBin)) THEN BEGIN
    
       self.domain = 'TIME'
       self->map             ;, /DOSHOW
       self->plot, _EXTRA = *self.PlotOptions

       span = SELECT (*combTimes, self.widgetID.draw, /SINGLEBIN, $
                COLOR = self.color->color('FG'),      $ 
                BACKGROUND = self.color->color('BG'), $ 
                HAVESELECTION = haveSelection)
       IF (NOT haveSelection) THEN BEGIN
           cancel = 1
           RETURN    
       ENDIF
       
       self->plot, _EXTRA = *self.PlotOptions
    ENDIF

    IF (KEYWORD_SET (singleInterval)) THEN BEGIN
    
       self.domain = 'TIME'
       self->map            ;, domap?
       self->plot, _EXTRA = *self.PlotOptions

       span = SELECT (*combTimes, self.widgetID.draw, /SINGLEINTERVAL, $
                COLOR = self.color->color('FG'),      $ 
                BACKGROUND = self.color->color('BG'), $ 
                HAVESELECTION = haveSelection)
       IF (NOT haveSelection) THEN BEGIN
           cancel = 1
           RETURN    
       ENDIF
    
       self->plot, _EXTRA = *self.PlotOptions
    ENDIF
    
    self.Lightcurve->integrateTimeFast, span, $
        HISTORY = history, ERRORS = errors, TOTALTIME = totalTime
        
    ;== Something wrong happened: no selection
    IF (totalTime EQ 0.0) THEN BEGIN
           cancel = 1
           RETURN    
    ENDIF        

    ;== Get user-selected energy channels and time range
    
    energySpan = self.Spectrum->energySpan ()
    eInt = NEAREST_EDGE (*combThresholds, energySpan[0, *], fitChannels) 
    eLookup = self.Spectrum->energyLookup()
    numBins = N_ELEMENTS (eLookup) - 1

    fitChannels[0] = eLookup[fitChannels[0]]
    fitChannels[1] = eLookup[fitChannels[1] + 1 < numBins] - 1
    tInt = NEAREST_EDGE (*combTimes, span[0, *]) 

    ;== Energy bin widths
    width = REFORM (((*data).eedges)[1, *] - ((*data).eedges)[0, *])

    ;== Convert to counts / s-energy
    
    obsCrate = history ; / width   
    
    ;== Integrate the background model over the selected time span
    ;using the full energy resolution
    lookupSave = eLookup     ;self->energyLookup()
    self->setEnergyLookup, lookupSave, /INITIALIZE
    self->integrateBackgroundSelections, span, backCrate, backCsig, error, $
          BATCH = batch
    IF error THEN BEGIN
        cancel = 1
        RETURN    
    ENDIF
    
    ;== 01/07/09 RDP: As per CCR# 122, we want to do someting special for synthesized 
    ; bursts; namely, re-randomize the background for each source spectrum:    
    IF self.rndseed NE 0 THEN BEGIN
    	;== Skip the user interface if we are in BATCH mode:
    	rndSeed = self.rndseed
		IF (N_ELEMENTS (selectByIndex) NE 0) THEN BEGIN
			rndseed = self.rndseed + selectByIndex
		ENDIF ELSE IF NOT batch THEN BEGIN
			;== Parameters for the synthesized data:
			result = DIALOG_INPUT (NFIELDS = 1, $
				   PROMPT = ['New seed: '], $
				   INITIAL = [0], $
				   TITLE = 'Input Unique Random Seed?')
			IF (result[0] NE 0) THEN rndseed = (self.rndseed = result[0])
		ENDIF
    	tempBack = backCrate * width * totalTime
    	synBack = tempBack
        FOR i = 0, numBins - 1 DO BEGIN
			synBack[i] = RANDOMN(rndseed, 1, POISSON = (tempBack[i] > 1), /DOUBLE)
        ENDFOR
        backCrate = synBack / width / totalTime
        backCsig  = SQRT(synBack) / width / totalTime
    ENDIF

        
    ;== Compute the livetime
    COUNT_SPAN, *combThresholds, energySpan, eIdx
    liveTime = FLTARR (N_ELEMENTS (obsCrate))
    nebin = N_ELEMENTS (eIdx)
    IF (self.Background->haveModel ()) THEN BEGIN
        FOR i = 0, nebin - 1 DO BEGIN
            eLow = elookup[eidx[i]]
            eHigh = elookup[eidx[i] + 1] - 1
            myLive = (errors[eLow] EQ 0.0) ? totalTime : history[eLow] / (errors[eLow])^2
            liveTime[eLow: eHigh] = myLive
        ENDFOR
    ENDIF ELSE BEGIN
        ;== Implement the kludge that accepts no background model
        backCsig = errors / width
        FOR i = 0, nebin - 1 DO BEGIN
            liveTime[elookup[eidx[i]]: elookup[eidx[i] + 1] - 1] = 9.99E26
        ENDFOR
    ENDELSE

    self->setEnergyLookup, lookupSave   ;== Restore previous lookup
    
    ;== Read DRM
        
    done = 0
    prevResp = self.Response->Filename()

    IF (prevResp EQ '' OR prevResp EQ 'none') THEN BEGIN   ; We don't have a DRM for this detector yet:
        ;== 07/11/07 RDP: Added code to automatically read in associated RSP files:
;        respFileName = '' 
        IF (NOT OBJ_ISA(self.Reader, "BFITSReader")) THEN BEGIN 
            prevResp = STRLOWCASE(STRTRIM(self.Reader->getRMFFile(), 2)) 
			self.Response->setFilename, prevResp
			self.Response->read, DATAREADER = self.Reader, ERROR = error
             IF (error) THEN BEGIN
                 self->setStatus, $
                    'Failed to read response file: ' + $
                     self.Response->filename (), 5, /REVERT	
             ENDIF ELSE BEGIN
                ;self->clearStatus 
                self->showSpan
                done = 1	
             ENDELSE
		ENDIF
		
	    WHILE (NOT done) DO BEGIN
	        
             path = self.Reader->filename (/PATH)
             root = self.Reader->filename (/ROOT)
			
             f = path + root + '.rsp'
             c = FINDFILE (f, COUNT = cnt)
;             IF (CNT NE 0) THEN BEGIN 
;                self.Response->setFilename, f, ERROR = error ;/INTERACTIVE, $ 
;                    ;/MUST_EXIST, FILTER = ['*.rmf','*.rsp','*.RMF','*.RSP'], DIALOG_PARENT = self.widgetID.top, $ 
;                    ;TITLE = 'Select Response File for Reading', ERROR = error  
;             ENDIF ELSE BEGIN 
			myPath = !RM_PARAMS.lastpath
			; 12/22/09 RDP CCR#223: Added filter for RSP type II files:
			self.Response->setFilename, f, /INTERACTIVE, $
				/MUST_EXIST, FILTER = ['*.rsp','*.rsp2','*.drm','*.rmf','*.RSP','*.DRM','*.RMF'], $
				DIALOG_PARENT = self.widgetID.top, $
				TITLE = 'Select Response File for Dataset "' + root + '.fits"', $
				PATH = myPath, ERROR = error 
			!RM_PARAMS.lastpath = self.Response->filename (/PATH)
;             ENDELSE

             IF (error) THEN BEGIN
                 cancel = 1
                 RETURN    
             ENDIF
             
             ; Check to see if the response reader is valid for the type of matrix:
             cacheResp = self.Response->Filename()
             ext = STRLOWCASE(self.Response->filename (/EXTENSION))
             ;PRINT, 'Found extension: ' + ext
             ; 12/22/09 RDP CCR#223: Added filter for RSP type II files:
             IF (OBJ_ISA(self.Response, "BFITSResponse")) AND ((ext EQ '.rmf') OR (ext EQ '.rsp') OR (ext EQ '.rsp2')) THEN BEGIN
             	; We had better change response readers!
             	OBJ_DESTROY, self.Response
             	self.Response = self.Detector->switchResponse('rmfResponse', cacheResp)
             ENDIF

             self.Response->read, DATAREADER = self.Reader, ERROR = error
             IF (error) THEN BEGIN

                 self->setStatus, $
                    'Failed to read response file: ' + $
                     self.Response->filename (), 5, /REVERT
	
             ENDIF ELSE BEGIN
	           
                ;self->clearStatus 
                self->showSpan
                done = 1
	
             ENDELSE
             self.RMFdirty = 1
	           
         ENDWHILE
    ENDIF

    resp = self.Response->response ()
	chan_width = resp.chan_width

    ;== Compute net (background subtracted) count rate and errors
    
    NETCRATE, $
        obsCrate, backCrate, backCsig, liveTime, resp.chan_width, $
        netCrate, netCsig
        
    ;== Try and fix up obsCrate:
    obsCrate = obsCrate / chan_width   
    backCrate = backCrate * width   
    backCrate = backCrate / chan_width   
    backCsig = backCsig * width   
    backCsig = backCsig / chan_width   

	;== Now fix up eInt: (03/30/2010 RDP: chan_energy is the midpoint!)
	eInt[0] = (resp.chan_energy)[fitChannels[0]] - (resp.chan_width)[fitChannels[0]] / 2.
	eInt[1] = (resp.chan_energy)[fitChannels[1]] + (resp.chan_width)[fitChannels[1]] / 2.

    filename = self.Reader->filename ()
    IF (self.Fitter->inList (filename)) THEN BEGIN ;== We have to delete the old data first:

       self.Fitter->deleteDetector, filename          

    ENDIF    
    
    ;== Add this detector to the Mfit list
    self.Fitter->addDetector, $          
           self, tInt, eInt, fitChannels, obsCrate, backCrate, backCsig, liveTime

END


;------------------------------------------------------------------------------
; Rebin 
;------------------------------------------------------------------------------
PRO PhaDisplay::rebin, $
    SELECTIONS     = selections, $
    FULLRESOLUTION = fullResolution, $
    SINGLEINTERVAL = singleInterval, $
    HAVEINTERVAL   = haveInterval,   $
    USERINTERVAL   = userInterval,   $
    BYHALF         = byHalf,         $
    REFINESINGLE   = refineSingle,   $
    BINBYSNR       = binBySNR

    ; COMPILE_OPT HIDDEN

    CASE (STRUPCASE (self.domain)) OF

        'TIME'   : BEGIN
            obj   = self.Lightcurve
            data  = obj->combinedTimes ()
            other = self.Spectrum
            thresholds = other->combinedThresholds ()
            END
            
        'ENERGY' : BEGIN
            obj   = self.Spectrum
            data  = obj->combinedThresholds ()
            other = self.LightCurve
            thresholds = other->combinedTimes ()
            END
            
         ELSE: MESSAGE, 'Internal error: unknown domain: ' + self.domain

    ENDCASE

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
                   obj->rebin, INTERVAL = interval, NBINS = nBins
               ENDIF ELSE BEGIN
                   obj->rebin, INTERVAL = interval
               ENDELSE
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
            	KEYWORD_SET (byHalf)         : obj->rebin, SINGLEBIN = singleBin, /BYHALF
            	KEYWORD_SET (refineSingle)   : obj->rebin, SINGLEBIN = singleBin, /REFINESINGLE
    	    END  
    
    	    CASE (STRUPCASE (self.domain)) OF
    
            	'TIME'   : BEGIN
            	    lookup = obj->timeLookup()  ;== obj->rebin has modified its own copy of the lookup
            	    self->setTimeLookup, lookup
            	    END
    
            	'ENERGY' : BEGIN
            	    lookup = obj->energyLookup()
            	    self->setEnergyLookup, lookup
            	    END
    
            	 ELSE: MESSAGE, 'Internal error: unknown domain: ' + self.domain
    
    	    ENDCASE
    
    	    self->integrateAndCombine
    	    IF (self.Background->haveModel ()) THEN BEGIN
            	self->fitBackground
            	self->integrateBackground, /TIME
            	self->integrateBackground, /ENERGY
    	    ENDIF
	    ENDIF

	    self->plot, _EXTRA = *self.PlotOptions

       ENDREP UNTIL (NOT haveSelection)
       self.dirty = 1   ;== Binning has changed
       RETURN
    ENDIF
        
    CASE (1) OF
    
        KEYWORD_SET (selections)     : obj->rebin, /SELECTIONS
        KEYWORD_SET (fullResolution) : obj->rebin, /FULLRESOLUTION
        ;KEYWORD_SET (singleInterval) : obj->rebin, INTERVAL = interval
        KEYWORD_SET (haveInterval)   : obj->rebin, INTERVAL = haveInterval
        ;KEYWORD_SET (userInterval)   : obj->rebin, INTERVAL = interval, NBINS = nbins
;        KEYWORD_SET (byHalf)         : obj->rebin, SINGLEBIN = singleBin, /BYHALF
;        KEYWORD_SET (refineSingle)   : obj->rebin, SINGLEBIN = singleBin, /REFINESINGLE
        KEYWORD_SET (binBySNR)       : BEGIN
             obj->binBySignificance, self.Background, thresholds, targetSNR, $
	          ERROR = error
			 IF error THEN BEGIN
				self->setStatus, 'No background model exists!', 10, /REVERT
			RETURN
	     ENDIF
        END
                    
        ELSE: 

    ENDCASE

    CASE (STRUPCASE (self.domain)) OF

        'TIME'   : BEGIN
            lookup = obj->timeLookup()  ;== obj->rebin has modified its own copy of the lookup
            self->setTimeLookup, lookup
            ;other->integrateTime
            END
            
        'ENERGY' : BEGIN
            lookup = obj->energyLookup()
            self->setEnergyLookup, lookup
            ;other->integrateEnergy
            END
            
         ELSE: MESSAGE, 'Internal error: unknown domain: ' + self.domain

    ENDCASE

    self->integrateAndCombine
    IF (self.Background->haveModel ()) THEN BEGIN
        self->fitBackground
        self->integrateBackground, /TIME
        self->integrateBackground, /ENERGY
    ENDIF
    
    self.dirty = 1   ;== Binning has changed
    self->plot, _EXTRA = *self.PlotOptions

END


;------------------------------------------------------------------------------
; Read/write lookup file
;------------------------------------------------------------------------------
PRO PhaDisplay::lookup, READ = read, WRITE = write, ERASE = erase, _EXTRA = extra

    ; COMPILE_OPT HIDDEN
                
    IF (KEYWORD_SET (read)) THEN BEGIN
       
       RESTORE, !MFIT.script_file                                                                  ; AMG
       IF script THEN self->readLookup, ERROR=error, FILENAME=(f=luFile[phanum-1]), /AUTO ELSE $   ;
       self->readLookup, ERROR = error, FILENAME = f
       IF (NOT error) THEN BEGIN
          self->setStatus, 'Read lookup file: ' + f ;, 5, /REVERT
          self->integrateAndCombine
          self->plot, _EXTRA = *self.PlotOptions
          self->showSpan
       ENDIF ELSE BEGIN
          self->setStatus, 'Failed to read lookup file: ' + f, 5, /REVERT
       ENDELSE
       
    ENDIF 

    IF (KEYWORD_SET (write)) THEN BEGIN
                
       ok = self->writeLookup (FILENAME = f, _EXTRA = extra)
       IF (ok) THEN BEGIN
          self->setStatus, 'Wrote lookup file: ' + f, 5, /REVERT
       ENDIF ELSE BEGIN
          IF (f NE '') THEN $
             self->setStatus, 'Failed to write lookup file: ' + f, 5, /REVERT
       ENDELSE
       
    ENDIF   

    self.dirty = 0   ;== Selections have been read or written, start clean again
    
    IF (KEYWORD_SET (erase)) THEN BEGIN

       self->setEnergyLookup, /INITIALIZE
       self->setTimeSpan,     /INITIALIZE
       self->setEnergySpan,   /INITIALIZE

       self.Lightcurve->rebin, /FULLRESOLUTION
       self->setTimeLookup,   /INITIALIZE
       self.Lightcurve->update, /FULL
       self.Lightcurve->combine, /FULL_RESET
       self.Spectrum->rebin, /FULLRESOLUTION
       self.Spectrum->update, /FULL
       self.Spectrum->combine, /FULL_RESET
       self.haveDefaultRange   = 0 

       self.Background->erase
              
       self.dirty = 1   ;== Selections have changed (obviously!)
       self.myLookup = ''
       self->plot, _EXTRA = *self.PlotOptions
       self->showSpan

    END
    
END


;------------------------------------------------------------------------------
; Save lookup file
;------------------------------------------------------------------------------
FUNCTION PhaDisplay::writeLookup, AUTO = auto, FILENAME = filename, FINESTAT = fineStat
    
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

    default_el = self.lightCurve->energyLookup ()
    diff_el = MAX (default_el[1:*] - default_el)
    
    default_tl = self.lightCurve->timeLookup ()
    diff_tl = MAX (default_tl[1:*] - default_tl)
    
    energySpan = self.lightCurve->energySpan ()
    n_espan = N_ELEMENTS (energySpan)
    e_span = FLTARR (n_espan)
    e_span[0:(n_espan / 2) - 1] = energySpan[*, 0]
    e_span[(n_espan / 2): *]    = energySpan[*, 1]

    timeSpan = self.lightCurve->timeSpan ()
    n_tspan = N_ELEMENTS (timeSpan)
    t_span = FLTARR (n_tspan)
    t_span[0:(n_tspan / 2) - 1] = timeSpan[*, 0]
    t_span[(n_tspan / 2): *]    = timeSpan[*, 1]

    IF (self.Background->haveModel ()) THEN BEGIN

       backSpan = self.Background->backSpan ()

       ; Omit convex hull (backward compatibility with WINGSPAN lookup files)

       n_bspan = N_ELEMENTS (backSpan) - 2 
       b_span = FLTARR (n_bspan)
       b_span[0:(n_bspan / 2) - 1] = backSpan[1:*, 0]
       b_span[(n_bspan / 2): *]    = backSpan[1:*, 1]

    ENDIF ELSE BEGIN

       n_bspan = 0

    ENDELSE
    
    
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
       
       logValues = self.spectrum->logStatus()
       PRINTF, FL, (logValues[0] EQ 1) ? 'LOG' : 'LIN'
       PRINTF, FL, (logValues[1] EQ 1) ? 'LOG' : 'LIN'
       
       histRange = self.Lightcurve->range ()
       specRange = self.Spectrum->range ()
       
       PRINTF, FL, histRange[0, *]
       PRINTF, FL, histRange[1, *]
       PRINTF, FL, specRange[0, *]
       PRINTF, FL, specRange[1, *]

       IF (n_bspan NE 0) THEN $
          PRINTF, FL, self.Background->order ()

       IF KEYWORD_SET(FINESTAT) THEN PRINTF, FL, fineStat
       
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


;------------------------------------------------------------------------------
; Zoom the plotted event
;------------------------------------------------------------------------------
PRO PhaDisplay::zoom, XZOOM = xZoom, YZOOM = yZoom, $
    DEFAULTRANGE = defaultRange, SELECTION = selection, $
    FULLSCREEN = fullscreen

    ; COMPILE_OPT HIDDEN

    IF (KEYWORD_SET (fullscreen)) THEN BEGIN
       self->fullscreen
       RETURN
    ENDIF   
    
    ;== Need to set all the !plot info, especially ![xy].crange for AXISRANGE
    self->plot, _EXTRA = *self.PlotOptions

    CASE (STRUPCASE (self.domain)) OF

         'TIME': BEGIN
             
             ;== First pass: set default axis ranges
             IF (NOT self.haveDefaultRange) THEN BEGIN
                self.haveDefaultRange   = 1  
                self.defaultRange = self.lightCurve->default ()
                self.lightCurve->setRange, self.lightCurve->default ()
             ENDIF
             
             self.currentRange = self.lightCurve->range ()

             IF (KEYWORD_SET (defaultRange)) THEN BEGIN
                
                self.Lightcurve->resetDefaultRange
                self->plot, _EXTRA = *self.PlotOptions                    
                
             ENDIF ELSE $
             IF (KEYWORD_SET (selection)) THEN BEGIN
                
                ; Get current plot range
                
                range = self.Lightcurve->range ()
                
                ; Get new zoomed xrange
                
                xRange = REFORM ((self.lightCurve->timeSpan ())[0, *])
                
                ; Snap out to nearest bin edge
                
                idx = INTARR (2)
                thresholds = self.Lightcurve->combinedTimes ()
                range[0, *] = NEAREST_EDGE (thresholds, xRange, idx)

                ; Get y-axis range
                                
                hist    = self.Lightcurve->combinedHist ()
                histErr = self.Lightcurve->combinedHistErr ()
                yRange = [ MIN (hist[idx[0]:idx[1]] - histErr[idx[0]:idx[1]]), $
                           MAX (hist[idx[0]:idx[1]] + histErr[idx[0]:idx[1]])]
                                           
                range[1, *] = yRange

                ; Store the new plot range, and update plot
                                 
                self.Lightcurve->setRange, range
                self->plot, _EXTRA = *self.PlotOptions                    
                             
             ENDIF ELSE BEGIN   
                
                self->DISPLAY::zoom, self.widgetID.draw, $
                    X = KEYWORD_SET (xZoom), $
                    Y = KEYWORD_SET (yZoom)

                xRange = AXISRANGE (/XAXIS)
                yRange = AXISRANGE (/YAXIS)
                range  = TRANSPOSE ([[xRange], [yRange]])
                self.Lightcurve->setRange, range
             
             ENDELSE
             END

         'ENERGY': BEGIN
             
             ;== First pass: set default axis ranges
             IF (NOT self.haveDefaultRange) THEN BEGIN
                self.haveDefaultRange   = 1  
                self.defaultRange = self.spectrum->default ()
                self.spectrum->setRange, self.spectrum->default ()
             ENDIF
             
             self.currentRange = self.spectrum->range ()

             IF (KEYWORD_SET (defaultRange)) THEN BEGIN
             
                self.Spectrum->resetDefaultRange
                self->plot, _EXTRA = *self.PlotOptions                    
                
             ENDIF ELSE $
             IF (KEYWORD_SET (selection)) THEN BEGIN
                    
                range  = self.Spectrum->range ()
                xRange = REFORM ((self.Spectrum->energySpan ())[0, *])
                
                idx = INTARR (2)
                thresholds = self.Spectrum->combinedThresholds ()
                range[0, *] = NEAREST_EDGE (thresholds, xRange, idx)
                                
                spec    = self.Spectrum->combinedSpec ()
                specErr = self.Spectrum->combinedSpecErr ()
                
                lo = spec[idx[0]:idx[1]] - specErr[idx[0]:idx[1]]
                hi = spec[idx[0]:idx[1]] + specErr[idx[0]:idx[1]]
                                
                yRange = [ MIN (lo[WHERE (lo GT 0)]), MAX (hi[WHERE (hi GT 0)])]
                range[1, *] = yRange
                                 
                self.Spectrum->setRange, range
                self->plot, _EXTRA = *self.PlotOptions                    

             ENDIF ELSE BEGIN   
                
                self->DISPLAY::zoom, self.widgetID.draw, $
                    X = KEYWORD_SET (xZoom), $
                    Y = KEYWORD_SET (yZoom)

                xRange = AXISRANGE (/XAXIS)
                yRange = AXISRANGE (/YAXIS)
                range  = TRANSPOSE ([[xRange], [yRange]])
                self.Spectrum->setRange, range

             ENDELSE 
             END

         ELSE: MESSAGE, 'Internal error: unknown domain: ' + self.domain
 
    ENDCASE
    
    self.dirty = 1   ;== Views have changed

END


; ----------------------------------------------------------------------------
FUNCTION PhaDisplay::detector & RETURN, self.Detector & END



; ----------------------------------------------------------------------------
; Set lookup arrays. 
; ----------------------------------------------------------------------------
PRO PhaDisplay::setTimeLookup, timeLookup, INITIALIZE = initialize

    self.lightCurve->setTimeLookup, timeLookup, INITIALIZE = initialize
    self.spectrum->setTimeLookup, timeLookup, INITIALIZE = initialize
    self.background->setTimeLookup, timeLookup, INITIALIZE = initialize

END


PRO PhaDisplay::setEnergyLookup, energyLookup, INITIALIZE = initialize
    
    self.lightCurve->setEnergyLookup, energyLookup, INITIALIZE = initialize
    self.spectrum->setEnergyLookup, energyLookup, INITIALIZE = initialize
    self.background->setEnergyLookup, energyLookup, INITIALIZE = initialize

END


; ----------------------------------------------------------------------------
; Set selection span arrays. 
; ----------------------------------------------------------------------------
PRO PhaDisplay::setTimeSpan, timeSpan, _EXTRA = extra ; INITIALIZE = initialize

    self.lightCurve->setTimeSpan, timeSpan, _EXTRA = extra
    self.spectrum->setTimeSpan, timeSpan, _EXTRA = extra

END
    
PRO PhaDisplay::setEnergySpan, energySpan, _EXTRA = extra ;INITIALIZE = initialize
    
    self.lightCurve->setEnergySpan, energySpan, _EXTRA = extra
    self.spectrum->setEnergySpan, energySpan, _EXTRA = extra

END


; ----------------------------------------------------------------------------
; Set the Response Matrix filename in the file associated with a PHA Reader. 
; ----------------------------------------------------------------------------
PRO PhaDisplay::setRespName, rmfFile

    ;== The other datareaders don't have this method defined:
	;IF (OBJ_ISA(self.Reader, "PHAReader") OR OBJ_ISA(self.Reader, "PHAIIReader")) THEN BEGIN
		;respFileName = self.Reader->getRMFFile()
		;IF (respFileName NE rmfFile) THEN RETURN ; We have already set it
		
		myPHAFile = self.Reader->filename ()
		;Loop through extensions until the right one is found:
		ebExt = 0
		found = 0
		REPEAT BEGIN
			ebExt += 1
			myHDR = HEADFITS (myPHAFile, EXTEN = ebExt)
			ebExtName = STRTRIM(SXPAR (myHDR, 'EXTNAME'), 2)
			IF (ebExtName EQ 'SPECTRUM') OR (ebExtName EQ 'EVENTS') THEN found = 1
		ENDREP UNTIL found
		;== Open the SPECTRUM extension and add the response filename to the header
		;myHDR = HEADFITS(myPHAFile, EXTEN = ebExt)
		SXADDPAR, myHDR, 'RESPFILE', rmfFile
		fits_add_checksum, myHDR
		MODFITS, myPHAFile, 0, myHDR, EXTEN = ebExt
	;ENDIF

END


; ----------------------------------------------------------------------------
; Define the subclass, inheriting Display and PHA
; ----------------------------------------------------------------------------
PRO PhaDisplay__define

    obj = { PHADISPLAY, INHERITS DISPLAY, $
         
        ;== Detector object reference
        
        Reader     : OBJ_NEW (), $
        Detector   : OBJ_NEW (), $
        Lightcurve : OBJ_NEW (), $
        Spectrum   : OBJ_NEW (), $
        Background : OBJ_NEW (), $
        Response   : OBJ_NEW (), $
        Fitter     : OBJ_NEW (), $
        ;BackSelect : OBJ_NEW (), $
        
        ;== Event ID String ==
        eventID	: '', $
	
        ;== Lookup filename
	
        myLookup   : '', $
                                   
        ;== 'TIME' or 'ENERGY'                 
       
        domain : '', $  
        
        ;== Maintain a record of whether the lookup has changed
        
        dirty  :  0, $
        RMFdirty  :  0, $
        
        ;== CCR #120: Keep track of the random seed for batch fitting synthetic spectra:
        rndseed   :  0L, $
        
        ;== Keep track of the log-linear axes choice for plotting:
        
        logChoiceID  : 0L $

    }

END
