; ----------------------------------------------------------------------------
;+
; NAME:
;
;     TRIGDisplay (OBJECT)
;
; PURPOSE:
;
;     An object for interactive manipulation of PHA data.
;
; CALLING SEQUENCE:
;
;     obj = OBJ_NEW ('TRIGDisplay', filename)
;
; INPUTS:
;
;     filename : A STRING containing a FITS filename
;
; KEYWORDS:
;
;     MAP : Set this keyword to map the widget to the display.
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
;     fgeom_quaternion__define.pro
;
;
; METHODS:     
;
; MODIFICATION HISTORY:
;
;     Based upon PHADisplay: written, 1999 Fall, Robert.Mallozzi@msfc.nasa.gov
;
;     07/11/07 RDP: Added code to automatically read in RSP files associated with 
;     PHA and PHAII files (filename in RESPFILE keyword).
;
;     04/17/09 VLC: 
;     Change to Data Structure:
;		New no change here, but the superclass PHADISPLAY
;		has the new field 'eventID' of type STRING. Added
;		code here to handle this new data.  See
;		'phadisplay__define.pro'
;     Changes to methods
;		init	: Added lines to parse the 'bn' number,
;					(ie, YYMMDDxxx) from the filename
;					and store it (self.eventID)
;		writeTCAT: New user-interface dialogue,
;					DIALOG_COMBOINPUT.
;				Changed TCAT keywords:
;					CLASS = FSSC classification
;					OBJ_CLASS = GBM classification
;					OBJECT = GBM Class + YYMMDDxxx
;
;     09/22/09 VLC : ÊChanged writeTCAT() method to use version 2.0 of dialog_comboinput.pro,
;				and the new class ADT_DHASH, which now handles the mapping from
;				GBM to FSSC classes.
;
;
;	  12/23/09 VLC : Many changes to handling localizations.
;					Locations are all stored in a list, and can be appended by calling DOL or 'Create TCAT'.
;					1. Fixed error in WriteTCAT() causing the dialog box to give an error.  Error
;						was caused by a change to the class to allow cached locations (rmfit 3.2).
;					2. Added two new class methods to encapsulate the addition or mod of a localization.
;						-updateLocation(RA,Dec,Err) adds a new Ra,Dec,Err onto the back of the list.
;						-updateMission() adds a new mission, or updates the current one (if the user selects
;									'Input Source Class').
;					3. Changed write_tcat and sourceLocation to call these methods. 'Create TCAT' locations now cached.
;					4. Changed eventHandler() - 
;						i. new behavior when 'Sky Map RA,Dec' pressed:
;							Previously it did not cal plot if a DOL localization was not performed.  Now calls plot
;							if any location is available (including FSW).
;						ii. new behavior for 'Clear Previous': Keeps the last DOL location, to avoid losing
;							parameters associated with the contour map.  If DOL has been called, that becomes
;							the first element of the new list.  If not, the list is emptied.  This is mainly useful
;							to retroactively get rid of FSW and GSW locations without losing the last DOL location.
;
;					5. Added lastDOLitem to store the index of the localization which corresponds to the last run of the DOL,
;						to match chi2 contour map to the right element.
;						'Plot' uses it to make sure it doesn't plot an error radius on the contour map.
;
;					***
;					New locations are not taken from the widget labels now, but directly as float parameters
;					to updateLocation(). The result is that the list of locations is the same size as the number
;					of F&GSW locations + DOL localizations + 'Create TCAT' calls, where before it had one less element.
;					In the new design, the 'current' or 'most recent' location corresponds to the last element of the
;					RA,Dec,Err,Mission lists.  In the old (v < 3.2), it was the label values.
;					***
;
;					*** 
;					Changes 2-4 allow for overplotting of locations.  To overplot, the user now just calls Create TCAT,
;					but does not need to write the tcat. However, TCATs are always written with
;					the most recently added parameters.
;					***
;
;		01/27/10 VLC :
;					Changes required by CCRs for GBM catalog production.
;					Support for the following keywords:
;					THETA   - zenith angle of Ra,Dec at T0
;					PHI     - azimuth angle (CCW from +x when looking down z-axis towards origin, ie -z direction)
;					LOC_VER - Version string of localizing software ( DOL )
;					LOC_ENRG - Energy range used for localization
;					GCN_FLAG - Was this file used for GCN Notice generation? = 'No' always in this context
;					
;					::getlocation() - Code was added to get the DOL version and localization energy range.
;					::WriteTCAT() - code added to get T0 attitude and calculate SC coordinates using fgeom object.
;
;
;					LOC_VER gets redefined in SourceLocation(), by reading the DOL's output token.
;					LOC_ENRG is also defined in this method, and its value is completely determined by RMFIT's state,
;					not the DOL return.  If the 'Hard' radio button is selected, the DOL is told to use channel 3+4
;					so LOC_ENRG = '(50, 300)' (the nominal keV bounds).  If the 'Soft' button is selected, the DOL will use
;					channel 1+2, so LOC_ENRG = '(10, 50)'.
;
;					If the user doesn't do a HITL, these keywords have either of two values. If the user chooses to 'Create TCAT',
;					supplying a new location from the keyboard, LOC_VER = 'N/A', and LOC_ENRG = 'N/A'.  This change occurs every time
;					'Create TCAT' is called.
;					However, if the user does not at any point supply a new location, whether through the DOL or manual input,
;					the values will be LOC_VER = 'Auto', and LOC_ENRG = '(50, 300)' (defined in the constructor), because in this case the
;					printed RA,Dec will be from a trigdat file, implying it was either a FSW or GSW automatic location.
; ----------------------------------------------------------------------------



; ----------------------------------------------------------------------------
; Constructor
; ----------------------------------------------------------------------------
FUNCTION TRIGDisplay::init, filename, MAP = map, LOOKUP = lookup, _EXTRA = extra
        
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
        self.Lightcurve = OBJ_NEW ('Lightcurve', self.Reader)
        self.Spectrum   = OBJ_NEW ('Spectrum',   self.Reader)    
        self.Background = OBJ_NEW ('Background', self.Reader)
        
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
    
    ;== Get TRIGDAT bnYYMMDDxxx string and set as the self.eventID field ==
    names = STREGEX(filename,'_bn(.*)_',/SUBEXPR,/EXTRACT)
    self->setEventID, EVENTAG=names[1]

    ;== Set default plotting colors
    
    self->setColors, /INITIALIZE

    ;== Build the interface
    
    self->buildGUI, MAP = map

    ;== Set default domain
     
    self.domain = 'TIME'

    ;== Try to read a lookup file
    self.fineTime = 0
    
    IF (lookup) THEN BEGIN
       ;== Assume no fine time lookup to start with:
       fineStat = 0
       self->readLookup, /AUTO, /SILENT, FILENAME = f, FINESTAT = fineStat, ERROR = error, _EXTRA = extra
       IF (NOT error) THEN BEGIN
          self.fineTime = fineStat
			myDetString = self.Reader->getDetName()
			self->changeDetectors, myDetString, /REPLOT  
          self->integrateAndCombine
          self->setStatus, 'Read lookup file: ' + f ;, 5, /REVERT
       ENDIF ELSE BEGIN        ;== No lookup file, must set plot ranges to the default
          self.lightCurve->resetDefaultRange
          self.spectrum->resetDefaultRange
          self.spectrum->setLogStatus, [1, 1]
       ENDELSE

    ENDIF 
    
    ;== Start clean
    
    self.dirty = 0
    
    ;== Initialize the cycle:
    
    self.currentDet = 0
    
    ;== There hasn't been a *DOL* location done yet:
    self.lastDOLitem = -1
    self.currentLoc = 0
	self.locenergy = '(50, 300)'
    self.locversion = 'Auto'
    ;== Plot lightcurve
    
    self->plot, _EXTRA = *self.PlotOptions
    self->showSpan

    RETURN, 1

END

   
; ----------------------------------------------------------------------------
; Destructor
; ----------------------------------------------------------------------------
PRO TRIGDisplay::cleanup
    
    PTR_FREE, self.chisqMap
    PTR_FREE, self.externalRA
    PTR_FREE, self.externalDec
    PTR_FREE, self.externalErr
    PTR_FREE, self.externalMission

    Detector = self->detector()
    Reader   = Detector->dataReader ()
    filename = Reader->filename (/BASE)
    
    IF self.dirty THEN BEGIN
        d = DIALOG_MESSAGE (/QUESTION, 'Save changed lookup parameters for dataset: ' +$
            STRTRIM (filename, 2) + '?')
        IF (d EQ 'Yes') THEN self->lookup, FINESTAT = self.fineTime, /WRITE
    ENDIF
    
    ;== Save the color info
    
    !RM_PARAMS.colors = self.colors
    
    OBJ_DESTROY, self.Detector
    OBJ_DESTROY, self.Lightcurve   
    OBJ_DESTROY, self.Spectrum     
    OBJ_DESTROY, self.Background   
           
    self->Display::cleanup

END


; ----------------------------------------------------------------------------
; Build the display widget
; ----------------------------------------------------------------------------
PRO TRIGDisplay::buildGUI, MAP = map

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
            id = menu->add (subMenu, VALUE = 'Data Table  (EXT 5)')
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

    menu = self->addMenu (VALUE = 'Help', /HELP)
        id = menu->add (VALUE = 'Full Help')
        mainMenu = menu->add (VALUE = 'Main Menu Items', /Menu)
              id = menu->add (mainMenu, VALUE = 'File Menu')
              id = menu->add (mainMenu, VALUE = 'Options Menu')
        otherMenu = menu->add (VALUE = 'Other Menu Items', /Menu)
              id = menu->add (otherMenu, VALUE = 'Zoom Items')
              id = menu->add (otherMenu, VALUE = 'Rebin Items')
              id = menu->add (otherMenu, VALUE = 'Selection Items')
              id = menu->add (otherMenu, VALUE = 'TRIGDAT Items')
              id = menu->add (otherMenu, VALUE = 'Background Fitting')
        ;id = menu->add (VALUE = 'Spectral Fitting')
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

    menu = self->addMenuButton (VALUE = 'Selection:', /TEAROFF)
;        id = menu->add (VALUE = 'Fit Background')
        id = menu->add (VALUE = 'Source Interactive')
        id = menu->add (VALUE = 'Source by Signal to Noise')
        id = menu->add (VALUE = 'Toggle Fine Time Resolution', /SEPARATOR)
        id = menu->add (VALUE = 'Change Detector Selection')
        id = menu->add (VALUE = 'Cycle Detector Selection')
        id = menu->add (VALUE = 'Restart Detector Cycle')
        id = menu->add (VALUE = 'Revert Detector Selection')
        id = menu->add (VALUE = '< Shift Selection', /SEPARATOR)
        id = menu->add (VALUE = '> Shift Selection')
        id = menu->add (VALUE = '< Left Selection')
        id = menu->add (VALUE = '> Left Selection')
        id = menu->add (VALUE = '< Right Selection')
        id = menu->add (VALUE = '> Right Selection')

;    id = self->addButton ('Select Source')

;    menu = self->addMenuButton (VALUE = 'Localization:', /TEAROFF) 
;        id = menu->add (VALUE = 'Source Location') 
;        id = menu->add (VALUE = 'Write TCAT File') 
;        id = menu->add (VALUE = 'Input Source Class') 
;        id = menu->add (VALUE = 'Create TCAT File') 
;        id = menu->add (VALUE = 'Sky Map: Az-Zen') 
;        id = menu->add (VALUE = 'Sky Map: RA-Dec') 
;        id = menu->add (VALUE = 'Hide Previous on Plot', /SEPARATOR) 
;        self.locChoice = 0 
;        id = menu->add (VALUE = 'Clear Previous Locations (keeps last DOL)') 
;     
;;    id = self->addButton ('Source Location') 
;	hardChoice = ['Hard Table', 'Soft Table'] 
;	self.hardChoiceID = CW_BGROUP (self.widgetID.buttonBase, hardChoice, /ROW, $ 
;            /EXCLUSIVE, SET_VALUE = 0) 

	logChoice = ['X Log', 'Y Log']
	self.logChoiceID = CW_BGROUP (self.widgetID.buttonBase, logChoice, /ROW, $
            /NONEXCLUSIVE, SET_VALUE = [0,0])
               
    header = self.Reader->header (/POINTER)
    notes  = (*header).notes
    nNotes = N_ELEMENTS (notes)
    
    self.sourceEvntID = self->addLabel (notes[0], /ALIGN_LEFT)
    FOR i = 1, 2 DO $
        id = self->addLabel (notes[i], /ALIGN_LEFT)
        
    self.detNamesID = self->addLabel (notes[3], /ALIGN_LEFT)
    
    FOR i = 4, 5 DO $
        id = self->addLabel (notes[i], /ALIGN_LEFT)
        
    PTR_FREE, self.externalRA
    PTR_FREE, self.externalDec
    PTR_FREE, self.externalErr
    PTR_FREE, self.externalMission
    myLocs = self.Reader->getLocations()
    
    self.sourceRA_ID = self->addLabel (notes[6], /ALIGN_LEFT)
	result1 = STRSPLIT(notes[6], ":",  /EXTRACT)
    self.sourceDecID = self->addLabel (notes[7], /ALIGN_LEFT)
	result2 = STRSPLIT(notes[7], ":",  /EXTRACT)
    self.sourceErrID = self->addLabel (notes[8], /ALIGN_LEFT)
	result3 = STRSPLIT(notes[8], ":",  /EXTRACT)

    IF (PTR_VALID(myLocs)) THEN BEGIN
    	numLocs = N_ELEMENTS((*myLocs).ra)
		self.externalRA = PTR_NEW((*myLocs).ra)
		self.externalDec = PTR_NEW((*myLocs).dec)
		self.externalErr = PTR_NEW((*myLocs).staterr)
		tempLabels = STRARR(numLocs)
		FOR ii = 0, numLocs - 1 DO BEGIN
			tempLabels[ii] = " FSW #" + STRTRIM(ii + 1 ,2)
		ENDFOR
		self.externalMission = PTR_NEW(tempLabels)
    ENDIF ELSE BEGIN
		self.externalRA = PTR_NEW([FLOAT(result1[1])])
		self.externalDec = PTR_NEW([FLOAT(result2[1])])
		self.externalErr = PTR_NEW([FLOAT(result3[1])])
		self.externalMission = PTR_NEW(["  GBM FSW"])
   ENDELSE

    ;== Add a text message in the status bar
    
    self->setStatus, 'Display initialized' ;, 5
     
    ;== Now that the custom widget is build, map it to the screen
       
    IF (map) THEN self->map
        
END


; ----------------------------------------------------------------------------
; Override the widget event handler
; ----------------------------------------------------------------------------
PRO TRIGDisplay::eventHandler, event

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
                'Screenshot': BEGIN                                                                ; AMG
                                                                                                ;
                             file=DIALOG_PICKFILE(DEFAULT_EXTENSION='png',  $                   ;
                                                  PATH=!RM_PARAMS.lastPath, $                   ;
                                                  FILE='location.png',      $                   ;
                                                  /WRITE)                                       ;
                             IF file EQ '' THEN RETURN                                          ;
                             image=TVRD(/TRUE)                                                  ;
                             TVLCT,r,g,b,/GET                                                   ;
                             WRITE_PNG, file, image, r, g, b                                    ;
                                                                                                ;
                           END                                                                  ;
                           
                ;'Spacecraft Location' : self->showCGRO

                'Primary     (EXT 0)' : self.Reader->showHeader, 0
                'Calibration (EXT 1)' : self.Reader->showHeader, 1
                'Data Table  (EXT 5)' : self.Reader->showHeader, 2
                'Notes'               : self.Reader->showNotes
                 
                'Selection -> PHA'    : self->makePHAFile, /SELECTION
                'Background -> PHA'   : self->makePHAFile, /BACKGROUND
                'Export IDL Data'     : self->export
                ;RMK
                'Export ASCII Data'   : self->exportasc

                'Read Lookup'         : BEGIN
					self.domain = "TIME" 
                	self->lookup, /READ
                	END
                'Save Lookup'         : self->lookup, FINESTAT = self.fineTime, /WRITE
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
                    
                'Toggle Fine Time Resolution'  : BEGIN
                    self.fineTime = self.fineTime ? 0 : 1
					myDetString = self.Reader->getDetName()
                    self->changeDetectors, myDetString, /REPLOT, /INITIALIZE  
                    END
                'Change Detector Selection' : self->changeDetectors, /INTERACTIVE, /REPLOT            
                'Cycle Detector Selection'  : self->changeDetectors, /CYCLE, /REPLOT            
                'Restart Detector Cycle'    : self.currentDet = 0            
                'Revert Detector Selection' : BEGIN
                	self.Reader->read
                	myDetString = self.Reader->getDetName()
                    self->changeDetectors, myDetString, /REPLOT
                    END
                    
                'Source Location': self->sourceLocation
                'Write TCAT File': self->writeTCAT
                'Input Source Class': self->writeTCAT, /NEW_SOURCE
                'Create TCAT File': self->writeTCAT, /INTERACTIVE
                'Hide Previous on Plot' : BEGIN
                    self.locChoice = 1
                    WIDGET_CONTROL, event.id, SET_VALUE = 'Show Previous on Plot'
                    self->Plot, _EXTRA = *self.PlotOptions
                    END
                
                'Show Previous on Plot' : BEGIN
                    self.locChoice = 0
                    WIDGET_CONTROL, event.id, SET_VALUE = 'Hide Previous on Plot'
                    self->Plot, _EXTRA = *self.PlotOptions
                    END

                'Clear Previous Locations (keeps last DOL)' : BEGIN
                	IF self.haveLocation then begin
                		lastDol = self.lastDOLitem
                		ra = (*self.externalRA)[lastDol]
                		dec = (*self.externalDec)[lastDol]
                		err = (*self.externalErr)[lastDol]
                		ins = (*self.externalMission)[lastDol]
                		PTR_FREE, [self.externalRA, self.externalDec, self.externalErr, self.externalMission]
                		self->updateLocation, ra,dec,err,MISSION=ins
						self.currentLoc = 1
						self.lastDOLitem = 0
                	endif else begin
						PTR_FREE, self.externalRA
						PTR_FREE, self.externalDec
						PTR_FREE, self.externalErr
						PTR_FREE, self.externalMission
						self.externalRA       = PTR_NEW()
						self.externalDec      = PTR_NEW()
						self.externalErr      = PTR_NEW()
						self.externalMission  = PTR_NEW()
						self.currentLoc = 0
						self.lastDOLitem = -1
					endelse
                    self->Plot, _EXTRA = *self.PlotOptions
                    END

                'Sky Map: Az-Zen'  : IF (self.haveLocation) THEN BEGIN
					self.domain = "AZZEN" 
					self.haveDefaultRange   = 0  
					self->Plot, _EXTRA = *self.PlotOptions
					END

                'Sky Map: RA-Dec'  : IF PTR_VALID(self.externalRA) THEN BEGIN
					self.domain = "RADEC" 
					self.haveDefaultRange   = 0  
					self->Plot, _EXTRA = *self.PlotOptions
					END

                '< Shift Selection'  : BEGIN
					self.domain = "TIME" 
                    self->adjustSource, /LEFT,  /EXPAND
                    self->adjustSource, /RIGHT, /CONTRACT
                    self->showSpan
                    END
                     
                '> Shift Selection'  : BEGIN
					self.domain = "TIME" 
                    self->adjustSource, /RIGHT,  /EXPAND
                    self->adjustSource, /LEFT, /CONTRACT
                    self->showSpan
                    END
                     
                '< Left Selection'  : BEGIN
					self.domain = "TIME" 
                    self->adjustSource, /LEFT,  /EXPAND
                    self->showSpan
                    END
                     
                '> Left Selection'  : BEGIN
					self.domain = "TIME" 
                    self->adjustSource, /LEFT,  /CONTRACT
                    self->showSpan
                    END
                     
                '< Right Selection' : BEGIN
					self.domain = "TIME" 
                    self->adjustSource, /RIGHT, /CONTRACT
                    self->showSpan
                    END
                     
                '> Right Selection' : BEGIN
					self.domain = "TIME" 
                    self->adjustSource, /RIGHT, /EXPAND
                    self->showSpan
                    END

                'Fit Background' : BEGIN
                    IF (STRUPCASE (self.domain) NE 'TIME' OR STRUPCASE (self.domain) NE 'ENERGY') THEN $
                       self->togglePlot
                    self->disable
                    self->setStatus, $
                       'Select background interval(s).  ' + $
                       'Use margins for special functions.' 
                    self->fitBackground, /INTERACTIVE
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
                ; INFO
                ;---------------  
                
                'Full Help'          : self->info, /HELP
                'File Menu'          : XDISPLAYFILE, !MFIT.HELP_PATH + 'file.hlp',    DONE = 'Done'
                'Options Menu'       : XDISPLAYFILE, !MFIT.HELP_PATH + 'options.hlp', DONE = 'Done'
                'Zoom Items'         : XDISPLAYFILE, !MFIT.HELP_PATH + 'zoom.hlp',    DONE = 'Done'
                'Rebin Items'        : XDISPLAYFILE, !MFIT.HELP_PATH + 'rebin.hlp',   DONE = 'Done'
                'Selection Items'    : XDISPLAYFILE, !MFIT.HELP_PATH + 'select.hlp',  DONE = 'Done'
                'TRIGDAT Items'      : XDISPLAYFILE, !MFIT.HELP_PATH + 'trigdat.hlp', DONE = 'Done'
                'Background Fitting' : XDISPLAYFILE, !MFIT.HELP_PATH + 'backfit.hlp', DONE = 'Done'
                'About'              : self->info, /ABOUT                    

                ELSE: ;self->setStatus, 'Event not handled: ' + value, 2

            ENDCASE
            
            END ; WIDGET_BUTTON events
        

        'WIDGET_TEXT_CH': self->keyBindings, event.ch

        ELSE: BEGIN

			IF (event.id EQ self.logChoiceID) THEN BEGIN
				;== Handle the radio buttons; name is *not* returned in 'value'
				WIDGET_CONTROL, event.id, GET_VALUE = value
				self->changeLog, value
				ENDIF 
            
            END;self->setStatus, 'Event not handled.' , 2
                       
    ENDCASE

    self->enable      
END

 
; ----------------------------------------------------------------------------
; Handle keyboard events in the drawing area
; ----------------------------------------------------------------------------
PRO TRIGDisplay::keyBindings, char
    
    ; COMPILE_OPT HIDDEN

    setXRange = 0
    setYRange = 0

    CASE (STRING (char)) OF

         'd' : WIDGET_CONTROL, self.widgetID.top, MAP = 0
         
         'i' : BEGIN     ;== interactive selection
                    IF (STRUPCASE (self.domain) NE 'TIME' AND STRUPCASE (self.domain) NE 'ENERGY') THEN $
                       self->togglePlot
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
         
         'r' : BEGIN
                	self.Reader->read
                	myDetString = self.Reader->getDetName()
                    self->changeDetectors, myDetString, /REPLOT
				END ;self->Plot, _EXTRA = *self.PlotOptions
         'c' : self->changeDetectors, /CYCLE, /REPLOT
         'm' : BEGIN ; Mark a detector for inclusion: 
                	self.Reader->read
					myDetString = self.Reader->getDetName()
					myDetArray = BYTE(myDetString)
					use_det = INTARR(14)
					currChoice = WHERE(myDetArray EQ 49)
					use_det[currChoice] = 1
					myDet = self.currentDet - 1
					IF myDet LT 0 THEN myDet = 13
					use_det[myDet] = 1
					newDetString = STRING(use_det, FORMAT = '(14i1)') 
					self->changeDetectors, newDetString, /REPLOT
				 END
         
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
           
           ELSE: self.currentRange = range

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
PRO TRIGDisplay::showKeyBindings

    ; COMPILE_OPT HIDDEN

    txt = [ $
    
        'd : Dismiss (unmap) the display widget', $         
        'z : Enter interactive zoom mode', $      
        'x : Zoom mode restricted to the x-axis', $   
        'y : Zoom mode restricted to the y-axis', $   
        'g : Go (and zoom in) to selected data', $
        'f : Fullscreen display of the current plot', $
        'i : Enter interactive source selection mode', $
        'c : Cycle through all the detectors.', $
        'r : Revert to the default detectors.', $
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
; Handle XY Log axes change events
; ----------------------------------------------------------------------------
PRO TRIGDisplay::changeLog, myValue

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

        ELSE: 

    ENDCASE        
    
    self->plot, _EXTRA = *self.PlotOptions
    
END


; ----------------------------------------------------------------------------
; Override the plot method.
; ----------------------------------------------------------------------------
PRO TRIGDisplay::errCircle, ra, dec, err, _EXTRA = extra

	zdata = fltarr(361,181)
	;zmir = fltarr(361,181)
	x=indgen(361)
	y=(indgen(181) - 90)
	for i=0,360 do for j=0,180 do begin
		zdata[i,j]=exp(-((ra -x[i])*cos(y[j]*!dtor)/err)^2-((dec-y[j])/err)^2)
		zdata[i,j]+=exp(-((ra + 360. -x[i])*cos(y[j]*!dtor)/err)^2-((dec-y[j])/err)^2)
		zdata[i,j]+=exp(-((ra - 360. -x[i])*cos(y[j]*!dtor)/err)^2-((dec-y[j])/err)^2)
	end
	contour,zdata,x,y,nlevels=1,level=[1./exp(1.)], $
	       /noerase, _EXTRA = extra
    
END

; ----------------------------------------------------------------------------
; Override the plot method.
; ----------------------------------------------------------------------------
PRO TRIGDisplay::plot, HARDCOPY = hardcopy, _EXTRA = extra
    
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

			self->plotSpan, _EXTRA = extra
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

			self->plotSpan, _EXTRA = extra
            END

        'AZZEN'  : BEGIN
			IF (self.haveLocation) THEN BEGIN
				x = *self.chisqMap
				minloc=0l
				minx=min(x(2,*),minloc)
				;print,x(0:2,minloc)
				maxx=max(x(2,*),maxloc)
				;print,x(0:2,maxloc)
				clevs=[minx+2.3, minx+6.14, minx+11.8]
				;print,clevs
				; only visible points
				y=x(0:5,where(x(3,*) eq 1.))
				; all points, az and zen
             
				 ;== First pass: set default axis ranges
				self.defaultRange = TRANSPOSE ([[0, 360], [0, 180]])
				 IF (NOT self.haveDefaultRange) THEN BEGIN
					self.haveDefaultRange   = 1  
					self.currentRange = self.defaultRange
				 ENDIF
				range = self.currentRange
;				IF TOTAL(range) EQ 0 THEN BEGIN 
;					xRange = [0, 360] 
;					yRange = [0, 180] 
;				ENDIF ELSE BEGIN 
					xRange = range[0, *] 
					yRange = range[1, *] 
;				ENDELSE 
				contour,x(2,*),x(0,*),x(1,*),/irregular,levels=clevs,c_annotation=['1','2','3'], $
					xtitle='az',ytitle='zen',C_COLORS = self.color->color('HIST'), $
					XSTYLE = 1, YSTYLE = 1, XRANGE = xRange, YRANGE = yRange, _EXTRA = extra
				oplot,[x(0,minloc),x(0,minloc)],[x(1,minloc),x(1,minloc)],psym=2, _EXTRA = extra
				oplot,x(0,where(x(3,*) eq 0)),x(1,where(x(3,*) eq 0)),psym=3, $
					COLOR = self.color->color('BKGD'), _EXTRA = extra

				fname = self.Reader->filename (/BASENAME)
				self->annotate, fname, YPOSITION = !Y.WINDOW[1] + 0.01, $
				  /SMALL, COLOR = self.color->color('FG'), _EXTRA = extra

			ENDIF ELSE self.domain = 'TIME'

            END

        'RADEC'  : BEGIN
        	IF PTR_VALID(self.externalRA) THEN BEGIN
			
				IF (self.haveLocation) THEN BEGIN
					x = *self.chisqMap
					minloc=0l
					minx=min(x(2,*),minloc)
					;print,x(0:2,minloc)
					maxx=max(x(2,*),maxloc)
					;print,x(0:2,maxloc)
					clevs=[minx+2.3, minx+6.17, minx+11.8]
					;print,clevs
					; only visible points
					y=x(0:5,where(x(3,*) eq 1.))
					; all points, az and zen
				 
					 ;== First pass: set default axis ranges
					self.defaultRange = TRANSPOSE ([[0, 360], [-90, 90]])
					 IF (NOT self.haveDefaultRange) THEN BEGIN
						self.haveDefaultRange   = 1  
						self.currentRange = self.defaultRange
					 ENDIF
					range = self.currentRange
	;				IF TOTAL(range) EQ 0 THEN BEGIN 
	;					xRange = [0, 360] 
	;					yRange = [-90, 90] 
	;				ENDIF ELSE BEGIN 
						xRange = range[0, *]
						yRange = range[1, *]
	;				ENDELSE
	
					contour,x(2,*),x(4,*),x(5,*),/irregular,levels=clevs,c_annotation=['1','2','3'], $
						xtitle='RA',ytitle='Dec', C_COLORS = self.color->color('HIST'), $
						XSTYLE = 1, YSTYLE = 1, XRANGE = xRange, YRANGE = yRange, _EXTRA = extra
					oplot,[x(4,minloc),x(4,minloc)],[x(5,minloc),x(5,minloc)],psym=2, _EXTRA = extra
					oplot,x(4,where(x(3,*) eq 0)),x(5,where(x(3,*) eq 0)),psym=3, $
						COLOR = self.color->color('BKGD'), _EXTRA = extra
				ENDIF
				
				numPos = N_ELEMENTS(*self.externalRA)
				if (self.locChoice EQ 0) THEN BEGIN
					FOR gg = 0, numPos - 1 DO BEGIN
						externRA = (*self.externalRA)[gg]
						externDc = (*self.externalDec)[gg]
						externErr = (*self.externalErr)[gg]
						IF gg ne self.lastDOLitem then begin
							self->errCircle, externRA, externDc, externErr, $
							  XSTYLE = 1, YSTYLE = 1, XRANGE = xRange, YRANGE = yRange, $
							  _EXTRA = extra
							oplot,[externRA, externRA],[externDc, externDc],psym=4
						endif
						extMi = (*self.externalMission)[gg]
						XYOUTS, externRA, externDc, extMi, $
							COLOR = self.color->color('FG'), _EXTRA = extra
					ENDFOR
				ENDIF
;				self->annotate, self.externalMission, $
;					XPOS = self.externalRA, YPOS=self.externalDec, $
;					COLOR = self.color->color('FG'), _EXTRA = extra
					
				fname = self.Reader->filename (/BASENAME)
				self->annotate, fname, YPOSITION = !Y.WINDOW[1] + 0.01, $
				  /SMALL, COLOR = self.color->color('FG'), _EXTRA = extra

			ENDIF ELSE self.domain = 'TIME'

            END

        ELSE: MESSAGE, 'Internal error: unknown domain: ' + self.domain

    ENDCASE        

    ;== Copy pixmap to screen

    IF (NOT hardcopy) THEN BEGIN
       WSET, win
       self->copyPixWindow
    ENDIF
        
END


;------------------------------------------------------------------------------
; Zoom the plotted event
;------------------------------------------------------------------------------
PRO TRIGDisplay::zoom, XZOOM = xZoom, YZOOM = yZoom, $
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

        'AZZEN'  : BEGIN
             
             ;== First pass: set default axis ranges
;             IF (NOT self.haveDefaultRange) THEN BEGIN
                self.haveDefaultRange   = 1  
                self.defaultRange = TRANSPOSE ([[0, 360], [0, 180]])
;             ENDIF
             IF (KEYWORD_SET (defaultRange)) THEN BEGIN
             
                self.currentRange = self.defaultRange
                self->plot, _EXTRA = *self.PlotOptions                    
                
             ENDIF ELSE BEGIN
                self->DISPLAY::zoom, self.widgetID.draw, $
                    X = KEYWORD_SET (xZoom), $
                    Y = KEYWORD_SET (yZoom)

                xRange = AXISRANGE (/XAXIS)
                yRange = AXISRANGE (/YAXIS)
                self.currentRange  = TRANSPOSE ([[xRange], [yRange]])
                ENDELSE
            END

        'RADEC'  : BEGIN
             
             ;== First pass: set default axis ranges
 ;            IF (NOT self.haveDefaultRange) THEN BEGIN
                self.haveDefaultRange   = 1  
                self.defaultRange = TRANSPOSE ([[0, 360], [-90, 90]])
 ;            ENDIF
             IF (KEYWORD_SET (defaultRange)) THEN BEGIN
             
                self.currentRange = self.defaultRange
                self->plot, _EXTRA = *self.PlotOptions                    
                
             ENDIF ELSE BEGIN
                self->DISPLAY::zoom, self.widgetID.draw, $
                    X = KEYWORD_SET (xZoom), $
                    Y = KEYWORD_SET (yZoom)

                xRange = AXISRANGE (/XAXIS)
                yRange = AXISRANGE (/YAXIS)
                self.currentRange  = TRANSPOSE ([[xRange], [yRange]])
                ENDELSE
            END

         ELSE: MESSAGE, 'Internal error: unknown domain: ' + self.domain
 
    ENDCASE
    
    self.dirty = 1   ;== Views have changed

END


;------------------------------------------------------------------------------
; Toggle the current plot
;------------------------------------------------------------------------------
PRO TRIGDisplay::togglePlot
 
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

        ELSE: BEGIN
            self.domain = 'TIME'
            WIDGET_CONTROL, self.logChoiceID, SET_VALUE = (self.lightcurve)->logStatus()
            END

    ENDCASE        
    
    self.haveDefaultRange = 0
    self->plot, _EXTRA = *self.PlotOptions

END

;------------------------------------------------------------------------------
; Change the number of detectors used in the display
;------------------------------------------------------------------------------
PRO TRIGDisplay::sourceLocation, _EXTRA = extra

    ;== Tell the user to hold on:
    self->setStatus, 'Performing the localization, may take some time!', 15, /REVERT
    
    ;== Save the current set of detectors:
	mySavedDets = self.Reader->getDetName()
	
	;using the full energy resolution
	span = self.Lightcurve->timeSpan()
	t_lookup_save = self.Lightcurve->timeLookup()
	t_range =  self.lightcurve->range()
	nrg_span_save = self.Spectrum->energySpan ()
	lookupSave = self.Spectrum->energyLookup()
	e_range =  self.spectrum->range()
    IF (self.Background->haveModel ()) THEN BEGIN
       backSpan = self.Background->backSpan ()
    ENDIF
	;== Integrate the user-selected span:  
	combTimes      = self.Lightcurve->combinedTimes ()
	self->setEnergyLookup, lookupSave, /INITIALIZE
	eLookup = self.Spectrum->energyLookup()
	combThresholds = self.Spectrum->combinedThresholds ()
	;deltaEnergy = combThresholds[1, *] - combThresholds[0, *]
	myHist = [0]
	myBack = [0]
	
	FOR dd = 0, 13 DO BEGIN
		use_det = INTARR(14)
		use_det[dd] = 1
		newDetString = STRING(use_det, FORMAT = '(14i1)') 
		self->changeDetectors, newDetString
		
		;== Return pointers for efficiency.  Do NOT alter these data!
		self.Lightcurve->integrateTime, span, $
			HISTORY = history, ERRORS = errors, TOTALTIME = totalTime ;<- livetime
			
		;== Something wrong happened: no selection
		IF (totalTime EQ 0.0) THEN BEGIN
			   cancel = 1
			   RETURN    
		ENDIF        
	
		;== Integrate the background model over the selected time span
		numBins = N_ELEMENTS (history)
		
		fitChannels = INTARR(2)
		fitChannels[0] = eLookup[0]
		fitChannels[1] = eLookup[numBins] - 1
		tInt = NEAREST_EDGE (combTimes, span[0, *],tind) 
	
		IF (self.Background->haveModel ()) THEN BEGIN
			brates = self.Background->evalModel (combTimes[0,tind[0]], combTimes[1,tind[1]])
			brates = transpose(brates)
			brerrs = self.Background->evalMsig (combTimes[0,tind[0]], combTimes[1,tind[1]])
			brerrs = transpose(brerrs)
		ENDIF ELSE BEGIN
			brates = FLTARR(numbins)
			brerrs = FLTARR(numbins)
		ENDELSE
;		print, brates  * deltaEnergy * totalTime
		myHist = [myHist, FIX(history)]
		myBack = [myBack, FIX(brates)]
		
	ENDFOR
	
	;== Gather the location driver info:
	myHist = myHist[1:*]
	myBack = myBack[1:*]
	myTime = TOTAL(span[0, *]) / 2.
	myEarth = self.Reader->getEarthPos(myTime)
	myQuats = self.Reader->getQuaternion(myTime)
	myRa    = self.Reader->getSourceRA()
	myDec   = self.Reader->getSourceDec()
	myObject= self.Reader->getObject()
	myObject = STRMID(myObject, 0, 8)
	WIDGET_CONTROL, self.hardChoiceID, GET_VALUE = hardvalue
	myLowChan = '3'
	myHiChan = '4'
	self.locenergy = '(50, 300)'
	IF (hardvalue EQ 1) THEN BEGIN
		myLowChan = '1'
		myHiChan = '2'
		self.locenergy = '(10, 50)'
	ENDIF
	
	mySeparatrix = '/'
    IF (STRUPCASE (!VERSION.OS_FAMILY)) EQ 'WINDOWS' THEN BEGIN
    	mySeparatrix = '\'
    ENDIF

	;== Find out where our dol is:
	CD, CURRENT = cur;	CD, !MFIT.HELP_PATH
;	CD, '..'
;	CD, CURRENT = dol_dir
;	myDOL = dol_dir + mySeparatrix + 'dol4.exe  '
	CD, cur
	SPAWN, "echo $DOL_ROOTDIR", dol_dir
	myDOL = dol_dir + mySeparatrix + 'dol4.exe  '	

	; Get some info:
    header = self.Reader->header (/POINTER)
    notes  = (*header).notes
	result = STRSPLIT(notes[5], ":",  /EXTRACT)
	trigtime = LONG(result[1])
	
	;== dol expects a single line of 243 parameters on the command line:
	dol_file = 'temp_rates.txt'
	myString = myDol
	myString += myLowChan + '   '
	myString += myHiChan  + '   '
	FOR ff = 0, N_ELEMENTS(myhist) - 1 DO BEGIN
		myString += STRING(myhist[ff])
	ENDFOR
	FOR ff = 0, N_ELEMENTS(myBack) - 1 DO BEGIN
		myString += STRING(myBack[ff])
	ENDFOR
	FOR ff = 0, 1 DO BEGIN
		myString += STRING(totalTime)
	ENDFOR
	FOR ff = 0, N_ELEMENTS(myEarth) - 1 DO BEGIN
		myString += STRING(myEarth[ff]) + '   '
	ENDFOR
	FOR ff = 0, N_ELEMENTS(myQuats) - 1 DO BEGIN
		myString += STRING(myQuats[ff]) + '   '
	ENDFOR
	FOR ff = 0, numBins - 1 DO BEGIN
		myString += STRING(combThresholds[0, ff])
	ENDFOR
	myString += STRING(combThresholds[1, numBins - 1]) + '   '
	myString += STRING(myRA) + '   '
	myString += STRING(myDec) + '   '
	myString += STRING(trigtime) + '   '
	myString += STRING(1) + '   '
	myString += myObject
	
	;== Write out a driver file for dol and SPAWN it:
    OPENW, lun, dol_file, /GET_LUN, WIDTH = 5000
	PRINTF, lun, myString
    ; Close
    ;   
    CLOSE, lun
    FREE_LUN, lun
;------------------------------------------------------------------------------------------------   
    ; Capture DOL output
    SPAWN, 'sh temp_rates.txt', teststring                                                     ; AMG
    PRINT, teststring                                                                          ;
    ; Write temp file                                                                          ;
    OPENW, lun2, 'dol_output.txt', /GET_LUN                                                    ;
    PRINTF, lun2, teststring                                                                   ; 
    CLOSE, lun2                                                                                ; 
    FREE_LUN, lun2
    ; Call parser                                                                              ;
    DOL_PARSE, tint	                                                              ;
;------------------------------------------------------------------------------------------------

    ;== Read the result from the output file:
    myOutFile = 'dol_' + myObject + '.txt'
    OPENR, lun2, myOutFile, /GET_LUN
    
   	newRA = 0.
    newDec = 0.
    newErr = 0.
    newAz = 0.
    newZen = 0.
    good_chi = 1B
    significance = 0.0
    dolver = ''
    READF, lun2, newRA, newDec, newErr, good_chi, significance, newAz, newZen, dolver
    CLOSE, lun2
    FREE_LUN, lun2
    
	self.locversion = 'dol v'+strtrim(dolver,2)
	
	;mission = "  Loc #" + STRTRIM(String(self.currentLoc), 2)
	mission = 'Fermi, GBM'
	;self->updateMission, mission
	self->updateLocation, newRA, newDec, newErr, MISSION=mission
	
    ;== Revert to the saved set of detectors:
	self->changeDetectors, mySavedDets ;, /REPLOT
	self->setTimeSpan, span
	self->setTimeLookup, t_lookup_save
	self.lightcurve->setRange, t_range
	self->setEnergySpan, nrg_span_save
	self->setEnergyLookup, lookupSave   ;== Restore previous lookup
	self.spectrum->setRange, e_range
    IF (self.Background->haveModel()) THEN BEGIN
          self.Background->setSpan, backSpan
          self->fitBackground ;, ORDER = order
    ENDIF
	self->integrateAndCombine
	;self->zoom, /SELECTION
	self->plot, _EXTRA = *self.PlotOptions
	
	;== Do we write out the TCAT file?
;	d = DIALOG_MESSAGE (/QUESTION, 'Write out TCAT file for this location?')
;	IF (d EQ 'Yes') THEN self->writeTCAT, newRA, newDec, newErr

	;== Now read in the chi-2 maps:
	npt = 0L
	OPENR, lun3, 'chi2grid.dat', /GET_LUN
	firstt = 1
	READF, lun3, npt
	chi2_map = fltarr(6,npt)
	READF, lun3, chi2_map
	CLOSE, lun3
    FREE_LUN, lun3
    PTR_FREE, self.chisqMap
    self.chisqMap = PTR_NEW(chi2_map)

	self.lastDOLitem = N_ELEMENTS(*self.externalRA)-1
	self.haveLocation = 1
	self.haveDefaultRange = 0      
	self.domain = 'RADEC'
    self->plot, _EXTRA = *self.PlotOptions
	
END

;------------------------------------------------------------------------------
; New methods to *hopefully* make it easier for more than one routine to affect
; the location.  Updates the externalRA,dec data members and the widget labels.
;------------------------------------------------------------------------------
PRO TRIGDisplay::updateMission, mission, CURRENT=current
	IF PTR_VALID(self.externalMission) THEN BEGIN
		if keyword_set(current) then begin
			(*self.externalMission)[n_elements((*self.externalMission))-1] = mission
			RETURN
		endif
		tempMi = *self.externalMission
		PTR_FREE, self.externalMission
		self.externalMission = PTR_NEW([tempMi,mission])
	ENDIF ELSE BEGIN
		PTR_FREE, self.externalMission
		self.externalMission = PTR_NEW([mission])
	ENDELSE
END

PRO TRIGDisplay::updateLocation, newRA, newDec, newErr, MISSION=mission

	;== Update the display with the newly-calculated RA and Dec:
	;IF self.currentLoc NE 0 THEN BEGIN
	;	WIDGET_CONTROL, self.sourceRA_ID, GET_VALUE = oldRA
	;	result1 = STRSPLIT(oldRA, ":",  /EXTRACT)
	;	WIDGET_CONTROL, self.sourceDecID, GET_VALUE = oldDec
	;	result2 = STRSPLIT(oldDec, ":",  /EXTRACT)
	;	WIDGET_CONTROL, self.sourceErrID, GET_VALUE = oldErr
	;	result3 = STRSPLIT(oldErr, ":",  /EXTRACT)
		
		IF n_elements(mission) ge 1 then begin
			self->updateMission, mission
		endif
		
		IF PTR_VALID(self.externalRA) THEN BEGIN
			tempRA = *self.externalRA
			PTR_FREE, self.externalRA
		;	self.externalRA = PTR_NEW([tempRA, FLOAT(result1[1])])
			self.externalRA = PTR_NEW([tempRA, newRA])
			
			tempDec = *self.externalDec
			PTR_FREE, self.externalDec
		;	self.externalDec = PTR_NEW([tempDec, FLOAT(result2[1])])
			self.externalDec = PTR_NEW([tempDec, newDec])
			
			tempErr = *self.externalErr
			PTR_FREE, self.externalErr
		;	self.externalErr = PTR_NEW([tempErr, FLOAT(result3[1])])
			self.externalErr = PTR_NEW([tempErr, newErr])
		ENDIF ELSE BEGIN
			PTR_FREE, self.externalRA
		;	self.externalRA = PTR_NEW([FLOAT(result1[1])])
			
			PTR_FREE, self.externalDec
		;	self.externalDec = PTR_NEW([FLOAT(result2[1])])
			
			PTR_FREE, self.externalErr
		;	self.externalErr = PTR_NEW([FLOAT(result3[1])])
		
			self.externalRA = PTR_NEW([newRA])
			self.externalDec = PTR_NEW([newDec])
			self.externalErr = PTR_NEW([newErr])
		ENDELSE
		
		print, transpose( [ [*self.externalRA], [*self.externalDec] ])
		
	;ENDIF
	
	self.currentLoc++
	
	WIDGET_CONTROL, self.sourceRA_ID, SET_VALUE = 'Source RA: ' + STRTRIM(String(newRA), 2)
	WIDGET_CONTROL, self.sourceDecID, SET_VALUE = 'Source Dec: ' + STRTRIM(String(newDec), 2)
	WIDGET_CONTROL, self.sourceErrID, SET_VALUE = 'Loc Error: ' + STRTRIM(String(newErr), 2)
END


;------------------------------------------------------------------------------
; Write out the tcat files:
;------------------------------------------------------------------------------
PRO TRIGDisplay::writeTCAT, INTERACTIVE = interactive, NEW_SOURCE = new_source

	GBM_Class = [	'GRB',$
					'SGR',$
					'SGR1806',$
					'UNRELOC',$
					'BELOWHZ',$
					'TGF',$
					'LOCLPAR',$
					'DISTPAR',$
					'SFL', $
					'ERROR', $
					'UNKNOWN',$
					'CYGX1',$
					'GROJ422',$
					'TRANSNT' $
					]
					
	;== CCR #255: 03/29/2010 RDP: FSSC expects SFLARE for our SFL classification:
	FSSC_Class = ['GRB', 'SGR', 'SGR', 'UNCERT', 'UNCERT', 'TGF', 'LOCLPAR', $
					'DISTPAR', 'SFLARE', 'UNCERT', 'UNCERT', 'GALBIN', $
					'GALBIN', 'TRANSNT']
					
	GRB_locsrc =[	$;'Fermi, GBM',$
					'Fermi, LAT',$
					'Swift, BAT',$
					'Swift, XRT',$
					'Super-AGILE',$
					'INTEGRAL', $
					'Known Source' $
					]

	IF self.externalObjID NE '' THEN BEGIN
		myObject= self.externalObjID
		myClass = self.externalClass
	ENDIF ELSE BEGIN
		myObject= self.Reader->getObject()
		myClass = stregex(myObject,'[^0-9]+',/EXTRACT)
	ENDELSE
	

	count = 0
	;print,'missions=',*self.externalMission
	IF PTR_VALID(self.externalMission) THEN $
		mission = WHERE(*self.externalMission EQ GRB_locsrc, count)
		
	IF count EQ 0 THEN mission = 0
	
	start = WHERE(myClass EQ GBM_Class, count)
	
	IF count EQ 0 THEN BEGIN
		start = WHERE(myClass EQ FSSC_Class, count)
		IF count EQ 0 THEN start = 0 ELSE start = start[0]
		;print,'mystartclass=',myClass,start[0]
	ENDIF

	WIDGET_CONTROL, self.sourceRA_ID, GET_VALUE = newRA
	result = STRSPLIT(newRA, ":",  /EXTRACT)
	RAVal = FLOAT(result[1])
	WIDGET_CONTROL, self.sourceDecID, GET_VALUE = newDec
	result = STRSPLIT(newDec, ":",  /EXTRACT)
	DecVal = FLOAT(result[1])
	WIDGET_CONTROL, self.sourceErrID, GET_VALUE = newErr
	result = STRSPLIT(newErr, ":",  /EXTRACT)
	ErrVal = FLOAT(result[1])

	mytrigID = self->getEventID()

	IF KEYWORD_SET(INTERACTIVE) THEN BEGIN
		help_text = ['This window creates a new TCAT file.',$
					'Enter a new RA,Dec,Err. Use the Mission list',$
					'to specify what instrument determined this',$
					'location. Type in a new value if necessary.', '',$
					'OBJ_CLASS must be used to set OBJECT and CLASS.',$
					'It is a mapping from GBM class names to those used',$
					'in the FSSC catalog of GBM triggers.',$
					'OBJ_CLASS must be one of the values in the list,',$
					'or a meaningful name for the event, such as',$
					'"GRB 081916C". If you name the event, first',$
					'select the best match in the list to ensure',$
					'it is mapped appropriately. A new box showing',$
					'you the mapped values will appear. You can change',$
					'this if necessary with the list.']
	
		Class_Map = OBJ_NEW('ADT_DHASH',30)
		FOR i=0,13 DO Class_Map->set, GBM_Class[i], FSSC_Class[i]
	
		;== Parameters for the synthesized data:
		result = DIALOG_COMBOINPUT( $
			   PROMPT = ['RA (deg): ', $
						 'Dec (deg):', $
						 'Err (deg):' $
						 ], $
				INITIAL = [RAVal,DecVal,ErrVal], $
				STATIC = ['OBJECT    :', 'CLASS     :'], $
			   	STATIN = [myObject, myClass], $
			   TITLE = 'Input *External* Localization: ', $
			   LISTNAMES = ['OBJ_CLASS:','Mission:'], $
			   LISTITEMS = [GBM_Class,GRB_locsrc],$
			   LISTSIZES = [N_ELEMENTS(GBM_Class),N_ELEMENTS(GRB_locsrc)], $
			   LISTEDIT = [0,1], $
			   LISTSTART = [start, mission], $
			   MAPLIST = {lists:[0], mappings:[Class_Map], completed:[1b]}, $
			   BIND_LSTATS_N = [0,0], $
			   BIND_TO_STATIC_N = [0,1], $
			   APPEND = [mytrigID], $
			   HELP = help_text)
		
		OBJ_DESTROY, Class_Map
		;print, 'RESULT', result
		IF ((SIZE (result))[0] EQ 0) THEN RETURN ; User cancelled!
		
		RAVal = FLOAT(result[0])
		DecVal = FLOAT(result[1])
		ErrVal = FLOAT(result[2])
		
		
		IF (result[3] NE "") THEN BEGIN
			myObject = result[3]
			self.externalObjID = myObject
			WIDGET_CONTROL, self.sourceEvntID, SET_VALUE = 'Event: ' + STRTRIM(String(myObject), 2)
		ENDIF
  		IF (result[4] NE "") THEN BEGIN
			myClass  = result[4]
			self.externalClass = myClass
		ENDIF
		IF (result[5] NE "") THEN BEGIN
			localClass = result[5]
		ENDIF
		IF (result[6] NE "") THEN BEGIN
			newmission  = result[6]
		ENDIF ELSE BEGIN
			newmission = "  GBM GSW"
		ENDELSE
		
		
		;print,newmission
		self->updateLocation, RAVal, DecVal, ErrVal
		self->updateMission,  newmission
		self.locversion = 'N/A'
		self.locenergy = 'N/A'
    	myMission = newmission
    	
        d = DIALOG_MESSAGE (/QUESTION, 'Write TCAT file?')
        IF (d NE 'Yes') THEN RETURN
		;mission = STRSPLIT(result[5], ",", /EXTRACT)
	ENDIF ELSE IF (self.currentLoc gt 0) THEN BEGIN
		WIDGET_CONTROL, self.sourceRA_ID, GET_VALUE = newRA
		result = STRSPLIT(newRA, ":",  /EXTRACT)
		RAVal = FLOAT(result[1])
		WIDGET_CONTROL, self.sourceDecID, GET_VALUE = newDec
		result = STRSPLIT(newDec, ":",  /EXTRACT)
		DecVal = FLOAT(result[1])
		WIDGET_CONTROL, self.sourceErrID, GET_VALUE = newErr
		result = STRSPLIT(newErr, ":",  /EXTRACT)
		ErrVal = FLOAT(result[1])
		
		stacksize = n_elements(*self.externalMission)
    	myMission = (*self.externalMission)[stacksize-1]
    	
	ENDIF ELSE BEGIN
		RETURN
	END

	IF KEYWORD_SET(NEW_SOURCE) THEN BEGIN
		help_text = ['This window creates a new TCAT file with',$
					'the current localization and a new class.',$
					'The Mission should be Fermi,GBM.','',$
					'OBJ_CLASS must be used to set OBJECT and CLASS.',$
					'It is a mapping from GBM class names to those used',$
					'in the FSSC catalog of GBM triggers.',$
					'OBJ_CLASS must be one of the values in the list,',$
					'or a meaningful name for the event, such as',$
					'"GRB 081916C". If you name the event, first',$
					'select the best match in the list to ensure',$
					'it is mapped appropriately. A new box showing',$
					'you the mapped values will appear. You can change',$
					'this if necessary with the list.']
					
		Class_Map = OBJ_NEW('ADT_DHASH',30)
		FOR i=0,13 DO Class_Map->set, GBM_Class[i], FSSC_Class[i]
	
		result = DIALOG_COMBOINPUT( $
				STATIC = ['OBJECT    :', 'CLASS     :'], $
			   	STATIN = [myObject, myClass], $
			   TITLE = 'Input Class: ', $
			   LISTNAMES = ['OBJ_CLASS:','Mission:'], $
			   LISTITEMS = [GBM_Class,['Fermi, GBM']],$
			   LISTSIZES = [N_ELEMENTS(GBM_Class),1], $
			   LISTEDIT = [0,1], $
			   LISTSTART = [start, 0], $
			   MAPLIST = {lists:[0], mappings:[Class_Map], completed:[1b]}, $
			   BIND_LSTATS_N = [0,0], $
			   BIND_TO_STATIC_N = [0,1], $
			   APPEND = [mytrigID], $
			   HELP = help_text)
		
		OBJ_DESTROY, Class_Map
		
;		print, 'RESULT', result
		IF ((SIZE (result))[0] EQ 0) THEN RETURN ; User cancelled!
		IF (result[0] NE "") THEN BEGIN
			myObject = result[0]
			self.externalObjID = myObject
			WIDGET_CONTROL, self.sourceEvntID, SET_VALUE = 'Event: ' + STRTRIM(String(myObject), 2)
		ENDIF
		IF (result[1] NE "") THEN BEGIN
			myClass  = result[1]
			self.externalClass = myClass
		ENDIF
		localClass = result[2]
		IF (result[3] NE "") THEN BEGIN
			newmision = result[3]
			self->updateMission,newmision,/CURRENT
		ENDIF
		
		stacksize = n_elements(*self.externalMission)
    	myMission = (*self.externalMission)[stacksize-1]
		
		d = DIALOG_MESSAGE (/QUESTION, 'Write TCAT file?')
        IF (d NE 'Yes') THEN RETURN
		
	ENDIF 
	
	header = self.Reader->header (/POINTER)
	myHeader = (*header).ext0
	
	sxdelpar, myHeader, ['DETTYPE', 'DATATYPE', 'FILE-VER']
	sxaddpar, myHeader, 'CREATOR', 'rmfit v' + !RM_PARAMS.VERSION, /SAVECOM
	sxaddpar, myHeader, 'FILETYPE', 'TRIGGER ENTRY', /SAVECOM
	;sxaddpar, myHeader, 'FILE-VER', '1.0.0', /SAVECOM
	;mySize = SIZE(mission)
	;IF (mySize[0] > 0) THEN BEGIN
	;	IF (mySize[1] > 1) THEN BEGIN
	;		sxaddpar, myHeader, 'INSTRUME', STRTRIM(mission[1], 2), /SAVECOM
	;	ENDIF
	;ENDIF
	
    filename = self.Reader->filename (/BASENAME)
    myPath = self.Reader->filename (/PATH)
    myTCATBase = 'glg_tc' + STRMID(filename, 9, 20)
    myTCATName = myPath + myTCATBase
    result = file_search(myTCATName + '*',count=fcnt)
    myTCATBase += string(fcnt,format='(i02)') + '.fit'
    myTCATName = myPath + myTCATBase
    
    get_date, dte, /timetag
    sxaddpar, myHeader, 'DATE', dte, /SAVECOM
	sxaddpar, myHeader, 'FILENAME', myTCATBase, /SAVECOM
	
	;Added 1/20/10
	sxaddpar, myHeader, 'LOC_VER', self.locversion, ' Version string of localizing software'
	sxaddpar, myHeader, 'LOC_ENRG', self.locenergy,' Energy range used for localization'
	
	
	sxaddpar, myHeader, 'OBJECT', myObject, /SAVECOM
	sxaddpar, myHeader, 'RA_OBJ', RAVal, /SAVECOM
	sxaddpar, myHeader, 'DEC_OBJ', DecVal, /SAVECOM
	sxaddpar, myHeader, 'ERR_RAD', ErrVal, /SAVECOM
	
	;Added 1/20/10
	srcvec = fltarr(3,1)
	srcvec[*,0] = CV_COORD(FROM_SPHERE=[RAVal,DecVal,1.0],/TO_RECT,/DEGREES)

	quat0 = OBJ_NEW('fgeom_quaternion')
	t0_quat = self.Reader->getQuaternion(0.)
	quat0->set, Q = [ t0_quat[3], t0_quat[0:2] ]
	ok = quat0->rotate(srcvec)
	srcvec = CV_COORD(FROM_RECT=srcvec[*,0],/TO_SPHERE,/DEGREES)
	obj_destroy,quat0
	
	sxaddpar, myHeader, 'THETA', 90-srcvec[1], ' [deg] Angle from spacecraft zenith', AFTER = 'ERR_RAD'
	sxaddpar, myHeader, 'PHI', srcvec[0], ' [deg] Angle from spacecraft +X axis toward +Y', AFTER = 'THETA'
	
	
	sxaddpar, myHeader, 'LOC_SRC', myMission, " Mission/Instrument providing the localization", AFTER = 'PHI'
    sxaddpar, myHeader, 'CLASS', myClass, ' Classification of trigger', AFTER = 'LOC_SRC'
	IF N_ELEMENTS(localClass) EQ 0 THEN localClass = myClass
    sxaddpar, myHeader, 'OBJ_CLASS', localClass, ' Classification of trigger', AFTER = 'CLASS'
	sxaddpar, myHeader, 'RELIABLT', 1.0, ' Reliability of classification', AFTER = 'OBJ_CLASS'
	
	;Added 1/20/10
	sxaddpar, myHeader, 'GCN_FLAG','No', ' Was this file used for GCN Notice generation?'
	
	fits_add_checksum, myHeader
	
	fxwrite, myTCATName, myHeader
	message = 'Wrote: ' + myTCATBase
	self->setStatus, message, 5, /REVERT
	;hprint, myHeader
        
END

;------------------------------------------------------------------------------
; Change the number of detectors used in the display
;------------------------------------------------------------------------------
PRO TRIGDisplay::changeDetectors, detSet, REPLOT = REPLOT, CYCLE = cycle, $
    INTERACTIVE = interactive, _EXTRA = extra

	;== Catch the odd keystrake that may occur when we are not in the TIME or ENERGY domains:
	IF ((STRUPCASE (self.domain) NE 'TIME') AND (STRUPCASE (self.domain) NE 'ENERGY')) THEN $
		self->togglePlot
	
	BGORange = 0
	IF (KEYWORD_SET(CYCLE)) THEN BEGIN
		use_det = INTARR(14)
		myDet = self.currentDet
		use_det[myDet] = 1
		nextDet = (myDet + 1) MOD 14
		newDetString = STRING(use_det, FORMAT = '(14i1)') 
		self.currentDet = nextDet
		IF (nextDet > 11) THEN BGORange = 1
	ENDIF ELSE IF (KEYWORD_SET(INTERACTIVE)) THEN BEGIN
		myDetString = self.Reader->getDetName()
		myDetArray = BYTE(myDetString)
		names = STRARR(14)
		FOR kk = 0, 11 DO BEGIN
			names[kk] = 'NaI_' + string(kk,format='(i02)')
		ENDFOR
		names[12] = 'BGO_' + string(0,format='(i02)')
		names[13] = 'BGO_' + string(1,format='(i02)')
		use_det = INTARR(14)
		currChoice = WHERE(myDetArray EQ 49)
		use_det[currChoice] = 1
		use_det = DIALOG_CHECKLIST (names, INITIAL = use_det, $
				  TITLE = 'Select Detectors for Display', /NONEXCLUSIVE) 
		IF TOTAL (use_det) EQ 0 THEN RETURN
		newDetString = STRING(use_det, FORMAT = '(14i1)') 
	ENDIF ELSE BEGIN
		newDetString = detSet
	ENDELSE
	
;	self.Reader->setDetName, newDetString
	IF self.fineTime THEN self.Reader->read, newDetString = newDetString, /FINE $
					ELSE self.Reader->read, newDetString = newDetString
    energySpan   = self.lightCurve->energySpan ()
    energyLookup = self.lightCurve->energyLookup ()
    energyRange  = self.Spectrum->range ()
    timeSpan   = self.lightCurve->timeSpan ()
    timeLookup = self.lightCurve->timeLookup ()
    timeRange  = self.lightCurve->range ()
    myBackSpan = self.Background->backSpan()
    myModel = self.Background->haveModel()
	IF myModel THEN myOrder = self.Background->order()
    OBJ_DESTROY, self.Lightcurve    
    OBJ_DESTROY, self.Spectrum      
    OBJ_DESTROY, self.Background    
	self.Lightcurve = OBJ_NEW ('Lightcurve', self.Reader)
	self.Spectrum   = OBJ_NEW ('Spectrum',   self.Reader)    
	self.Background = OBJ_NEW ('Background', self.Reader)
;	IF (self.myLookup NE '') AND (NOT KEYWORD_SET(CYCLE)) THEN BEGIN
;    	self->readLookup, /AUTO, /SILENT, FILENAME = f, ERROR = error
;		self->integrateAndCombine
;    ENDIF ELSE BEGIN
	self->setTimeSpan, timeSpan
	self->setEnergySpan, energySpan
	;== Use the _EXTRA mechanism to pass /INITIALIZE to setTimeLookup:
	self->setTimeLookup, timeLookup, _EXTRA = extra
	self->setEnergyLookup, energyLookup
	self->integrateAndCombine
	energyDefRan = self.Spectrum->default ()
	myERange = [energyRange[0, *], energyDefRan[1, *]]
	timeDefRan = self.lightCurve->default ()
	myTRange = [timeRange[0, *], timeDefRan[1, *]]
;		self.lightCurve->resetDefaultRange
;		self.spectrum->resetDefaultRange
	;== Update the defaults
	self.Lightcurve->update, myTRange
	;self.Lightcurve->setDefaultRange
	self.Spectrum->update, myERange
	xRange = REFORM ((self.lightCurve->timeSpan ())[0, *])
	 ; Snap out to nearest bin edge
	idx = INTARR (2)
	thresholds = self.Lightcurve->combinedTimes ()
	duhRange = NEAREST_EDGE (thresholds, xRange, idx)
	hist    = self.Lightcurve->combinedHist ()
	histErr = self.Lightcurve->combinedHistErr ()
	yRange = [ MIN (hist[idx[0]:idx[1]] - histErr[idx[0]:idx[1]]), $
			   MAX (hist[idx[0]:idx[1]] + histErr[idx[0]:idx[1]])]
							   
	myTRange[1, *] = yRange
	; Store the new plot range, and update plot
	self.Lightcurve->setRange, myTRange
;		self.lightCurve->setRange, myTRange
;		self.spectrum->setRange, myERange
	self.spectrum->setLogStatus, [1, 1]
	IF myModel THEN BEGIN
		self.Background->setSpan, myBackSpan
		self->fitBackground, ORDER = myOrder
		self->integrateBackground, /TIME
		self->integrateBackground, /ENERGY
	ENDIF
;    ENDELSE
	self.haveDefaultRange = 0

	IF (KEYWORD_SET(REPLOT)) THEN BEGIN
		myDets = ['0','1','2','3','4','5','6','7','8','9','a','b','B0','B1']
		blanks = REPLICATE('-',14)
		myDetArray = BYTE(self.Reader->getDetName())
		currChoice = WHERE(myDetArray EQ 49)
;		use_det = INTARR(14) 
;		use_det[currChoice] = 1 
;		detPos = STRSPLIT(newDetString, '1') - 1 
;		dIdx = WHERE(detPos > 0, detCnt) 
;		IF detCnt NE 0 THEN detPos = detPos[dIdx] ELSE detPos = [0]
		blanks[currChoice] = myDets[currChoice]
		displayName = STRJOIN(blanks)
		WIDGET_CONTROL, self.detNamesID, SET_VALUE = 'Detector: ' + displayName
		;self->zoom, /SELECTION
		self->plot, _EXTRA = *self.PlotOptions
	ENDIF

END

; ----------------------------------------------------------------------------
; Define the subclass, inheriting Display and PHA
; ----------------------------------------------------------------------------
PRO TRIGDisplay__define

    obj = { TRIGDisplay, INHERITS PHADISPLAY, $
         
        chisqMap     : PTR_NEW (), $   ;FLTARR(3)
        currentDet   : 0L, $
        fineTime     : 0L, $
        haveLocation : 0L, $
        detNamesID   : 0L, $
        hardChoiceID : 0L, $
        locChoice    : 0L, $
        lastDOLitem  : 0L, $
        currentLoc   : 0L, $
        externalRA   : PTR_NEW (), $
        externalDec  : PTR_NEW (), $
        externalErr  : PTR_NEW (), $
        externalMission : PTR_NEW (), $
        externalObjID   : '', $
        externalClass   : '', $
        sourceEvntID : 0L, $
        sourceRA_ID  : 0L, $
        sourceDecID  : 0L, $
        sourceErrID  : 0L, $
        $
        locenergy	: '',$
        locversion	: '' $
    }

END
