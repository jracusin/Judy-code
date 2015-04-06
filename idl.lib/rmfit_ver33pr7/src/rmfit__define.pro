;------------------------------------------------------------------------------
; Initialize the RMFIT object.  This object is responsible 
; for control of all display objects.
;------------------------------------------------------------------------------

FUNCTION RMFit::init

    ;== Software version

    self.version = '3.3pr7'  ;Release candidate #

    ;== Show the "About" dialog
   RESTORE, !MFIT.script_file
   IF script EQ 0 THEN BEGIN
     ;IF !VERSION.OS_NAME EQ 'Mac OS X' THEN SPAWN, 'say "R, M, Fit.  Its in the code."'
     self->banner, /DISMISS, TIMER = 3 ; s
   ENDIF
    ;== Session timer

    self.startTime = SYSTIME (1)
        
    ;== Set current data file search path

    CD, CURRENT = c
    self.lastFilter = '*.pha'
        
    ;== Instantiate the List that holds the dataset object references

    self.List = OBJ_NEW ('List')

    ;== Set default display options

    self.displayOnLoad = 1
    self.autoLookup    = 1
    param_last_path    = c
    
    ;== Define a global system variable for preserving the photon model picks
    ;If the parameter list changes, make sure this is reflected in rmfit.pro!
    RESTORE, /RELAXED_STRUCTURE_ASSIGNMENT, FILE = !MFIT.param_file

    RM_PARAMS = { MFIT_GLOBALS, $
    
        PHOTTERMS   : param_photterms,   $
        COLORS      : param_colors,      $
        FLUXINT     : param_flux_int,    $
        DURINT      : param_dur_int,     $
        LASTPATH    : param_last_path,   $
        VERSION     : self.version       $
        
    }    
    DEFSYSV, '!RM_PARAMS', RM_PARAMS
    
    ;== No anonymous structure members allowed in structures
    ;DEFSYSV, '!PLOTOPTIONS', param_plotOptions
    IF (param_last_path NE c) THEN $
        self.lastPath = !RM_PARAMS.lastpath $
    ELSE BEGIN
        self.lastPath = c
        !RM_PARAMS.lastpath = c
    ENDELSE
    
;    CATCH, Error
;    IF Error NE 0 THEN BEGIN
;            print, 'Error status: ', Error
;            print, 'Error string: ', !ERR_STRING
;            msg = "Can't restore the saved parameters from an old save file."
;            msg = [msg, 'Please delete the file "my_parms.sav" and run again.']
;            junk = DIALOG_MESSAGE (/ERROR, msg)
;            error = 0
;            RETURN, 0
;    ENDIF
            
    ;== Instantiate the two singleton objects that pervade the user interface
    
    self.Color = OBJ_SINGLETON ('Color')
    self.Fitter = OBJ_SINGLETON ('Mfit', INFO_FILE = !MFIT.INFO_FILE, myRMFIT = self)

    ;== Tear down the about banner
;    WIDGET_CONTROL, bannerID, /DESTROY
    
    ;== Build and display the main GUI

    self->build
    
    RESTORE, !MFIT.script_file                                                                    ; AMG    
    IF script THEN BEGIN                                                                          ;
      self->delete, /ALL                                                                          ;
    ENDIF                                                                                         ;
    
    RETURN, 1

END


;------------------------------------------------------------------------------
; Destructor
;------------------------------------------------------------------------------
PRO RMFit::cleanup
    RESTORE, !MFIT.script_file
    IF script THEN WIDGET_CONTROL, self.topID, /DESTROY
    OBJ_DESTROY, self.List
    OBJ_DESTROY, self.Color
    OBJ_DESTROY, self.Fitter
    RESTORE, !MFIT.script_file
    param_photterms = !RM_PARAMS.photterms
    ;param_plotOptions = !PLOTOPTIONS
    param_colors = !RM_PARAMS.colors
    param_flux_int = !RM_PARAMS.fluxint
    param_dur_int  = !RM_PARAMS.durint
    param_last_path = !RM_PARAMS.lastpath
    script=(phaNum=(numfits=0)) & lcFile=(rspFile=(model=''))                                         ; AMG

    ;This next step may not work, allow it to fail
    CALL_PROCEDURE, 'SAVE', file=!MFIT.param_file, param_photterms, param_colors,$
                    param_flux_int, param_dur_int, param_last_path
    CALL_PROCEDURE, 'SAVE', file=!MFIT.script_file, script, lcFile, rspFile, luFile, phaNum, model, $ ; AMG
                                 vals, statistic, eInterval, numfits, iter, scriptVerbose, table      ;
   
END


;------------------------------------------------------------------------------
; Event handler
;------------------------------------------------------------------------------
PRO RMFit_EVENT, event

    ; COMPILE_OPT HIDDEN

    ;== Forward events to the object event handler
    
    WIDGET_CONTROL, event.top, GET_UVALUE = self
    self->eventHandler, event

END

PRO RMFit::eventHandler, event

    ; COMPILE_OPT HIDDEN

    type = TAG_NAMES (event, /STRUCTURE)

    CASE (type) OF

         'WIDGET_KILL_REQUEST': BEGIN

            ;== Dismiss, not kill
            IF (DIALOG_MESSAGE (/QUESTION, 'Are You Sure? rmfit will remain in memory!', $
                DIALOG_PARENT = event.top) EQ 'Yes') THEN $
                WIDGET_CONTROL, event.top, MAP = 0 
            END

        'WIDGET_BUTTON': BEGIN

        WIDGET_CONTROL, event.id, GET_VALUE = value
        
        CASE (value) OF

             'Exit IDL': BEGIN

                 self->delete, /ALL
                 self->statistics
                 OBJ_DESTROY, self
                 EXIT
                 END

             'Quit': BEGIN

                 self->delete, /ALL
                 WIDGET_CONTROL, event.top, /DESTROY
                 self->statistics                 
                 OBJ_DESTROY, self
                 END

             'Help'    : XDISPLAYFILE, !MFIT.HELP_PATH + 'rmfit.hlp', DONE = 'Done'

             'Tutorial': XDISPLAYFILE, !MFIT.HELP_PATH + 'help.hlp', DONE = 'Done'

             'About'   : self->banner, /DISMISS, PARENT = event.top

             'Load'    : self->load, PARENT = event.top

             'Display' : self->show, 1

             'Hide'    : self->show, 0 

             'Delete'  : self->delete

             '* Display On Load' : BEGIN
                 WIDGET_CONTROL, event.id, GET_UVALUE = uvalue
                 WIDGET_CONTROL, event.id, SET_VALUE = uvalue, SET_UVALUE=value
                 self.displayOnLoad = 0
                 END
                                  
             '  Display On Load' : BEGIN
                 WIDGET_CONTROL, event.id, GET_UVALUE = uvalue
                 WIDGET_CONTROL, event.id, SET_VALUE = uvalue, SET_UVALUE=value
                 self.displayOnLoad = 1
                 END

             '* Auto Read Lookup' : BEGIN
                 WIDGET_CONTROL, event.id, GET_UVALUE = uvalue
                 WIDGET_CONTROL, event.id, SET_VALUE = uvalue, SET_UVALUE=value
                 self.autoLookup = 0
                 END
             
             '  Auto Read Lookup' : BEGIN
                 WIDGET_CONTROL, event.id, GET_UVALUE = uvalue
                 WIDGET_CONTROL, event.id, SET_VALUE = uvalue, SET_UVALUE=value
                 self.autoLookup = 1
                 END

             ELSE : MESSAGE, /INFO, 'Button event not handled: ' + value

        ENDCASE
        END

    'WIDGET_LIST': BEGIN

        IF (event.clicks EQ 2) THEN self->show, 1
        
        END

    ELSE : MESSAGE, /INFO, 'Widget event not handled: ' + type

    ENDCASE ; type

END


;------------------------------------------------------------------------------
; Build the GUI
;------------------------------------------------------------------------------
PRO RMFit::build

    ; COMPILE_OPT HIDDEN

    topBase = WIDGET_BASE (TITLE = 'rmfit ' + self.version, /COLUMN, $
        MBAR = menuBar, /BASE_ALIGN_CENTER, /TLB_KILL_REQUEST_EVENTS)
        
    self.topID = topBase
     
    menu = WIDGET_BUTTON (menuBar, VALUE = 'File', /MENU)
        m = WIDGET_BUTTON (menu, VALUE = 'Quit')
        m = WIDGET_BUTTON (menu, VALUE = 'Exit IDL')

    menu = WIDGET_BUTTON (menuBar, VALUE = 'Options', /MENU)
        m = WIDGET_BUTTON (menu, $
            VALUE = '* Display On Load', UVALUE = '  Display On Load')
        m = WIDGET_BUTTON (menu, $
            VALUE = '* Auto Read Lookup', UVALUE = '  Auto Read Lookup')

    menu = WIDGET_BUTTON (menuBar, VALUE = 'Help', /HELP, /MENU)
        m = WIDGET_BUTTON (menu, VALUE = 'Help')
        m = WIDGET_BUTTON (menu, VALUE = 'Tutorial')
        m = WIDGET_BUTTON (menu, VALUE = 'About', /SEPARATOR)
    
    self.listID = WIDGET_LIST (topBase, XSIZE = 60, YSIZE = 8, /MULTIPLE)
    
    buttonBase = WIDGET_BASE (topBase, ROW = 1, /GRID_LAYOUT)
    w = WIDGET_BUTTON (buttonBase, VALUE = 'Load')
    w = WIDGET_BUTTON (buttonBase, VALUE = 'Display')
    w = WIDGET_BUTTON (buttonBase, VALUE = 'Hide')
    w = WIDGET_BUTTON (buttonBase, VALUE = 'Delete')

    ;== Realize
    
    WIDGET_CONTROL, topBase, /REALIZE

    ;== Import new data
    
    RESTORE, !MFIT.script_file                                                                    ; AMG
    IF script THEN self->load, PARENT = topBASE, FILENAME=lcFile ELSE $                           ;
    self->load, PARENT = topBase

    ;== Store the object reference on the display widget
    
    WIDGET_CONTROL, topBase, SET_UVALUE = self, SET_UNAME = 'rmfit'

    ;== Start event loop
    
    XMANAGER, OBJ_CLASS (self), topBase, /NO_BLOCK

END


;----------------------------------------------------------------------------
; Show dataset(s)
;----------------------------------------------------------------------------
PRO RMFit::show, doShow

    ; COMPILE_OPT HIDDEN

    IF (N_ELEMENTS (doShow) EQ 0) THEN doShow = 1
    
    idx = WIDGET_INFO (self.listID, /LIST_SELECT)
    IF (idx[0] EQ -1) THEN RETURN
    
    names = self.List->names ()
    
    FOR i = 0, N_ELEMENTS (idx) - 1 DO BEGIN
    
        ;== Get the name selected in the list
        
        name = names[idx[i]]
        
        ;== Get the object reference from the list
        
        obj = self.List->data (name, ERROR = error)
        IF (error) THEN BEGIN
            
           MESSAGE, /CONTINUE, 'ERROR - data not found in List: ' + name
    
        ENDIF ELSE BEGIN
        
           obj->map, doShow
           
        ENDELSE
        
    ENDFOR
        
END


;----------------------------------------------------------------------------
; Delete dataset(s)
;----------------------------------------------------------------------------
PRO RMFit::delete, ALL = all

    ; COMPILE_OPT HIDDEN

    ;== No data in list?    
    
    IF (self.List->count () EQ 0) THEN RETURN

    ;== Delete all datasets
    
    IF (KEYWORD_SET (all)) THEN BEGIN
       
       ;== Select all current list entries 
    
       n = WIDGET_INFO (self.listID, /LIST_NUMBER)
       IF (n GT 0) THEN BEGIN
          v = INDGEN (n)
          WIDGET_CONTROL, self.listID, SET_LIST_SELECT = v
       ENDIF
       
    ENDIF
  
    idx = WIDGET_INFO (self.listID, /LIST_SELECT)
    IF (idx[0] EQ -1) THEN RETURN
    
    names = self.List->names ()
    newNames = names
    
    FOR i = 0, N_ELEMENTS (idx) - 1 DO BEGIN
    
        ;== Get the name selected in the list
        
        name = names[idx[i]]
        
        ;== Get the object reference from the list
        
        obj = self.List->data (name, ERROR = error)
        IF (error) THEN BEGIN
            
           MESSAGE, /CONTINUE, 'ERROR - data not found in List: ' + name
    
        ENDIF ELSE BEGIN
           
           OBJ_DESTROY, obj
           self.List->delete, name
           
           index = WHERE (newNames NE name, cnt)
           IF (cnt NE 0) THEN newNames = newNames[index]

        ENDELSE
        
    ENDFOR
    
    count = self.List->count ()
    IF (count EQ 0) THEN newNames = ''

    WIDGET_CONTROL, self.ListID, SET_VALUE = newNames
    
    ;== If the fitter is there, update the dataset menu
    
    IF OBJ_VALID (self.fitter) THEN BEGIN
        IF OBJ_VALID ((self.fitter).display) THEN (self.fitter).display->updateDataMenu
        ENDIF

    ;== If only a single data entry exists, select it by default
    
    IF (count EQ 1) THEN $
       WIDGET_CONTROL, self.listID, SET_LIST_SELECT = 0
    
END


;------------------------------------------------------------------------------
; About widget.  If TIMER is set to an integer number of seconds, the 
; widget is destroyed automatically after TIMER seconds have elapsed. 
;------------------------------------------------------------------------------
PRO RMFit::banner, topBase, LABEL = label, PARENT = parent, TIMER = timer, $
    DISMISS = dismiss

    ; COMPILE_OPT HIDDEN
    
    @banner_text.pro

;    text = [ $
;        
;        'RMFIT ' + self.version,                      $
;        'A Lightcurve and Spectral Analysis Tool',    $ 
;        '   ',                                        $
;        'Robert S. Mallozzi',                         $
;        'Robert D. Preece',                           $
;        'Michael S. Briggs',                          $
;        '   ',                                        $
;        'UNIVERSITY OF ALABAMA HUNTSVILLE',           $
;        '   ',                                        $
;        'Copyright (C) 2000 Robert S. Mallozzi',      $
;        'Portions (C) 2008 Robert D. Preece',         $
;        'This program is free software; you can',     $
;        'redistribute it and/or modify it under the', $
;        'terms of the GNU General Public License as', $
;        'published by the Free Software Foundation.', $
;        '   '                                         $         
;        ]
    text[0] = text[0] + ' version ' + self.version

    IF (N_ELEMENTS (label) NE 0) THEN $
       text = [text, label]

    haveParent = (N_ELEMENTS (parent) NE 0)
      
    IF (haveParent) THEN BEGIN
       topBase = WIDGET_BASE (TITLE = ' ', /COLUMN, $
           TLB_FRAME_ATTR = 1, /MODAL, /FLOATING, GROUP_LEADER = parent) 
    ENDIF ELSE BEGIN
       topBase = WIDGET_BASE (TITLE = ' ', /COLUMN, MAP = 0, $
           TLB_FRAME_ATTR = 1)
    ENDELSE

    base = WIDGET_BASE (topBase, /COLUMN, /FRAME)

    FOR i = 0, N_ELEMENTS (text) - 1 DO $
        w = WIDGET_LABEL (base, VALUE = text[i])

    IF KEYWORD_SET (dismiss) THEN $
        dismissID = WIDGET_BUTTON (topBase, VALUE = 'Dismiss')

    IF (NOT haveParent) THEN $
       DIALOG_PLACE, topBase, /CENTER    
    
    WIDGET_CONTROL, topBase, /REALIZE 
    
    IF (N_ELEMENTS (timer) NE 0) THEN BEGIN
        
       FOR i = 0, timer - 1 DO BEGIN
       
           evt = WIDGET_EVENT (dismissID, /NOWAIT, /SAVE)
           IF (evt.id EQ dismissID) THEN BEGIN
              WIDGET_CONTROL, topBase, /DESTROY
              RETURN
           ENDIF
       
           WAIT, 1
           
       ENDFOR    
    
       WIDGET_CONTROL, topBase, /DESTROY

    ENDIF ELSE BEGIN   
       IF KEYWORD_SET (dismiss) THEN $
           XMANAGER, '__unused', topBase, EVENT_HANDLER = 'DIALOG_DISMISS'    
       RETURN

    ENDELSE
    
END


; ----------------------------------------------------------------------------
; Allow user to select filename(s)
; ----------------------------------------------------------------------------
FUNCTION RMFit::selectFile, $
    TITLE = title, PARENT = parent, FILTER = filter, COUNT = count

    ; COMPILE_OPT HIDDEN

	; 12/22/09 RDP CCR#223: Added filter for RSP type II files:
    filterArr = ['*.fit', '*.pha', '*.fits', '*.bfits', '*.bak', '*.rsp', '*.rsp2', '*.drm', '*.rmf', '*.par', '*.lu']
    filterArr = [filterArr, STRUPCASE(filterArr)]

    IF (N_ELEMENTS (filter) NE 0) THEN BEGIN
       fidx = WHERE(filterArr NE filter)
       filterArr = [filter, filterArr[fidx]]
    ENDIF ELSE BEGIN
       fidx = WHERE(filterArr NE self.lastFilter)
       filterArr = [self.lastFilter, filterArr[fidx]]
    ENDELSE
    

    fileList = DIALOG_PICKFILE (TITLE = title, $
        PATH = self.lastPath, GET_PATH = lastPath, /READ, $
        DIALOG_PARENT = parent, FILTER = filterArr, /MULTIPLE)

    count = 0L
    IF (fileList[0] NE '') THEN BEGIN

       ;== DIALOG_PICKFILE returns a directory if the user does not
       ;== select a file, but clicks the 'Ok' button
       OPENR, FL, fileList[0], /GET_LUN, ERROR = error
       IF (error NE 0) THEN BEGIN
          count = 0L
          RETURN, ''       
       ENDIF
	   CLOSE, fl
       FREE_LUN, FL
       
       count = N_ELEMENTS (fileList)
       
	   myFile = OBJ_NEW ('File', fileList[0])
	   myExt = myfile->get(/EXTENSION)
	   OBJ_DESTROY, myfile
	
       self.lastFilter = '*' + STRTRIM (myExt, 2)
       
       self.lastPath = lastPath
       !RM_PARAMS.lastpath = lastpath
    
    ENDIF

    RETURN, fileList
     
END


;----------------------------------------------------------------------------
; Load new dataset(s)
;----------------------------------------------------------------------------
PRO RMFit::load, PARENT = parent, FILENAME = filename
  
    ; COMPILE_OPT HIDDEN

    ;== Get user's selection(s)
    
    IF NOT (KEYWORD_SET(FILENAME)) THEN BEGIN
        filename = self->selectFile (PARENT = parent, COUNT = numRequested, $
            TITLE = 'Select File(s) for Reading')        
        IF (numRequested EQ 0) THEN RETURN
    ENDIF ELSE BEGIN
        numRequested = N_ELEMENTS(filename)                                                       ; AMG
        ENDELSE
    
    ;== If loading a lot of datasets, confirm that the user
    ;== really wants to display each dataset on load

    doDisplay = self.displayOnLoad 
    
    RESTORE, !MFIT.script_file                                                                    ; AMG
    IF (script AND KEYWORD_SET(scriptVerbose)) THEN doDisplay=1                                   ;
    IF (script AND NOT KEYWORD_SET(scriptVerbose)) THEN doDisplay=0                               ;
    IF ((doDisplay) AND (numRequested GT 7)) AND (NOT script) THEN BEGIN                          ;

    IF (DIALOG_MESSAGE (/QUESTION, /DEFAULT_NO, $
	  ['You have "Options->Display On Load" enabled.', $
	   'Really display all datasets while loading?', $
	   ' ', $
	   'Select "Yes" to load and display all', $
       'datasets, or "No" to load data only.'], $
	   DIALOG_PARENT = self.topID) EQ 'No') THEN $
          doDisplay = 0

    ENDIF

    ;== Show a progress bar is loading more than 5 datasets
    
    showProgress = (numRequested GE 5)
    IF (showProgress) THEN $
       Progress = OBJ_NEW ('Widget_Progress', $
           CANCEL_LABEL = 'Interrupt', TITLE = 'Loading Data...')

    ;== Load each dataset
    
    count = 0.0
    FOR i = 0, numRequested - 1 DO BEGIN
        RESTORE, !MFIT.script_file
        IF script THEN BEGIN                                                                      ; AMG
          phaNum=i+1                                                                              ;
          SAVE, file=!MFIT.script_file, script, lcFile, rspFile, luFile, phaNum, model, vals, $   ;
                statistic, eInterval, numfits, iter, scriptVerbose, table                         ;
        ENDIF                                                                                     ;
        
        ;== PhaDisplay files are the only ones that are handled right now
        myFile = filename[i]
        myObj = self->parseFITS (myFile, myReader, myResponse, myLookup)
	
		IF (myObj EQ 'Error') THEN RETURN ;Very bad error: no AstroLib?
        
        WIDGET_CONTROL, /HOURGLASS 
        
        IF (myObj EQ 'MFITDisplay') THEN BEGIN
            ;obj = OBJ_NEW (myObj, FILENAME = filename[i], MAP = doDisplay)
            (self.fitter).display->readFit, FILENAME = filename[i], /junk
        ENDIF ELSE BEGIN
            obj = OBJ_NEW (myObj, myFile, $
                MAP = doDisplay, READER = myReader, RESPONSE = myResponse, $
                LOOKUP = self.autoLookup, LUFILE = myLookup, MYRMFIT = self)
        ENDELSE
        
        IF (OBJ_VALID (obj)) THEN BEGIN
        
           self.List->add, filename[i], obj

           names = self.List->names ()
           WIDGET_CONTROL, self.listID, SET_VALUE = names
        
        ENDIF 
                     
        IF (showProgress) THEN BEGIN

           count = count + 1.0
           fraction = count / numRequested
           Progress->update, fraction

           IF (Progress->cancel ()) THEN $
              GOTO, ABORT

        ENDIF

    ENDFOR
    
ABORT:

    IF (showProgress) THEN $
       OBJ_DESTROY, Progress
  
    ;== If only a single data entry exists, select it by default

    IF (self.List->count () EQ 1) THEN $
       WIDGET_CONTROL, self.listID, SET_LIST_SELECT = 0

    RESTORE, !MFIT.script_file                                                                     ; AMG
    IF script AND iter GT 0 THEN BEGIN                                                             ;
      self->delete, /ALL                                                                           ;
    ENDIF                                                                                          ;

END


;------------------------------------------------------------------------------
; Parse the FITS file for type
; Add CASE branches as RMFIT adds the capability to handle them
;------------------------------------------------------------------------------
FUNCTION RMFit::parseFITS, filename, reader, response, lookup

    CATCH, Error
    IF Error NE 0 THEN BEGIN
	    print, 'Error status: ', Error
	    print, 'Error string: ', !ERR_STRING
	    msg = "Can't read the FITS file. Perhaps you need to install the "
	    msg = [msg, "IDL Astronomy Users' Library: http://idlastro.gsfc.nasa.gov"]
	    junk = DIALOG_MESSAGE (/ERROR, msg)
	    error = 0
	    RETURN, 'Error'
    ENDIF
            
    result = ''
    reader = 'BfitsReader'
    response = 'BfitsResponse'
    
    ;== Check that we're not trying to read a lookup file:
	myFile = OBJ_NEW ('File', filename)
	myExt = myfile->get(/EXTENSION)
	myBase = myfile->get(/ROOT)
	myPath = myFile->get(/PATH)
	OBJ_DESTROY, myfile
	
	;== Read in the associated data file instead:
	lookup = ''
	IF (myExt EQ ".lu") THEN BEGIN
		tempF = STRMID(myBase, 0, STRLEN(myBase) - 2) + "??"
		c = file_search (myPath + tempF + ".pha", COUNT = cnt)
		IF cnt GE 1 THEN BEGIN
			lookup = filename
			filename = c[cnt - 1]
		ENDIF ELSE BEGIN
			c = file_search (myPath + tempF + ".fit", COUNT = cnt)
			IF cnt GE 1 THEN BEGIN
				lookup = filename
				filename = c[cnt - 1]
			ENDIF
		ENDELSE
	ENDIF

    header = HEADFITS (filename)
    fileType = STRTRIM(SXPAR (header, 'FILETYPE'), 2)
    ; Kludge for Andrew's file type:
    IF STRTRIM(fileType, 2) EQ 'GBM DRM' THEN fileType = 'SPECRESP MATRIX'
    IF STRTRIM(fileType, 2) EQ 'GBM BACK' THEN fileType = 'SPECTRUM'
    IF STRTRIM(STRING(filetype), 2) EQ '0' THEN BEGIN   ;== Possibly a .PHA or .rmf file
    	;== Check for PHA type II:
    	
        jj = 1
        found = 0
        REPEAT BEGIN
            newHDR = HEADFITS (filename, EXTEN = jj)
            extName = STRTRIM(SXPAR (newHDR, 'EXTNAME'), 2)
            IF (extName EQ 'SPECRESP MATRIX') OR (extName EQ 'EBOUNDS') OR $
                (extName EQ 'MATRIX') OR (extName EQ 'SPI.-RMF1-RSP') OR $
                (extName EQ 'ISRG-RMF1-RSP') THEN BEGIN
                    fileType = 'SPECRESP MATRIX'
                    found = 1
            ENDIF ELSE IF (extName EQ 'SPECTRUM') OR (extName EQ 'PHOTON_COUNTS')  THEN BEGIN
            	phaType = STRTRIM(SXPAR (newHDR, 'HDUCLAS3', COUNT = hcnt), 2)
            	IF (hcnt GT 0) THEN BEGIN
            		IF (phaType EQ 'TYPE:II') THEN BEGIN
						fileType = 'PHAII'
					ENDIF ELSE fileType = 'SPECTRUM'
            	ENDIF ELSE BEGIN
					fileType = 'SPECTRUM'
                ENDELSE
                found = 1
            ENDIF ELSE IF (extName EQ 'GROUPING') THEN BEGIN
                ;== May be an INTEGRAL PHA file
                newHDR = HEADFITS (filename, EXTEN = 2)
                extName = STRTRIM(SXPAR (newHDR, 'EXTNAME'), 2)
                IF (extName EQ 'SPI.-PHA1-SPE') OR (extName EQ 'ISRG-PHA1-SPE') OR $
                   (extName EQ 'SPECTRUM') THEN BEGIN
                    fileType = 'SPECTRUM'
                    found = 1
                ENDIF
            ENDIF
            jj += 1
        ENDREP UNTIL (N_ELEMENTS(newHDR) EQ 1) OR found
    ENDIF
;print, 'My file type:' + filetype + ':'    
    CASE fileType OF 
    
        'BATSE BURST SPECTRA'  : result = 'PhaDisplay'
        'SPECTRAL FITS'        : result = 'MFITDisplay'
        'BATSE_DRM'            : result = 'DRMDisplay'
        'SPECRESP MATRIX'      : BEGIN
             result = 'DRMDisplay'
             reader = 'NULL'
             response = 'rmfResponse'
             END
        'PHAII'                : BEGIN
             result = 'PhaDisplay'
             reader = 'PHAIIReader'
             response = 'rmfResponse'
             END
        'GBM PHOTON LIST'      : BEGIN
             result = 'PhaDisplay'
             reader = 'TTEReader'
             response = 'rmfResponse'
             END
        'SPECTRUM'             : BEGIN
             result = 'SpectrumDisplay'
             reader = 'PHAReader'
             response = 'rmfResponse'
             END
        'TRIGDAT'              : BEGIN
             result = 'TRIGDisplay'
             reader = 'TRIGReader'
             response = 'rmfResponse'
             END
        'BATSE_CONT'           : BEGIN
             result = 'PhaDisplay'
             reader = 'CDFReader'
             END
        'BATSE_DISCLA'         : BEGIN
             result = 'PhaDisplay'
             reader = 'CDFReader'
             END
        'BATSE_HER_COR'        : BEGIN
             result = 'PhaDisplay'
             reader = 'HERSHERReader'
             END
        'BATSE_SHER_COR'       : BEGIN
             result = 'PhaDisplay'
             reader = 'HERSHERReader'
             END
         ELSE: BEGIN
             result   = 'Error'
             reader   = 'NULL'
             response = 'NULL'
             END
    
    ENDCASE
    
    RETURN, result
    
END       


;------------------------------------------------------------------------------
; Update the widget status text
;------------------------------------------------------------------------------
PRO RMFit::setStatus, message, CLEAR = clear

    ; COMPILE_OPT HIDDEN

    IF (KEYWORD_SET (clear)) THEN $
       message = ' '
       
    WIDGET_CONTROL, self.statusID, SET_VALUE = STRING (message)

END       


;------------------------------------------------------------------------------
; Print session statistics
;------------------------------------------------------------------------------
PRO RMFit::statistics

    ; COMPILE_OPT HIDDEN

    HELP, /MEMORY, OUTPUT = memInfo

    IF (FLOAT (!VERSION.RELEASE) GE 5.3) THEN BEGIN
       memInfo = CALL_FUNCTION ('STRSPLIT', memInfo[0], ',', /EXTRACT)
    ENDIF ELSE BEGIN
       memInfo = STR_SEP (memInfo[0], ',')
    ENDELSE
    
    PRINT

    PRINT, 'Elapsed time = ' + $
        STRTRIM (STRING (SYSTIME (1) - self.startTime, F = '(F15.2)'), 2) + ' s'

    PRINT, 'Memory usage (bytes)'
    FOR i = 0, N_ELEMENTS (memInfo) - 1 DO $
        PRINT, '    ' + STRTRIM (STRCOMPRESS (memInfo[i]), 2)

    PRINT

END


;------------------------------------------------------------------------------
; Object definition
;------------------------------------------------------------------------------
PRO RMFit__define

    obj = { RMFIT, $
        
        ;== Software version
        
        version:  '', $
        
        ;== Random access storage list to hold dataset object references
        
        List     : OBJ_NEW (), $
        
        ;== Singleton objects that must be destroyed only once
        Color    : OBJ_NEW (), $
        Fitter   : OBJ_NEW (), $

        ;== User options
         
        displayOnLoad : 0, $
        autoLookup    : 0, $ 

        ;== Widget IDs
        
        topID    : 0L, $        
        listID   : 0L, $
        statusID : 0L, $

        ;== Last path and filter used in the file selection box
        
        lastPath  : '', $
        lastFilter: '', $

        ;== Track the time used in the session
        
        startTime: 0.0D $

    }

END
