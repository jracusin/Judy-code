; ----------------------------------------------------------------------------
;+
; NAME:
;
;     DRMDisplay (OBJECT)
;
; PURPOSE:
;
;     An object for interactive manipulation of a BATSE DRM.
;
; CALLING SEQUENCE:
;
;     obj = OBJ_NEW ('DRMDisplay', filename)
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
;     Written, 1999 Fall, Robert.Mallozzi@msfc.nasa.gov
;
;-
; ----------------------------------------------------------------------------



; ----------------------------------------------------------------------------
; Constructor
; ----------------------------------------------------------------------------
FUNCTION DRMDisplay::init, filename, RESPONSE = response, MAP = map, _EXTRA = extra
        
    ;== Verify input
        
    IF (N_ELEMENTS (filename) EQ 0) THEN BEGIN   
       MESSAGE, /CONTINUE, 'Incorrect number of arguments.'
       RETURN, 0
    ENDIF

    IF (NOT FILE_EXISTS (filename)) THEN BEGIN   
       MESSAGE, /CONTINUE, 'File not found: ' + STRING (filename)
       RETURN, 0
    ENDIF

    self.File = OBJ_NEW ('File', filename)
       
    IF (N_ELEMENTS (lookup) EQ 0) THEN lookup = 1
    IF (N_ELEMENTS (map) EQ 0)    THEN map = 1
        
    ;== Create a new BFITS response object
    IF NOT KEYWORD_SET(RESPONSE) THEN RESPONSE = 'BfitsResponse'
    self.Response = OBJ_NEW (RESPONSE, filename, /READ, _EXTRA = extra)

    ;== Extract some object references from the Detector for convenience
    
    IF NOT (OBJ_VALID (self.Response)) THEN BEGIN
        
       MESSAGE, /CONTINUE, 'You have not chosen a DRM FITS file.'
       RETURN, 0
    
    ENDIF

    ;== Create the display object
    
    IF (NOT self->Display::init (MAP = 0, /NO_DEFAULT_MENUS, $
        TITLE = self.File->get (/BASENAME), _EXTRA = extra)) THEN BEGIN
       
       MESSAGE, /CONTINUE, 'Display initialization failed.'
       RETURN, 0
    
    ENDIF    

    ;== Set the display
    
    ok = self->PLOTTER::INIT (self)
    
    ;== Set default plotting colors
    
    self->setColors, /INITIALIZE
    
    ;== Start out with standard display:
    
    self.flip = 0L

    ;== Initial color table:
    
    self.colorTable = 0

    ;== Build the interface
    
    self->buildGUI, MAP = map
    
    ;== Plot the response
        
    self->plot, _EXTRA = *self.PlotOptions

    self->setStatus, filename
    
    RETURN, 1

END

   
; ----------------------------------------------------------------------------
; Destructor
; ----------------------------------------------------------------------------
PRO DRMDisplay::cleanup

    ;== Save the color info
    
    !RM_PARAMS.colors = self.colors
    
    OBJ_DESTROY, self.Response
    OBJ_DESTROY, self.File
    PTR_FREE, self.colorModel
    
    self->Plotter::cleanup          
    self->Display::cleanup

END


; ----------------------------------------------------------------------------
; Build the display widget
; ----------------------------------------------------------------------------
PRO DRMDisplay::buildGUI, MAP = map

    ; COMPILE_OPT HIDDEN

    map = KEYWORD_SET (map)
         
    ;== Menubar
    
    menu = self->addMenu (VALUE = 'File')
        id = menu->add (VALUE = 'Print')
        ;id = menu->add (VALUE = 'Print Setup...')
        id = Menu->add (VALUE = 'PS Configure...')
        id = menu->add (VALUE = 'Screenshot')                                                       ; AMG

        subMenu = menu->add (VALUE = 'Headers', /MENU)
            id = menu->add (subMenu, VALUE = 'Primary  (EXT 0)')
            id = menu->add (subMenu, VALUE = 'EBOUNDS  (EXT 1)')
            id = menu->add (subMenu, VALUE = 'SPECRESP (EXT 2)')

        id = menu->add (VALUE = 'Dismiss', /SEPARATOR)
   
    menu = self->addMenu (VALUE = 'Options')
        subMenu = menu->add (VALUE = 'Show rmfit Window')
        subMenu = menu->add (VALUE = 'Refresh Plot')
        subMenu = menu->add (VALUE = 'Colors', /MENU)
            id = menu->add (subMenu, VALUE = 'Load Color Table')
            id = menu->add (subMenu, VALUE = 'Background')
            id = menu->add (subMenu, VALUE = 'Foreground')
            id = menu->add (subMenu, VALUE = 'History')
            id = menu->add (subMenu, VALUE = 'Spectrum')
            id = menu->add (subMenu, VALUE = 'Background Model')
        subMenu = menu->add (VALUE = 'Plot Configuration')
        
    menu = self->addMenu (VALUE = 'Help', /HELP)
        id = menu->add (VALUE = 'Help')
        id = menu->add (VALUE = 'Show Keyboard Bindings')
        id = menu->add (VALUE = 'About', /SEPARATOR)


    ;== Application buttons
     
    menu = self->addMenuButton (VALUE = 'Zoom:', /TEAROFF)
        id = menu->add (VALUE = 'Zoom')
        id = menu->add (VALUE = 'X Zoom')
        id = menu->add (VALUE = 'Y Zoom')
        id = menu->add (VALUE = 'Zoom In: Selection')
        id = menu->add (VALUE = 'Zoom Out: Full Range')
        id = menu->add (VALUE = 'Full Screen')
        
    id = self->addButton('Flip Axes')

    label = self->addLabel ('Color Options:')
	colorChoice = ['Lines', 'Colors']
	self.showColorID = CW_BGROUP (self.widgetID.buttonBase, colorChoice, /ROW, $
            /EXCLUSIVE, BUTTON_UVALUE=colorChoice, SET_VALUE = 1)

    ;== Add a text message in the status bar
    
    self->setStatus, 'Display initialized' ;, 5, /REVERT
     
    ;== Now that the custom widget is build, map it to the screen
       
    IF (map) THEN self->map
        
END


; ----------------------------------------------------------------------------
; Override the widget event handler
; ----------------------------------------------------------------------------
PRO DRMDisplay::eventHandler, event

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
                'Screenshot': BEGIN                                                             ; AMG
                                                                                                ;
                             file=DIALOG_PICKFILE(DEFAULT_EXTENSION='png',  $                   ;
                                                  PATH=!RM_PARAMS.lastPath, $                   ;
                                                  FILE='drm.png',           $                   ;
                                                  /WRITE)                                       ;
                             IF file EQ '' THEN RETURN                                          ;
                             image=TVRD(/TRUE)                                                  ;
                             TVLCT,r,g,b,/GET                                                   ;
                             WRITE_PNG, file, image, r, g, b                                    ;
                                                                                                ;
                           END                                                                  ;

                'Primary  (EXT 0)'   : self.Response->showHeader, 0
                'EBOUNDS  (EXT 1)'   : self.Response->showHeader, 1
                'SPECRESP (EXT 2)'   : self.Response->showHeader, 2

                'Show rmfit Window'  : WIDGET_CONTROL, (self.myrmfit).topID, /show                 
                
                'Show Keyboard Bindings'  : self->showKeyBindings
                'Refresh Plot'            : self->plot, _EXTRA = *self.PlotOptions

                ;---------------
                ; COLORS
                ;---------------  
                    
                'Load Color Table'   : BEGIN
                     XLOADCT, /USE_CURRENT, UPDATECALLBACK =  'updateCT', $
                         UPDATECBDATA = self
                     END
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
                ; ZOOM EVENTS
                ;---------------  

                'Zoom' : BEGIN
                    self->disable
                    self->setStatus, $
                       'Select zoom region.  ' + $
                       'Use margins for special functions.' 
                    self->zoom            
                    ;self->clearStatus
                    self->enable
                    END

                'X Zoom' : BEGIN
                    self->disable
                    self->setStatus, $
                       'Select zoom interval.  ' + $
                       'Use margins for special functions.' 
                    self->zoom, /X
                    ;self->clearStatus
                    self->enable
                    END

                'Y Zoom' : BEGIN
                    self->disable
                    self->setStatus, $
                       'Select zoom interval.  ' + $
                       'Use margins for special functions.' 
                    self->zoom, /Y
                    ;self->clearStatus
                    self->enable
                    END

                'Zoom Out: Full Range' : self->zoom, /DEFAULTRANGE            
                ;'Zoom In: Selection'   : self->zoom, /SELECTION
                'Full Screen'          : self->zoom, /FULLSCREEN
                
                'Flip Axes'            : BEGIN
                     self.flip = self.flip XOR 1L
                     self->plot, _EXTRA = *self.PlotOptions
                     END
                     
                'Lines'  : self->plot, _EXTRA = *self.PlotOptions


                ;---------------
                ; INFO
                ;---------------  
                
                'Help'  : self->info, /HELP
                'About' : self->info, /ABOUT                    

                ELSE: ;self->setStatus, 'Event not handled: ' + value, 2

            ENDCASE
            
            END ; WIDGET_BUTTON events
        

        'WIDGET_TEXT_CH': self->keyBindings, event.ch

        ELSE: BEGIN

            ;== Handle the radio buttons; name is *not* returned in 'value'
            IF event.id EQ self.showColorID THEN BEGIN
                CASE event.value OF
                    'Lines'   : self->plot, _EXTRA = *self.PlotOptions
                    'Colors'  : self->plot, _EXTRA = *self.PlotOptions                    
                    ELSE: ;self->setStatus, 'Event not handled: ' + value, 2
                ENDCASE
            ENDIF
            
            END; ;self->setStatus, 'Event not handled.' , 2
                       
    ENDCASE
      
END


; ----------------------------------------------------------------------------
; Help and About information
; ----------------------------------------------------------------------------
;PRO DRMDisplay::info, HELP = help, ABOUT = about
;    
;     COMPILE_OPT HIDDEN
;
;    IF (KEYWORD_SET (help)) THEN BEGIN
;
;       d = DIALOG_MESSAGE (/INFO, DIALOG_PARENT = self.widgetID.top, $
;           TITLE = 'Help', 'No Help Available!')
;    
;    ENDIF
;
;    IF (KEYWORD_SET (about)) THEN BEGIN
;
;       @banner_text.pro
;
;       d = DIALOG_MESSAGE (/INFO, DIALOG_PARENT = self.widgetID.top, $
;           TITLE = 'About', text)
;    
;    ENDIF
;
;
;END

 
; ----------------------------------------------------------------------------
; Handle keyboard events in the drawing area
; ----------------------------------------------------------------------------
PRO DRMDisplay::keyBindings, char
    
    ; COMPILE_OPT HIDDEN

    setXRange = 0
    setYRange = 0

    CASE (STRING (char)) OF

         'd' : WIDGET_CONTROL, self.widgetID.top, MAP = 0
         
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
         
         'r' : self->Plot, _EXTRA = *self.PlotOptions
         
         'f' : self->zoom, /FULLSCREEN
         
         ',' : BEGIN ; zoom in by steps: x-axis
             xr = SCALEAXIS (/XAXIS, -0.05) 
             yr = AXISRANGE (/YAXIS)
             setXRange = 1
             END

         '.' : BEGIN ; zoom out by steps: x-axis
             xr = SCALEAXIS (/XAXIS, 0.05) 
             yr = AXISRANGE (/YAXIS)
             setXRange = 1
             END

         'k' : BEGIN ; pan left: x-axis
             xr = PANAXIS (/XAXIS, 0.03) 
             yr = AXISRANGE (/YAXIS)
             setXRange = 1
             END

         'l' : BEGIN ; pan right: x-axis
             xr = PANAXIS (/XAXIS, -0.03) 
             yr = AXISRANGE (/YAXIS)
             setXRange = 1
             END

         '<' : BEGIN ; zoom in by steps: y-axis
             yr = SCALEAXIS (/YAXIS, -0.05) 
             xr = AXISRANGE (/XAXIS)
             setYRange = 1
             END

         '>' : BEGIN ; zoom out by steps: y-axis
             yr = SCALEAXIS (/YAXIS, 0.05) 
             xr = AXISRANGE (/XAXIS)
             setYRange = 1
             END

         'K' : BEGIN ; pan left: y-axis
             yr = PANAXIS (/YAXIS, 0.03) 
             xr = AXISRANGE (/XAXIS)
             setYRange = 1
             END

         'L' : BEGIN ; pan right: y-axis
             yr = PANAXIS (/YAXIS, -0.03) 
             xr = AXISRANGE (/XAXIS)
             setYRange = 1
             END

         ELSE: 
         
    ENDCASE
         
    IF ((setXRange) OR (setYRange)) THEN BEGIN
    
       ;self->adjustRanges, xr, yr, 'DRMPlot', /XLOG, /YLOG
 
       self->plot, _EXTRA = *self.PlotOptions, $
           XRANGE = xr, XSTYLE = setXRange, $
           YRANGE = yr, YSTYLE = setYRange
           
       ;self->finishRanges, 'DRMPlot', /XLOG, /YLOG

    ENDIF
    
END


; ----------------------------------------------------------------------------
; Display available key bindings
; ----------------------------------------------------------------------------
PRO DRMDisplay::showKeyBindings

    ; COMPILE_OPT HIDDEN

    txt = [ $
    
        'd : Dismiss (unmap) the display widget', $         
        'z : Enter interactive zoom mode', $      
        'x : Zoom mode restricted to the x-axis', $   
        'y : Zoom mode restricted to the y-axis', $   
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
        'L : Pan right : y-axis'  $
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
; Get the color model.
; ----------------------------------------------------------------------------
PRO DRMDisplay::getColorModel, r, g, b

    model = *self.colorModel
    r = REFORM(model[0, *])
    g = REFORM(model[1, *])
    b = REFORM(model[2, *])
   
END

 
; ----------------------------------------------------------------------------
; Set the color model.
; ----------------------------------------------------------------------------
PRO DRMDisplay::setColorModel, model

    PTR_FREE, self.colorModel
    myModel = TRANSPOSE(REFORM(model, N_ELEMENTS(model) / 3, 3))
    self.colorModel = PTR_NEW(myModel)
   
END

 
; ----------------------------------------------------------------------------
; Change the color table.
; ----------------------------------------------------------------------------
PRO updateCT, DATA = data 

    TVLCT, nred, ngreen, nblue, /GET
    data->setColorModel, [nred, ngreen, nblue]
    data->plot
   
END

 
; ----------------------------------------------------------------------------
; Override the plot method.
; ----------------------------------------------------------------------------
PRO DRMDisplay::plot, HARDCOPY = hardcopy, _EXTRA = extra
    
    ; COMPILE_OPT HIDDEN

    hardcopy = KEYWORD_SET (hardcopy)
    
    ;== Plot to offscreen pixmap    
    
    IF (NOT hardcopy) THEN BEGIN
       win = !D.WINDOW
       self->setPixWindow
    ENDIF
     
    WIDGET_CONTROL, self.showColorID, GET_VALUE = colorChoice 

    TVLCT, red, green, blue, /GET
    IF colorChoice THEN BEGIN
        IF NOT self.colorTable THEN BEGIN
            LOADCT, 8, /SILENT 
            self.colorTable = 1
            TVLCT, nred, ngreen, nblue, /GET
            self->setColorModel, [nred, ngreen, nblue]
        ENDIF
        self->getColorModel, nred, ngreen, nblue
        TVLCT, nred, ngreen, nblue
    ENDIF ELSE BEGIN
        ;TVLCT, red, green, blue
        self->setColors, /INITIALIZE
    ENDELSE
        
    ;== Get the data
    respData = self.response->response()
    num_ebin = (SIZE (respData.drm))[1]
    num_chan = (SIZE (respData.drm))[2]

    ch_width = (respData.chan_width)[0: num_chan - 2]
    eb_width = (respData.phot_width)[0: num_ebin - 2]

    resp = (respData.drm)[0: num_ebin - 2, 0: num_chan - 2]
    FOR i = 0, num_ebin - 2 DO $
        resp[i, *] = resp[i, *] / ch_width

    ln_resp  = ALOG (resp + 0.01)
    
    ch_e     = (respData.chan_edges)[0: num_chan - 2] + 0.5 * ch_width
    eb_e     = (respData.phot_edges)[0: num_ebin - 2] + 0.5 * eb_width
    
    XTITLE = 'Photon Energy (keV)'
    YTITLE = 'Channel Energy (keV)'

    IF (self.flip EQ 1) THEN BEGIN
        ln_resp = TRANSPOSE (ln_resp)
        tempChan = ch_e
        ch_e = eb_e
        eb_e = tempChan
        tempTitle = XTITLE
        XTITLE = YTITLE
        YTITLE = tempTitle
        END
    xr       = [MIN (eb_e), MAX (eb_e)]
    yr       = [MIN (ch_e), MAX (ch_e)]
    ln_ch_e  = ALOG10 (ch_e)
    ln_eb_e  = ALOG10 (eb_e)
    self->adjustRanges, xr, yr, (self.flip EQ 1) ? 'FLIPPlot' : 'DRMPlot', /XLOG, /YLOG

    IF colorChoice THEN BEGIN
        CONTOUR, ln_resp, eb_e, ch_e, /XLOG, /YLOG, NLEVELS = 255, $ 
            XSTYLE = 4 + 1, YSTYLE = 4 + 1, $ ;TITLE = 'DRM Contour Map', $
            C_COLORS = INDGEN(255), /FILL, $
            XRANGE = xr, YRANGE = yr, $
            _EXTRA = extra; [*self.PlotOptions, 'XRANGE', 'YRANGE']
            
        CONTOUR, ln_resp, eb_e, ch_e, /XLOG, /YLOG, NLEVELS = 10, $ 
            XSTYLE = 4 + 1, YSTYLE = 4 + 1, $ ;TITLE = 'DRM Contour Map', $
            COLOR = self.Color->color ('SPEC'), /NOERASE, $
            XRANGE = xr, YRANGE = yr, $
            _EXTRA = extra; [extra, 'XRANGE', 'YRANGE']
    ENDIF ELSE BEGIN
        CONTOUR, ln_resp, eb_e, ch_e, /XLOG, /YLOG, NLEVELS = 10, $ 
            XSTYLE = 4 + 1, YSTYLE = 4 + 1, $ ;TITLE = 'DRM Contour Map', $
            COLOR = self.Color->color ('SPEC'), $
            XRANGE = xr, YRANGE = yr, $
            _EXTRA = extra; [extra, 'XRANGE', 'YRANGE']
    ENDELSE

    PLOT, xr, yr, /XLOG, /YLOG, /NOERASE, /NODATA, $
        TITLE  = 'DRM Contour Map', $
        XTITLE = xTitle, YTITLE = yTitle, $
        XRANGE = xr, YRANGE = yr, $
        XSTYLE = 1, YSTYLE = 1, _EXTRA = extra; [extra, 'XRANGE', 'YRANGE']

	fname = self.Response->filename (/BASENAME)
	self->annotate, fname, YPOSITION = !Y.WINDOW[1] + 0.01, $
	  /SMALL, _EXTRA = extra

    self->finishRanges, (self.flip EQ 1) ? 'FLIPPlot' : 'DRMPlot', /XLOG, /YLOG
    
    ;== Copy pixmap to screen

    IF (NOT hardcopy) THEN BEGIN
       WSET, win
       self->copyPixWindow
    ENDIF
    TVLCT, red, green, blue
        
END


;------------------------------------------------------------------------------
; Choose plotting colors
;------------------------------------------------------------------------------
;PRO DRMDisplay::setColors, choice, INITIALIZE = initialize
;
;    ; COMPILE_OPT HIDDEN
;
;    IF (KEYWORD_SET (initialize)) THEN BEGIN
;
;       self->InitColors
;       model = self.Color->model ()
;       idx = WHERE (model.name EQ 'BG')
;       self.colors.bg = idx
;       idx = WHERE (model.name EQ 'FG')
;       self.colors.fg = idx
;       idx = WHERE (model.name EQ 'HIST')
;       self.colors.hist = idx
;       idx = WHERE (model.name EQ 'SPEC')
;       self.colors.spec = idx
;       idx = WHERE (model.name EQ 'BKGD')
;       self.colors.bkgd = idx
;
;       RETURN    
;    
;    ENDIF
;
;    TVLCT, r, g, b, /GET
;    
;    CASE (choice) OF
;
;        'Background'       : idx = self.colors.bg
;        'Foreground'       : idx = self.colors.fg 
;        'History'          : idx = self.colors.hist
;        'Spectrum'         : idx = self.colors.spec
;        'Background Model' : idx = self.colors.bkgd
;        
;        ELSE: MESSAGE, 'Unknown color choice: ' + STRING (choice)
;    
;    ENDCASE
;     
;    r = r[idx]
;    g = g[idx]
;    b = b[idx]
;    
;    color = DIALOG_COLOR (CANCEL = cancel, $
;        PARENT = self.widgetID.top, INITIAL = [r, g, b])
;    IF (cancel) THEN $
;       RETURN
;
;    CASE (choice) OF
;
;        'Background'       : TVLCT, color, self.colors.bg
;        'Foreground'       : TVLCT, color, self.colors.fg 
;        'History'          : TVLCT, color, self.colors.hist
;        'Spectrum'         : TVLCT, color, self.colors.spec
;        'Background Model' : TVLCT, color, self.colors.bkgd
;        
;        ELSE: MESSAGE, 'Unknown color choice: ' + STRING (choice)
;    
;    ENDCASE
;    
;    self->plot, _EXTRA = *self.PlotOptions
;
;END
;
;
; ----------------------------------------------------------------------------
; Define the subclass, inheriting Display and PHA
; ----------------------------------------------------------------------------
PRO DRMDisplay__define

    obj = { DRMDisplay, INHERITS DISPLAY, INHERITS PLOTTER, $
         
        ;== Holds reference to a BFITS response
        Response    : OBJ_NEW (),  $
        File        : OBJ_NEW (),  $
        flip        : 0L,          $
        colorTable  : 0L,          $
        showColorID : 0L,          $
        
        ;== The color model
         
        colorModel  : PTR_NEW()    $
        
    }

END
