;------------------------------------------------------------------------------
;+
; NAME:
;
;     Display (OBJECT)
;
; PURPOSE:
;
;     This base class object creates a general purpose data display widget.
;     The widget is composed of a main drawing area, and user-defined 
;     application buttons and menubar entries.  It is a "smart" widget
;     that can be resized, and plots can be zoomed interactively with
;     the mouse.
;
;     The class is not meant to be instantiated directly.  You must derive 
;     a subclass that overrides the plot and eventHandler methods.
;     See class mydisplay_example.pro for an example.
;
; INPUTS:
;
;     NONE
;
; KEYWORDS:
;
;     TITLE
;         STRING containing the window title (default = 'Display').
;
;     MAP
;         INTEGER (0 or 1) specifying the initial screen mapping of 
;         the Display widget (default = 1).
;
;     LAYOUT
;         Set this keyword to the STRING 'LEFT' or 'RIGHT' to place 
;         application buttons on the specified side of the drawing 
;         area (default = 'LEFT'). 
;
;     XSIZE
;         The initial xsize (pixels) of drawing area (default = 600).
;
;     YSIZE
;         The initial ysize (pixels) of drawing area (default = 400).
;
;     NO_DEFAULT_MENUS
;         Set this keyword to omit the default "File" and "Help" menus  
;
; AGGREGATE OBJECTS:
;
;     Color
;     Container
;     StatusBar
;     SelectBox
;     WMenu 
;     WMenuFactory
;
; DEPENDENCIES:
;
;     color__define.pro  
;     container__define.pro
;     statusbar__define.pro
;     selectbox__define.pro
;     wmenu__define.pro
;     wmenufactory__define.pro
;     psconfig.pro
;     track_mouse.pro
;     axisrange.pro
;     dialog_dismiss.pro
;
; PUBLIC METHODS:
;     
;     plot (PROCEDURE, VIRTUAL) - Plot to the Display widget draw area.  
;                 The default plot method takes no action; subclasses must 
;                 override this method to display data.
;         Inputs: NONE
;        Outputs: NONE 
;       Keywords: NONE
;
;                 NOTE: The plot method must include IDL's _EXTRA keyword.
;                 Your procedure definition should be as follows:
;
;                     PRO myClass::plot, _EXTRA = extra
;                        
;                          PLOT, ... , _EXTRA = extra
;
;                     END
;
;                 NOTE: If "Print" is selected (which will call the plot
;                 method), then the keyword HARDCOPY will be set to 1 and
;                 passed to the plot method.  Clients can query this keyword
;                 to determine if the plot method is currently begin used
;                 to create hardcopy output.
;
;     eventHandler (PROCEDURE, VIRTUAL) - Handle widget events.  Subclasses 
;                 must override this method to handle application specific
;                 widget events.
;         Inputs: event - an IDL widget event structure
;        Outputs: NONE 
;       Keywords: NONE
;
;     map (PROCEDURE) - Map the Display widget to the screen.
;         Inputs: doMap - INTEGER, 0 to unmap widget, 1 to map
;        Outputs: NONE 
;       Keywords: NONE
;
;     window (FUNCTION) - Return the window ID of the Display draw area.
;         Inputs: NONE
;        Outputs: Draw area window ID 
;       Keywords: NONE
;
;     setWindow (PROCEDURE) - Set the Display draw area as IDL's current window.
;         Inputs: NONE
;        Outputs: NONE 
;       Keywords: NONE
;
;     addButton (FUNCTION) - Add an application button. 
;         Inputs: This function accepts one or two input parameters.  
;                 If one input parameter is given, it must be a STRING that 
;                 contains the button text (bitmap buttons not supported).  
;                 If two input parameters are given, the first parameter 
;                 must be a valid widget ID that denotes the button's parent, 
;                 and the second parameter must be  a STRING that contains 
;                 the button text.  Specify a parent when creating 
;                 button pulldown menus.
;        Outputs: The WIDGET_BUTTON ID
;       Keywords: Any valid WIDGET_BUTTON keywords.  Unless the EVENT_PRO or
;                 EVENT_FUNC keywords are set, the eventHandler method will
;                 be called when the button is pressed.  The eventHandler 
;                 method must be overridden by subclasses of Display.
;
;                 If the EVENT_FUNC or EVENT_PRO keyword is used, then the
;                 self object reference will be available in the specified
;                 event handler as the UVALUE of event.top
;
;     removeButton (PROCEDURE) - Remove an application button that was
;                 previously added with the addButton method.  Can also
;                 be used to remove menu items and menus.
;         Inputs: ID - a valid WIDGET_BUTTON ID
;        Outputs: NONE
;       Keywords: NONE
;
;     addMenu (FUNCTION) - Add an application pulldown menu to the menubar.  
;         Inputs: NONE
;        Outputs: A reference to a WMenu object.  Note that this object
;                 will be automatically handled by this class, and does 
;                 not need to be destroyed by the caller.
;       Keywords: VALUE (REQUIRED) - a STRING label for the menu.
;
;                 Any valid WIDGET_BUTTON keywords.  Unless the EVENT_HANDLER
;                 keyword is set, the eventHandler method will be called when 
;                 the button is pressed.  The eventHandler  method must be 
;                 overridden by subclasses of Display.
;
;                 If the EVENT_FUNC or EVENT_PRO keyword is used, then the
;                 self object reference will be available in the specified
;                 event handler as the UVALUE of event.top
;
;                 Note that by default, a "File" menu and a "Help" menu
;                 are added to the menubar.  If you wish to provide your
;                 own menus, simply adding a menu with either of these names
;                 will override the default menus.  Don't forget to use
;                 the "/HELP" keyword for your Help menu.
;
;     addLabel (FUNCTION) - Add a text label. 
;         Inputs: label - STRING label to add.  If the label is a STRARR, then
;                 each element will be added on a separate line.
;        Outputs: The WIDGET_LABEL ID (or array of IDs).
;       Keywords: Any valid WIDGET_LABEL keywords.  
;
;     addMenuButton (FUNCTION) - Add an applicaion button menu (not on menubar) 
;
;     zoom (PROCEDURE) - Zoom the plot interactively with the mouse.
;         Inputs: NONE
;        Outputs: NONE 
;       Keywords: X - zoom the x-axis only
;                 Y - zoom the y-axis only
;                 FULLSCREEN - create a modal, full screen widget to display
;                   the current plot.  This method will block all IDL events 
;                   until the plot is dismissed.
;
;     setStatus (PROCEDURE) - The Display class includes a status bar at the
;                 bottom of the widget.  Use this method to show messages
;                 in the status bar.  
;         Inputs: TEXT - STRING containing the current text
;                 INTERVAL - Optional FLOAT input specifying the number of
;                 seconds to show the text. 
;        Outputs: NONE 
;       Keywords: REVERT - If the optional INTERVAL input parameter is given,
;                 the default is to clear the text to a null string after
;                 the interval expires.  Set this keyword to revert to the
;                 previous text when the interval expires.  
;
;                 Note that changing the status text repeatedly before a set 
;                 interval expires may result in the reverted text not being 
;                 the STRING you intended.
;
;     clearStatus (PROCEDURE) - Clear the current text from the status bar.
;         Inputs: NONE
;        Outputs: NONE
;       Keywords: NONE
;
;     annotate (PROCEDURE) - Add a text annotation to the plot region.
;         Inputs: txt - STRING to add to the plot region
;        Outputs: NONE
;       Keywords: XPOS, YPOS - Set these optional keyword to specify the 
;                 position of the text.  XYOUTS coordinate keywords 
;                 (e.g., /NORMAL) may also be used.
;
;     disable (PROCEDURE) - Disable selection of application and menubar buttons
;         Inputs: NONE
;        Outputs: NONE
;       Keywords: APPBUTTONS - Set this keyword to disable application buttons.
;                 MENUBAR - Set this keyword to disable menubar buttons.
;
;                 If no keywords are supplied, both the application buttons
;                 and the menubar are disabled.
;
;     enable (PROCEDURE) - Enable selection of application and menubar buttons.
;         Inputs: NONE
;        Outputs: NONE
;       Keywords: APPBUTTONS - Set this keyword to enable application buttons.
;                 MENUBAR - Set this keyword to enable menubar buttons.
;
;                 If no keywords are supplied, both the application buttons
;                 and the menubar are enabled.
;
; EVENT HANDLING:
;
;     Default handling is supplied for some internal events, but in 
;     general, subclasses must implement the eventHandler method to 
;     trap widget events, for example, WIDGET_BUTTON events generated from
;     user-defined application buttons or menu items. 
;
;     Note also that mouse clicks in the plot area, and keyboard events
;     will be returned to the eventHandler method.  For examples of how
;     to intercept these events, see the myDisplay class example.
;
; EXAMPLE:
;
;     See example subclass in file display_example.pro
; 
; MODIFICATION HISTORY:
;
;     Written, 1999 September, Robert.Mallozzi@msfc.nasa.gov
;
;-
;------------------------------------------------------------------------------



;------------------------------------------------------------------------------
; Constructor
;------------------------------------------------------------------------------
FUNCTION Display::init, TITLE = title, MAP = map, LAYOUT = layout, $
    NO_DEFAULT_MENUS = no_default_menus, XSIZE = xsize, YSIZE = ysize, $
    MYRMFIT = myRMFIT
        
    self.useDefaultMenus  = (KEYWORD_SET (no_default_menus)) ? 0 : 1
    self.haveDefaultRange = 0

    self.Color = OBJ_SINGLETON ('Color')

    self.MenuFactory = OBJ_NEW ('WMenuFactory')
    
    IF KEYWORD_SET (MYRMFIT) THEN $
        IF OBJ_VALID (myRMFIT) THEN $
            self.myRMFIT = myRMFIT  ;== Make a reference to the top-level widget
            
    self.psConfig = PTR_NEW ( $
        PSCONFIG (/INITIALIZE, DEFAULT = 'Letter Portrait (3/4)'))

    self.PlotOptions = PTR_NEW ({ NULL : 0 })
;    self.PlotOptions = PTR_NEW (!PLOTOPTIONS)

    self->build_display, TITLE = title, MAP = map, LAYOUT = layout, $
        XSIZE = xsize, YSIZE = ysize
    
    RETURN, 1

END


;------------------------------------------------------------------------------
; Destuctor
;------------------------------------------------------------------------------
PRO Display::cleanup

    ;OBJ_DESTROY, self.Color
    OBJ_DESTROY, self.Container
    OBJ_DESTROY, self.Status    
    OBJ_DESTROY, self.MenuFactory
    
    PTR_FREE, self.psConfig
    
    ;== Save the plot options into the global parameters
    ;!PLOTOPTIONS = *self.PlotOptions
    PTR_FREE, self.PlotOptions
    
    IF (WIDGET_INFO (self.widgetID.top, /VALID)) THEN $
       WIDGET_CONTROL, self.widgetID.top, /DESTROY
   
    WDELETE, self.widgetID.pixmap
            
END


;------------------------------------------------------------------------------
; Plot
;
; This method must be overridden by subclasses, and must accept 
; the _EXTRA keyword
;------------------------------------------------------------------------------
PRO Display::plot, _EXTRA = extra

    MESSAGE, 'You forgot to override the PLOT method.'    

END


;------------------------------------------------------------------------------
; Create a modal, full screen widget to display a plot
;------------------------------------------------------------------------------
PRO Display::fullScreen, METHOD = method

    IF (N_ELEMENTS (method) EQ 0) THEN $
       method = 'plot'

    base = WIDGET_BASE (TITLE = 'Click Plot to Dismiss', $
        /MODAL, GROUP_LEADER = self.widgetID.top)
    
    s = GET_SCREEN_SIZE ()
    w = WIDGET_DRAW (base, /BUTTON_EVENTS, $
        XSIZE = s[0] * 0.99, YSIZE = s[1] * 0.95) 
    
    WIDGET_CONTROL, /REALIZE, base
    WIDGET_CONTROL, w, GET_VALUE = win
    WSET, win
        
    CALL_METHOD, method, self, _EXTRA = *self.PlotOptions
         
    XMANAGER, '_unused', base, EVENT_HANDLER = 'DIALOG_DISMISS'

    self->setWindow

END


;-------------------------------------------------------------------------------
; Interactively zoom a plot
;
; Set the METHOD keyword to a STRING containing the method to call
; to plot.  If the keyword is not set, the default method is named PLOT.
; In this case, the subclass of this class must have implemented a
; PLOT method.
;
; Graphic must be a WIDGET_DRAW, not a direct graphics window.
; Although not used in this class, the input parameter drawWidgetID allows
; this method to be called for any WIDGET_DRAW.
;-------------------------------------------------------------------------------
PRO Display::zoom, drawWidgetID, $
    XZOOM = get_xrange, YZOOM = get_yrange, FULLSCREEN = fullscreen, $
    METHOD = method
  
    IF (KEYWORD_SET (fullscreen)) THEN BEGIN
       self->fullscreen, METHOD = method
       RETURN
    ENDIF   

    IF (N_ELEMENTS (drawWidgetID) EQ 0) THEN $
       drawWidgetID = self.widgetID.draw
       
    IF (N_ELEMENTS (method) EQ 0) THEN $
       method = 'plot'

    ;== First pass: set default axis ranges
    
    IF (NOT self.haveDefaultRange) THEN BEGIN
       self.haveDefaultRange   = 1  
       self.defaultRange[0, *] = AXISRANGE (/XAXIS)
       self.defaultRange[1, *] = AXISRANGE (/YAXIS)
    ENDIF

    ;== Store the current axis ranges
        
    self.currentRange[0, *] = AXISRANGE (/XAXIS)
    self.currentRange[1, *] = AXISRANGE (/YAXIS)

    ;== Set the current window
    
    wSave = !D.WINDOW
    IF ((WIDGET_INFO (drawWidgetID, /VALID)) AND $
        (WIDGET_INFO (drawWidgetID, /NAME) EQ 'DRAW')) THEN BEGIN

       self->setWindow

    ENDIF ELSE BEGIN

       MESSAGE, /CONTINUE, 'Input widget ID not a valid WIDGET_DRAW.'
       RETURN

    ENDELSE

    ;== Set the current axis ranges 
    
    ;CALL_METHOD, method, self, _EXTRA = *self.PlotOptions

    doExit = 0

    REPEAT BEGIN

        IF (TRACK_MOUSE (drawWidgetID, region, coords, $
           /NOWAIT, YTOLERANCE = 0.03, TOP_TEXT = 'RESET to Default Range', $
           GET_XRANGE = get_xrange, GET_YRANGE = get_yrange, $
           COLOR = self.colors.fg, BACKGROUND = self.colors.bg)) $
           THEN BEGIN

           ;== If zooming only one axis
           
           xRange = $ 
               KEYWORD_SET (get_xrange) ? coords[0, *] : self.currentRange[0, *]
           yRange = $
               KEYWORD_SET (get_yrange) ? coords[1, *] : self.currentRange[1, *]

           ;== Else zooming both axes
           
           IF (NOT KEYWORD_SET (get_xrange) AND NOT KEYWORD_SET (get_yrange)) $
              THEN BEGIN
              xRange = coords[0, *]
              yRange = coords[1, *]
           ENDIF

           xRange = xRange[SORT (xRange)]
           yRange = yRange[SORT (yRange)]

           ;== Refresh the plot
           
           CALL_METHOD, method, self, XRANGE = xRange, YRANGE = yRange, $
                  _EXTRA = *self.PlotOptions

        ENDIF ELSE BEGIN

           CASE (REGION) OF

               ;== Reset to initial plot range
               
               2: CALL_METHOD, method, self, $
                      XRANGE = self.currentRange[0, *], $
                      YRANGE = self.currentRange[1, *], $
                      _EXTRA = *self.PlotOptions

               ;== Numerical entry
               
               3: BEGIN

                  obj = OBJ_NEW ('selectBox', drawWidgetID, $
                      AXISRANGE (/XAXIS), AXISRANGE (/YAXIS), /OPLOT)
                  coords = obj->get ('selection')
                  exact  = obj->get ('exact')
                  OBJ_DESTROY, obj

                  xRange = coords[0, *]
                  yRange = coords[1, *]

                  xRange = xRange[SORT (xRange)]
                  yRange = yRange[SORT (yRange)]

                  CALL_METHOD, method, self, $
                      XRANGE = xRange, YRANGE = yRange, $
                      XSTYLE = exact, YSTYLE = exact, $
                      _EXTRA = *self.PlotOptions
                  END

               ;== Reset to total plot range
                
               4: CALL_METHOD, method, self, $
                      XRANGE = self.defaultRange[0, *], $
                      YRANGE = self.defaultRange[1, *], $
                      _EXTRA = *self.PlotOptions

               ELSE: doExit = 1

           ENDCASE ; REGION

        ENDELSE

    ENDREP UNTIL (doExit EQ 1)

    WSET, wSave

END


;-------------------------------------------------------------------------------
; Forward events to the object event handlers
;-------------------------------------------------------------------------------
PRO DisplayInternal_EVENT, event

    ; COMPILE_OPT HIDDEN

    WIDGET_CONTROL, event.top, GET_UVALUE = self
    self->Display::eventHandler, event

END

PRO Display_EVENT, event

    ; COMPILE_OPT HIDDEN

    WIDGET_CONTROL, event.top, GET_UVALUE = self
    
    self->setKeyboardEvents
    
    IF (TAG_NAMES (event, /STRUCTURE) EQ 'WIDGET_BASE') THEN BEGIN
    
       self->resizeHandler, event
    
    ENDIF ELSE BEGIN
       
       self->eventHandler, event

    ENDELSE
    
END


;------------------------------------------------------------------------------
; Resize event handler
;------------------------------------------------------------------------------
PRO Display::resizeHandler, event

    ; COMPILE_OPT HIDDEN

    self->setWindow
    self->setKeyboardEvents

    WIDGET_CONTROL, event.top, TLB_GET_SIZE = newTlbSize
        
    delta = newTlbSize / self.widgetGEO.top
    newDrawSize = FIX (self.widgetGEO.draw * delta)
    
;    IF (STRUPCASE (!VERSION.OS_FAMILY) EQ 'MACOS') THEN adjust = 150
    WIDGET_CONTROL, self.widgetID.draw, $
        XSIZE = newDrawSize[0], YSIZE = newDrawSize[1]

    self.widgetGEO.top  = FIX (newTlbSize)
    self.widgetGEO.draw = FIX (newDrawSize)
     
    WDELETE, self.widgetID.pixmap    
    WINDOW, /FREE, /PIXMAP, $
        XSIZE = self.widgetGEO.draw[0], YSIZE = self.widgetGEO.draw[1]
    self.widgetID.pixmap = !D.WINDOW

    self->setWindow
    self->plot, _EXTRA = *self.PlotOptions

END


;------------------------------------------------------------------------------
; Default event handler
;------------------------------------------------------------------------------
PRO Display::eventHandler, event

    ; COMPILE_OPT HIDDEN

    self->setWindow
    self->setKeyboardEvents

    CASE (TAG_NAMES (event, /STRUCTURE)) OF

    'WIDGET_BUTTON': BEGIN

        WIDGET_CONTROL, event.id, GET_VALUE = value

        CASE (value) OF

            'Help'  : self->info, /HELP
            'About' : self->info, /ABOUT   
            
            'Print' : self->print

            'Print Setup...' : result = DIALOG_PRINTERSETUP ()
            
            'PS Configure...' : BEGIN
                
                config = PSCONFIG (CANCEL = cancel,  $
                    DEFAULT = (*self.psconfig).name)
                IF (NOT cancel) THEN BEGIN
                   PTR_FREE, self.psconfig
                   self.psconfig = PTR_NEW (config)
                ENDIF
                END
            
            'Quit' : WIDGET_CONTROL, self.widgetID.top, /DESTROY

            ELSE : MESSAGE, /INFO, 'Event not handled: ' + value

        ENDCASE
        END ; WIDGET_BUTTON events

    'WIDGET_KILL_REQUEST': BEGIN
        WIDGET_CONTROL, self.widgetID.top, /DESTROY
        END
        
    ELSE: ; Ignore events         
    
    ENDCASE ; event type
    
    
END


;-------------------------------------------------------------------------------
; Build the GUI
;-------------------------------------------------------------------------------
PRO Display::build_display, TITLE = title, MAP = map, LAYOUT = layout, $
    XSIZE = xsize, YSIZE = ysize

    ; COMPILE_OPT HIDDEN
    
    
    ;== A container to hold WMenu objects for easier memory management
    
    self.Container = OBJ_NEW ('IDL_Container')
    
    IF (N_ELEMENTS (title) EQ 0) THEN $
       TITLE = 'Display'

    IF (N_ELEMENTS (layout) EQ 0) THEN $
       layout = 'LEFT'
     
    IF (N_ELEMENTS (xsize) EQ 0) THEN $
       xsize = 600

    IF (N_ELEMENTS (ysize) EQ 0) THEN $
       ysize = 400

    layout = STRUPCASE (layout)
    IF ((layout NE 'LEFT') AND (layout NE 'RIGHT')) THEN $
       layout = 'LEFT'

    topBase = WIDGET_BASE (/COLUMN, MBAR = menuBar, $
        TITLE = title, MAP = map, /TLB_SIZE_EVENTS, /TLB_KILL_REQUEST_EVENTS)
       
    self.widgetID.top     = topBase
    self.widgetID.menuBar = menuBar

    IF (self.useDefaultMenus) THEN BEGIN

       Menu = self.MenuFactory->create (menuBar, $
           VALUE = 'File', EVENT_HANDLER = 'DisplayInternal_EVENT')
       id = Menu->add (VALUE = 'Print')
       id = Menu->add (VALUE = 'Print Setup...') 
       id = Menu->add (VALUE = 'PS Configure...')
       id = Menu->add (VALUE = 'Quit', /SEPARATOR)
       self.Container->add, Menu

       Menu = self.MenuFactory->create (menuBar, /HELP, $
           VALUE = 'Help', EVENT_HANDLER = 'DisplayInternal_EVENT')
       id = Menu->add (VALUE = 'Help')
       id = Menu->add (VALUE = 'About', /SEPARATOR)
       self.Container->add, Menu

    ENDIF
         
    base = WIDGET_BASE (topBase, /ROW)

    IF (layout EQ 'LEFT') THEN BEGIN
    
       self.widgetID.buttonBase = WIDGET_BASE (base, /COLUMN, /FRAME)
       buttonBase = self.widgetID.buttonBase

    ENDIF

    subBase = WIDGET_BASE (base)
    self.widgetID.draw = WIDGET_DRAW (subBase, $ 
        XSIZE = xsize, YSIZE = ysize, $
        /FRAME, /BUTTON_EVENTS) 

    ;== Hidden text widget to trap keyboard events
    
    self.widgetID.hidden = WIDGET_TEXT (subBase, $ 
        SCR_XSIZE = 1, SCR_YSIZE = 1, /ALL, FRAME = 0)
        
    IF (layout EQ 'RIGHT') THEN BEGIN
    
       self.widgetID.buttonBase = WIDGET_BASE (base, /COLUMN, /FRAME)
       buttonBase = self.widgetID.buttonBase

    ENDIF

    self.Status = OBJ_NEW ('StatusBar', topBase)

    WIDGET_CONTROL, /REALIZE, topBase

    WIDGET_CONTROL, self.widgetID.top, TLB_GET_SIZE = s
    self.widgetGEO.top = FIX (s)
    
    g = WIDGET_INFO (self.widgetID.draw, /GEOMETRY)

    self.widgetGEO.draw = FIX ([g.xsize, g.ysize]) ;
    
    ;== Create a pixmap window
    
    WINDOW, /FREE, /PIXMAP, XSIZE = g.xsize, YSIZE = g.ysize
    self.widgetID.pixmap = !D.WINDOW

    self->setWindow
    self->setKeyboardEvents

    WIDGET_CONTROL, self.widgetID.top, SET_UVALUE = self

    XMANAGER, 'Display', self.widgetID.top, /NO_BLOCK
        
END


; ----------------------------------------------------------------------------
; Help and About information
; ----------------------------------------------------------------------------
PRO Display::info, HELP = help, ABOUT = about
    
    ; COMPILE_OPT HIDDEN

    IF (KEYWORD_SET (help)) THEN BEGIN
    
       d = DIALOG_MESSAGE (/INFO, DIALOG_PARENT = self.widgetID.top, $
           TITLE = 'Help', 'No Help Available!')
           
    ENDIF

    IF (KEYWORD_SET (about)) THEN BEGIN

       @banner_text.pro

       d = DIALOG_MESSAGE (/INFO, DIALOG_PARENT = self.widgetID.top, $
           TITLE = 'About', text)
    
    ENDIF


END
; ----------------------------------------------------------------------------
; Implement the plot configuration.
; ----------------------------------------------------------------------------
PRO Display::setPlotOptions, options
    
;    PTR_FREE, self.PlotOptions
    IF (N_ELEMENTS (options) EQ 0) THEN $
         *self.PlotOptions = plotconfig (SETTINGS = *self.PlotOptions) $
    ELSE *self.PlotOptions = options
    
END


;------------------------------------------------------------------------------
; Show/hide display widget
;------------------------------------------------------------------------------
PRO Display::map, doMap

    IF (N_ELEMENTS (doMap) EQ 0) THEN doMap = 1
    
    WIDGET_CONTROL, self.widgetID.top, MAP = doMap, SHOW = doMap

    WIDGET_CONTROL, self.widgetID.top, TLB_GET_SIZE = s
    self.widgetGEO.top = FIX (s)

    self->setWindow
    self->setKeyboardEvents

END


;------------------------------------------------------------------------------
; Return draw widget window ID
;------------------------------------------------------------------------------
FUNCTION Display::window

    WIDGET_CONTROL, self.widgetID.draw, GET_VALUE = win
    RETURN, win

END
     

;------------------------------------------------------------------------------
; Set the display drawing area as IDL's current window
;------------------------------------------------------------------------------
PRO Display::setWindow

    WDELETE, self.widgetID.pixmap	
    WINDOW, /FREE, /PIXMAP, $
        XSIZE = self.widgetGEO.draw[0], YSIZE = self.widgetGEO.draw[1]
    self.widgetID.pixmap = !D.WINDOW

    WIDGET_CONTROL, self.widgetID.draw, GET_VALUE = win
    WSET, win

END
     

;------------------------------------------------------------------------------
; Initialize the color model to some common values
;------------------------------------------------------------------------------
PRO Display::InitColors, BW = bw

       ;== First 6 colors are the default colors provided by the Color object

       model = self.Color->model ()
       nColors = N_ELEMENTS (model)
       
       ;i = nColors
       bidx = WHERE (model.name EQ 'BLACK')
       idx = bidx
       m = model[idx]
       m.name = 'BG'
       jdx = WHERE (model.name EQ m.name, jcnt)
       IF (jcnt EQ 0) THEN BEGIN
           model = [model, m]
       ENDIF ELSE BEGIN
           model[jdx[0]] = m
       ENDELSE
       ;self.colors.bg = i
       ;i = i + 1
      
       idx = KEYWORD_SET (BW) ? bidx : WHERE (model.name EQ 'WHITE')
       m = model[idx]
       m.name = 'FG'
       jdx = WHERE (model.name EQ m.name, jcnt)
       IF (jcnt EQ 0) THEN BEGIN
           model = [model, m]
       ENDIF ELSE BEGIN
           model[jdx[0]] = m
       ENDELSE
       ;self.colors.fg = i
       ;i = i + 1

       idx = KEYWORD_SET (BW) ? bidx : WHERE (model.name EQ 'YELLOW')
       m = model[idx]
       m.name = 'HIST'
       jdx = WHERE (model.name EQ m.name, jcnt)
       IF (jcnt EQ 0) THEN BEGIN
           model = [model, m]
       ENDIF ELSE BEGIN
           model[jdx[0]] = m
       ENDELSE
       ;self.colors.hist = i
       ;i = i + 1

       idx = KEYWORD_SET (BW) ? bidx : WHERE (model.name EQ 'YELLOW')
       m = model[idx]
       m.name = 'SPEC'
       jdx = WHERE (model.name EQ m.name, jcnt)
       IF (jcnt EQ 0) THEN BEGIN
           model = [model, m]
       ENDIF ELSE BEGIN
           model[jdx[0]] = m
       ENDELSE
       ;self.colors.spec = i
       ;i = i + 1

       idx = KEYWORD_SET (BW) ? bidx : WHERE (model.name EQ 'CYAN')
       m = model[idx]
       m.name = 'BKGD'
       jdx = WHERE (model.name EQ m.name, jcnt)
       IF (jcnt EQ 0) THEN BEGIN
           model = [model, m]
       ENDIF ELSE BEGIN
           model[jdx[0]] = m
       ENDELSE
       ;self.colors.bkgd = i

       n = model.name
       r = (model.value[0, 0])[*]
       g = (model.value[0, 1])[*]
       b = (model.value[0, 2])[*]       
       
       self.Color->setModel, n, r, g, b

END


;------------------------------------------------------------------------------
; Choose plotting colors
;------------------------------------------------------------------------------
PRO Display::setColors, choice, INITIALIZE = initialize

    ; COMPILE_OPT HIDDEN

    IF (KEYWORD_SET (initialize)) THEN BEGIN

       self->InitColors
       model = self.Color->model ()
       idx = WHERE (model.name EQ 'BG')
       self.colors.bg = idx
       idx = WHERE (model.name EQ 'FG')
       self.colors.fg = idx
       idx = WHERE (model.name EQ 'HIST')
       self.colors.hist = idx
       idx = WHERE (model.name EQ 'SPEC')
       self.colors.spec = idx
       idx = WHERE (model.name EQ 'BKGD')
       self.colors.bkgd = idx
       
       RETURN    
    
    ENDIF

    TVLCT, r, g, b, /GET
    
    CASE (choice) OF

        'Background'       : idx = self.colors.bg
        'Foreground'       : idx = self.colors.fg 
        'History'          : idx = self.colors.hist
        'Spectrum'         : idx = self.colors.spec
        'Background Model' : idx = self.colors.bkgd
        
        ELSE: MESSAGE, 'Unknown color choice: ' + STRING (choice)
    
    ENDCASE
     
    r = r[idx]
    g = g[idx]
    b = b[idx]
    
    color = DIALOG_COLOR (CANCEL = cancel, $
        PARENT = self.widgetID.top, INITIAL = [r, g, b])
    IF (cancel) THEN $
       RETURN

    CASE (choice) OF

        'Background'       : TVLCT, color, self.colors.bg
        'Foreground'       : TVLCT, color, self.colors.fg 
        'History'          : TVLCT, color, self.colors.hist
        'Spectrum'         : TVLCT, color, self.colors.spec
        'Background Model' : TVLCT, color, self.colors.bkgd
        
        ELSE: MESSAGE, 'Unknown color choice: ' + STRING (choice)
    
    ENDCASE

    self.color->setByIndex, idx, color
    
    self->plot, _EXTRA = *self.PlotOptions

END


;------------------------------------------------------------------------------
; Set the display drawing area to the internal pixmap window
;------------------------------------------------------------------------------
PRO Display::setPixWindow

    ;== Check if window size has changed
    
    IF (!D.X_SIZE NE self.widgetGEO.draw[0] OR $
        !D.Y_SIZE NE self.widgetGEO.draw[1]) THEN BEGIN
	
	WDELETE, self.widgetID.pixmap	
        WINDOW, /FREE, /PIXMAP, XSIZE = !D.X_SIZE, YSIZE = !D.Y_SIZE
        self.widgetID.pixmap = !D.WINDOW

    ENDIF
    	
    WSET, self.widgetID.pixmap

END


;------------------------------------------------------------------------------
; Copy the internal pixmap contents to the current window
;------------------------------------------------------------------------------
PRO Display::copyPixWindow

    DEVICE, COPY = [0, 0, !D.X_SIZE, !D.Y_SIZE, $
        0, 0, self.widgetID.pixmap]

END


;------------------------------------------------------------------------------
; Set up handler for keyboard events
;------------------------------------------------------------------------------
PRO Display::setKeyboardEvents
    
    WIDGET_CONTROL, self.widgetID.hidden, /INPUT_FOCUS

END


;------------------------------------------------------------------------------
; Add an application button groups
;------------------------------------------------------------------------------
FUNCTION Display::addButtonGroup, p1, p2, _EXTRA = extra

    CASE (N_PARAMS ()) OF
    
       0: BEGIN       
          RETURN, 0
          END
       
       1: BEGIN       
          parent = self.widgetID.buttonBase
          label  = STRING (p1)           
          END
       
       ELSE: BEGIN
          parent = p1
          label  = STRING (p2)      	      
          END
    
    ENDCASE      
    
    WIDGET_CONTROL, self.widgetID.top, UPDATE = 0
    buttonGroup = CW_BGROUP (parent, label, _EXTRA = extra) 
    WIDGET_CONTROL, self.widgetID.top, UPDATE = 1

    RETURN, buttonGroup

END


;------------------------------------------------------------------------------
; Add an application button
;------------------------------------------------------------------------------
FUNCTION Display::addButton, p1, p2, _EXTRA = extra

    CASE (N_PARAMS ()) OF
    
       0: BEGIN       
          parent = self.widgetID.buttonBase
          label  = 'button'           
          END
       
       1: BEGIN       
          parent = self.widgetID.buttonBase
          label  = STRING (p1)           
          END
       
       ELSE: BEGIN
          parent = p1
          label  = STRING (p2)       	      
          END
    
    ENDCASE      
    
    WIDGET_CONTROL, self.widgetID.top, UPDATE = 0
    button = WIDGET_BUTTON (parent, VALUE = label, _EXTRA = extra) 
    WIDGET_CONTROL, self.widgetID.top, UPDATE = 1

    RETURN, button
    
END


;------------------------------------------------------------------------------
; Remove an application button, menu, or menu item
;------------------------------------------------------------------------------
PRO Display::removeButton, id

    WIDGET_CONTROL, id, /DESTROY

END


;------------------------------------------------------------------------------
; Add an application button menu
;------------------------------------------------------------------------------
FUNCTION Display::addMenuButton, VALUE = value, TEAROFF = tearoff, $
    PARENT = parent, _EXTRA = extra

    IF KEYWORD_SET (parent) THEN id = parent ELSE id = self.widgetID.buttonBase
    
    menuObj = self.MenuFactory->create (id, $
        VALUE = value, TEAROFF = tearoff, _EXTRA = extra)
    self.Container->add, menuObj
    
    RETURN, menuObj
    
END


;------------------------------------------------------------------------------
; Add a menubar menu
;------------------------------------------------------------------------------
FUNCTION Display::addMenu, VALUE = value, TEAROFF = tearoff, _EXTRA = extra

    menuObj = self.MenuFactory->create (self.widgetID.menuBar, $
        VALUE = value, TEAROFF = tearoff, _EXTRA = extra)
    self.Container->add, menuObj

    RETURN, menuObj
    
END


;------------------------------------------------------------------------------
; Add a text label
;------------------------------------------------------------------------------
FUNCTION Display::addLabel, label, _EXTRA = extra

    n  = N_ELEMENTS (label)
    id = LONARR (n)
    
    FOR i = 0, n - 1 DO BEGIN
     
        id[i] = WIDGET_LABEL (self.widgetID.buttonBase, $
            VALUE = STRING (label[i]), _EXTRA = extra)
    
    ENDFOR
    
    RETURN, (n EQ 1 ? id[0] : id)
    
END


;------------------------------------------------------------------------------
; Show a text message in the status bar
;------------------------------------------------------------------------------
PRO Display::setStatus, text, interval, REVERT = revert

    self.Status->setText, text, interval, REVERT = revert

END


;------------------------------------------------------------------------------
; Clear the text in the status bar
;------------------------------------------------------------------------------
PRO Display::clearStatus

    self.Status->clear

END


;-------------------------------------------------------------------------------
; Enable/disable application and menubar buttons
;-------------------------------------------------------------------------------
PRO Display::enable, state, APPBUTTONS = appbuttons, MENUBAR = menubar
    
    IF (N_ELEMENTS (state) EQ 0) THEN state = 1
    
    buttons = KEYWORD_SET (appbuttons)
    menubar = KEYWORD_SET (menubar)
    
    IF ((NOT buttons) AND (NOT menubar)) THEN BEGIN
       buttons = 1
       menubar = 1
    ENDIF
    
    IF (buttons) THEN $
       WIDGET_CONTROL, self.widgetID.buttonBase, SENSITIVE = state

    IF (menubar) THEN $
       WIDGET_CONTROL, self.widgetID.menuBar, SENSITIVE = state

END

PRO Display::disable, APPBUTTONS = appbuttons, MENUBAR = menubar

    self->enable, 0, APPBUTTONS = appbuttons, MENUBAR = menubar    

END


;------------------------------------------------------------------------------
; Print Postscript
;------------------------------------------------------------------------------
PRO Display::print
    
    fSave = !P.FONT
    dSave = !D.NAME
    ;== User may have slipped up:
    myFile = (*self.psconfig).filename
    IF ((STRPOS(myFile, '/', /REVERSE_SEARCH) + 1) EQ STRLEN(myFile)) THEN BEGIN
        IF (*self.psconfig).encapsulated THEN $
            myFile = myFile + 'idl.eps' $
        ELSE $
            myFile = myFile + 'idl.ps'
        (*self.psconfig).filename = myFile
    ENDIF
    
    result = DIALOG_MESSAGE (/QUESTION, /CANCEL, $
          'Send Plot to Default Printer?') ;
          ;DIALOG_PRINTJOB ();DIALOG_PARENT = event.top
    IF (result EQ 'Cancel') THEN RETURN
    ok = (result EQ 'Yes') ? 1 : 0
    ;IF ok THEN BEGIN
	SET_PLOT, 'PS' ;'PRINTER'
	;!P.FONT = 1                        ; TrueType
	IF NOT (*self.psconfig).color THEN BEGIN
	    self->InitColors, /BW
	ENDIF ELSE BEGIN
	    ; Set FG color to black (TODO: check to see if FG is different than black)
	    myFG = self.color->color('FG')
	    self.color->setByIndex, myFG, [0,0,0]
	ENDELSE
	
	DEVICE, _EXTRA = *self.psconfig ;, FONT = 1
	;self.color->setmodel, theNames, redvector, greenvector, bluevector
	
	self->plot, _EXTRA = *self.PlotOptions, /HARDCOPY
	DEVICE, /CLOSE
	IF ok THEN $
		SPAWN, 'lp ' + (*self.psconfig).filename $
	ELSE self->setStatus, 'Wrote file: ' + (*self.psconfig).filename, 5, /REVERT
	SET_PLOT, dSave
	!P.FONT = fSave
	self->InitColors
	;self->setStatus, 'Wrote file: ' + (*self.psconfig).filename, 5, /REVERT
    ;ENDIF
                
;    config = PSCONFIG (CANCEL = cancel, PARENT = self.widgetID.top)
;
;    IF (NOT cancel) THEN BEGIN
;       
;       dSave = !D.NAME
;       result = DIALOG_PRINTJOB ();DIALOG_PARENT = self.widgetID.top
;       SET_PLOT, 'PRINTER'
;       DEVICE, DECOMPOSED = 0
;       DEVICE, _EXTRA = config
;
;       self->plot, _EXTRA = *self.PlotOptions
;
;       DEVICE, /CLOSE
;       SET_PLOT, dSave
;
;    ENDIF

END


;------------------------------------------------------------------------------
; Add a text string to the plot region
;------------------------------------------------------------------------------
PRO Display::annotate, text, STEP = step, ALIGN = align, $
    XPOSITION = xposition, YPOSITION = yposition, SMALL = small, $
    _REF_EXTRA = extra

    IF (N_ELEMENTS (ALIGN) EQ 0) THEN $
       align = 1  
           
    IF (N_ELEMENTS (xposition) EQ 0) THEN $
       xposition = !X.WINDOW[1] - 0.02  
           
    IF (N_ELEMENTS (yposition) EQ 0) THEN $
       yposition = !Y.WINDOW[1] - 0.05  
    
    IF KEYWORD_SET (STEP) THEN $
       yposition = yposition - 0.05 * step
       
    IF KEYWORD_SET (SMALL) THEN $
       myCharsize = 0.75 $
    ELSE $
       myCharsize = 1.25

    XYOUTS, /NORMAL, ALIGN = align, CHARSIZE = myCharsize, $ ;
        xposition, yposition, text, _EXTRA = ['CHARTHICK', 'COLOR']    

END


;------------------------------------------------------------------------------
; Accessor procedures for range setting
;------------------------------------------------------------------------------
PRO Display::setDefaultRange, range

       self.haveDefaultRange   = 1  
       self.defaultRange = range
       ;self.defaultRange[1, *] = range[1, *]

END

PRO Display::setCurrentRange, range

       self.currentRange = range
       ;self.currentRange[1, *] = range[1, *]

END


;------------------------------------------------------------------------------
; An object that implements general display (plotting) facilities
;------------------------------------------------------------------------------
PRO Display__define

    obj = { DISPLAY, $

        widgetID : { WIDGETID, $
            
            top         : 0L, $            
            menuBar     : 0L, $
            buttonBase  : 0L, $
            hidden      : 0L, $
            draw        : 0L, $
            pixmap      : 0L  $
            
        }, $

        widgetGEO : { WIDGETGEO, $

            top  : INTARR (2), $        
            draw : INTARR (2)  $        

        }, $

        Color            : OBJ_NEW (),    $
        Container        : OBJ_NEW (),    $        
        Status           : OBJ_NEW (),    $
        MenuFactory      : OBJ_NEW (),    $        
        myRMFIT          : OBJ_NEW (),    $        

        psConfig         : PTR_NEW (),    $        
        ;== Space for the plot options
        PlotOptions      : PTR_NEW (),    $
                                   
        currentRange     : FLTARR (2, 2), $
        defaultRange     : FLTARR (2, 2), $
        haveDefaultRange : 0,             $
        
        useDefaultMenus  : 0,             $
        
        ;== Plotting colors
         
        colors: { DISPLAY_COLORS, $
        
            bg   : 0, $              ; plot background
            fg   : 0, $              ; plot foreground  
            hist : 0, $              ; lightcurve
            spec : 0, $              ; spectrum
            bkgd : 0  $              ; background model
        
        } $
    }

END
