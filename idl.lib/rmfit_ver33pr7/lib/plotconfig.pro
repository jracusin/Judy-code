; -----------------------------------------------------------------------------
;+
; NAME:
;     PLOTCONFIG (FUNCTION)
;
; PURPOSE:
;     A widget to configure many plot parameters.
;     PLOTCONFIG uses an internal object to do all the work.
;
; CALLING SEQUENCE:
;     s = PLOTCONFIG ()
;
; INPUTS:
;     NONE 
;    
; OUTPUTS:
;     s   A structure that can be passed to the PLOT command via 
;         the _EXTRA keyword.
;      
; KEYWORDS:
;
;     CANCEL  
;         Set this keyword to a named variable that is set to 1 if the
;         "Cancel" button was selected, and 0 otherwise.  Note however
;         that you do not have to use this keyword to test the return 
;         value before calling the PLOT procedure.  If "Cancel" is selected, 
;         the structure { NULL : 0B } is returned, which can be harmlessly 
;         passed to PLOT.  See the example below.
;
;     TITLE
;         STRING containing the window title
;         (default = 'Configure Plot Parameters')
;
;     PARENT
;         A widget ID specifying the parent of the PLOTCONFIG widget.
;
;     INITIALIZE
;         Set this keyword to immediately return the default configuration
;         device structure without invoking the GUI.
;
;     SETTINGS
;         Set this keyword to a plotconfig structure with desired default settings. 
;
;     FILENAME
;         Set this keyword to a name of a plotConfig configuration file
;         to read on startup (a sample file can be written from the GUI using
;         the Config->Save button).  Using this keyword does not alter the 
;         default settings that are restored when the "Defaults" button 
;         is clicked.
;
; EXAMPLE:
;
;     config = PLOTCONFIG (CANCEL = cancel)
;     
;     ; You don't need to worry if the user cancelled the dialog.  
;     ; If so, plotConfig() will have no effect.
;     PLOT, ..., _EXTRA = config
;
;     ; So, you could actually configure and plot in one command
;     PLOT, ..., _EXTRA = plotConfig ()
;
; MODIFICATION HISTORY:
;
;     RDP, 26 Sept 2000
;        Added hackery in self->p to return only those settings that were 
;        changed from the defaults. This allows clients to make their own 
;        modifications of the plot without them getting ovewritten.
;
;     RSM, 21 Dec 1999
;        Added capability to load/store settings in a file.
;        Added keyword FILENAME to load settings from a file at startup.
;        Added keyword INITIALIZE to return a default configuration without
;            invoking the GUI.
;        Removed redundant "Apply" button.
;
;     RSM, 23 Nov 1999
;        Added {X|Y}STYLE, {X|Y}MARGIN, and SYMSIZE settings.
;        Added TITLE keyword.
;        Miscellaneous layout changes.
;
;     Written, Robert.Mallozzi@msfc.nasa.gov, 1999 November
;
;-
; -----------------------------------------------------------------------------



; -----------------------------------------------------------------------------
; Constructor
; -----------------------------------------------------------------------------
FUNCTION PlotConfig::init, PARENT = parent, $
    TITLE = title, FILENAME = filename, INITIALIZE = initialize, $
    SETTINGS = settings

    IF (NOT KEYWORD_SET (initialize)) THEN BEGIN

        self->build, PARENT = parent, TITLE = title, $
              FILENAME = filename, SETTINGS = settings
   
    ENDIF ELSE BEGIN
    
        userDefault = 0
        IF (N_ELEMENTS (filename) NE 0) THEN BEGIN

           userDefault = 1
           self->settings, /LOAD, FILENAME = filename, ERROR = error
           IF (error) THEN userDefault = 0           

        ENDIF 

        IF (NOT userDefault) THEN $
           self->defaults, SETTINGS = settings      

    ENDELSE
     
    RETURN, 1
    
END


; -----------------------------------------------------------------------------
; Destructor
; -----------------------------------------------------------------------------
PRO PlotConfig::cleanup
    
    IF (self.pixID NE 0) THEN $
       WDELETE, self.pixID
       
    PTR_FREE, self.settings
    
END


; -----------------------------------------------------------------------------
; Build the widget
; -----------------------------------------------------------------------------
PRO PlotConfig::build, PARENT = parent, TITLE = title, $
    FILENAME = filename, SETTINGS = settings
    
    IF (N_ELEMENTS (title) EQ 0) THEN $
       title = 'Configure Plot Parameters'
       
    IF (N_ELEMENTS (parent) NE 0) THEN $
       extra = { GROUP_LEADER : parent, MODAL : 1 }

    self.topID = WIDGET_BASE (TITLE = title, $
        /COLUMN, TLB_FRAME_ATTR = 9, _EXTRA = extra)
    
    topBase = WIDGET_BASE (self.topID, /COLUMN, /FRAME)

    base      = WIDGET_BASE (topBase, /ROW)
    leftBase  = WIDGET_BASE (base, /COLUMN)
    rightBase = WIDGET_BASE (base, /COLUMN)
            
    base = WIDGET_BASE (leftBase, ROW = 7, /GRID)
    
    w = WIDGET_LABEL (base, VALUE = 'Title', /ALIGN_RIGHT)
    w = WIDGET_TEXT  (base, XSIZE = 5, /EDIT, VALUE = '', /KBRD_FOCUS_EV)
    self.titleID = w

    w = WIDGET_LABEL (base, VALUE = 'Subtitle', /ALIGN_RIGHT)
    w = WIDGET_TEXT  (base, XSIZE = 5, /EDIT, VALUE = '' , /KBRD_FOCUS_EV)
    self.subtitleID = w

    w = WIDGET_LABEL (base, VALUE = 'X Title', /ALIGN_RIGHT)
    w = WIDGET_TEXT  (base, XSIZE = 5, /EDIT, VALUE = '' , /KBRD_FOCUS_EV)
    self.xtitleID = w

    w = WIDGET_LABEL (base, VALUE = 'Y Title', /ALIGN_RIGHT)
    w = WIDGET_TEXT  (base, XSIZE = 5, /EDIT, VALUE = '' , /KBRD_FOCUS_EV)
    self.ytitleID = w

    w = WIDGET_LABEL (base, VALUE = 'Char Size', /ALIGN_RIGHT)
    w = WIDGET_TEXT  (base, XSIZE = 5, /EDIT, VALUE = '1.00', /KBRD_FOCUS_EV)
    self.charSizeID = w

    w = WIDGET_LABEL (base, VALUE = 'Char Thick', /ALIGN_RIGHT)
    w = WIDGET_TEXT  (base, XSIZE = 5, /EDIT, VALUE = '1.00', /KBRD_FOCUS_EV)
    self.charThickID = w


    w = WIDGET_LABEL (base, VALUE = 'Thick', /ALIGN_RIGHT)
    w = WIDGET_TEXT  (base, XSIZE = 5, /EDIT, VALUE = '1.00', /KBRD_FOCUS_EV)
    self.thickID = w
    
    w = WIDGET_LABEL (base, VALUE = 'X Thick', /ALIGN_RIGHT)
    w = WIDGET_TEXT  (base, XSIZE = 5, /EDIT, VALUE = '1.00', /KBRD_FOCUS_EV)
    self.xThickID = w

    w = WIDGET_LABEL (base, VALUE = 'Tick Length', /ALIGN_RIGHT)
    w = WIDGET_TEXT  (base, XSIZE = 5, /EDIT, VALUE = '0.02', /KBRD_FOCUS_EV)
    self.tickLenID = w
    
    w = WIDGET_LABEL (base, VALUE = 'Y Thick', /ALIGN_RIGHT)
    w = WIDGET_TEXT  (base, XSIZE = 5, /EDIT, VALUE = '1.00', /KBRD_FOCUS_EV)
    self.yThickID = w

  
    w = WIDGET_LABEL (base, VALUE = 'X Ticks', /ALIGN_RIGHT)
    w = WIDGET_TEXT  (base, XSIZE = 5, /EDIT, VALUE = '0', /KBRD_FOCUS_EV)
    self.xTicksID = w
    
    w = WIDGET_LABEL (base, VALUE = 'X Minor', /ALIGN_RIGHT)
    w = WIDGET_TEXT  (base, XSIZE = 5, /EDIT, VALUE = '0', /KBRD_FOCUS_EV)
    self.xMinorID = w

    w = WIDGET_LABEL (base, VALUE = 'Y Ticks', /ALIGN_RIGHT)
    w = WIDGET_TEXT  (base, XSIZE = 5, /EDIT, VALUE = '0', /KBRD_FOCUS_EV)
    self.yTicksID = w

    w = WIDGET_LABEL (base, VALUE = 'Y Minor', /ALIGN_RIGHT)
    w = WIDGET_TEXT  (base, XSIZE = 5, /EDIT, VALUE = '0', /KBRD_FOCUS_EV)
    self.yMinorID = w

    base = WIDGET_BASE (leftBase, /ROW)
    n = ['Solid',    'Dotted',    'Dashed', $
         'Dash Dot', 'Dash 3Dot', 'Long Dashed']

    w = WIDGET_DROPLIST (base, VALUE = n, TITLE = '  Line')
    self.lineStyleID = w

    w = WIDGET_DROPLIST (base, VALUE = n, TITLE = 'Grid')
    self.gridStyleID = w

    base = WIDGET_BASE (leftBase, /ROW)
    n = ['None', 'Plus', 'Asterisk', 'Period', 'Diamond', 'Triangle', $
         'Square', 'X', 'Histogram']
    w = WIDGET_DROPLIST (base, VALUE = n, TITLE = 'Symbol')
    self.psymID = w 
    w = WIDGET_TEXT  (base, XSIZE = 5, /EDIT, VALUE = '1.00', /KBRD_FOCUS_EV)
    self.psymSizeID = w
    b = WIDGET_BASE (base, /COLUMN, /NONEXCLUSIVE)   
    self.psymLineID = WIDGET_BUTTON (b, VALUE = 'With Line')
    
    base = WIDGET_BASE (leftBase, /ROW)
    n = ['Hershey', 'Device', 'TrueType']
    w = WIDGET_DROPLIST (base, VALUE = n, TITLE = '  Font')
    self.fontID = w
    
    topRightBase = WIDGET_BASE (rightBase, /COLUMN)
    botRightBase = WIDGET_BASE (rightBase, /COLUMN)
    
    self.xsize = 280
    self.ysize = 300
    drawID = WIDGET_DRAW (topRightBase, XSIZE = self.xsize, YSIZE = self.ysize)

    botRow = WIDGET_BASE (topBase, /ROW)
        
    names = ['Exact', 'Extend', 'None', 'No Box', 'Y No Zero']
    lBase = WIDGET_BASE (botRow, /COLUMN)

    base = WIDGET_BASE (lBase, /ROW)
    w = WIDGET_LABEL (base, VALUE = 'X Style')
    b = WIDGET_BASE (base, /ROW, /NONEXCLUSIVE)
    self.xStyleID[0] = WIDGET_BUTTON (b, VALUE = names[0], UVALUE = 'x0')
    self.xStyleID[1] = WIDGET_BUTTON (b, VALUE = names[1], UVALUE = 'x1')
    self.xStyleID[2] = WIDGET_BUTTON (b, VALUE = names[2], UVALUE = 'x2')
    self.xStyleID[3] = WIDGET_BUTTON (b, VALUE = names[3], UVALUE = 'x3')
    self.xStyleID[4] = WIDGET_BUTTON (b, VALUE = names[4], UVALUE = 'x4')
    
    base = WIDGET_BASE (lBase, /ROW)
    w = WIDGET_LABEL (base, VALUE = 'Y Style')
    b = WIDGET_BASE (base, /ROW, /NONEXCLUSIVE)
    self.yStyleID[0] = WIDGET_BUTTON (b, VALUE = names[0], UVALUE = 'y0')
    self.yStyleID[1] = WIDGET_BUTTON (b, VALUE = names[1], UVALUE = 'y1')
    self.yStyleID[2] = WIDGET_BUTTON (b, VALUE = names[2], UVALUE = 'y2')
    self.yStyleID[3] = WIDGET_BUTTON (b, VALUE = names[3], UVALUE = 'y3')
    self.yStyleID[4] = WIDGET_BUTTON (b, VALUE = names[4], UVALUE = 'y4')

    rBase = WIDGET_BASE (botRow, /COLUMN)

    base = WIDGET_BASE (rBase, ROW = 2, /GRID)
    
    w = WIDGET_LABEL (base, VALUE = 'X Margins', /ALIGN_RIGHT)
    w = WIDGET_TEXT  (base, XSIZE = 5, /EDIT, VALUE = '10', /KBRD_FOCUS_EV)
    self.xMarginID[0] = w
    w = WIDGET_TEXT  (base, XSIZE = 5, /EDIT, VALUE = '3' , /KBRD_FOCUS_EV)
    self.xMarginID[1] = w

    w = WIDGET_LABEL (base, VALUE = 'Y Margins', /ALIGN_RIGHT)
    w = WIDGET_TEXT  (base, XSIZE = 5, /EDIT, VALUE = '4', /KBRD_FOCUS_EV)
    self.yMarginID[0] = w
    w = WIDGET_TEXT  (base, XSIZE = 5, /EDIT, VALUE = '2' , /KBRD_FOCUS_EV)
    self.yMarginID[1] = w
    

    base = WIDGET_BASE (self.topID, /ROW, /ALIGN_CENTER, SPACE = 20, /GRID)
    w = WIDGET_BUTTON (base, VALUE = 'Accept')
    w = WIDGET_BUTTON (base, VALUE = 'Defaults')
    w = WIDGET_BUTTON (base, VALUE = 'Cancel')
    w = WIDGET_BUTTON (base, VALUE = 'Config', /MENU)
        ww = WIDGET_BUTTON (w, VALUE = 'Load')
        ww = WIDGET_BUTTON (w, VALUE = 'Save')
	
;    geo = WIDGET_INFO (leftBase, /GEOMETRY)
;    self.xsize = geo.xsize
;    self.ysize = geo.ysize
;    WIDGET_CONTROL, drawID, XSIZE = self.xsize, YSIZE = self.ysize
   
    WIDGET_CONTROL, self.topID, /REALIZE
    WIDGET_CONTROL, drawID, GET_VALUE = win
    self.drawID = win
    
    WINDOW, /FREE, /PIXMAP, XSIZE = self.xsize, YSIZE = self.ysize
    self.pixID = !D.WINDOW

    userDefault = 0
    IF (N_ELEMENTS (filename) NE 0) THEN BEGIN
       
       userDefault = 1
       self->settings, /LOAD, FILENAME = filename, ERROR = error
       IF (error) THEN userDefault = 0           
    
    ENDIF
    
    IF (NOT userDefault) THEN BEGIN
       IF KEYWORD_SET(SETTINGS) THEN self.settings = PTR_NEW(settings)
       self->defaults, SETTINGS = settings
    ENDIF
    
    self->updateForm
    self->updatePlot
    
    WIDGET_CONTROL, self.topID, SET_UVALUE = self

    XMANAGER, 'PlotConfig', self.topID


END


; -----------------------------------------------------------------------------
; Event handler
; -----------------------------------------------------------------------------
PRO PlotConfig_EVENT, event
       
    WIDGET_CONTROL, event.top, GET_UVALUE = self
    self->eventHandler, event

END

PRO PlotConfig::eventHandler, event
        
    type = TAG_NAMES (event, /STRUCTURE)
    CASE (type) OF
            
        'WIDGET_BUTTON': BEGIN
       
            WIDGET_CONTROL, event.id, GET_VALUE = value
            
            CASE (STRUPCASE (STRTRIM (value, 2))) OF

                 'CANCEL' : BEGIN
                     self.cancel = 1
                     WIDGET_CONTROL, event.top, /DESTROY
                     END

                 'ACCEPT' : BEGIN
                     self.cancel = 0
                     WIDGET_CONTROL, event.top, /DESTROY
                     END

                 'DEFAULTS' : BEGIN
                     self->defaults
                     self->updateForm
                     self.redraw = 1                     
                     END

                 'WITH LINE' : BEGIN
                     self.psymLine = FIX (event.select)
                     self.redraw = 1
                     END

                 'LOAD' : BEGIN
                     self->settings, /LOAD, ERROR = error
                     IF (NOT error) THEN BEGIN
                        self->updateForm
                        self.redraw = 1                     
                     ENDIF
                     END

                 'SAVE' : BEGIN
                     self->settings, /SAVE
                     END

                 ELSE : BEGIN
                 
                     WIDGET_CONTROL, event.id, GET_UVALUE = style
                     index = FIX (STRMID (style, 1, 1))
                     style = STRMID (style, 0, 1)

                     IF (style EQ 'x') THEN $
                        self.xStyle[index] = event.select

                     IF (style EQ 'y') THEN $
                        self.yStyle[index] = event.select

                     self.redraw = 1
                     END
                     
            ENDCASE ; value
            END
        
        'WIDGET_KBRD_FOCUS' : BEGIN
            IF (event.enter EQ 0) THEN $
               self.redraw = 1  
            END
        
        'WIDGET_TEXT_CH' : BEGIN
            self.redraw = 1
            END

        'WIDGET_DROPLIST' : BEGIN
            self.redraw = 1
            END

        ELSE: ; do nothing
            
    ENDCASE ; type
    
    IF (self.redraw) THEN BEGIN        
       self->queryForm
       self->updatePlot
    ENDIF
    
END


; ----------------------------------------------------------------------------
; Read widget values
; ----------------------------------------------------------------------------
PRO PlotConfig::queryForm

              
    WIDGET_CONTROL, self.titleID, GET_VALUE = v
    self.p.title = STRING (v[0])

    WIDGET_CONTROL, self.subtitleID, GET_VALUE = v
    self.p.subTitle = STRING (v[0])

    WIDGET_CONTROL, self.xtitleID, GET_VALUE = v
    self.p.xTitle = STRING (v[0])

    WIDGET_CONTROL, self.ytitleID, GET_VALUE = v
    self.p.yTitle = STRING (v[0])


    WIDGET_CONTROL, self.charSizeID, GET_VALUE = v
    self.p.charSize = FLOAT (v[0])

    WIDGET_CONTROL, self.charThickID, GET_VALUE = v
    self.p.charThick = FLOAT (v[0])


    WIDGET_CONTROL, self.thickID, GET_VALUE = v
    self.p.thick = FLOAT (v[0])
    
    WIDGET_CONTROL, self.tickLenID, GET_VALUE = v
    self.p.tickLen = FLOAT (v[0])

    WIDGET_CONTROL, self.xThickID, GET_VALUE = v
    self.p.xthick = FLOAT (v[0])
    
    WIDGET_CONTROL, self.yThickID, GET_VALUE = v
    self.p.ythick = FLOAT (v[0])


    WIDGET_CONTROL, self.xTicksID, GET_VALUE = v
    self.p.xTicks = FLOAT (v[0])

    WIDGET_CONTROL, self.xMinorID, GET_VALUE = v
    self.p.xMinor = FLOAT (v[0])

    WIDGET_CONTROL, self.yTicksID, GET_VALUE = v
    self.p.yTicks = FLOAT (v[0])

    WIDGET_CONTROL, self.yMinorID, GET_VALUE = v
    self.p.yMinor = FLOAT (v[0])


    idx = WIDGET_INFO (self.lineStyleID, /DROPLIST_SELECT)
    self.p.lineStyle = FIX (idx[0])

    idx = WIDGET_INFO (self.gridStyleID, /DROPLIST_SELECT)
    self.p.xGridStyle = FIX (idx[0])
    self.p.yGridStyle = FIX (idx[0])

    idx = WIDGET_INFO (self.fontID, /DROPLIST_SELECT)
    self.p.font = FIX (idx[0]) - 1

    WIDGET_CONTROL, self.psymSizeID, GET_VALUE = v
    self.p.symsize = FLOAT (v[0])

    idx = WIDGET_INFO (self.psymID, /DROPLIST_SELECT)
    v = FIX (idx[0])
    self.p.psym = (v EQ 8 ? 10 : v)
    IF (self.psymLine AND self.p.psym LT 10) THEN self.p.psym = -self.p.psym


    ; Can't query NONEXCLUSIVE buttons -- arrrgggh
    
    self.p.xstyle = 0
    self.p.ystyle = 0
    FOR i = 0, 4 DO BEGIN
        IF (self.xStyle[i]) THEN self.p.xstyle = self.p.xstyle + 2^i
        IF (self.yStyle[i]) THEN self.p.ystyle = self.p.ystyle + 2^i
    ENDFOR        
     
    WIDGET_CONTROL, self.xMarginID[0], GET_VALUE = v
    self.p.xmargin[0] = FIX (v[0])

    WIDGET_CONTROL, self.xMarginID[1], GET_VALUE = v
    self.p.xmargin[1] = FIX (v[0])
     
    WIDGET_CONTROL, self.yMarginID[0], GET_VALUE = v
    self.p.ymargin[0] = FIX (v[0])

    WIDGET_CONTROL, self.yMarginID[1], GET_VALUE = v
    self.p.ymargin[1] = FIX (v[0])

END


; ----------------------------------------------------------------------------
; Set configuration default values
; ----------------------------------------------------------------------------
PRO PlotConfig::defaults, SETTINGS = settings
    
    self.p.title      = ''
    self.p.subtitle   = ''
    self.p.xTitle     = ''
    self.p.yTitle     = ''
    self.p.charSize   = 1.0
    self.p.charThick  = 1.0
    self.p.thick      = 1.0
    self.p.xThick     = 1.0
    self.p.yThick     = 1.0
    self.p.tickLen    = 0.02
    self.p.xTicks     = 0
    self.p.yTicks     = 0
    self.p.xMinor     = 0
    self.p.yMinor     = 0
    self.p.lineStyle  = 0
    self.p.xGridStyle = 0
    self.p.yGridStyle = 0
    self.p.psym	      = 0
    self.p.symSize    = 1.0
    self.p.font	      = -1
    self.p.xStyle     = 0
    self.p.yStyle     = 0
    self.p.xMargin    = [10, 3]
    self.p.yMargin    = [ 4, 2]
    
    IF KEYWORD_SET (SETTINGS) THEN BEGIN
        setNames = TAG_NAMES (settings)
        pNames   = TAG_NAMES (self.p)
        IF (setNames[0] NE 'NULL') THEN BEGIN
            FOR k = 0, N_TAGS (settings) - 1 DO BEGIN
                nind = WHERE (pNames EQ setNames[k])
                self.p.(nind[0]) = settings.(k)
            ENDFOR
        ENDIF
    ENDIF

    self.xStyle = INTARR (5)
    self.yStyle = INTARR (5)
    self.psymLine = 0
        
END


; ----------------------------------------------------------------------------
; Set widget values
; ----------------------------------------------------------------------------
PRO PlotConfig::updateForm
    
    WIDGET_CONTROL, self.thickID,     $
        SET_VALUE = STRING (self.p.thick, FORMAT = '(F4.2)')
    WIDGET_CONTROL, self.xThickID,    $
        SET_VALUE = STRING (self.p.xThick, FORMAT = '(F4.2)')
    WIDGET_CONTROL, self.yThickID,    $
        SET_VALUE = STRING (self.p.yThick, FORMAT = '(F4.2)')
    WIDGET_CONTROL, self.tickLenID,   $
        SET_VALUE = STRING (self.p.tickLen, FORMAT = '(F4.2)') 
    WIDGET_CONTROL, self.charSizeID,  $
        SET_VALUE = STRING (self.p.charSize, FORMAT = '(F4.2)') 
    WIDGET_CONTROL, self.charThickID, $
        SET_VALUE = STRING (self.p.charThick, FORMAT = '(F4.2)')        
    WIDGET_CONTROL, self.xTicksID,    $
        SET_VALUE = STRTRIM (self.p.xTicks, 2)
    WIDGET_CONTROL, self.xMinorID,    $
        SET_VALUE = STRTRIM (self.p.xMinor, 2)
    WIDGET_CONTROL, self.yTicksID,    $
        SET_VALUE = STRTRIM (self.p.yTicks, 2)
    WIDGET_CONTROL, self.yMinorID,    $
        SET_VALUE = STRTRIM (self.p.yMinor, 2)

    WIDGET_CONTROL, self.titleID,     SET_VALUE = self.p.title
    WIDGET_CONTROL, self.subtitleID,  SET_VALUE = self.p.subTitle
    WIDGET_CONTROL, self.xtitleID,    SET_VALUE = self.p.xTitle
    WIDGET_CONTROL, self.ytitleID,    SET_VALUE = self.p.yTitle

    WIDGET_CONTROL, self.lineStyleID, SET_DROPLIST_SELECT = self.p.lineStyle
    WIDGET_CONTROL, self.gridStyleID, SET_DROPLIST_SELECT = self.p.xGridStyle
    WIDGET_CONTROL, self.fontID,      SET_DROPLIST_SELECT = self.p.font
    WIDGET_CONTROL, self.psymID,      SET_DROPLIST_SELECT = self.p.psym
    WIDGET_CONTROL, self.psymSizeID,  $
        SET_VALUE = STRING (self.p.symSize, FORMAT = '(F4.2)')        
    WIDGET_CONTROL, self.psymLineID,  SET_BUTTON = (self.p.psym LT 0)

    FOR i = 0, 4 DO BEGIN
        WIDGET_CONTROL, self.xStyleID[i], SET_BUTTON = self.xStyle[i]
        WIDGET_CONTROL, self.yStyleID[i], SET_BUTTON = self.yStyle[i]
    ENDFOR

    WIDGET_CONTROL, self.xMarginID[0], $
        SET_VALUE = STRTRIM (self.p.xMargin[0], 2)
    WIDGET_CONTROL, self.xMarginID[1], $
        SET_VALUE = STRTRIM (self.p.xMargin[1], 2)

    WIDGET_CONTROL, self.yMarginID[0], $
        SET_VALUE = STRTRIM (self.p.yMargin[0], 2)
    WIDGET_CONTROL, self.yMarginID[1], $
        SET_VALUE = STRTRIM (self.p.yMargin[1], 2)

END


; ----------------------------------------------------------------------------
; Update widget
; ----------------------------------------------------------------------------
PRO PlotConfig::updatePlot

    ; Plot to the pixmap window
    WSET, self.pixID    
    x = (FINDGEN (30) / (30 - 1)) * 6.0   
    PLOT, x, SIN (x), _EXTRA = self.p
    
    ; Copy pixmap to screen    
    WSET, self.drawID
    DEVICE, COPY = [0, 0, self.xsize, self.ysize, 0, 0, self.pixID]    
    self.redraw = 0
            
END


; -----------------------------------------------------------------------------
; Return object data, but only for keywords that are different than the default.
; The reason for this is to ensure that only the user's changes will override 
; the defaults. In particular, we don't want to mess up the stored parameters. 
; -----------------------------------------------------------------------------
FUNCTION PlotConfig::p       

    ;== We want to check against the default, but that messes up the stored p
    pSave = self.p
    self->defaults
    
    ;== An array to hold the results
    temp = ''
    count = 0
    
    pNames = TAG_NAMES (self.p)
    FOR i = 0, N_TAGS (self.p) - 1 DO BEGIN 
        IF ((pSave.(i))[0] NE (self.p.(i))[0]) THEN BEGIN 
            myName = pNames[i]
            myValue = pSave.(i)
            ;== Need to keep track of first time:
            count = count + 1
			IF (count EQ 1) THEN BEGIN
				myStruct = CREATE_STRUCT(myName, myValue)
			ENDIF ELSE BEGIN
				IF (count GT 1) THEN myStruct = CREATE_STRUCT(myStruct, myName, myValue)
			ENDELSE
        ENDIF 
    ENDFOR
    
    ;== Check that no value has been changed
    IF (count EQ 0) THEN pReturn = { NULL : 0 } $
        ELSE pReturn = myStruct
        
;    pNames = TAG_NAMES (self.p)
;    FOR i = 0, N_TAGS (self.p) - 1 DO BEGIN 
;        IF ((pSave.(i))[0] NE (self.p.(i))[0]) THEN BEGIN 
;            vType = SIZE (pSave.(i), /TYPE) 
;            value = STRING (pSave.(i)) 
;            vNum = N_ELEMENTS (pSave.(i)) 
;            ;== Type STRING 
;            IF (vType EQ 7) THEN BEGIN  -->
;                value = "'" + pSave.(i) + "'" ;'"' + pSave.(i) + '"'  -->
;            ENDIF  -->
;            IF (vNum GT 1) THEN BEGIN 
;                value = '' 
;                FOR j = 0, vNum - 1 DO BEGIN 
;                    value = value + ',' + STRING ((pSave.(i))[j]) 
;                ENDFOR 
;                value = '[' + STRMID (value, 1) + ']' 
;            ENDIF 
;            
;            temp = temp + ',"' + pNames[i] + '",' + value 
;        ENDIF 
;    ENDFOR
;        
;    IF (STRLEN (temp) EQ 0) THEN pReturn = { NULL : 0 } $
;    ELSE BEGIN
;        temp = STRMID (temp, 1)
;        temp = 'pReturn=CREATE_STRUCT('+temp+')'
;        result = E*X*E*C*U*T*E (temp)
;    ENDELSE
    
    ;== Restore the member data!
    self.p = pSave    
    RETURN, pReturn

END


; -----------------------------------------------------------------------------
; Acessor functions
; -----------------------------------------------------------------------------
FUNCTION PlotConfig::cancel  &  RETURN, self.cancel &  END


FUNCTION PlotConfig::getSettings

    IF PTR_VALID (self.settings) THEN BEGIN
        RETURN, *self.settings
    ENDIF ELSE BEGIN
        RETURN, { NULL : 0 }
    ENDELSE

END


; -----------------------------------------------------------------------------
; Load or save settings from a file
; -----------------------------------------------------------------------------
PRO PlotConfig::settings, LOAD = load, SAVE = save, $
    FILENAME = filename, ERROR = error
    
    error = 0

    loading = KEYWORD_SET (load)
    saving  = KEYWORD_SET (save)
    IF (loading + saving EQ 0) THEN loading = 1
        

    IF (loading) THEN BEGIN
    
       IF (N_ELEMENTS (filename) EQ 0) THEN BEGIN

          filename = DIALOG_PICKFILE (/READ, /MUST_EXIST, $
              FILTER = '*.cfg', DIALOG_PARENT = self.topID)

       ENDIF
       
       c = FINDFILE (filename, COUNT = found)
       IF ((found EQ 0) OR (filename EQ '')) THEN BEGIN ; FINDFILE is broken
          error = 1
          RETURN      	  
       ENDIF

       ; If the user accidently puts a space in front of the filename...
       retpos = STRPOS(filename, STRING(10B))
       IF (retpos NE 0) THEN BEGIN
          filename = STRMID(filename, retpos + 1)
       ENDIF

       OPENR, FL, filename, /GET_LUN
       
       ; Read the header
       line = ''
       READF, FL, line       
       IF (STRPOS (STRLOWCASE (line), 'plotconfig') EQ -1) THEN BEGIN

          error = 1
          w = DIALOG_MESSAGE (/ERROR, TITLE = 'File Read Error', $
	      ['This file does not appear to be', $
	       'a valid plotConfig settings file:', $
               file], DIALOG_PARENT = self.topID)
          
       ENDIF ELSE BEGIN

          WHILE (NOT EOF (FL)) DO BEGIN
          
              READF, FL, line
	      IF ((STRMID (STRTRIM (line, 2), 0, 1) NE ';') AND $
                  (STRCOMPRESS (line, /REMOVE_ALL) NE '')) $
                 THEN BEGIN

		 IF (FLOAT (!VERSION.RELEASE) GE 5.3) THEN BEGIN
		    tokens = CALL_FUNCTION ('STRSPLIT', line, '=', /EXTRACT)
		 ENDIF ELSE BEGIN
		    tokens = STR_SEP (line, '=')
		 ENDELSE
	      
                 IF (N_ELEMENTS (tokens) GT 1) THEN BEGIN

		    key   = STRTRIM (tokens[0], 2)
		    value = STRTRIM (tokens[1], 2)

		    CASE (STRUPCASE (key)) OF

		        'TITLE'      : self.p.title      = STRJOIN (value)
		        'SUBTITLE'   : self.p.subtitle   = STRJOIN (value)
		        'XTITLE'     : self.p.xTitle     = STRJOIN (value)
		        'YTITLE'     : self.p.yTitle     = STRJOIN (value)
		        'CHARSIZE'   : self.p.charSize   = FLOAT (value)
		        'CHARTHICK'  : self.p.charThick  = FLOAT (value)
		        'THICK'      : self.p.thick      = FLOAT (value)
		        'XTHICK'     : self.p.xThick     = FLOAT (value)
		        'YTHICK'     : self.p.yThick     = FLOAT (value)
		        'TICKLEN'    : self.p.tickLen    = FLOAT (value)
		        'XTICKS'     : self.p.xTicks     = FIX (value)
		        'YTICKS'     : self.p.yTicks     = FIX (value)
		        'XMINOR'     : self.p.xMinor     = FIX (value)
		        'YMINOR'     : self.p.yMinor     = FIX (value)
		        'LINESTYLE'  : self.p.lineStyle  = FIX (value)
		        'XGRIDSTYLE' : self.p.xGridStyle = FIX (value)
		        'YGRIDSTYLE' : self.p.yGridStyle = FIX (value)
		        'PSYM'       : self.p.psym       = FIX (value)
		        'SYMSIZE'    : self.p.symSize    = FLOAT (value)
		        'FONT'       : self.p.font       = FIX (value)
		        'XSTYLE'     : self.p.xStyle     = FIX (value) 
		        'YSTYLE'     : self.p.yStyle     = FIX (value)
		        'XMARGIN_LO' : self.p.xMargin[0] = FIX (value)
		        'XMARGIN_HI' : self.p.xMargin[1] = FIX (value)
		        'YMARGIN_LO' : self.p.yMargin[0] = FIX (value)
		        'YMARGIN_HI' : self.p.yMargin[1] = FIX (value)

		        ELSE         : $
                           MESSAGE, /CONTINUE, 'Unrecognized parameter: ' + key

		    ENDCASE

	         ENDIF

	      ENDIF
          
          ENDWHILE

          FOR i = 0, 4 DO BEGIN

              bitSet = (self.p.xStyle AND 2^i) NE 0
              self.xStyle[i] = bitSet
	  
              bitSet = (self.p.yStyle AND 2^i) NE 0
              self.yStyle[i] = bitSet

              ENDFOR
          
        ENDELSE
       
    ENDIF ; load


    
    IF (saving) THEN BEGIN
    
       IF (N_ELEMENTS (filename) EQ 0) THEN BEGIN
       
          filename = DIALOG_PICKFILE (/WRITE, /OVERWRITE_PROMPT, $
              FILTER = '*.cfg', DIALOG_PARENT = self.topID)
       
       ENDIF
        
       IF (filename EQ '') THEN BEGIN
          error = 1
          RETURN      	  
       ENDIF

       ; If the user accidently puts a space in front of the filename...
       retpos = STRPOS(filename, STRING(10B))
       IF (retpos NE 0) THEN BEGIN
          filename = STRMID(filename, retpos + 1)
       ENDIF

       OPENW, FL, filename, /GET_LUN
       
       ; Write the header
       PRINTF, FL, '; plotConfig settings'
       PRINTF, FL, ';'
             
       PRINTF, FL, 'TITLE      = ', self.p.title 
       PRINTF, FL, 'SUBTITLE   = ', self.p.subtitle   
       PRINTF, FL, 'XTITLE     = ', self.p.xTitle     
       PRINTF, FL, 'YTITLE     = ', self.p.yTitle     
       PRINTF, FL, 'CHARSIZE   = ', self.p.charSize   
       PRINTF, FL, 'CHARTHICK  = ', self.p.charThick  
       PRINTF, FL, 'THICK      = ', self.p.thick      
       PRINTF, FL, 'XTHICK     = ', self.p.xThick     
       PRINTF, FL, 'YTHICK     = ', self.p.yThick     
       PRINTF, FL, 'TICKLEN    = ', self.p.tickLen    
       PRINTF, FL, 'XTICKS     = ', self.p.xTicks     
       PRINTF, FL, 'YTICKS     = ', self.p.yTicks     
       PRINTF, FL, 'XMINOR     = ', self.p.xMinor     
       PRINTF, FL, 'YMINOR     = ', self.p.yMinor     
       PRINTF, FL, 'LINESTYLE  = ', self.p.lineStyle  
       PRINTF, FL, 'XGRIDSTYLE = ', self.p.xGridStyle 
       PRINTF, FL, 'YGRIDSTYLE = ', self.p.yGridStyle 
       PRINTF, FL, 'PSYM       = ', self.p.psym       
       PRINTF, FL, 'SYMSIZE    = ', self.p.symSize    
       PRINTF, FL, 'FONT       = ', self.p.font       
       PRINTF, FL, 'XSTYLE     = ', self.p.xStyle     
       PRINTF, FL, 'YSTYLE     = ', self.p.yStyle
       PRINTF, FL, 'XMARGIN_LO = ', self.p.xMargin[0]
       PRINTF, FL, 'XMARGIN_HI = ', self.p.xMargin[1]
       PRINTF, FL, 'YMARGIN_LO = ', self.p.yMargin[0]
       PRINTF, FL, 'YMARGIN_HI = ', self.p.yMargin[1]
       
    ENDIF ; save

    CLOSE, fl
    FREE_LUN, FL
    
END


; -----------------------------------------------------------------------------
; Plot parameter configuration widget
; -----------------------------------------------------------------------------
PRO PlotConfig__define

    template = { PLOT_CONFIG, $
    
        thick      : 0.0, $
        xThick     : 0.0, $
        yThick     : 0.0, $
        tickLen    : 0.0, $
        charSize   : 0.0, $
        charThick  : 0.0, $
        xTicks     : 0,   $
        xMinor     : 0,   $
        yTicks     : 0,   $
        yMinor     : 0,   $
        title      : '',  $
        xTitle     : '',  $
        yTitle     : '',  $
        subTitle   : '',  $
        xStyle     : 0,   $
        yStyle     : 0,   $
        lineStyle  : 0,   $
        xGridStyle : 0,   $
        yGridStyle : 0,   $
        font       : 0,   $
        psym       : 0,   $                 
        symSize    : 0.0, $
        xMargin    : INTARR (2), $
        yMargin    : INTARR (2)  $

    }
    
    obj = { PLOTCONFIG, $

        topID       : 0L, $
        drawID      : 0L, $                
        pixID       : 0L, $
        thickID     : 0L, $
        xThickID    : 0L, $
        yThickID    : 0L, $
        tickLenID   : 0L, $
        charSizeID  : 0L, $
        charThickID : 0L, $
        xTicksID    : 0L, $
        xMinorID    : 0L, $
        yTicksID    : 0L, $
        yMinorID    : 0L, $
        titleID     : 0L, $
        xtitleID    : 0L, $
        ytitleID    : 0L, $
        subtitleID  : 0L, $
        lineStyleID : 0L, $
        gridStyleID : 0L, $
        fontID      : 0L, $
        psymID      : 0L, $
        psymLineID  : 0L, $
        psymSizeID  : 0L, $                 
        xStyleID    : LONARR (5), $
        yStyleID    : LONARR (5), $
        xStyle      : INTARR (5), $
        yStyle      : INTARR (5), $
        xMarginID   : LONARR (2), $
        yMarginID   : LONARR (2), $
        
        settings    : PTR_NEW (), $

        psymLine    : 0,  $                 
        cancel      : 0,  $        
        redraw      : 0,  $
        
        xsize       : 0,  $
        ysize       : 0,  $
        
        p : { PLOT_CONFIG } $
    }

END


; -----------------------------------------------------------------------------
FUNCTION PLOTCONFIG, PARENT = parent, CANCEL = cancel, $
    TITLE = title, FILENAME = filename, INITIALIZE = initialize, $
    SETTINGS = settings

    wSave = !D.WINDOW
    pSave = !P
    xSave = !X
    ySave = !Y
    
    dName = !D.NAME
           
    o = OBJ_NEW ('plotConfig', PARENT = parent, $
        TITLE = title, FILENAME = filename, INITIALIZE = initialize, $
        SETTINGS = settings)
 
;    result = E*X*E*C*U*T*E ('p = o->p ()') ; OR (result EQ 0)
    
    p = o->p ()
    CATCH, createErr
    IF (createErr NE 0) THEN p = { NULL : 0 }
    
    cancel = o->cancel ()
    IF (cancel) THEN p = o->getSettings () ;{ NULL : 0 }
              
    OBJ_DESTROY, o
    
    !P = pSave    
    !X = xSave    
    !Y = ySave    
    WSET, wSave
    SET_PLOT, dName

    RETURN, p
     
END    
