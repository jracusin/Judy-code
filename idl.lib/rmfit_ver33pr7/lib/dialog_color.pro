;+
; NAME:
;    DIALOG_COLOR (FUNCTION)
;
; PURPOSE:
;    A dialog widget for interactive selection of an RGB color triple.
;    Selections can be made from integral values [0, 255], normalized
;    floating values [0.0, 1.0], slider widgets, or hexidecimal value.
;    DIALOG_COLOR uses an internal object to do all the work.
;   
; CALLING SEQUENCE:
;    rgb = DIALOG_COLOR ()
;
; INPUTS:
;    NONE
;
; OUTPUTS:
;    rgb
;        A (1, 3) array of values of the selected RGB color triple.
;        The color can then be loaded into a color index using TVLCT:
;
;           rgb = DIALOG_COLOR (CANCEL = cancel)  
;           IF (NOT cancel) THEN TVLCT, rgb, 0
;
;        Note that if the NORMAL or HEX keyword is used, the rgb return 
;        value can not be used as input into TVLCT.
;
; KEYWORDS:
;
;    PARENT
;        A widget ID specifying the parent of the PICKCOLOR widget.
;
;    TITLE
;        STRING containing the window title (default = 'DialogColor')
;
;    CANCEL
;        Set this keyword to a named variable that is set to 1 if the 
;        'Cancel' button was selected, or 0 if the 'Accept' button was 
;        selected.
;
;    INITIAL
;        Set this keyword to a 3-element INTARR specifying the initial RGB 
;        values to show.  The array elements must be 0-255.  For example, 
;        to set an initial value of blue, set INITIAL = [0, 0, 255].
;
;    INDEX
;        Set this keyword to return indexed color values [0, 255].
;        This is the default.
;
;    NORMAL
;        Set this keyword to return normalized color values [0.0, 1.0]
;
;    HEX
;        Set this keyword to return a STRING containing the hexidecimal 
;        color value.  Note that you can use IDL's EXECUTE function to 
;        convert this hex string to a integral type.
;
; SIDE EFFECTS:
;    The color index 0 is modified when the program is running. 
;    Prior to IDL 5.2, color decomposition remains set to zero when
;      this program exits.
;
; MODIFICATION HISTORY:
;
;    Written, Robert.Mallozzi@msfc.nasa.gov, 1999 November
;-


; -----------------------------------------------------------------------------
; Constructor
; -----------------------------------------------------------------------------
FUNCTION PickColor::init, PARENT = parent, TITLE = title, INITIAL = initial

    IF (FLOAT (!VERSION.RELEASE) GE 5.2) THEN BEGIN
       DEVICE, GET_DECOMPOSED = decomposed
       self.dSave = decomposed
    ENDIF ELSE BEGIN
       self.dSave = 0
    ENDELSE

    self.wSave = !D.WINDOW
    DEVICE, DECOMPOSED = 0

    TVLCT, rSave, gSave, bSave, /GET    
    self.rSave = rSave
    self.gSave = gSave
    self.bSave = bSave
        
    self->build, PARENT = parent, TITLE = title, INITIAL = initial
    
    RETURN, 1
    
END


; -----------------------------------------------------------------------------
; Destructor
; -----------------------------------------------------------------------------
PRO PickColor::cleanup
    
    IF (FLOAT (!VERSION.RELEASE) GE 5.2) THEN $
       DEVICE, DECOMPOSED = self.dSave

    TVLCT, self.rSave, self.gSave, self.bSave
    WSET, self.wSave
        
END


; ----------------------------------------------------------------------------
; Return the current values of the slider widgets
; ----------------------------------------------------------------------------
FUNCTION PickColor::getSliders

    WIDGET_CONTROL, self.sliderID[0], GET_VALUE = r
    WIDGET_CONTROL, self.sliderID[1], GET_VALUE = g
    WIDGET_CONTROL, self.sliderID[2], GET_VALUE = b

    RETURN, { PICKCOLOR_RGB, r : r, g : g, b : b }

END


; ----------------------------------------------------------------------------
; Set the color based on the current slider values
; ----------------------------------------------------------------------------
PRO PickColor::setColor
    
    c = self->getSliders ()
     
    WSET, self.winID
    TVLCT, c.r, c.g, c.b, 0
    POLYFILL, [0, 0, 1, 1, 0], [0, 1, 1, 0, 0], /NORMAL, COLOR = 0
    
END


; ----------------------------------------------------------------------------
; Update the widget values
; ----------------------------------------------------------------------------
PRO PickColor::setValue, index, value, $
    INTEGER = integer, FLOATING = floating, HEX = hex

    IF (KEYWORD_SET (integer)) THEN BEGIN
       
       ival = FIX (value)
       fval = ival / 255.0
       
    ENDIF
   
    IF (KEYWORD_SET (floating)) THEN BEGIN
       
       fval = FLOAT (value)
       ival = FIX (ROUND (fval * 255.0))
       
    ENDIF

    IF (KEYWORD_SET (hex)) THEN BEGIN
       
       ival = self->fromHex (value)
       fval = ival / 255.0
       
    ENDIF

    ival =   0 > ival < 255
    fval = 0.0 > fval < 1.0

    WIDGET_CONTROL, self.sliderID[index], SET_VALUE = ival

    ival = STRTRIM (ival, 2)
    fval = STRTRIM (STRING (fval, FORMAT = '(F5.3)'), 2)
        
    WIDGET_CONTROL, self.iTextID[index], SET_VALUE = ival
    WIDGET_CONTROL, self.fTextID[index], SET_VALUE = fval

    c = self->getSliders ()    
    s = self->toHex (c.r, c.g, c.b)
    WIDGET_CONTROL, self.hexID, SET_VALUE = s
    WIDGET_CONTROL, self.hexID, SET_TEXT_SELECT = 7
    

END


; ----------------------------------------------------------------------------
; Event handler
; ----------------------------------------------------------------------------
PRO PICKCOLOR_EVENT, event

    WIDGET_CONTROL, event.top, GET_UVALUE = self
    self->eventHandler, event

END
    
PRO PickColor::eventHandler, event
    
    type = TAG_NAMES (event, /STRUCTURE)
    CASE (type) OF

    'WIDGET_BUTTON': BEGIN

        WIDGET_CONTROL, event.id, GET_VALUE = value

        CASE (value) OF

            'Cancel': BEGIN

                self.cancel = 1
                WIDGET_CONTROL, event.top, /DESTROY
                END
                 
            'Accept': BEGIN

                self.cancel = 0
                c = self->getSliders ()
                self.colors = REFORM ([c.r, c.g, c.b], 1, 3)
                WIDGET_CONTROL, event.top, /DESTROY
                END

        ENDCASE
        END
        
    'WIDGET_KBRD_FOCUS': BEGIN

        IF (event.enter EQ 1) THEN RETURN
        
        integer  = 0
        floating = 0
        hex      = 0

        CASE (event.id) OF
        
            self.iTextID[0] : BEGIN & index = 0 & integer = 1  & END
            self.iTextID[1] : BEGIN & index = 1 & integer = 1  & END
            self.iTextID[2] : BEGIN & index = 2 & integer = 1  & END

            self.fTextID[0] : BEGIN & index = 0 & floating = 1 & END
            self.fTextID[1] : BEGIN & index = 1 & floating = 1 & END
            self.fTextID[2] : BEGIN & index = 2 & floating = 1 & END

            self.hexID : BEGIN
                hex = 1
                WIDGET_CONTROL, event.id, SET_TEXT_SELECT = 7
                END

            ELSE :
                       
        ENDCASE

        WIDGET_CONTROL, event.id, GET_VALUE = value
        value = value[0]
        
        IF (integer) THEN BEGIN
           value = FIX (value)
           self->setValue, index, value, /INTEGER
        ENDIF

        IF (floating) THEN BEGIN
           value = FLOAT (value)
           self->setValue, index, value, /FLOATING
        ENDIF
        
        IF (hex) THEN BEGIN
           
           len = STRLEN (value)               
           IF (len LT 6) THEN BEGIN
           
              c = self->getSliders ()
           
              self->setValue, 0, c.r, /INTEGER
              self->setValue, 0, c.g, /INTEGER
              self->setValue, 0, c.b, /INTEGER
              self->setColor

              RETURN              
           ENDIF
                       
           bs = STRMID (value, len - 2, 2)
           gs = STRMID (value, len - 4, 2)
           rs = STRMID (value, len - 6, 2)

           self->setValue, 0, rs, /HEX
           self->setValue, 1, gs, /HEX
           self->setValue, 2, bs, /HEX

        ENDIF

        self->setColor
        END
        
    'WIDGET_TEXT_CH': BEGIN

        integer  = 0
        floating = 0
        hex      = 0
        
        CASE (event.id) OF
        
            self.iTextID[0] : BEGIN & index = 0 & integer = 1  & END
            self.iTextID[1] : BEGIN & index = 1 & integer = 1  & END
            self.iTextID[2] : BEGIN & index = 2 & integer = 1  & END

            self.fTextID[0] : BEGIN & index = 0 & floating = 1 & END
            self.fTextID[1] : BEGIN & index = 1 & floating = 1 & END
            self.fTextID[2] : BEGIN & index = 2 & floating = 1 & END
           
            self.hexID      : hex = 1

            ELSE :
            
        ENDCASE

        WIDGET_CONTROL, event.id, GET_VALUE = value
        value = value[0]
        
        IF (integer) THEN BEGIN
           value = FIX (value)
           self->setValue, index, value, /INTEGER
        ENDIF

        IF (floating) THEN BEGIN
           value = FLOAT (value)
           self->setValue, index, value, /FLOATING
        ENDIF
        
        IF (hex) THEN BEGIN

           len = STRLEN (value)
           IF (len LT 6) THEN BEGIN
           
              c = self->getSliders ()
           
              self->setValue, 0, c.r, /INTEGER
              self->setValue, 0, c.g, /INTEGER
              self->setValue, 0, c.b, /INTEGER
              self->setColor

              RETURN              
           ENDIF
               
           bs = STRMID (value, len - 2, 2)
           gs = STRMID (value, len - 4, 2)
           rs = STRMID (value, len - 6, 2)

           self->setValue, 0, rs, /HEX
           self->setValue, 1, gs, /HEX
           self->setValue, 2, bs, /HEX

        ENDIF

        self->setColor
        END

    'WIDGET_SLIDER': BEGIN
        
        CASE (event.id) OF
        
            self.sliderID[0] : index = 0
            self.sliderID[1] : index = 1
            self.sliderID[2] : index = 2
           
        ENDCASE

        WIDGET_CONTROL, event.id, GET_VALUE = value
        value = FIX (value[0])
        self->setValue, index, value, /INTEGER

        self->setColor
        END

    ELSE :
    
    ENDCASE
   
END


; ----------------------------------------------------------------------------
; Create a hex string of the current color
; ----------------------------------------------------------------------------
FUNCTION PickColor::toHex, r, g, b

    rs = STRTRIM (STRING (r, FORMAT = '(z)'), 2)
    IF (r LT 16) THEN rs = '0' + rs
      
    gs = STRTRIM (STRING (g, FORMAT = '(z)'), 2)
    IF (g LT 16) THEN gs = '0' + gs

    bs = STRTRIM (STRING (b, FORMAT = '(z)'), 2)
    IF (b LT 16) THEN bs = '0' + bs

    s = '#' + rs + gs + bs
    
    RETURN, s

END


; ----------------------------------------------------------------------------
; Parse a hex string into an integral value
; ----------------------------------------------------------------------------
FUNCTION PickColor::fromHex, value

    status = EXECUTE ("v ='" + value + "'x")
    IF (status EQ 0) THEN BEGIN
       MESSAGE, /CONTINUE, 'EXECUTE failed.'
       RETURN, ''
    ENDIF
       
    RETURN, v
        
END


; ----------------------------------------------------------------------------
; Build the widget
; ----------------------------------------------------------------------------
PRO PickColor::build, PARENT = parent, TITLE = title, INITIAL = initial

    IF (N_ELEMENTS (title) EQ 0) THEN $
       title = 'DialogColor'
       
    IF (N_ELEMENTS (parent) NE 0) THEN BEGIN
    
       top = WIDGET_BASE (TITLE = title, /COLUMN, /BASE_ALIGN_CENTER, $
           TLB_FRAME_ATTR = 9, GROUP_LEADER = parent, /MODAL)
    
    ENDIF ELSE BEGIN
    
       top = WIDGET_BASE (TITLE = title, /COLUMN, /BASE_ALIGN_CENTER)

    ENDELSE
    
    drawID = WIDGET_DRAW (top, YSIZE = 50)
        
    base = WIDGET_BASE (top, /ROW)
    self.iTextID[0] = WIDGET_TEXT (base, XSIZE = 5, VALUE = '0', $
        /EDIT, /KBRD_FOCUS_EVENTS)
    self.sliderID[0] = WIDGET_SLIDER (base, MIN = 0, MAX = 255, VALUE = 0, $
        /SUPPRESS_VALUE, /DRAG)
    self.fTextID[0] = WIDGET_TEXT (base, XSIZE = 5, VALUE = '0.000', $
        /EDIT, /KBRD_FOCUS_EVENTS)
        
    base = WIDGET_BASE (top, /ROW)
    self.iTextID[1] = WIDGET_TEXT (base, XSIZE = 5, VALUE = '0', $
        /EDIT, /KBRD_FOCUS_EVENTS)
    self.sliderID[1] = WIDGET_SLIDER (base, MIN = 0, MAX = 255, VALUE = 0, $
        /SUPPRESS_VALUE, /DRAG)    
    self.fTextID[1] = WIDGET_TEXT (base, XSIZE = 5, VALUE = '0.000', $
        /EDIT, /KBRD_FOCUS_EVENTS)

    base = WIDGET_BASE (top, /ROW)
    self.iTextID[2] = WIDGET_TEXT (base, XSIZE = 5, VALUE = '0', $
        /EDIT, /KBRD_FOCUS_EVENTS)
    self.sliderID[2] = WIDGET_SLIDER (base, MIN = 0, MAX = 255, VALUE = 0, $
        /SUPPRESS_VALUE, /DRAG)
    self.fTextID[2] = WIDGET_TEXT (base, XSIZE = 5, VALUE = '0.000', $
        /EDIT, /KBRD_FOCUS_EVENTS)

    self.hexID = WIDGET_TEXT (top, VALUE = '#000000', /EDIT, /KBRD_FOCUS_EVENTS)

    base = WIDGET_BASE (top, /ROW, /ALIGN_CENTER, SPACE = 20)
    button = WIDGET_BUTTON (base, VALUE = 'Accept')
    button = WIDGET_BUTTON (base, VALUE = 'Cancel')
      
    geo = WIDGET_INFO (top, /GEOMETRY)
    WIDGET_CONTROL, drawID,  SCR_XSIZE = geo.scr_xsize         
    WIDGET_CONTROL, self.hexID, SCR_XSIZE = geo.scr_xsize         

    WIDGET_CONTROL, top, SET_UVALUE = self, /REALIZE
    WIDGET_CONTROL, drawID, GET_VALUE = winID
    self.winID = winID
    
    c = REFORM ([0, 0, 0], 1, 3)
    IF (N_ELEMENTS (initial) NE 0) THEN $
       c = REFORM (initial, 1, 3)
    TVLCT, c, 0

    self->setValue, 0, c[0, 0], /INTEGER
    self->setValue, 1, c[0, 1], /INTEGER
    self->setValue, 2, c[0, 2], /INTEGER

    self->setColor    

    XMANAGER, 'PICKCOLOR', top 


END


; ----------------------------------------------------------------------------
; Return object data
; ----------------------------------------------------------------------------
FUNCTION PickColor::cancel & RETURN, self.cancel & END
FUNCTION PickColor::colors & RETURN, self.colors & END


; -----------------------------------------------------------------------------
; 
; -----------------------------------------------------------------------------
PRO PickColor__define
    
    TVLCT, rSave, gSave, bSave, /GET    

    obj = { PICKCOLOR, $

        winID         : 0L,           $
        sliderID      : LONARR (3),   $
        iTextID       : LONARR (3),   $
        fTextID       : LONARR (3),   $         
        hexID         : 0L,           $ 
        
        dSave         : 0L,           $
        wSave         : 0L,           $
        rSave         : rSave,        $
        gSave         : gSave,        $
        bSave         : bSave,        $
        
        cancel        : 0,            $
        colors        : INTARR (1, 3) $ 
    }

END


; ----------------------------------------------------------------------------
; Dialog to allow interactive selection of color triples
; ----------------------------------------------------------------------------
FUNCTION DIALOG_COLOR, $
    INITIAL = initial, $
    TITLE   = title,   $
    PARENT  = parent,  $
    CANCEL  = cancel,  $
    INDEX   = index,   $
    NORMAL  = normal,  $
    HEX     = hex 
    
    o = OBJ_NEW ('PickColor', INITIAL = initial, TITLE = title, PARENT = parent)
    
    cancel = o->cancel ()
    colors = o->colors ()         

    IF (KEYWORD_SET (normal)) THEN $
       colors = colors / 255.0
                   
    IF (KEYWORD_SET (hex)) THEN BEGIN
       r = colors[0, 0]
       g = colors[0, 1]
       b = colors[0, 2]
       colors = o->toHex (r, g, b)
    ENDIF

    OBJ_DESTROY, o
    
    RETURN, colors
    
END
