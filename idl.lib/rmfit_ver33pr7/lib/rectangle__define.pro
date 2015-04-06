; -----------------------------------------------------------------------------
;+
; NAME:
;     Rectangle (OBJECT)
;
; PURPOSE:
;     This object implements a custom widget that contains a resizable 
;     2d rectangular polygon in its own WIDGET_DRAW.
;
;     I'm not sure it has any general purpose use; it was written as a 
;     utility object for PsConfig to allow the user to interactively create 
;     a Postscript page layout.
;
; CALLING SEQUENCE:
;     obj = OBJ_NEW ('Rectangle')
;
; INPUTS:
;     VERTICES (optional) 
;         A (2, 4) FLTARR of the vertices of the rectangular polygon
;         in NORMAL coordinates.  The (*, 0) elements must be the lower 
;         left corner, with the remaining vertices specified in a 
;         clockwise direction.  The (0, *) elements are the x coordinates
;         of the vertices, and the (1, *) elements are the y coordinates.
;     
; KEYWORDS:
;
;     XSIZE  
;         An integer specifying the xsize of the widget (pixels, default = 400)
;
;     YSIZE
;         An integer specifying the ysize of the widget (pixels, default = 400)
;
;     PARENT
;         A WIDGET_BASE ID specifying the parent of the widget
;
;     COLOR
;         A three element INTARR containing the RGB color of the polygon,
;         range = [0, 255] (default = [255, 255, 255])
;
;     BACKGROUND
;         A three element INTARR containing the RGB color of the background
;         range = [0, 255] (default = [0, 0, 0])
;
;     PRESS
;         Set this keyword to a STRING denoting a procedure to be called 
;         when the mouse is clicked on the rectangle.  The procedure can 
;         be defined to accept two optional parameters: the x mouse position, 
;         and the y mouse position.  
;
;         To call an object method instead of an external procedure, set
;         the OBJECT keyword to a valid object reference.  On mouse press,
;         the object method specified by PRESS will be called.
;
;     RELEASE
;         Set this keyword to a STRING denoting a procedure to be called 
;         when the mouse is released on the rectangle.  The procedure can 
;         be defined to accept two optional parameters: the x mouse position, 
;         and the y mouse position.
;
;         To call an object method instead of an external procedure, set
;         the OBJECT keyword to a valid object reference.  On mouse release,
;         the object method specified by RELEASE will be called.
;
;         * NOTE: Special case - this keyword will also call the RELEASE
;           procedure (or method) if the user moves the rectangle using
;           the keyboard.  In this case, the x and y coordinates will be 
;           set to scalar zero.
;
;     OBJECT
;         See the PRESS and RELEASE keywords.
;
; PUBLIC METHODS:
;     
;     show (PROCEDURE) - Show the widget
;         Inputs: NONE
;        Outputs: NONE
;       Keywords: NONE
;     
;     center (PROCEDURE) - Center the polygon
;         Inputs: NONE
;        Outputs: NONE
;       Keywords: If no keywords are given, center polygon in both x and y
;                 X    : center the polygon in x
;                 Y    : center the polygon in y
;                 FILL : fill the window, and center in x and y  
;     
;     vertices (FUNCTION) - Return polygon vertices (NORMAL coordinates)
;         Inputs: NONE
;        Outputs: FLTARR (2, 4) containing the polygon vertices
;       Keywords: NONE
;
;     setVertices (PROCEDURE) - Set polygon vertices (NORMAL coordinates)
;         Inputs: FLTARR (2, 4) containing the polygon vertices
;        Outputs: NONE
;       Keywords: NONE
;
;     size (FUNCTION) - Return size of the drawing area
;         Inputs: NONE
;        Outputs: INTARR (2) containing the drawing area size (pixels)
;       Keywords: NONE
;
;     setSize (PROCEDURE) - Set the size of the drawing area
;         Inputs: NONE
;        Outputs: NONE
;       Keywords: XSIZE : x size of the drawing area
;                 YSIZE : y size of the drawing area
;                 UNITS : Set UNITS equal to 0 (zero) to specify 
;                         that all measurements are in pixels (default), 
;                         to 1 to specify that all measurements are in 
;                         inches, or to 2 to specify that all measurements 
;                         are in centimeters.
;
;     move (PROCEDURE) - Move the rectangle
;         Inputs: PERCENT (OPTIONAL, DEFAULT = 2.0) : percentage of the drawing
;                 area to use for the move increment
;        Outputs: NONE
;       Keywords: NORTH : move the rectangle up
;                 SOUTH : move the rectangle down
;                 EAST  : move the rectangle to the right
;                 WEST  : move the rectangle to the left
;
;  MOUSE BINDINGS
;
;      LEFT BUTTON drag
;          Move or resize the polygon
;
;      LEFT BUTTON double click
;          Center the polygon in x and y
;      
;      RIGHT BUTTON double click
;          Center and fill the polygon
;
;      MIDDLE BUTTON click
;          Center the polygon in x
;      
;      RIGHT BUTTON  click
;          Center the polygon in y
;
; MODIFICATION HISTORY:
;
;     Written, 1999 December, Robert.Mallozzi@msfc.nasa.gov
;
;-
; -----------------------------------------------------------------------------



; -----------------------------------------------------------------------------
; Constructor
; -----------------------------------------------------------------------------
FUNCTION Rectangle::init, vertices, $
    PRESS = press, RELEASE = release, OBJECT = object, $
    _EXTRA = extra

    IF (OBJ_VALID (object)) THEN self.object = object
    
    self.press   = ''
    self.release = ''
    IF (N_ELEMENTS (press) NE 0)   THEN self.press   = STRING (press)
    IF (N_ELEMENTS (release) NE 0) THEN self.release = STRING (release)
    
    RETURN, self->create (vertices, _EXTRA = extra)

END


; -----------------------------------------------------------------------------
; Destructor
; -----------------------------------------------------------------------------
PRO Rectangle::cleanup

    IF (WIDGET_INFO (self.top, /VALID)) THEN $
       WIDGET_CONTROL, self.top, /DESTROY    

    OBJ_DESTROY, self.thisContainer

    PTR_FREE, self.vertices
    
END


; -----------------------------------------------------------------------------
; Transform 2d object vertices from original data to vertices based
; on the current transformation matrix.
;
; VERTICES is FLTARR (2, n)
; XFORM is the current transformation matrix
;
; -----------------------------------------------------------------------------
FUNCTION Rectangle::transformVertices, vertices, xform

    c = vertices
    FOR i = 0, (SIZE (vertices))[2] - 1 DO BEGIN

        v = [vertices[*, i], 0.0, 1.0]                  
        c[*, i] = (xform ## v)[0:1]

    ENDFOR

    RETURN, c
    
END

; -----------------------------------------------------------------------------
; Transform 2d object vertices from current transform to original data
; -----------------------------------------------------------------------------
FUNCTION Rectangle::untransformVertices, vertices, xform

    c = vertices
    FOR i = 0, (SIZE (vertices))[2] - 1 DO BEGIN

        v = [vertices[*, i], 0.0, 1.0]                  
        c[*, i] = (INVERT (xform) ## v)[0:1]

    ENDFOR

    RETURN, c
    
END


; -----------------------------------------------------------------------------
; Get the zone containing the point (xDev, yDev).  
; xDev, yDev are in device coordinates.
;
; Zones are defined by a band around the outside of the rectangular polygon
; object.  By default, the band is 5% of the width and height of the polygon, 
; but this can be changed with the PERCENT keyword.  There are three
; zones along the x-axis, and three zones along the y-axis.  Zones are numbered
; starting at the lower left corner (zone 0, 0) and increase to the right 
; and upward.  The upper right corner zone is (2, 2).
; -----------------------------------------------------------------------------
FUNCTION Rectangle::getZone, xDev, yDev, vertices, xform, PERCENT = percent

    IF (N_ELEMENTS (percent) EQ 0) THEN percent = 5.0
     
    ; Transform vertices based on the current transformation matrix
    v = self->transformVertices (vertices, xform)
        
    xMin = MIN (v[0, *], MAX = xMax)
    yMin = MIN (v[1, *], MAX = yMax)

    xBand = (xMax - xMin) * (percent / 100.0)
    yBand = (yMax - yMin) * (percent / 100.0)

    xLo = xMin + xBand
    xHi = xMax - xBand

    yLo = yMin + yBand
    yHi = yMax - yBand

    xPt = xDev / FLOAT (self.xsize)
    yPt = yDev / FLOAT (self.ysize)

    xZone = 1
    IF (xPt LT xLo) THEN xZone = 0
    IF (xPt GT xHi) THEN xZone = 2

    yZone = 1
    IF (yPt LT yLo) THEN yZone = 0
    IF (yPt GT yHi) THEN yZone = 2

    RETURN, { ZONE, x: xZone, y: yZone }

END


; -----------------------------------------------------------------------------
; Resize the rectangle according to the zone that is selected.
; This function should be called repeatedly during the processing of
; MOTION events as the object is resized.
; -----------------------------------------------------------------------------
FUNCTION Rectangle::resize, v, deltaX, deltaY, zone

    vout = v
    
    ; SW
    IF (zone.x EQ 0 AND zone.y EQ 0) THEN BEGIN
       vout[0, 0:1] = v[0, 0:1] + deltaX
       vout[1, 0]   = v[1, 0]   + deltaY
       vout[1, 3]   = v[1, 3]   + deltaY
    ENDIF

    ; EW - left
    IF (zone.x EQ 0 AND zone.y EQ 1) THEN BEGIN
       vout[0, 0:1] = v[0, 0:1] + deltaX
    ENDIF

    ; NW
    IF (zone.x EQ 0 AND zone.y EQ 2) THEN BEGIN
       vout[0, 0:1] = v[0, 0:1] + deltaX
       vout[1, 1:2] = v[1, 1:2] + deltaY
    ENDIF

    ; NS - bottom
    IF (zone.x EQ 1 AND zone.y EQ 0) THEN BEGIN
       vout[1, 0] = v[1, 0] + deltaY
       vout[1, 3] = v[1, 3] + deltaY
    ENDIF

    ; NS - top
    IF (zone.x EQ 1 AND zone.y EQ 2) THEN BEGIN
       vout[1, 1:2] = v[1, 1:2] + deltaY
    ENDIF

    ; SE
    IF (zone.x EQ 2 AND zone.y EQ 0) THEN BEGIN
       vout[0, 2:3] = v[0, 2:3] + deltaX
       vout[1, 0] = v[1, 0] + deltaY
       vout[1, 3] = v[1, 3] + deltaY
    ENDIF

    ; EW - right
    IF (zone.x EQ 2 AND zone.y EQ 1) THEN BEGIN
       vout[0, 2:3] = v[0, 2:3] + deltaX
    ENDIF

    ; NE
    IF (zone.x EQ 2 AND zone.y EQ 2) THEN BEGIN
       vout[0, 2:3] = v[0, 2:3] + deltaX
       vout[1, 1:2] = v[1, 1:2] + deltaY
    ENDIF

    RETURN, vout

END


; -----------------------------------------------------------------------------
; Create the resizeable rectangle.  Vertices is FLTARR (2, n).
; -----------------------------------------------------------------------------
FUNCTION Rectangle::create, vertices, PARENT = parent, $
    XSIZE = xsize, YSIZE = ysize, COLOR = color, BACKGROUND = background

    haveParent = (N_ELEMENTS (parent) NE 0)

    IF (haveParent) THEN BEGIN
    
       IF (NOT WIDGET_INFO (parent, /VALID)) THEN BEGIN
          MESSAGE, /CONTINUE, 'PARENT not a valid widget ID.'
          RETURN, 0
       ENDIF
        
       self.top = WIDGET_BASE (parent, /COLUMN, UNAME = 'RECT_OBJ_TOPID', MAP=0)
    
    ENDIF ELSE BEGIN
    
       self.top = WIDGET_BASE (/COLUMN, UNAME = 'RECT_OBJ_TOPID', MAP = 1)

    ENDELSE
    
    IF (N_ELEMENTS (xsize)      EQ 0) THEN xsize      = 400
    IF (N_ELEMENTS (ysize)      EQ 0) THEN ysize      = 400
    IF (N_ELEMENTS (color)      EQ 0) THEN color      = [255, 255, 255]
    IF (N_ELEMENTS (background) EQ 0) THEN background = [0, 0, 0]

    self.xsize = FIX (xsize)
    self.ysize = FIX (ysize)
    
    PTR_FREE, self.vertices
    IF (N_ELEMENTS (vertices) EQ 0) THEN BEGIN

       v = FLTARR (2, 4)
       v[0, *] = [0.3, 0.3, 0.7, 0.7]
       v[1, *] = [0.3, 0.7, 0.7, 0.3]

       self.vertices = PTR_NEW (v)

    ENDIF ELSE BEGIN
    
       self.vertices = PTR_NEW (vertices)
    
    ENDELSE
       
    subBase = WIDGET_BASE (self.top)
    self.drawID = WIDGET_DRAW (subBase, $
        XSIZE = self.xsize, YSIZE = self.ysize, $
        /BUTTON_EVENTS, /EXPOSE_EVENTS, /MOTION_EVENTS, $
        RETAIN = 0, GRAPHICS_LEVEL = 2, EVENT_PRO = 'RECT_OBJ_EVENT')

    WIDGET_CONTROL, self.top, /REALIZE
    WIDGET_CONTROL, self.drawID, GET_VALUE = thisWindow
    self.thisWindow = thisWindow

    self.thisView = OBJ_NEW ('IDLgrView', $
        VIEWPLANE_RECT = [0, 0, 1, 1], COLOR = background)
    
    self.bbox = OBJ_NEW ('IDLgrPolygon', *self.vertices, COLOR = color)
    self.bboxModel = OBJ_NEW ('IDLgrModel', NAME = 'BBMODEL', SELECT_TARGET = 1)
    self.bboxModel->add, self.bbox

    self.thisView->add, self.bboxModel

    self.thisContainer = OBJ_NEW ('IDL_Container')
    self.thisContainer->add, self.thisView
    self.thisContainer->add, self.thisWindow
    self.thisContainer->add, self.bbox
    self.thisContainer->add, self.bboxModel

    WIDGET_CONTROL, self.top, SET_UVALUE = self    

    IF (haveParent) THEN BEGIN

       XMANAGER, 'RECT_OBJ', self.top, /JUST_REG
    
    ENDIF ELSE BEGIN
    
       XMANAGER, 'RECT_OBJ', self.top, /NO_BLOCK
    
    ENDELSE

    RETURN, 1


END



; -----------------------------------------------------------------------------
; Main event handler
; -----------------------------------------------------------------------------
PRO RECT_OBJ_EVENT, event

    id = WIDGET_INFO (event.top, FIND_BY_UNAME = 'RECT_OBJ_TOPID')
    IF (id EQ 0) THEN $
       MESSAGE, 'Failed to find top level widget ID in hierarchy.'
       
    WIDGET_CONTROL, id, GET_UVALUE = self
    self->eventHandler, event

END

PRO Rectangle::eventHandler, event

    type = TAG_NAMES (event, /STRUCT)

    IF (type EQ 'WIDGET_DRAW') THEN BEGIN

       ; Discard multi-button mouse events
       IF (event.press GT 4) THEN RETURN 
       
       btns = ['NONE', 'LEFT', 'MIDDLE', 'NONE', 'RIGHT']
       btn  = btns[event.press]

       evts = ['PRESS', 'RELEASE', 'MOTION', 'SCROLL', 'EXPOSE']
       evt  = evts[event.type]

       CASE (evt) OF

          'EXPOSE': self.thisWindow->draw, self.thisView

          'PRESS': BEGIN

              ; Left button double click: center rectangle
              IF (btn EQ 'LEFT' AND event.clicks EQ 2) THEN BEGIN
                 self->center
                 RETURN
              ENDIF

              ; Right button double click: center fill rectangle
              IF (btn EQ 'RIGHT' AND event.clicks EQ 2) THEN BEGIN
                 self->center, /FILL
                 RETURN
              ENDIF

              ; Middle button: center in x    
              IF (btn EQ 'MIDDLE') THEN BEGIN
                 self->center, /X
                 RETURN
              ENDIF

              ; Right button: center in y   
              IF (btn EQ 'RIGHT') THEN BEGIN
                 self->center, /Y
                 RETURN
              ENDIF   

              ; Find a selected object
              obj = self.thisWindow->select (self.thisView, [event.x, event.y])
              IF (NOT OBJ_VALID (obj[0])) THEN $
                 RETURN

              ; Get the first (closest) object in the list of selected objects
              obj = obj[0]

              ; Get the zone in which the user clicked
              m = obj->getCTM ()
              self.bbox->getProperty, DATA = vertices
              self.zone = self->getZone (event.x, event.y, vertices, m)

              ; All zones are resize zones, except the center 

              self.inResize = 1
              IF (self.zone.x EQ 1 AND self.zone.y EQ 1) THEN self.inResize = 0

              ; Move the selected object
              IF (NOT self.inResize) THEN BEGIN

                 ; Turn motion events ON            
                 self.inMotion = 1 

              ENDIF

              ; Record starting coordinates
              self.xstart = event.x
              self.ystart = event.y

              ; Save the selected item
              self.selectedObj = obj
              self.selectedObj->getProperty, NAME = selectedName
              self.selectedName = selectedName

              IF (self.press NE '') THEN BEGIN                     
                 IF (OBJ_VALID (self.object)) THEN BEGIN
                    CALL_METHOD, self.press, self.object, event.x, event.y
                 ENDIF ELSE BEGIN
                    CALL_PROCEDURE, self.press, event.x, event.y              
                 ENDELSE               
              ENDIF
              END

          'RELEASE': BEGIN

              ; Turn motion events OFF. Reinitialize selected object fields.

              self.inMotion = 0
              self.inResize = 0
              self.selectedName = ''
              self.selectedObj = OBJ_NEW ()

              IF (self.release NE '') THEN BEGIN                     
                 IF (OBJ_VALID (self.object)) THEN BEGIN
                    CALL_METHOD, self.release, self.object, event.x, event.y
                 ENDIF ELSE BEGIN
                    CALL_PROCEDURE, self.release, event.x, event.y              
                 ENDELSE               
              ENDIF
              END

          'MOTION': BEGIN

              IF (self.inMotion) THEN BEGIN

                 ; Calculate movement in normalized coordinates
                 deltaX = (event.x - self.xstart) / FLOAT (self.xsize)
                 deltaY = (event.y - self.ystart) / FLOAT (self.ysize)

                 ; Update start coordinates
                 self.xstart = event.x
                 self.ystart = event.y

                 ; Translate the selected model
                 self.selectedObj->translate, deltaX, deltaY, 0

                 ; Draw the view
                 self.thisWindow->draw, self.thisView

              ENDIF ELSE $
              IF (self.inResize) THEN BEGIN

                 ; Calculate resize in normalized coordinates
                 deltaX = (event.x - self.xstart) / FLOAT (self.xsize)
                 deltaY = (event.y - self.ystart) / FLOAT (self.ysize)

                 ; Update start coordinates
                 self.xstart = event.x
                 self.ystart = event.y
                 
                 ; Current transformation matrix 
                 m = self.selectedObj->getCTM ()

                 ; Transform vertex data
                 self.bbox->getProperty, DATA = vertices
                 v = self->transformVertices (vertices, m)

                 ; Resize the object
                 v = self->resize (v, deltaX, deltaY, self.zone)

                 ; Transform vertices back to current location
                 v = self->untransformVertices (v, m)

                 ; Update object vertices
                 self.bbox->setProperty, DATA = v

                 ; Redraw the view
                 self.thisWindow->draw, self.thisView

              ENDIF ELSE BEGIN

                 self.thisWindow->setCurrentCursor, 'CROSSHAIR'

                 obj = self.thisWindow->select ( $
                     self.thisView, [event.x, event.y])

                 IF (OBJ_VALID (obj[0])) THEN BEGIN

                    ; Get the first (closest) object in list of selected objects
                    obj = obj[0]

                    obj->getProperty, NAME = selectedName
                    IF (STRUPCASE (selectedName) EQ 'BBMODEL') THEN BEGIN

                       m = obj->getCTM ()
                       self.bbox->getProperty, DATA = vertices
                       zone = self->getZone (event.x, event.y, vertices, m)
                                             
                       IF (zone.x EQ 0 AND zone.y EQ 0) THEN c = 'SIZE_SW'
                       IF (zone.x EQ 0 AND zone.y EQ 1) THEN c = 'SIZE_EW'
                       IF (zone.x EQ 0 AND zone.y EQ 2) THEN c = 'SIZE_NW'
                       IF (zone.x EQ 1 AND zone.y EQ 0) THEN c = 'SIZE_NS'
                       IF (zone.x EQ 1 AND zone.y EQ 1) THEN c = 'MOVE'
                       IF (zone.x EQ 1 AND zone.y EQ 2) THEN c = 'SIZE_NS'
                       IF (zone.x EQ 2 AND zone.y EQ 0) THEN c = 'SIZE_SE'
                       IF (zone.x EQ 2 AND zone.y EQ 1) THEN c = 'SIZE_EW'
                       IF (zone.x EQ 2 AND zone.y EQ 2) THEN c = 'SIZE_NE'

                       self.thisWindow->setCurrentCursor, c

                    ENDIF

                 ENDIF

              ENDELSE
              END

          ELSE: ; do nothing

       ENDCASE

    ENDIF ; WIDGET_BUTTON

END


; -----------------------------------------------------------------------------
; Show the widget
; -----------------------------------------------------------------------------
PRO Rectangle::show 

    WIDGET_CONTROL, self.top, MAP = 1

END


; -----------------------------------------------------------------------------
; Center the rectangle in the drawing area
; -----------------------------------------------------------------------------
PRO Rectangle::center, X = x, Y = y, FILL = fill

    centerX = KEYWORD_SET (x)
    centerY = KEYWORD_SET (y)
    fillXY  = KEYWORD_SET (fill)    

    IF ((centerX + centerY + fillXY) EQ 0) THEN BEGIN
       centerX = 1
       centerY = 1
    ENDIF

    ; Current transformation matrix 
    m = self.bboxModel->getCTM ()

    ; Transform vertex data
    self.bbox->getProperty, DATA = vertices
    v = self->transformVertices (vertices, m)
              
    IF (centerX) THEN BEGIN
    
       xMin = MIN (v[0, *], MAX = xMax)
       span = xMax - xMin
       
       xLo = 0.5 - (span / 2.0)
       xHi = 0.5 + (span / 2.0)
       
       v[0, 0] = xLo
       v[0, 1] = xLo
       v[0, 2] = xHi
       v[0, 3] = xHi
       
    ENDIF ; x

    
    IF (centerY) THEN BEGIN

       yMin = MIN (v[1, *], MAX = yMax)
       span = yMax - yMin
       
       yLo = 0.5 - (span / 2.0)
       yHi = 0.5 + (span / 2.0)
       
       v[1, 0] = yLo
       v[1, 3] = yLo
       v[1, 1] = yHi       
       v[1, 2] = yHi       
    
    ENDIF ; y


    IF (fillXY) THEN BEGIN

       self->center
       
       lo = 0.050
       hi = 0.950
              
       v[0, 0] = lo
       v[0, 1] = lo
       v[0, 2] = hi       
       v[0, 3] = hi       

       v[1, 0] = lo
       v[1, 3] = lo
       v[1, 1] = hi       
       v[1, 2] = hi       
    
    ENDIF ; fillXY

    ; Transform vertices back
    v = self->untransformVertices (v, m)

    ; Update object vertices
    self.bbox->setProperty, DATA = v

    ; Redraw the view
    self.thisWindow->draw, self.thisView

END


; -----------------------------------------------------------------------------
; Move the rectangle in the drawing area
; -----------------------------------------------------------------------------
PRO Rectangle::move, percent, $
    NORTH = north, SOUTH = south, EAST = east, WEST = west

    IF (N_ELEMENTS (percent) EQ 0) THEN percent = 2.0
    
    ; Current transformation matrix 
    m = self.bboxModel->getCTM ()

    ; Transform vertex data
    self.bbox->getProperty, DATA = vertices
    v = self->transformVertices (vertices, m)
              
    xMin = MIN (v[0, *], MAX = xMax)
    yMin = MIN (v[1, *], MAX = yMax)

    IF (KEYWORD_SET (north)) THEN BEGIN
       
       incrX = 0.0     
       incrY = (yMax - yMin) * (percent / 100.0)       

    ENDIF ; north

    IF (KEYWORD_SET (south)) THEN BEGIN
    
       incrX = 0.0
       incrY = -1.0 * (yMax - yMin) * (percent / 100.0)
       
    ENDIF ; south

    IF (KEYWORD_SET (east)) THEN BEGIN
    
       incrX = (xMax - xMin) * (percent / 100.0)       
       incrY = 0.0
       
    ENDIF ; east

    IF (KEYWORD_SET (west)) THEN BEGIN
    
       incrX = -1.0 * (xMax - xMin) * (percent / 100.0)
       incrY = 0.0
       
    ENDIF ; west
    
    v[0, *] = v[0, *] + incrX
    v[1, *] = v[1, *] + incrY
     
    ; Transform vertices back
    v = self->untransformVertices (v, m)

    ; Update object vertices
    self.bbox->setProperty, DATA = v

    ; Redraw the view
    self.thisWindow->draw, self.thisView

END


; -----------------------------------------------------------------------------
; Get the current rectangle vertices
; -----------------------------------------------------------------------------
FUNCTION Rectangle::vertices

    self.bbox->getProperty, DATA = v

    xform = self.bboxModel->getCTM ()
    vertices = self->transformVertices (v, xform)
    
    RETURN, vertices
    
END


; -----------------------------------------------------------------------------
; Set the current rectangle vertices
; -----------------------------------------------------------------------------
PRO Rectangle::setVertices, v

    xform = self.bboxModel->getCTM ()
    vertices = self->untransformVertices (v, xform)

    self.bbox->setProperty, DATA = vertices
    self.thisWindow->draw, self.thisView

END


; -----------------------------------------------------------------------------
; Return the drawing area size
; -----------------------------------------------------------------------------
FUNCTION Rectangle::size

    RETURN, [self.xsize, self.ysize]
    
END


; -----------------------------------------------------------------------------
; Resize the drawing area
; -----------------------------------------------------------------------------
PRO Rectangle::setSize, XSIZE = xsize, YSIZE = ysize, UNITS = units

    WIDGET_CONTROL, self.top, UPDATE = 0
    
    IF (KEYWORD_SET (xsize)) THEN BEGIN
       WIDGET_CONTROL, self.drawID, XSIZE = xsize, UNITS = units
    ENDIF
    
    IF (KEYWORD_SET (ysize)) THEN BEGIN
       WIDGET_CONTROL, self.drawID, YSIZE = ysize, UNITS = units
    ENDIF
    
    g = WIDGET_INFO (self.drawID, /GEOMETRY)
    self.xsize = FIX (g.xsize)
    self.ysize = FIX (g.ysize)
    
    WIDGET_CONTROL, self.top, UPDATE = 1
    
END


; -----------------------------------------------------------------------------
; 2d resizeable rectangle object
; -----------------------------------------------------------------------------
PRO Rectangle__define

    obj = { RECTANGLE, $

        top           : 0L,            $ ; top level widget ID
        drawID        : 0L,            $ ; draw widget ID

        bbox          : OBJ_NEW (),    $ ; resizable rectangle object
        bboxModel     : OBJ_NEW (),    $ ; model to hold the rectangle object
         
        thisContainer : OBJ_NEW (),    $ ; graphics container
        thisWindow    : OBJ_NEW (),    $ ; window object
        thisView      : OBJ_NEW (),    $ ; view to be displayed in the window

        xsize         : 0,             $ ; current X size of the draw widget
        ysize         : 0,             $ ; current Y size of the draw widget

        inMotion      : 0,             $ ; set when an object is in motion 
        inResize      : 0,             $ ; set when an object is being resized
        xstart        : 0,             $ ; starting x coord in a drag action
        ystart        : 0,             $ ; starting y coord in a drag action

        vertices      : PTR_NEW (),    $ ; object vertices
         
        selectedObj   : OBJ_NEW (),    $ ; a selected object
        selectedName  : '',            $ ; name of the selected object

        zone : { ZONE, x: 0, y: 0 },   $ ; a region on the rectangle object

        press         : '',            $ ; call PRO on mouse press
        release       : '',            $ ; call PRO on mouse release        
        object        : OBJ_NEW ()     $ ; call object method if valid
    }


END
