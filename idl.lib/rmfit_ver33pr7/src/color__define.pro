; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;+
;
; NAME:
;    color
;
; PURPOSE:
;    This object creates an indexed color model.  Default is 6 colors.
;    It is a Singleton object (only one instance of this object will be 
;    created).
;
; CATEGORY:
;    Object
;
; CALLING SEQUENCE:
;    obj = OBJ_SINGLETON ('Color')
;
; INPUTS:
;    None
;
; KEYWORD PARAMETERS:
;    START_INDEX : index at which to start loading the colors (DEFAULT = 0)
;
; OUTPUTS:
;    None
;
; SIDE EFFECTS:
;    Calls "DEVICE, DECOMPOSED = 0" on initialization
;
; PUBLIC METHODS:
;     
;     color (FUNCTION) - Get a color index by name
;         Inputs: NAME : STRING of the color name index to retrieve
;        Outputs: The index corresponding to the color given by NAME
;       Keywords: NONE
;   
;     model (FUNCTION) - Retrieve color model information
;         Inputs: NONE
;        Outputs: Structure containing color model information
;       Keywords: NAMES : Return a STRARR of available color names instead
;                         of the color model structure
;                 PRINT : Print the indices and names of the color model
;    
;     setModel (PROCEDURE) - Set up a custom indexed color model
;         Inputs: NAME  : STRARR of color names
;                 RED   : INTARR of red   component, same length as NAMES
;                 GREEN : INTARR of green component, same length as NAMES 
;                 BLUE  : INTARR of blue  component, same length as NAMES
;        Outputs: NONE 
;       Keywords: START_INDEX : index at which to start loading the colors
;                               (DEFAULT = 0)
;                 COLORMODEL : Array of Color Model structures
;
;     display (FUNCTION) - Choose color values from a widget
;         Inputs: CHOICES : STRARR of names for which to choose colors
;                 COLORS  : STRARR of color names used as the initial color
;                           indices for CHOICES, same length as CHOICES
;        Outputs: STRARR of selected color names, same length as CHOICES
;       Keywords: GROUP_LEADER : The group leader of the widget.  See help
;                     for WIDGET_BASE for an explanation of the group leader.  
;
;      
; EXAMPLE:
;
;    ; Use the default 6-index color model
;    ;
;    o = OBJ_NEW ('Color')
;    PLOT, INDGEN (10), COLOR = o->color ('blue')
;
;    ; Choose colors for some parameters
;    ;
;    NAMES  = ['Plot foreground', 'Plot background']
;    COLORS = ['red', 'black']
;    userColors = o->display (NAMES, COLORS)
;    PLOT, INDGEN (10), $
;        COLOR = o->color (userColors[0]), $
;        BACKGROUND = o->color (userColors[1])
;
;    ; Create a custom 2-index color model.  Don't trample
;    ;  the color model set up in object o, which uses indices 0-5.
;    ;
;    myColors = OBJ_NEW ('Color')
;
;    names = ['FOREGROUND', 'BACKGROUND']
;    red   = [          0,           255]
;    green = [          0,           255]
;    blue  = [        255,           255]
;    
;    myColors->setModel, names, red, green, blue, START_INDEX = 6
;
;    PLOT, INDGEN (10), $
;        COLOR = myColors->color ('foreground'), $
;        BACKGROUND = myColors->color ('background')
;
;    OBJ_DESTROY, o
;    OBJ_DESTROY, myColors    
;    
;
; MODIFICATION HISTORY:
;
;	Written, Robert.Mallozzi@msfc.nasa.gov, 1999 February
;
; -
; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *




;------------------------------------------------------------------------------
; Constructor
;------------------------------------------------------------------------------
FUNCTION Color::init, START_INDEX = start_index
               
    ok = self->Singleton::init ()

    ; Cause the least-significant 8 bits of the color index value to 
    ; be interpreted as a PseudoColor index
    ;
    DEVICE, DECOMPOSED = 0

    ; Set up the color vectors
    ;     
    n = ['BLACK', 'RED', 'GREEN', 'BLUE', 'YELLOW', 'WHITE', 'CYAN', 'PURPLE']
    r = [     0,    255,      0,      0,       255,     255,      0,      255]
    g = [     0,      0,    255,      0,       255,     255,    255,        0]
    b = [     0,      0,      0,    255,         0,     255,    255,      255]

    self->setModel, n, r, g, b, START_INDEX = start_index
    
    RETURN, ok
    
END


;------------------------------------------------------------------------------
; Destructor
;------------------------------------------------------------------------------
PRO Color::cleanup
   
    PTR_FREE, self.names   
    PTR_FREE, self.index
    PTR_FREE, self.vectors   

    self->Singleton::cleanup

END


;------------------------------------------------------------------------------
; Retrieve color index by name
;------------------------------------------------------------------------------
FUNCTION Color::color, name  

    IF (N_PARAMS () LT 1) THEN BEGIN
       MESSAGE, /CONTINUE, 'Incorrect number of arguments.' 
       RETURN, -1
    ENDIF
       
    IF (STRUPCASE (SIZE (name, /TNAME)) NE 'STRING') THEN BEGIN
       MESSAGE, /CONTINUE, 'Argument must be of type STRING.' 
       RETURN, -1
    ENDIF
    
    i = WHERE (STRUPCASE (name) EQ *self.names, found)
    IF (found EQ 0) THEN BEGIN
       MESSAGE, /CONTINUE, 'Color not found: ' + name
       RETURN, -1
    ENDIF
       
    RETURN, (*self.index)[i[0]]
    
END


;------------------------------------------------------------------------------
; Set a custom indexed color model.  No error checking on input parameters!
;
; n = STRARR of names
; r = INTARR of red   component, same length as n
; g = INTARR of green component, same length as n
; b = INTARR of blue  component, same length as n
;
;------------------------------------------------------------------------------
PRO Color::setModel, n, r, g, b, START_INDEX = start_index
    
    IF (N_ELEMENTS (start_index) EQ 0) THEN $
       start_index = 0
       
    PTR_FREE, self.names
    PTR_FREE, self.vectors
    PTR_FREE, self.index
   
    self.names     = PTR_NEW (STRUPCASE (n))
    self.vectors   = PTR_NEW (REFORM ([r, g, b], N_ELEMENTS (n), 3))
    self.numColors = N_ELEMENTS (n)
    self.index     = PTR_NEW (INDGEN (self.numColors) + start_index)

    FOR i = 0, self.numColors - 1 DO $
        TVLCT, (*self.vectors)[i, *], i + start_index   

END


;------------------------------------------------------------------------------
; Set a color by index
;------------------------------------------------------------------------------
PRO Color::setByIndex, index, color

    IF index GT self.numColors THEN RETURN
    (*self.vectors)[index, *] = TRANSPOSE(color)
    
    FOR i = 0, self.numColors - 1 DO $
        TVLCT, (*self.vectors)[i, *], i   

END


;------------------------------------------------------------------------------
; Retrieve color model information
;------------------------------------------------------------------------------
FUNCTION Color::model, NAMES = names, PRINT = print

    IF (KEYWORD_SET (print)) THEN BEGIN
    
       FOR i = 0, self.numColors - 1 DO $
           PRINT, STRTRIM ((*self.index)[i], 2) + '  ' + (*self.names)[i]

    ENDIF

    IF (KEYWORD_SET (names)) THEN $
       RETURN, *self.names
     
    c = REPLICATE ({ COLORVAL }, self.numColors)

    FOR i = 0, self.numColors - 1 DO BEGIN
    
        c[i].name  = (*self.names)[i]
        c[i].value = (*self.vectors)[i, *]
        c[i].index = (*self.index)[i]
         
    ENDFOR
   
    RETURN, c
    
END


;------------------------------------------------------------------------------
; Display the color selection widget
;------------------------------------------------------------------------------
PRO Color_EVENT, event

    WIDGET_CONTROL, event.top, GET_UVALUE = state
    (*state).self->display_event, event

END
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
PRO Color::display_event, event

    WIDGET_CONTROL, event.top, GET_UVALUE = info

    type = TAG_NAMES (EVENT, /STRUCTURE)
    CASE (type) OF
	    
        'WIDGET_BUTTON': BEGIN

            WIDGET_CONTROL, event.id, GET_VALUE = value

            CASE (value) OF

                 'CANCEL': BEGIN

                     WIDGET_CONTROL, event.top, /DESTROY 
                     RETURN
                     END

                 'Accept': BEGIN

                     FOR i = 0, N_ELEMENTS ((*info).values) - 1 DO BEGIN
		         (*info).values[i] = $
			     WIDGET_INFO ((*info).colorID[i], /DROPLIST_SELECT)
                     ENDFOR
                     		     
                     WIDGET_CONTROL, event.top, /DESTROY 
                     RETURN
                     END

                 ELSE: MESSAGE, /INFO, 'Not handled: ' + uvalue

            ENDCASE 
            END

        ELSE: ; not handled
	    
    ENDCASE ; type
    

END
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
FUNCTION Color::display, choices, colors, GROUP_LEADER = group_leader

    ; Create parent of modal base if needed
    ;
    haveLeader = N_ELEMENTS (group_leader) NE 0
    IF (NOT haveLeader) THEN BEGIN
       leader = WIDGET_BASE (MAP = 0)
    ENDIF ELSE BEGIN
       leader = group_leader
    ENDELSE
    
    numChoices = N_ELEMENTS (choices)
    values = INTARR (numChoices)
    FOR i = 0, numChoices - 1 DO $
        values[i] = self->color (colors[i])
    
    topBase = WIDGET_BASE (/COLUMN, TITLE = 'Color', $
        /MODAL, /FLOATING, GROUP_LEADER = leader)

    b = WIDGET_BASE (topBase, COLUMN = 2, /GRID_LAYOUT, /FRAME)
        
    FOR i = 0, numChoices - 1 DO $ 
	w = WIDGET_LABEL (b, VALUE = choices[i],  /ALIGN_RIGHT)

    colorID = LONARR (numChoices)

    FOR i = 0, numChoices - 1 DO BEGIN
	colorID[i] = WIDGET_DROPLIST (b, VALUE = (*self.names))
	WIDGET_CONTROL, colorID[i], SET_DROPLIST_SELECT = values[i]
    ENDFOR
	 
    w = WIDGET_BUTTON (topBase, VALUE = 'Accept')
    w = WIDGET_BUTTON (topBase, VALUE = 'CANCEL')

    ; Data passing mechanism
    ;
    state = PTR_NEW ({ $
        
	self    : self, $
	colorID : colorID, $
        values  : values $      
    })

    ; Set data and realize
    ;
    WIDGET_CONTROL, topBase, SET_UVALUE = state
    WIDGET_CONTROL, topBase, /REALIZE 

    ; Event loop
    ;
    XMANAGER, OBJ_CLASS (self), topBase
    
    values = (*state).values
    PTR_FREE, state

    ; Destroy parent of modal base
    ;
    IF (NOT haveLeader) THEN $
       WIDGET_CONTROL, leader, /DESTROY
    
    retNames = (*self.names)[values]
    RETURN, retNames
    
END


;------------------------------------------------------------------------------
; Definition of a color model object
;------------------------------------------------------------------------------
PRO Color__define

    tmpl = { COLORVAL,         $
        name  : '',            $
        value : INTARR (1, 3), $
        index : 0              $
    }

    obj = { COLOR, INHERITS SINGLETON, $

         numColors : 0, $         	 
         names     : PTR_NEW (), $
         vectors   : PTR_NEW (), $    
         index     : PTR_NEW ()  $    
    }

END
