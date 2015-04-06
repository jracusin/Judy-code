; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
; NAME:
;	SelectBox (OBJECT)
;
; PURPOSE:
;       Allows interactive (widget) numerical selection of a box
;
; CALLING SEQUENCE:
;	obj = OBJ_NEW ('selectBox', parent, inputXRange, inputYrange)
;       obj->getProperty, SELECTION = selection, EXACT = exact
;       OBJ_DESTROY, obj 
;
; INPUTS:
;       parent:      Parent of the selectBox modal widget
;       inputXrange: Independent axis range (e.g., !X.CRANGE)
;       inputYrange: Dependent axis range
; 
; KEYWORD PARAMETERS:
;
;       Method init:
;
;           OPLOT     : Overplot the selected interval
;           XSELECT   : Allow selections from x-axis only
;           YSELECT   : Allow selections from y-axis only
;           NOEXACT   : Disable 'Accept Exact' button
;           LINESTYLE : If oplotting, use specified linestyle (default = 1)
;           COLOR     : If oplotting, use specified color (default = !P.COLOR)
;
;       Method get:
;
;           SELECTION: FLTARR(2, 2) = the selected range
;               selection[0, *] = selected x range
;               selection[1, *] = selected y range
;
;           If either XSELECT or YSELECT keyword is set, the selection
;           of the unspecified axis will be 0.0.  For example, if XSELECT
;           is set, 
;
;               selection[0, *] = selected x range
;               selection[1, *] = [0.0, 0.0]
;
;           Note that the input parameters inputXrange, inputYrange
;           are still both required.
;
;           EXACT: 0 if 'Accept' button selected, 1 if 'Accept Exact' selected
;
; OUTPUTS:
;       via method getProperty
;
; MODIFICATION HISTORY:
;	Written, Robert.Mallozzi@msfc.nasa.gov, 1998 March
;
; -
; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *


;-------------------------------------------------------------------------------
; Constructor
;-------------------------------------------------------------------------------
FUNCTION selectBox::init, parent, inputXrange, inputYrange, $
    XSELECT = xselect, YSELECT = yselect, $
    OPLOT = oplot, NOEXACT = noexact, $
    LINESTYLE = linestyle, COLOR = color
   
   
    IF (N_PARAMS() LT 3) THEN BEGIN
       MESSAGE, /CONTINUE, 'Initialization failed.'
       RETURN, 0
    ENDIF
       
    self.format = '(g)'
    self.active = 0
 
    self.inputXrange = inputXrange
    self.inputYrange = inputYrange
    
    self.value[0, 0] = inputXrange[0]
    self.value[0, 1] = inputXrange[1]
    self.value[1, 0] = inputYrange[0]
    self.value[1, 1] = inputYrange[1]

    self.selection = self.value
        
    self.oplot   = (N_ELEMENTS (oplot) NE 0)
    self.noexact = (N_ELEMENTS (noexact) NE 0)
    
    self.linestyle = 1
    IF (N_ELEMENTS (linestyle) NE 0) THEN $
       self.linestyle = linestyle
          
    self.color = !P.COLOR
    IF (N_ELEMENTS (color) NE 0) THEN $
       self.color = color

    self->display, parent, XSELECT = xselect, YSELECT = yselect    

    
    RETURN, 1
    
END


;-------------------------------------------------------------------------------
; Generic method to return instance data from any object
;
; By default, object internal pointers are dereferenced and returned 
; since returning a pointer into an object breaks encapsulation.  Set 
; the keyword RETURN_POINTERS to override this behavior.  This might 
; be desired, for example, to avoid copying large datasets on return.  
; In this case, one should ensure that the data to which the pointer 
; refers is not altered.
;
; Returns instance_data on success, or 0 on error.
;-------------------------------------------------------------------------------
FUNCTION selectBox::get, instance_data, RETURN_POINTERS = return_pointers

    error = 0
    ON_ERROR, 2
    
    ; Ensure request is a string variable
    ;
    s = SIZE (instance_data)
    IF (s[s[0]+1] NE 7) THEN BEGIN
       MESSAGE, /CONTINUE, 'Argument must be of type STRING: ' + instance_data
       RETURN, 0
    ENDIF

    ; Get the name of the object class
    ;
    thisClass = Obj_Class(self)

    ; Create a local structure of this type
    ;
    thisStruct = CREATE_STRUCT(NAME = thisClass)
;    IF (NOT EXE_CUTE('thisStruct = {' + thisClass + '}')) THEN BEGIN
;       MESSAGE, /CONTINUE, 'EXE_CUTE failed.'
;       RETURN, 0
;    ENDIF

    ; Find the field identifier (index) in the local structure
    ;
    structFields = TAG_NAMES(thisStruct)
    index = WHERE (structFields EQ STRUPCASE(instance_data), count)

    ; Extract and return the field if it is found
    ;
    IF (count EQ 1) THEN BEGIN

;       retVal = self.structFields[index[0]]
       retVal = self.(index[0])
;       IF (EXE_CUTE ('retVal = self.' + structFields[index[0]])) THEN BEGIN
       IF (error EQ 0) THEN BEGIN

          ; Don't return a pointer into an object
          ;
          IF (PTR_VALID (retVal[0])) THEN BEGIN

             RETURN, *retVal

          ENDIF ELSE BEGIN   

	     ; Case of an invalid pointer: issue warning, and return 0   
	     ;
	     s = SIZE (retVal)
	     IF (s[s[0] + 1] EQ 10) THEN BEGIN
		MESSAGE, /CONTINUE, $
                    'Requested instance variable is invalid pointer.'
        	retVal = 0
	     ENDIF   

             RETURN, retVal

          ENDELSE

       ENDIF

       MESSAGE, /CONTINUE, 'EXE_CUTE failed.'

    ENDIF ELSE BEGIN

       MESSAGE, /CONTINUE, 'Invalid request for object ' + $
           OBJ_CLASS (self) + ': ' + instance_data

    ENDELSE


    RETURN, 0

END


;-------------------------------------------------------------------------------
; Forward event to object event handler
;-------------------------------------------------------------------------------
PRO selectBox_EVENT, event

    WIDGET_CONTROL, event.top, GET_UVALUE = info
    info.self->display_event, event

END
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
PRO selectBox::display_event, event


    XR = self.inputXrange
    YR = self.inputYrange
    
    numSteps = 100
    IF (!X.TYPE EQ 1) THEN BEGIN
       xStep = (ALOG10 (XR[1]) - ALOG10 (XR[0])) / numSteps
    ENDIF ELSE BEGIN
       xStep = (XR[1] - XR[0]) / numSteps
    ENDELSE
    
    IF (!Y.TYPE EQ 1) THEN BEGIN
       yStep = (ALOG10 (YR[1]) - ALOG10 (YR[0])) / numSteps
    ENDIF ELSE BEGIN
       yStep = (YR[1] - YR[0]) / numSteps
    ENDELSE
    
    WIDGET_CONTROL, event.top, GET_UVALUE = info, /NO_COPY

    type = TAG_NAMES (EVENT, /STRUCTURE)
    CASE (type) OF

        ; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        ; 
        ; Text widget events
        ;
        'WIDGET_TEXT_CH': BEGIN
	
            WIDGET_CONTROL, event.id, GET_VALUE = value, GET_UVALUE = uvalue

            CASE (uvalue) OF
                 
		 'W': BEGIN
                     OPLOT, self.value[0, 0] * [1, 1], self.inputYrange, $
                         LINESTYLE = self.linestyle, COLOR = !P.BACKGROUND
		     
		     self.value[0, 0] = FLOAT (value[0]) 
                     WIDGET_CONTROL, event.id, SET_VALUE = $
                         STRTRIM(STRING (self.value[0, 0], F = self.format), 2) 
                     
		     OPLOT, self.value[0, 0] * [1, 1], $
                         self.inputYrange, LINESTYLE = self.linestyle, $
                         COLOR = self.color
                     END
		 
		 'E': BEGIN 
                     OPLOT, self.value[0, 1] * [1, 1], self.inputYrange, $
                         LINESTYLE = self.linestyle, COLOR = !P.BACKGROUND
		     
		     self.value[0, 1] = FLOAT (value[0]) 
                     WIDGET_CONTROL, event.id, SET_VALUE = $
                         STRTRIM(STRING (self.value[0, 1], F = self.format), 2) 
                     
		     OPLOT, self.value[0, 1] * [1, 1], $
		         self.inputYrange, LINESTYLE = self.linestyle, $
                         COLOR = self.color
                     END

                 'S': BEGIN 
                     OPLOT, self.inputXrange, self.value[1, 0] * [1, 1], $
		         LINESTYLE = self.linestyle, COLOR = !P.BACKGROUND
		     
		     self.value[1, 0] = FLOAT (value[0]) 
                     WIDGET_CONTROL, event.id, SET_VALUE = $
                         STRTRIM(STRING (self.value[1, 0], F = self.format), 2) 
                     
		     OPLOT, self.inputXrange, $
                         self.value[1, 0] * [1, 1], $
                         LINESTYLE = self.linestyle, COLOR = self.color
                     END

                 'N': BEGIN
                     OPLOT, self.inputXrange, self.value[1, 1] * [1, 1], $
		         LINESTYLE = self.linestyle, COLOR = !P.BACKGROUND
		     
		     self.value[1, 1] = FLOAT (value[0]) 
                     WIDGET_CONTROL, event.id, SET_VALUE = $
                         STRTRIM(STRING (self.value[1, 1], F = self.format), 2) 
                     
		     OPLOT, self.inputXrange, $
                         self.value[1, 1] * [1, 1], $
                         LINESTYLE = self.linestyle, COLOR = self.color
                     END

	    ENDCASE
	    
	    END
	    
        ; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        ; 
        ; Button widget events
        ;
        'WIDGET_BUTTON': BEGIN

            WIDGET_CONTROL, event.id, GET_UVALUE = uvalue

            CASE (uvalue) OF

                 'CANCEL': BEGIN

                     WIDGET_CONTROL, event.top, /DESTROY 
                     self.canceled = 1
                     RETURN
                     END

                 'Accept': BEGIN

                     self.exact = 0

		     WIDGET_CONTROL, info.textID[0], GET_VALUE = v
		     self.selection[0, 0] = FLOAT(v[0])

		     WIDGET_CONTROL, info.textID[1], GET_VALUE = v
		     self.selection[0, 1] = FLOAT(v[0])

		     WIDGET_CONTROL, info.textID[2], GET_VALUE = v
		     self.selection[1, 0] = FLOAT(v[0])

		     WIDGET_CONTROL, info.textID[3], GET_VALUE = v
		     self.selection[1, 1] = FLOAT(v[0])
		     
                     OPLOT, self.value[0, 0] * [1, 1], self.inputYrange, $
                         LINESTYLE = self.linestyle, COLOR = !P.BACKGROUND
                     OPLOT, self.value[0, 1] * [1, 1], self.inputYrange, $
                         LINESTYLE = self.linestyle, COLOR = !P.BACKGROUND

                     OPLOT, self.inputXrange, self.value[1, 0] * [1, 1], $
		         LINESTYLE = self.linestyle, COLOR = !P.BACKGROUND
                     OPLOT, self.inputXrange, self.value[1, 1] * [1, 1], $
		         LINESTYLE = self.linestyle, COLOR = !P.BACKGROUND

                     WIDGET_CONTROL, event.top, /DESTROY 
                     self.canceled = 0
                     RETURN
                     END

                 'Exact': BEGIN

                     self.exact = 1

		     WIDGET_CONTROL, info.textID[0], GET_VALUE = v
		     self.selection[0, 0] = FLOAT(v[0])

		     WIDGET_CONTROL, info.textID[1], GET_VALUE = v
		     self.selection[0, 1] = FLOAT(v[0])

		     WIDGET_CONTROL, info.textID[2], GET_VALUE = v
		     self.selection[1, 0] = FLOAT(v[0])

		     WIDGET_CONTROL, info.textID[3], GET_VALUE = v
		     self.selection[1, 1] = FLOAT(v[0])
		     
                     OPLOT, self.value[0, 0] * [1, 1], self.inputYrange, $
                         LINESTYLE = self.linestyle, COLOR = !P.BACKGROUND
                     OPLOT, self.value[0, 1] * [1, 1], self.inputYrange, $
                         LINESTYLE = self.linestyle, COLOR = !P.BACKGROUND

                     OPLOT, self.inputXrange, self.value[1, 0] * [1, 1], $
		         LINESTYLE = self.linestyle, COLOR = !P.BACKGROUND
                     OPLOT, self.inputXrange, self.value[1, 1] * [1, 1], $
		         LINESTYLE = self.linestyle, COLOR = !P.BACKGROUND

                     WIDGET_CONTROL, event.top, /DESTROY 
                     self.canceled = 0
                     RETURN
                     END

                 'ARROW': BEGIN
                   
                     CASE (event.id) OF
			  
			  info.arrowID[0]: disc = -1.0      
			  info.arrowID[1]: disc =  1.0
			  info.arrowID[2]: disc = -1.0
			  info.arrowID[3]: disc =  1.0
		     
		     ENDCASE

                     WIDGET_CONTROL, info.textID[self.active], GET_VALUE = v
		     currentValue = FLOAT (v[0]) 

		     CASE (self.active) OF
	        	
			  0: BEGIN
			     step = xStep
			     type = !X.TYPE
			     maxv = self.value[0, 1]
			     minv = self.inputXrange[0]
                             ax   = 0
			     END
	        	
			  1: BEGIN
			     step = xStep
			     type = !X.TYPE
			     maxv = self.inputXrange[1]
			     minv = self.value[0, 0]
			     ax   = 0
			     END
	        	
			  2: BEGIN
			     step = yStep
			     type = !Y.TYPE
			     maxv = self.value[1, 1]
			     minv = self.inputYrange[0]
			     ax   = 1
			     END
	        	
			  3: BEGIN
			     step = yStep
			     type = !Y.TYPE
			     maxv = self.inputYrange[1]
			     minv = self.value[1, 0]
			     ax   = 1
			     END
		     
		     ENDCASE

		     oldValue = currentValue

                     ; Step through the range
                     ;
		     IF (type EQ 1) THEN BEGIN
                	currentValue = 10.0^(ALOG10(currentValue) + (step*disc))
                     ENDIF ELSE BEGIN
			currentValue = currentValue + (step * disc)
                     ENDELSE

                     ; Crossed the other value?
                     ;
		     IF (currentValue GE maxv) THEN $
                	currentValue = maxv - step

                     ; Past edge?
                     ;
		     IF (currentValue LT minv) THEN $
                	currentValue = minv

		     range    = [currentValue, currentValue]
                     rangeOld = [oldValue, oldValue]

		     WIDGET_CONTROL, info.textID[self.active], $
                	 SET_VALUE = STRTRIM(STRING(currentValue, $
                	 F = self.format), 2)

                     IF (self.oplot) THEN BEGIN
                	IF (ax EQ 0) THEN BEGIN
			   OPLOT, rangeOld, self.inputYrange, $
                               LINESTYLE = self.linestyle, COLOR = !P.BACKGROUND
                	   OPLOT, range, self.inputYrange, $
                               LINESTYLE = self.linestyle, COLOR = self.color
                        ENDIF ELSE BEGIN
			   OPLOT, self.inputXrange, rangeOld, $
                               LINESTYLE = self.linestyle, COLOR = !P.BACKGROUND
                	   OPLOT, self.inputXrange, range, $
                               LINESTYLE = self.linestyle, COLOR = self.color
			ENDELSE
		     ENDIF

		     CASE (self.active) OF
                          0: self.value[0, 0] = currentValue
                          1: self.value[0, 1] = currentValue
                          2: self.value[1, 0] = currentValue
                          3: self.value[1, 1] = currentValue
		     ENDCASE

                     END

                 ELSE: MESSAGE, /INFO, 'Not handled: ' + uvalue

            ENDCASE 
            END

        ; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        ; 
        ; Radio button events
        ;
        '': BEGIN

	    self.active = event.value
; TODO: ??
WIDGET_CONTROL, event.id, SET_VALUE = event.value

	    IF ((self.active EQ 0) OR (self.active EQ 1)) THEN BEGIN
	       WIDGET_CONTROL, info.arrowID[0], SENSITIVE = 1
	       WIDGET_CONTROL, info.arrowID[1], SENSITIVE = 1
	       WIDGET_CONTROL, info.arrowID[2], SENSITIVE = 0
	       WIDGET_CONTROL, info.arrowID[3], SENSITIVE = 0
	    ENDIF

	    IF ((self.active EQ 2) OR (self.active EQ 3)) THEN BEGIN
	       WIDGET_CONTROL, info.arrowID[0], SENSITIVE = 0
	       WIDGET_CONTROL, info.arrowID[1], SENSITIVE = 0
	       WIDGET_CONTROL, info.arrowID[2], SENSITIVE = 1
	       WIDGET_CONTROL, info.arrowID[3], SENSITIVE = 1
	    ENDIF

            END
	    
    ENDCASE ; type
    

    WIDGET_CONTROL, event.top, SET_UVALUE = info, /NO_COPY


END

;-------------------------------------------------------------------------------
; Build GUI
;-------------------------------------------------------------------------------
PRO selectBox::display, parent, XSELECT = xselect, YSELECT = yselect 


    topBase = WIDGET_BASE (/COLUMN, TITLE = 'SelectBox', $
        /MODAL, /FLOATING, GROUP_LEADER = parent)

    ; Define some bit map buttons
    ;
    arrow_west = [ $
	[000B, 004B], [000B, 006B], [000B, 007B], [128B, 007B], $
	[192B, 007B], [224B, 007B], [240B, 007B], [248B, 007B], $
	[248B, 007B], [240B, 007B], [224B, 007B], [192B, 007B], $
	[128B, 007B], [000B, 007B], [000B, 006B], [000B, 004B]  $
    ]
                
    arrow_east = [ $
	[032B, 000B], [096B, 000B], [224B, 000B], [224B, 001B], $
	[224B, 003B], [224B, 007B], [224B, 015B], [224B, 031B], $
	[224B, 031B], [224B, 015B], [224B, 007B], [224B, 003B], $
	[224B, 001B], [224B, 000B], [096B, 000B], [032B, 000B]  $
    ]

    arrow_north = [ $
	[000B, 000B], [000B, 000B], [000B, 000B], [128B, 001B], $
	[192B, 003B], [224B, 007B], [240B, 015B], [248B, 031B], $
	[252B, 063B], [254B, 127B], [255B, 255B], [000B, 000B], $
	[000B, 000B], [000B, 000B], [000B, 000B], [000B, 000B]  $
    ]                

    arrow_south = [ $
	[000B, 000B], [000B, 000B], [000B, 000B], [000B, 000B], $
	[000B, 000B], [255B, 255B], [254B, 127B], [252B, 063B], $
	[248B, 031B], [240B, 015B], [224B, 007B], [192B, 003B], $
	[128B, 001B], [000B, 000B], [000B, 000B], [000B, 000B]  $
    ]

                
    ;  Set the initial selections 
    ;
    self.value[0, 0] = self.inputXrange[0]
    self.value[0, 1] = self.inputXrange[1]
    self.value[1, 0] = self.inputYrange[0]
    self.value[1, 1] = self.inputYrange[1]  

    b = WIDGET_BASE (topBase, COLUMN = 2, /GRID_LAYOUT, /FRAME)

        w = WIDGET_LABEL (b, VALUE = 'X LO',  /ALIGN_CENTER)
        w = WIDGET_LABEL (b, VALUE = 'X HI',  /ALIGN_CENTER)
        w = WIDGET_LABEL (b, VALUE = 'Y LO', /ALIGN_CENTER)
        w = WIDGET_LABEL (b, VALUE = 'Y HI', /ALIGN_CENTER)

        textID = LONARR(4)
	width = 12
	
        textID[0] = WIDGET_TEXT (b, VALUE = STRTRIM(STRING(self.value[0, 0], $
            F = self.format), 2), /EDIT, XSIZE = width, UVALUE = 'W')

        textID[1] = WIDGET_TEXT (b, VALUE = STRTRIM(STRING(self.value[0, 1], $
            F = self.format), 2), /EDIT, XSIZE = width, UVALUE = 'E')

        textID[2] = WIDGET_TEXT (b, VALUE = STRTRIM(STRING(self.value[1, 0], $
            F = self.format), 2), /EDIT, XSIZE = width, UVALUE = 'S')

        textID[3] = WIDGET_TEXT (b, VALUE = STRTRIM(STRING(self.value[1, 1], $
            F = self.format), 2), /EDIT, XSIZE = width, UVALUE = 'N')


    subBase = WIDGET_BASE (topBase, COLUMN = 2, /GRID_LAYOUT, /FRAME)

	b = WIDGET_BASE (subBase, COLUMN = 3, /GRID_LAYOUT)

            arrowID = LONARR(4)

	    w = WIDGET_LABEL  (b, VALUE = ' ')
	    arrowID[0] = WIDGET_BUTTON (b, VALUE = arrow_west, UVALUE = 'ARROW')
	    w = WIDGET_LABEL  (b, VALUE = ' ')

	    arrowID[3] = WIDGET_BUTTON (b, VALUE = arrow_north, UVALUE ='ARROW')
	    w = WIDGET_LABEL  (b, VALUE = ' ')
	    arrowID[2] = WIDGET_BUTTON (b, VALUE = arrow_south, UVALUE ='ARROW')

	    w = WIDGET_LABEL  (b, VALUE = ' ')
	    arrowID[1] = WIDGET_BUTTON (b, VALUE = arrow_east, UVALUE = 'ARROW')
	    w = WIDGET_LABEL  (b, VALUE = ' ')

	radio = CW_BGROUP (subBase, ['X LO', 'X HI', 'Y LO', 'Y HI'], $
            /COLUMN, /EXCLUSIVE, /NO_RELEASE, IDS = radioID)

    w = WIDGET_BUTTON (topBase, VALUE = 'Accept', UVALUE = 'Accept')
    
    IF (self.noexact EQ 0) THEN $
       w = WIDGET_BUTTON (topBase, VALUE = 'Accept Exact', UVALUE = 'Exact')
     
    w = WIDGET_BUTTON (topBase, VALUE = 'CANCEL', UVALUE = 'CANCEL')


    IF (KEYWORD_SET (xselect)) THEN BEGIN
    
       WIDGET_CONTROL, arrowID[2], SENSITIVE = 0
       WIDGET_CONTROL, arrowID[3], SENSITIVE = 0
       WIDGET_CONTROL, radioID[2], SENSITIVE = 0
       WIDGET_CONTROL, radioID[3], SENSITIVE = 0
       WIDGET_CONTROL, textID[2],  SENSITIVE = 0
       WIDGET_CONTROL, textID[3],  SENSITIVE = 0

       WIDGET_CONTROL, radio, SET_VALUE = 0 

    ENDIF ELSE $
    IF (KEYWORD_SET (yselect)) THEN BEGIN
    
       WIDGET_CONTROL, arrowID[0], SENSITIVE = 0
       WIDGET_CONTROL, arrowID[1], SENSITIVE = 0
       WIDGET_CONTROL, radioID[0], SENSITIVE = 0
       WIDGET_CONTROL, radioID[1], SENSITIVE = 0
       WIDGET_CONTROL, textID[0],  SENSITIVE = 0
       WIDGET_CONTROL, textID[1],  SENSITIVE = 0

       WIDGET_CONTROL, radio, SET_VALUE = 2 

    ENDIF ELSE BEGIN
    
       WIDGET_CONTROL, arrowID[2], SENSITIVE = 0
       WIDGET_CONTROL, arrowID[3], SENSITIVE = 0

       WIDGET_CONTROL, radio, SET_VALUE = 0 

    ENDELSE

    state = { $
        
	self:    self, $
	textID:  textID, $
	arrowID: arrowID $
    
    } 

    ; Set data and realize
    ;
    WIDGET_CONTROL, topBase, SET_UVALUE = state, /NO_COPY
    WIDGET_CONTROL, topBase, /REALIZE 
    
    XMANAGER, OBJ_CLASS (self), topBase, /NO_BLOCK


END


;-------------------------------------------------------------------------------
; Dialog widget to obtain numerical axis ranges
;-------------------------------------------------------------------------------
PRO selectBox__define

    obj = { SELECTBOX, $

        inputXrange: FLTARR(2), $
        inputYrange: FLTARR(2), $

        active:  0,  $        
        oplot:   0,  $
        format:  '', $
        noexact: 0,  $ ; flag denoting if "Exact" button is activated
        exact:   0,  $ ; return value if user selected "Exact"
		
        value     : FLTARR(2, 2), $
        selection : FLTARR(2, 2), $
        canceled  : 0,            $ 
                 
        linestyle : 0, $
        color     : 0  $
    
    }


END
