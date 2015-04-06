; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
;+
; NAME:
;     DIALOG_CHECKLIST
;
; PURPOSE:
;     A modal (blocking) dialog widget to display a selectable checklist
;     of exclusive (radio) or non-exclusive buttons.  The dialog must be 
;     dismissed by selecting an item before execution of the calling program 
;     can continue.
;
; TYPE:
;     FUNCTION
;
; CATEGORY:
;     WIDGETS
;
; CALLING SEQUENCE:
;     result = DIALOG_CHECKLIST (menu_items)
;
; INPUTS:
;     menu_items : STRARR of choices
;
; KEYWORD PARAMETERS:
;
;     TITLE         : The menu title.  Use STRARR for multi-line titles.
;
;     XSIZE         : The x size of the widget.
;     YSIZE         : The y size of the widget.
;
;     XPOSITION     : The x position of the upper left corner of the widget.
;     XPOSITION     : The y position of the upper left corner of the widget.
;
;     SCROLL        : Adds a scroll bar.
;     X_SCROLL_SIZE : Size of x scroll region, if /SCROLL is set.
;     Y_SCROLL_SIZE : Size of y scroll region, if /SCROLL is set.
;
;     EXCLUSIVE     : Only a single choice can be selected (DEFAULT).
;     NONEXCLUSIVE  : Multiple, simultaneous choices can be selected.
;
;     INITIAL       : INTARR denoting which buttons should be selected
;                     when the widget is mapped.  The array elements are
;                     indices into MENU_ITEMS denoting the buttons to set
;                     to on.
;
;     DIALOG_PARENT : Set this keyword to the widget ID of a widget over
;                     which the message dialog should be positioned. When 
;                     displayed, the dialog will be positioned over the 
;                     specified widget. Dialogs are often related to a 
;                     non-dialog widget tree.  The ID of the widget in that 
;                     tree to which the dialog is most closely related should 
;                     be specified.
;          
;                     If this keyword is not specified, the default placement 
;                     of the menu is the center of the screen (window manager 
;                     dependent).
;    
; OUTPUTS:
;     result: INTARR of length STRLEN (menu_items).  Elements of the array
;         are 1 if the item was selected, otherwise 0.  For an EXCLUSIVE
;         base (DEFAULT), only a single element will be set to 1.
;
; COMMON BLOCKS:
;     NONE
;
; SIDE EFFECTS:
;     Creates a modal widget.
;
; RESTRICTIONS:
;     None known.
;
; DEPENDENCIES:
;     NONE
;
; EXAMPLES:
;     result = DIALOG_CHECKLIST (['Item 1', 'Item 2', 'Item3'], $
;         TITLE = 'Select an Item')
;
;     result = DIALOG_CHECKLIST (['Item 1', 'Item 2', 'Item3'], $
;         TITLE = 'Select Some Items', /NONEXCLUSIVE)
;
; MODIFICATION HISTORY:
;
;     26 Nov 1999, RSM, changed return type from INT to LONG
;     Written, Robert.Mallozzi@msfc.nasa.gov, July 1998.
;
;-
; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 

FUNCTION DIALOG_CHECKLIST, choices, $
    TITLE = title, $
    XSIZE = xsize, YSIZE = ysize, $
    XPOSITION = xposition, YPOSITION = yposition, $
    SCROLL = SCROLL, $
    X_SCROLL_SIZE = X_SCROLL_SIZE, Y_SCROLL_SIZE = Y_SCROLL_SIZE, $
    DIALOG_PARENT = dialog_parent, $
    INITIAL = initial, $
    EXCLUSIVE = exclusive, NONEXCLUSIVE = nonexclusive, _EXTRA = extra


    IF (N_PARAMS() LT 1) THEN $
       MESSAGE, 'Incorrect number of arguments.'
       
    numChoices  = N_ELEMENTS (choices)
    HAVE_SCROLL = N_ELEMENTS (scroll) NE 0
    HAVE_PARENT = N_ELEMENTS (dialog_parent) NE 0
    HAVE_XPOS   = N_ELEMENTS (xposition) NE 0
    HAVE_YPOS   = N_ELEMENTS (yposition) NE 0

    exclusive    = KEYWORD_SET (exclusive)
    nonexclusive = KEYWORD_SET (nonexclusive)
    
    ; Exclusive buttons are the default
    ;
    IF (TOTAL ([exclusive, nonexclusive]) EQ 0) THEN $
       exclusive = 1

    IF (TOTAL ([exclusive, nonexclusive]) EQ 2) THEN BEGIN
       exclusive    = 1
       nonexclusive = 0
    ENDIF
    
    ; Top level base
    ;
    IF (HAVE_PARENT) THEN BEGIN

       ; Check for a valid widget id
       ;
       HAVE_PARENT = WIDGET_INFO (dialog_parent, /VALID_ID)

    ENDIF   

    IF (HAVE_PARENT) THEN BEGIN

       topLevel = WIDGET_BASE (TITLE = ' ', /COLUMN, $
	   XSIZE = xsize, YSIZE = ysize, $
	   /FLOATING, /MODAL, GROUP_LEADER = dialog_parent, _EXTRA = extra)

    ENDIF ELSE BEGIN

       topLevel = WIDGET_BASE (TITLE = ' ', /COLUMN, $
	   XSIZE = xsize, YSIZE = ysize, MAP = 0, _EXTRA = extra)   

    ENDELSE


    ; Title
    ;
    FOR i = 0, N_ELEMENTS (title) - 1 DO $
	theTitle = WIDGET_LABEL (topLevel, VALUE = title[i])


    ; Selection buttons
    ;
    IF (HAVE_SCROLL) THEN BEGIN

       buttonBase = WIDGET_BASE (topLevel, /COLUMN, /FRAME, /SCROLL, $
	   X_SCROLL_SIZE = x_scroll_size, Y_SCROLL_SIZE = y_scroll_size, $
	   EXCLUSIVE = exclusive, NONEXCLUSIVE = nonexclusive)

    ENDIF ELSE BEGIN

       buttonBase = WIDGET_BASE (topLevel, /COLUMN, /FRAME, $
	   EXCLUSIVE = exclusive, NONEXCLUSIVE = nonexclusive)

    ENDELSE

    choiceID = LONARR (numChoices)
    FOR i = 0, numChoices - 1 DO $
	choiceID[i] = WIDGET_BUTTON (buttonBase, VALUE = choices[i], UVALUE = i)

    ; Set the initial selection(s)
    ;
    selected = LONARR (numChoices)

    IF (exclusive) THEN BEGIN
       initial = initial[0]    
       ;selected = 1L
    ENDIF
       
    FOR i = 0, N_ELEMENTS (initial) - 1 DO BEGIN

	WIDGET_CONTROL, choiceID[i], SET_BUTTON = initial[i]
	selected[i] = LONG (initial[i])

    ENDFOR
    
    w = WIDGET_BUTTON (topLevel, VALUE = 'Accept')
    w = WIDGET_BUTTON (topLevel, VALUE = 'Cancel')

    ; Map widget
    ;
    WIDGET_CONTROL, topLevel, /REALIZE 

    ; Place the dialog: window manager dependent
    ;
    IF (NOT HAVE_PARENT) THEN BEGIN

       currentScreen = GET_SCREEN_SIZE()
       WIDGET_CONTROL, topLevel, TLB_GET_SIZE = dialogSize

       dialogXY = [ (currentScreen[0] / 2.0) - (dialogSize[0] / 2.0), $ 
                    (currentScreen[1] / 2.0) - (dialogSize[1] / 2.0) ] 

       IF (HAVE_XPOS) THEN $
	  dialogXY[0] = xposition 

       IF (HAVE_YPOS) THEN $
	  dialogXY[1] = yposition 

       WIDGET_CONTROL, topLevel, $
	   TLB_SET_XOFFSET = dialogXY[0], TLB_SET_YOFFSET = dialogXY[1]

       WIDGET_CONTROL, topLevel, MAP = 1

    ENDIF

    
    ; Event loop
    ;
    value = ''    
    REPEAT BEGIN
    
        event = WIDGET_EVENT (topLevel)
	
	WIDGET_CONTROL, event.id, GET_VALUE = value

        CASE (value[0]) OF

	    'Cancel': BEGIN
		selected = LONARR (numChoices)
                WIDGET_CONTROL, event.top, /DESTROY
		END

            'Accept': BEGIN
                WIDGET_CONTROL, event.top, /DESTROY
		END

	    ELSE: BEGIN
		WIDGET_CONTROL, event.id, GET_UVALUE = i
                selected[i] = event.select 
	        END

	ENDCASE
        
    ENDREP UNTIL ((value EQ 'Accept') OR (value EQ 'Cancel'))
    

    RETURN, selected

END
