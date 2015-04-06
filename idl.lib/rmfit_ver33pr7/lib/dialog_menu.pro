; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
;+
; NAME:
;     DIALOG_MENU
;
; PURPOSE:
;     A modal (blocking) dialog widget to display a selectable menu.
;     The dialog must be dismissed by selecting a menu item before
;     execution of the calling program can continue.
;
; TYPE:
;     FUNCTION
;
; CATEGORY:
;     WIDGETS
;
; CALLING SEQUENCE:
;     result = DIALOG_MENU (menu_items)
;
; INPUTS:
;     menu_items: STRARR of menu items
;
; KEYWORD PARAMETERS:
;
;     TITLE: The menu title.  Use STRARR for multi-line titles.
;
;     XSIZE, YSIZE: x, y size of the widget.
;     XPOSITION, YPOSITION: The position of the upper left corner of the
;            dialog, relative to the upper left corner of the screen.
;
;     SCROLL: Adds a scroll bar
;     X_SCROLL_SIZE: Size of x scroll region, if /SCROLL is set
;     Y_SCROLL_SIZE: Size of y scroll region, if /SCROLL is set
;
;     DIALOG_PARENT: Set this keyword to the widget ID of a widget over
;            which the message dialog should be positioned. When displayed,
;            the DIALOG_MENU dialog will be positioned over the specified
;            widget. Dialogs are often related to a non-dialog widget tree.
;            The ID of the widget in that tree to which the dialog is most
;            closely related should be specified.
;          
;            If this keyword is not specified, the default placement of the
;            menu is the center of the screen (window manager dependent).
;
;     INDEX: Optionally return the index of the selected item instead of
;            the item's text.  The index of the first menu_item is zero.
;
;     DECORATIONS: Set to 0 to suppress the window title bar, or 1 to 
;            include.  This request might not be honored by the window
;            manager (DEFAULT = 1)
;    
; OUTPUTS:
;     result: String of selected menu_item, or integer index if INDEX keyword
;         is used, starting at index = 0
;
; COMMON BLOCKS:
;     None
;
; SIDE EFFECTS:
;     Creates a modal widget
;
; RESTRICTIONS:
;     None known
;
; DEPENDENCIES:
;     None
;
; EXAMPLES:
;     result = DIALOG_MENU (['Item 1', 'Item 2', 'Item3'], $
;         TITLE = 'Select an Item')
;
; MODIFICATION HISTORY:
;
;     v1.05: RSM, February 1999
;            Added DECORATIONS keyword to send a hint to the window manager
;            that the window system menu and title bar should be suppressed.
;            This hint might is not guaranteed to be honored by the window
;            manager (although it usually is).  Set DECORATIONS to 0 to
;            suppress the window title bar, or 1 to include (DEFAULT = 1)
;
;     v1.04: RSM, May 1998
;            Added XPOSITION, YPOSITION keywords.  Removed the UL, LL, UR, LR 
;            keywords.  Changed WIDTH keyword to XSIZE, and added YSIZE 
;            keyword.  These bring the widget in line with common IDL widget
;            size and position keywords.  
;
;            THESE CHANGES WILL BREAK OLD CODE THAT USED ANY OF THESE KEYWORDS!
;
;     v1.03: RSM, Mar 1998
;            If not using /SCROLL, removed the sub-base for the menu buttons 
;            so that they span the full width of the top level base. 
;
;     v1.02: RSM, Mar 1998
;            Changed to return selected item's text, not index (index can
;            now be returned with the INDEX keyword).
;                
;     v1.01: RSM, Mar 1998
;            Fixed error when used with a modal toplevel base.
;
;     v1.0:  Written, Robert.Mallozzi@msfc.nasa.gov, November 1997.
;
;-
; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 

FUNCTION DIALOG_MENU, selections, $

    TITLE = title, $
    XSIZE = xsize, ysize = ysize, $
    XPOSITION = xposition, YPOSITION = yposition, $
    SCROLL = SCROLL, $
    X_SCROLL_SIZE = X_SCROLL_SIZE, $
    Y_SCROLL_SIZE = Y_SCROLL_SIZE, $
    DIALOG_PARENT = dialog_parent, $
    INDEX = index, $
    DECORATIONS = decorations


    HAVE_SCROLL = N_ELEMENTS (scroll) NE 0
    HAVE_PARENT = N_ELEMENTS (dialog_parent) NE 0
    HAVE_XPOS   = N_ELEMENTS (xposition) NE 0
    HAVE_YPOS   = N_ELEMENTS (yposition) NE 0

    attributes = 0
    IF (N_ELEMENTS (decorations) NE 0) THEN $
       attributes = ((decorations EQ 0) ? 1 + 2 + 4 : 0)
    
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
	   /FLOATING, /MODAL, GROUP_LEADER = dialog_parent, $
           TLB_FRAME_ATTR = attributes)

    ENDIF ELSE BEGIN

       topLevel = WIDGET_BASE (TITLE = ' ', /COLUMN, $
	   XSIZE = xsize, YSIZE = ysize, MAP = 0, $
           TLB_FRAME_ATTR = attributes)

    ENDELSE


    ; Add the title, if it's supplied
    ;
    FOR i = 0, N_ELEMENTS (title) - 1 DO $
	theTitle = WIDGET_LABEL (topLevel, VALUE = title[i])


    ; Selection buttons
    ;
    IF (HAVE_SCROLL) THEN BEGIN

       buttonBase = WIDGET_BASE (topLevel, /COLUMN, /FRAME, /SCROLL, $
	   X_SCROLL_SIZE = X_SCROLL_SIZE, Y_SCROLL_SIZE = Y_SCROLL_SIZE)

    ENDIF ELSE BEGIN

       buttonBase = topLevel

    ENDELSE

    FOR i = 0, N_ELEMENTS (selections) - 1 DO $
	button = WIDGET_BUTTON (buttonBase, VALUE = selections[i], UVALUE = i)


    ; Map widget
    ;
    WIDGET_CONTROL, topLevel, /REALIZE 

    ; Place the dialog: window manager dependent
    ;
    IF (NOT HAVE_PARENT) THEN BEGIN

       SCREEN = GET_SCREEN_SIZE()
       WIDGET_CONTROL, topLevel, TLB_GET_SIZE = DIALOG_SIZE

       DIALOG_PT = [(SCREEN[0] / 2.0) - (DIALOG_SIZE[0] / 2.0), $ 
                    (SCREEN[1] / 2.0) - (DIALOG_SIZE[1] / 2.0)] 

       IF (HAVE_XPOS) THEN $
	  DIALOG_PT[0] = xposition 

       IF (HAVE_YPOS) THEN $
	  DIALOG_PT[1] = yposition 

       WIDGET_CONTROL, topLevel, $
	   TLB_SET_XOFFSET = DIALOG_PT[0], TLB_SET_YOFFSET = DIALOG_PT[1]

       WIDGET_CONTROL, topLevel, MAP = 1

    ENDIF


    ; Get the event, without using XMANAGER
    ;
    event = WIDGET_EVENT (topLevel)

    ; Process the event
    ;
    WIDGET_CONTROL, event.id, GET_VALUE = value, GET_UVALUE = uvalue

    FOR i = 0, N_ELEMENTS (selections) - 1 DO BEGIN

	IF (uvalue EQ i) THEN BEGIN

	   WIDGET_CONTROL, event.top, /DESTROY
	   IF (N_ELEMENTS (index) NE 0) THEN BEGIN
              RETURN, i
	   ENDIF ELSE BEGIN   
              RETURN, selections[i]
	   ENDELSE

	ENDIF

    ENDFOR


    RETURN, -1



END
