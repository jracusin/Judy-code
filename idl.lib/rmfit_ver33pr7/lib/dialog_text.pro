; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;+
; NAME:
;     DIALOG_TEXT
;
; PURPOSE:
;     A modal (blocking) dialog widget to display a STRARR of text, or an
;     ASCII text file.  The dialog must be dismissed 'Dismiss' button before
;     execution of the calling program can continue.
;
; TYPE:
;     FUNCTION
;
; CATEGORY:
;     WIDGETS
;
; CALLING SEQUENCE:
;     result = DIALOG_TEXT (text)
;
; INPUTS:
;     text: STRARR to be displayed on the widget.
;
; KEYWORD PARAMETERS:
;     TITLE: Window title [default = 'dialog_text']
;
;     FILE: Set this keyword to an ASCII filename to display the file contents
;           in the widget.  If this keyword is set, the 'text' parameter,
;           if supplied, is ignored.
;
;     XSIZE: The width of the dialog (characters) [default = 80]
;     YSIZE: The height of the dialog (characters) [default = 20]
;
;     DIALOG_PARENT: Set this keyword to the widget ID of a widget over
;            which the message dialog should be positioned. When displayed,
;            the DIALOG_TEXT dialog will be positioned over the specified
;            widget. Dialogs are often related to a non-dialog widget tree.
;            The ID of the widget in that tree to which the dialog is most
;            closely related should be specified.
;
;     FIXED: Request font 'fixed' for display in the text widget
;
; OUTPUTS:
;     result: 1 (or 0 for failure, i.e., missing parameters)
;
; COMMON BLOCKS:
;     NONE
;
; SIDE EFFECTS:
;     Creates a modal widget
;
; RESTRICTIONS:
;     NONE
;
; DEPENDENCIES:
;     NONE
;
; MODIFICATION HISTORY:
;     v1.05: RSM, Sep 1999, widget layout improvements
;     v1.04: RSM, Dec 1998, added WRAP keyword to the call to WIDGET_TEXT()
;     v1.03: RSM, Mar 1998, converted obsolete !ERR_STRING to !ERROR_STATE.
;     v1.02: RSM, Mar 1998, added SAVE_OPTION keyword to allow saving to file.
;     v1.01: RSM, Mar 1998, fixed error when used with a modal toplevel base.
;     v1.0: written, Robert.Mallozzi@msfc.nasa.gov, October 1997.
;
;-
; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

FUNCTION DIALOG_TEXT, text, $
    TITLE = title, $
    XSIZE = xSize, YSIZE = ySize, $
    FILE = file, $
    SAVE_OPTION = save_option, $
    DIALOG_PARENT = dialog_parent, $
    FIXED = fixed


    ; This will fail if FILE is passed in as an undefined parameter
    ;
    IF ((N_ELEMENTS (text) EQ 0) AND (N_ELEMENTS (file) EQ 0)) THEN BEGIN
       MESSAGE, /CONTINUE, 'Incorrect number of arguments.'
       RETURN, 0
    ENDIF

    IF (N_ELEMENTS (title) EQ 0) THEN $
       title = 'dialog_text'

    HAVE_PARENT = N_ELEMENTS (dialog_parent) NE 0
    HAVE_FILE   = N_ELEMENTS (file) NE 0

    useSaveOption = N_ELEMENTS (save_option) NE 0

    IF (N_ELEMENTS (XSIZE) EQ 0) THEN BEGIN
       xSize = 80
    ENDIF ELSE BEGIN
       xSize = XSIZE
    ENDELSE

    IF (N_ELEMENTS (YSIZE) EQ 0) THEN BEGIN
       ySize = 20
    ENDIF ELSE BEGIN
       ySize = YSIZE
    ENDELSE

    ; Top level base
    ;
    IF (HAVE_PARENT) THEN BEGIN

       ; Check for a valid widget id
       ;
       HAVE_PARENT = WIDGET_INFO (dialog_parent, /VALID_ID)

    ENDIF

    IF (HAVE_PARENT) THEN BEGIN
       base = WIDGET_BASE (TITLE = title, /COLUMN, $
           /FLOATING, /MODAL, GROUP_LEADER = dialog_parent, /TLB_KILL_REQUEST_EVENTS)
    ENDIF ELSE BEGIN
       base = WIDGET_BASE (TITLE = title, /COLUMN, MAP = 0, /TLB_KILL_REQUEST_EVENTS)
    ENDELSE

    ; Text
    ;
    errVal = 0
    IF (HAVE_FILE) THEN BEGIN
       OPENR, FL, file, /GET_LUN, ERROR = ferr
       IF (ferr NE 0) THEN BEGIN
          text = [!ERROR_STATE.MSG, !ERROR_STATE.SYS_MSG]
          errVal = 1
       ENDIF ELSE BEGIN
          text = '' & text_in = ''
          READF, FL, text
          WHILE (NOT EOF(FL)) DO BEGIN
                READF, FL, text_in
                text = [text, text_in]
          ENDWHILE
		  CLOSE, fl
          FREE_LUN, FL
       ENDELSE
    ENDIF

    IF (KEYWORD_SET (fixed)) THEN BEGIN
       button = WIDGET_TEXT (base, VALUE = text, $
           XSIZE = xsize, YSIZE = ysize, /SCROLL, /WRAP, FONT = 'fixed')
    ENDIF ELSE BEGIN
       button = WIDGET_TEXT (base, VALUE = text, $
           XSIZE = xsize, YSIZE = ysize, /SCROLL, /WRAP)
    ENDELSE

    b = WIDGET_BASE (base, /ROW, /GRID, /ALIGN_CENTER, SPACE = 20)

    ; Save option
    ;
    IF (useSaveOption) THEN $
       button = WIDGET_BUTTON (b, VALUE = 'Save to File')

    ; Dismiss button
    ;
    button = WIDGET_BUTTON (b, VALUE = 'Dismiss')

    ; Map to screen
    ;
    WIDGET_CONTROL, base, /REALIZE


    ; Place the dialog: window manager dependent
    ;
    IF (NOT HAVE_PARENT) THEN BEGIN

       CURRENT_SCREEN = GET_SCREEN_SIZE()
       WIDGET_CONTROL, base, TLB_GET_SIZE = DIALOG_SIZE

       DIALOG_PT = [(CURRENT_SCREEN[0] / 2.0) - (DIALOG_SIZE[0] / 2.0), $
                    (CURRENT_SCREEN[1] / 2.0) - (DIALOG_SIZE[1] / 2.0)]

       WIDGET_CONTROL, base, $
                       TLB_SET_XOFFSET = DIALOG_PT[0], $
                       TLB_SET_YOFFSET = DIALOG_PT[1]
       WIDGET_CONTROL, base, MAP = 1

    ENDIF


    ; Get the event, without using XMANAGER
    ;
    event = WIDGET_EVENT (base)
    type = TAG_NAMES (event, /STRUCTURE)

    IF type EQ 'WIDGET_KILL_REQUEST' THEN BEGIN
        WIDGET_CONTROL, base, /DESTROY
        RETURN, errVal
		END
		
    WIDGET_CONTROL, event.id, GET_VALUE = value

    WHILE (value EQ 'Save to File') DO BEGIN

       fileName = DIALOG_PICKFILE (/WRITE, DIALOG_PARENT = event.top, $
           FILE = 'dlg.txt')
       IF (fileName NE '') THEN BEGIN

          d = FINDFILE (fileName, COUNT = fileExists)
          IF (fileExists NE 0) THEN BEGIN
             IF (DIALOG_MESSAGE (/QUESTION, ['File ' + fileName + ' exists.', $
                'Overwrite?'], DIALOG_PARENT = event.top) EQ 'No') THEN $
                GOTO, CONTINUE
          ENDIF

          OPENW, FL, fileName, /GET_LUN, ERROR = ferr
          IF (ferr EQ 0) THEN BEGIN
             FOR i = 0, N_ELEMENTS(text) - 1 DO $
             PRINTF, FL, text[i]
			 CLOSE, fl
             FREE_LUN, FL
          ENDIF ELSE BEGIN
             d = DIALOG_MESSAGE (/ERROR, $
                 [!ERROR_STATE.MSG, !ERROR_STATE.SYS_MSG], $
                 DIALOG_PARENT = event.top)
          ENDELSE

       ENDIF

    CONTINUE:

       event = WIDGET_EVENT (base)
       WIDGET_CONTROL, event.id, GET_VALUE = value

    ENDWHILE

    WIDGET_CONTROL, base, /DESTROY
    RETURN, errVal



END
