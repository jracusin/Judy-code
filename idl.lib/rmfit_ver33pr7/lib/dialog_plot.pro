; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
;+
; NAME:
;     DIALOG_PLOT
;
; PURPOSE:
;     A modal (blocking) dialog widget to display a plot.  
;     The dialog must be dismissed with the 'Dismiss' button before
;     execution of the calling program can continue.
;
; TYPE:
;     FUNCTION
;
; CATEGORY:
;     WIDGETS
;
; CALLING SEQUENCE:
;     result = DIALOG_PLOT (XDATA = xData, YDATA = yData)
;
; INPUTS:
;     XDATA and YDATA (via keyword, see below)
;
; KEYWORD PARAMETERS:
;
;     XDATA:         Array of independent data.
;     YDATA:         Array of dependent data.  
;
;     CALL_PROC:     STRARR containing one or more IDL statements to further process. 
;                    The parameters xData, yData will be passed to the 
;                    CALL_PROC string. See below for an example.
;  
;     PS_OPTION:     Add optional button to create a Postscript file
;     GIF_OPTION:    Add optional button to create a gif image
;
;     WINTITLE:      Window title [default = 'dialog_plot']
;
;     XSIZE:         The width of the plot region  (pixels) [default = 500]
;     YSIZE:         The height of the plot region (pixels) [default = 400]
;
;     DIALOG_PARENT: Set this keyword to the widget ID of a widget over
;                    which the message dialog should be positioned. When 
;                    displayed, the DIALOG_PLOT dialog will be positioned over 
;                    the specified widget. Dialogs are often related to a 
;                    non-dialog widget tree.  The ID of the widget in that 
;                    tree to which the dialog is most closely related should 
;                    be specified.
;
;     All PLOT keywords also accepted.
;
; OUTPUTS:
;     result: 1, or 0 for failure (missing parameters, or CALL_PROC failed)
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
; EXAMPLE:
;
;     xData = [0, 100]
;     yData = [-100, 100]
;
;     ; Create a simple plot, with an option to print to Postscript file.
;     ;   
;     result = DIALOG_PLOT (XDATA = xData, YDATA = yData, $
;         winTitle = 'Demo', /PS_OPTION)
;
;     ; Create a custom plot, with an option to print to gif file.  Note 
;     ; that CALL_PROC will utilize the XDATA, YDATA keywords.
;     ;   
;     result = DIALOG_PLOT (XDATA = xData, YDATA = yData, CALL_PROC = [ $
;         'OPLOT'], $
;         XTITLE = 'xTitle', YTITLE = 'yTitle', winTitle = 'Demo', /GIF_OPTION)
;
; MODIFICATION HISTORY:
;
;     v2.0: RDP: 2004 September
;           Removed EXECUTE, so that RMFIT could run in the Virtual Machine.
;           This reduces the ability of the code to do further processing, but 
;           it is only called once, so this is not too onerous.
;
;     v1.0: RSM: 1999 February
;           Fixed error when using exe_cute keyword and selection 
;           the PS option button
;
;     v0.1: written, Robert.Mallozzi@msfc.nasa.gov, March 1998.
;
;-
; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 

FUNCTION DIALOG_PLOT, $
    XDATA = xData, YDATA = yData, $
    CALL_PROC = execute_s, $
    WINTITLE = winTitle, $
    XSIZE = xSize, YSIZE = ySize, $
    PS_OPTION = ps_option, GIF_OPTION = gif_option, $
    DIALOG_PARENT = dialog_parent, $
    _EXTRA = EXTRA


haveXdata = (N_ELEMENTS(xData) NE 0)
haveYdata = (N_ELEMENTS(yData) NE 0)

doexecute = (N_ELEMENTS(execute_s) NE 0)

IF (NOT haveXdata) OR (NOT haveYdata) THEN BEGIN
   MESSAGE, /INFO, 'Usage: result = DIALOG_PLOT (XDATA = xData, YDATA = yData)'
   RETURN, 0
ENDIF

pSave = !P
xSave = !X
ySave = !Y

IF (N_ELEMENTS(winTitle) EQ 0) THEN $
   winTitle = 'dialog_plot'

HAVE_PARENT = N_ELEMENTS(dialog_parent) NE 0

usePsOption  = N_ELEMENTS(ps_option) NE 0
useGifOption = N_ELEMENTS(gif_option) NE 0

IF (N_ELEMENTS(XSIZE) EQ 0) THEN BEGIN
   xSize = 500
ENDIF ELSE BEGIN
   xSize = XSIZE
ENDELSE

IF (N_ELEMENTS(YSIZE) EQ 0) THEN BEGIN
   ySize = 400
ENDIF ELSE BEGIN
   ySize = YSIZE
ENDELSE

; Top level base
;
IF (HAVE_PARENT) THEN BEGIN

   ; Check for a valid widget id
   ;
   HAVE_PARENT = WIDGET_INFO (LONG(dialog_parent), /VALID_ID)

ENDIF   

IF (HAVE_PARENT) THEN BEGIN
   base = WIDGET_BASE (TITLE = winTitle, /COLUMN, $
       /FLOATING, /MODAL, GROUP_LEADER = dialog_parent)
ENDIF ELSE BEGIN
   base = WIDGET_BASE (TITLE = winTitle, /COLUMN, MAP = 0)
ENDELSE

; Plotting canvas
;
winSave = !D.WINDOW
w = WIDGET_DRAW (base, XSIZE = xsize, YSIZE = ysize, /FRAME)
WIDGET_CONTROL, w, GET_VALUE = win
WSET, win

; Option buttons
;
IF (usePsOption) THEN $
   w = WIDGET_BUTTON (base, VALUE = 'Save to Postscript File')

IF (useGifOption) THEN $
   w = WIDGET_BUTTON (base, VALUE = 'Save to gif File')

w = WIDGET_BUTTON (base, VALUE = 'Dismiss')


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

; Plot data
;
status = 1
IF (doexecute) THEN BEGIN

   ; Build the scalar command required by IDL's exe_cute function
   ;
   ;command = ''
   
   ; Pass along any additional keywords to the PLOT call
   ; TODO: verify that first command calls a plot command (PLOT, MAP_SET, etc)
   ;
   ;command = command + execute_s[0] + ', _EXTRA = EXTRA' 
   PLOT, xData, yData, _EXTRA = EXTRA

   FOR i = 0, N_ELEMENTS(execute_s) - 1 DO BEGIN
   ;    command = command + ' & ' + execute_s[i]
       CALL_PROCEDURE, execute_s[i], xData, yData
   
   ENDFOR

;   status = exe_cute (command)
;   IF (NOT status) THEN BEGIN
;      MESSAGE, /INFO, 'exe_cute failed: ' + command
;      GOTO, ERROR
;   ENDIF
   
ENDIF ELSE BEGIN

   PLOT, xData, yData, _EXTRA = EXTRA

ENDELSE

; Get the event, without using XMANAGER
;
NEXT:
event = WIDGET_EVENT (base)
WIDGET_CONTROL, event.id, GET_VALUE = value

IF (value EQ 'Save to Postscript File') THEN BEGIN

   fileName = DIALOG_PICKFILE (/WRITE, FILTER = '*.ps', $
       DIALOG_PARENT = event.top)
   IF (fileName NE '') THEN BEGIN

      d = FINDFILE (fileName, COUNT = fileExists)
      IF (fileExists) THEN BEGIN
         IF (DIALOG_MESSAGE (/QUESTION, ['File ' + fileName + ' exists.', $
	     'Overwrite?'], DIALOG_PARENT = event.top) EQ 'No') THEN $
            GOTO, NEXT
      ENDIF

      deviceSave = !D.NAME
      SET_PLOT, 'PS'
      DEVICE, FILE = fileName

      IF (doexecute) THEN BEGIN
         
          PLOT, xData, yData, _EXTRA = EXTRA

          FOR i = 0, N_ELEMENTS(execute_s) - 1 DO BEGIN
              CALL_PROCEDURE, execute_s[i], xData, yData
          ENDFOR

;         status = exe_cute (command)
;         IF (NOT status) THEN BEGIN
;            MESSAGE, /INFO, 'exe_cute failed: ' + command
;            GOTO, ERROR
;         ENDIF

      ENDIF ELSE BEGIN

         PLOT, xData, yData, _EXTRA = EXTRA
      
      ENDELSE
      
      DEVICE, /CLOSE_FILE
      SET_PLOT, deviceSave

   ENDIF

   GOTO, NEXT
   
ENDIF

IF (value EQ 'Save to gif File') THEN BEGIN

   fileName = DIALOG_PICKFILE (/WRITE, FILTER = '*.gif', $
       DIALOG_PARENT = event.top)
   IF (fileName NE '') THEN BEGIN

      d = FINDFILE (fileName, COUNT = fileExists)
      IF (fileExists) THEN BEGIN
         IF (DIALOG_MESSAGE (/QUESTION, ['File ' + fileName + ' exists.', $
	     'Overwrite?'], DIALOG_PARENT = event.top) EQ 'No') THEN $
            GOTO, NEXT
      ENDIF

      WRITE_GIF, fileName, TVRD()      
      
   ENDIF

   GOTO, NEXT
   
ENDIF


ERROR:

!P = pSave
!X = xSave
!Y = ySave

WSET, winSave


WIDGET_CONTROL, base, /DESTROY
RETURN, status



END
