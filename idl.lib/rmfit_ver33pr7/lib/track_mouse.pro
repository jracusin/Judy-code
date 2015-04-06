;+
; NAME:
;      TRACK_MOUSE
;
; VERSION:
;      0.2
;
; PURPOSE:
;      Tracks the mouse cursor in a DRAW_WIDGET, and returns coordinates
;      of a selected region
;
; TYPE:
;      Function
;
; CATEGORY:
;      Widgets
;
; CALLING SEQUENCE:
;      HAVE_SELECTION = TRACK_MOUSE(drawWidgetID, REGION, COORDINATES)
;
; REQUIRED INPUTS:
;      drawWidgetID: Widget ID of a valid WIDGET_DRAW()
;
; OPTIONAL INPUTS:
;      NONE
;
; OUTPUTS:
;      HAVE_SELECTION: INTEGER specifying if selection was completed: 0 if
;          selection was aborted, 1 if it was completed
;
;      REGION: INTEGER specifying the plot region in which the mouse was
;          clicked to exit TRACK_MOUSE()
;
;                      4
;               ----------------
;               |              |
;               |              |
;             1 |      0       | 2
;               |              |
;               ----------------
;                      3
;
;      COORDINATES: FLTARR(2, 2) = coordinates of the selection
;          COORDINATES[0, *] = [X0, X1]
;          COORDINATES[1, *] = [Y0, Y1]
;
;          If a selection is aborted midway through (i.e., only one mouse
;          click), COORDINATES is returned as scalar zero.
;
; KEYWORD PARAMETERS:
;
;      GET_BOX:     Select a box (rubberbanding).  This is the default.
;      GET_XRANGE:  Select an interval from the x-axis
;      GET_YRANGE:  Select an interval from the y-axis
;      GET_SINGLE:  Select a single point (one mouse click).
;
;                   Only one GET keyword can be specified, else an error
;                   is generated.
;
;      LEFT_TEXT:   Text displayed when mouse enters left margin (D = 'EXIT')
;      RIGHT_TEXT:  Right margin text (D = 'RESET')
;      TOP_TEXT:    Top margin text (D = '')
;      BOTTOM_TEXT: Bottom margin text (D = 'Numerical Entry')
;
;                   NOTE: The default text values expect you to perform
;                   certain actions when TRACK_MOUSE() exits in order to
;                   make sense.  For example, when clicking in the right margin
;                   (RESET), you might reset the plot axes, and replot.  For
;                   the left margin (EXIT), you might replot the zoomed region.
;
;      NOWAIT:      By default, TRACK_MOUSE() waits for a margin click before
;                   exiting.  Set NOWAIT to return immediately after a selection
;                   is completed (i.e., after the second mouse click).  REGION
;                   is returned as 0 in this case.  Use this to make multiple
;                   selections by calling TRACK_MOUSE() again.
;
;      XTOLERANCE:  Allow selections slightly outside the left and right
;                   axes, in normalized units (D = 0.0)
;      YTOLERANCE:  Allow selections slightly outside the top and bottom
;                   axes, in normalized units (D = 0.0)
;
;                   WARNING: Large tolerance values (i.e., more than about
;                   0.05) may prevent you from accessing the plot margins.
;
;      COLOR:       Color index of the plotted selections.
;
;      BACKGROUND:  Color index of the plot background.
;
;      CALL_PROCEDURE:  A string denoting a user defined procedure to call
;                   when a selection has been completed.  This procedure must
;                   accept the argument COORDINATES as its only parameter.
;
;      CALL_METHOD:  A string denoting a user defined object method to call
;                   when a selection has been completed.  This method must
;                   accept the argument COORDINATES as its only parameter.
;                   Also the OBJ keyword must be used to supply the object
;                   reference needed for IDL's CALL_METHOD.
;
;      OBJ: See CALL_METHOD keyword.
;
;      _EXTRA:      Any valid OPLOT keywords.
;
;
; COMMON BLOCKS:
;      NONE
;
; SIDE EFFECTS:
;      NONE
;
; RESTRICTIONS:
;      IDL v5.0 or later
;      You must trap WIDGET_DRAW events in your event handler, since
;          TRACK_MOUSE() enables BUTTON and MOTION events in the DRAW widget.
;
; EXAMPLE:
;      See TRACK_MOUSE_EXAMPLE.PRO
;
; MODIFICATION HISTORY:
;
;       RDP, 2005 May, added BACKGROUND keyword.
;       RSM, 1998 August, added GET_SINGLE keyword.
;       RSM, 1998 March, added CALL_PROCEDURE, CALL_METHOD keywords.
;       Written, Robert.Mallozzi@msfc.nasa.gov, March, 1998.
;
;                Idea based on TRACK() for direct graphics windows
;                written by Rob.Preece@msfc.nasa.gov
;
;-
; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

FUNCTION TRACK_MOUSE, drawWidgetID, $                                     ; in
                      REGION, $                                           ; out
                      COORDINATES, $                                      ; out

                      GET_BOX    = GET_BOX, $
                      GET_XRANGE = GET_XRANGE, $
                      GET_YRANGE = GET_YRANGE, $
                      GET_SINGLE = GET_SINGLE, $

                      LEFT_TEXT   = LEFT_TEXT, $
                      RIGHT_TEXT  = RIGHT_TEXT, $
                      TOP_TEXT    = TOP_TEXT, $
                      BOTTOM_TEXT = BOTTOM_TEXT, $

                      NOWAIT = NOWAIT, $

                      XTOLERANCE = XTOLERANCE, $
                      YTOLERANCE = YTOLERANCE, $

                      COLOR = COLOR, $
                      BACKGROUND = BACKGROUND, $

                      CALL_PROCEDURE = call_procedure, $
                      CALL_METHOD = call_method, $
                      OBJ = obj, $

                      _EXTRA = EXTRA



; ----- ERROR CHECKING -----

; Is drawWidgetID a current, valid DRAW_WIDGET?
;
theWidgetValid = WIDGET_INFO(drawWidgetID, /VALID_ID)
IF (NOT theWidgetValid) THEN BEGIN
   MESSAGE, /INFO, 'Input widget ID is not valid.'
   RETURN, 0
ENDIF

theWidgetName = WIDGET_INFO(drawWidgetID, /NAME)
IF (theWidgetName NE 'DRAW') THEN BEGIN
   MESSAGE, /INFO, 'Input widget ID is not a WIDGET_DRAW.'
   RETURN, 0
ENDIF

; ----- KEYWORDS -----

doGetBox    = KEYWORD_SET (GET_BOX)
doGetXrange = KEYWORD_SET (GET_XRANGE)
doGetYrange = KEYWORD_SET (GET_YRANGE)
doGetSingle = KEYWORD_SET (GET_SINGLE)

CASE (TOTAL([doGetBox, doGetXrange, doGetYrange, doGetSingle])) OF
     0: doGetBox = 1
     1:
     ELSE: BEGIN
         MESSAGE, /INFO, 'Multiple GET keywords not allowed.'
         RETURN, 0
         END
ENDCASE

IF (N_ELEMENTS (LEFT_TEXT) EQ 0) THEN $
   LEFT_TEXT = 'EXIT'

IF (N_ELEMENTS (RIGHT_TEXT) EQ 0) THEN $
   RIGHT_TEXT = 'RESET'

IF (N_ELEMENTS (TOP_TEXT) EQ 0) THEN $
   TOP_TEXT = ''

IF (N_ELEMENTS (BOTTOM_TEXT) EQ 0) THEN $
   BOTTOM_TEXT = 'Numerical Entry'

doWait = 1
IF (N_ELEMENTS (NOWAIT) NE 0) THEN $
   doWait = 0

xTol = 0.0
IF (N_ELEMENTS (XTOLERANCE) NE 0) THEN $
   xTol = FLOAT(XTOLERANCE)

yTol = 0.0
IF (N_ELEMENTS (YTOLERANCE) NE 0) THEN $
   yTol = FLOAT(YTOLERANCE)

IF (N_ELEMENTS (color) EQ 0) THEN $
   color = !P.COLOR

IF (N_ELEMENTS (background) EQ 0) THEN $
   background = !P.BACKGROUND

; Ensure that the DRAW widget accepts BUTTON and MOTION events
;
haveButtonEvents = WIDGET_INFO (drawWidgetID, /DRAW_BUTTON_EVENTS)
IF (NOT haveButtonEvents) THEN $
   WIDGET_CONTROL, drawWidgetID, DRAW_BUTTON_EVENTS = 1

haveMotionEvents = WIDGET_INFO (drawWidgetID, /DRAW_MOTION_EVENTS)
IF (NOT haveMotionEvents) THEN $
   WIDGET_CONTROL, drawWidgetID, DRAW_MOTION_EVENTS = 1


; ----- TRACKING CODE -----

PRESS   = 0
RELEASE = 1
MOTION  = 2

; Allow selections slightly outside the plot box
;
TOL_X = (!X.CRANGE[1] - !X.CRANGE[0]) * xTol
TOL_Y = (!Y.CRANGE[1] - !Y.CRANGE[0]) * yTol

X_LO  = !X.CRANGE[0] - TOL_X
X_HI  = !X.CRANGE[1] + TOL_X

Y_LO  = !Y.CRANGE[0] - TOL_Y
Y_HI  = !Y.CRANGE[1] + TOL_Y

; Handle LOG scales
;
IF (!X.TYPE EQ 1) THEN BEGIN
   X_LO = 10.0^(!X.CRANGE[0] - TOL_X)
   X_HI = 10.0^(!X.CRANGE[1] + TOL_X)
ENDIF

IF (!Y.TYPE EQ 1) THEN BEGIN
   Y_LO = 10.0^(!Y.CRANGE[0] - TOL_Y)
   Y_HI = 10.0^(!Y.CRANGE[1] + TOL_Y)
ENDIF

CLIPRECT = [X_LO, Y_LO, X_HI, Y_HI]

; Initialize
;
HAVE_ONE_CLICK = 0
HAVE_TWO_CLICK = 0
DONE_SELECTING = 0
COORDINATES    = 0

PREV_STRING = '(X: ' + ', Y: ' + ')'
XYOUTS, 10, 10, PREV_STRING, /DEVICE, COLOR = color


WHILE (1) DO BEGIN

CONTINUE:

      ; Get mouse coodinates
      ;
      EVENT = WIDGET_EVENT (drawWidgetID)

      ; No double clicks
      ;
      IF (EVENT.CLICKS EQ 2) THEN $
         GOTO, CONTINUE

      XY = CONVERT_COORD (EVENT.X, EVENT.Y, /DEVICE, /TO_DATA)
      XY = XY[0:1]

      CASE (EVENT.TYPE) OF

           MOTION: BEGIN

               CASE (1) OF
                    XY[0] LT X_LO: NEW_STRING = STRING (LEFT_TEXT)
                    XY[0] GT X_HI: NEW_STRING = STRING (RIGHT_TEXT)
                    XY[1] LT Y_LO: NEW_STRING = STRING (BOTTOM_TEXT)
                    XY[1] GT Y_HI: NEW_STRING = STRING (TOP_TEXT)

                    ELSE: NEW_STRING = '(X: ' + STRTRIM(XY[0], 2) + $
                                      ', Y: ' + STRTRIM(XY[1], 2) + ')'
               ENDCASE

               XYOUTS, 10, 10, PREV_STRING, /DEVICE, COLOR = background
               XYOUTS, 10, 10, NEW_STRING, /DEVICE, COLOR = color
               PREV_STRING = NEW_STRING

               IF ((doGetBox) AND (HAVE_ONE_CLICK) AND $
                  (NOT DONE_SELECTING)) THEN BEGIN

                  ; Starting coordinates
                  ;
                  START_XY = FIRST_CLICK_COORDS

                  ; Current coordinates
                  ;
                  CURRENT_XY = XY

                  ; Initialize previous coordinates
                  ;
                  IF (N_ELEMENTS (PREV_XY) EQ 0) THEN $
                     PREV_XY = START_XY

                  ; Erase previous box
                  ;
                  PLOTS, [START_XY[0], START_XY[0], PREV_XY[0],  $
                          PREV_XY[0], START_XY[0]],  $
                         [START_XY[1], PREV_XY[1], PREV_XY[1], $
                          START_XY[1], START_XY[1]], $
                         COLOR = background, LINESTYLE = 1, $
                         CLIP = CLIPRECT, NOCLIP = 0

                  ; Draw current box
                  ;
                  PLOTS, [START_XY[0], START_XY[0], CURRENT_XY[0],  $
                          CURRENT_XY[0], START_XY[0]],  $
                         [START_XY[1], CURRENT_XY[1], CURRENT_XY[1], $
                          START_XY[1], START_XY[1]], $
                         COLOR = color, LINESTYLE = 1, $
                         CLIP = CLIPRECT, NOCLIP = 0

                  ; Save current coordinates
                  ;
                  PREV_XY = CURRENT_XY

               ENDIF
               END ; MOTION

           PRESS: BEGIN

               RETURN_FLAG = 0
               REGION = -1

               IF (doGetSingle) THEN BEGIN
	          RETURN_FLAG = 1            ; Needs to be set to clear "EXIT"
                  REGION = 0
		ENDIF

               ; Bottom margin
               ;
               IF (XY[1] LT Y_LO) THEN BEGIN
                  RETURN_FLAG = 1
                  REGION = 3
               ENDIF

               ; Right margin
               ;
               IF (XY[0] GT X_HI) THEN BEGIN
                  RETURN_FLAG = 1
                  REGION = 2
               ENDIF

               ; Top margin
               ;
               IF (XY[1] GT Y_HI) THEN BEGIN
                  RETURN_FLAG = 1
                  REGION = 4
               ENDIF

               ; Left margin
               ;
               IF (XY[0] LT X_LO) THEN BEGIN
                  RETURN_FLAG = 1
                  REGION = 1
               ENDIF

               IF (doGetSingle) THEN BEGIN
                  COORDINATES = XY
                  RETURN, 1
               ENDIF

               IF (RETURN_FLAG) THEN BEGIN

                  XYOUTS, 10, 10, PREV_STRING, /DEVICE, COLOR = background

                  IF (doGetXrange) THEN BEGIN
                     IF ((HAVE_ONE_CLICK) OR (DONE_SELECTING)) THEN $
                        OPLOT, [1, 1] * FIRST_CLICK_COORDS[0], [Y_LO, Y_HI], $
                               COLOR = background, _EXTRA = EXTRA
                     IF ((HAVE_TWO_CLICK)  OR (DONE_SELECTING)) THEN $
                        OPLOT, [1, 1] * SECOND_CLICK_COORDS[0], [Y_LO, Y_HI], $
                               COLOR = background, _EXTRA = EXTRA
                  ENDIF

                  IF (doGetYrange) THEN BEGIN
                
                     IF ((HAVE_ONE_CLICK) OR (DONE_SELECTING)) THEN $
                        OPLOT, [X_LO, X_HI], [1, 1] * FIRST_CLICK_COORDS[1], $
                                  COLOR = background, _EXTRA = EXTRA
                     IF ((HAVE_TWO_CLICK) OR (DONE_SELECTING)) THEN $
                        OPLOT, [X_LO, X_HI], [1, 1] * SECOND_CLICK_COORDS[1], $
                                  COLOR = background, _EXTRA = EXTRA
                  ENDIF

                  IF (doGetBox) THEN BEGIN
                  
                     IF ((HAVE_ONE_CLICK) OR (DONE_SELECTING)) THEN $
                        PLOTS, [START_XY[0], START_XY[0], PREV_XY[0],  $
                               PREV_XY[0], START_XY[0]],  $
                                  [START_XY[1], PREV_XY[1], PREV_XY[1], $
                               START_XY[1], START_XY[1]], $
                                  COLOR = background, LINESTYLE = 1, $
                                  CLIP = CLIPRECT, NOCLIP = 0
                  ENDIF

                  ; Restore DRAW widget event handler state
                  ;
                  IF (NOT haveButtonEvents) THEN $
                     WIDGET_CONTROL, drawWidgetID, DRAW_BUTTON_EVENTS = 0
                  IF (NOT haveMotionEvents) THEN $
                     WIDGET_CONTROL, drawWidgetID, DRAW_MOTION_EVENTS = 0

                  IF (DONE_SELECTING) THEN $
                     RETURN, 1

                  RETURN, 0

               ENDIF ; RETURN_FLAG


               IF (DONE_SELECTING) THEN BEGIN

                  IF (doGetXrange) THEN BEGIN
                     OPLOT, [1, 1] * FIRST_CLICK_COORDS[0], [Y_LO, Y_HI], $
                                    COLOR = background, _EXTRA = EXTRA
                     OPLOT, [1, 1] * SECOND_CLICK_COORDS[0], [Y_LO, Y_HI], $
                                    COLOR = background, _EXTRA = EXTRA
                  ENDIF
                  
                  IF (doGetYrange) THEN BEGIN
                     OPLOT, [X_LO, X_HI], [1, 1] * FIRST_CLICK_COORDS[1], $
                         COLOR = background, _EXTRA = EXTRA
                     OPLOT, [X_LO, X_HI], [1, 1] * SECOND_CLICK_COORDS[1], $
                         COLOR = background, _EXTRA = EXTRA
                  ENDIF
                  
                  IF (doGetBox) THEN BEGIN
                     PLOTS, [START_XY[0], START_XY[0], PREV_XY[0],  $
                             PREV_XY[0], START_XY[0]],  $
                            [START_XY[1], PREV_XY[1], PREV_XY[1], $
                             START_XY[1], START_XY[1]], $
                            COLOR = background, LINESTYLE = 1, $
                            CLIP = CLIPRECT, NOCLIP = 0
                  ENDIF

               ENDIF

               ; Did not return, so this must be the third click -> reset
               ;
               IF ((HAVE_ONE_CLICK) AND (HAVE_TWO_CLICK)) THEN BEGIN
                  HAVE_ONE_CLICK = 0
                  HAVE_TWO_CLICK = 0
                  DONE_SELECTING = 0
                  COORDINATES    = 0
               ENDIF

               IF (NOT HAVE_ONE_CLICK) THEN BEGIN

                  HAVE_ONE_CLICK = 1
                  FIRST_CLICK_COORDS = XY

                  IF (doGetXrange) THEN $
                     OPLOT, [1, 1] * FIRST_CLICK_COORDS[0], [Y_LO, Y_HI], $
                     LINESTYLE = 2, COLOR = color, _EXTRA = EXTRA

                  IF (doGetYrange) THEN $
                     OPLOT, [X_LO, X_HI], [1, 1] * FIRST_CLICK_COORDS[1], $
                     LINESTYLE = 2, COLOR = color, _EXTRA = EXTRA

               ENDIF ELSE $
               IF ((HAVE_ONE_CLICK) AND (NOT HAVE_TWO_CLICK)) THEN BEGIN

                  HAVE_TWO_CLICK = 1
                  SECOND_CLICK_COORDS = XY

                  IF (doGetXrange) THEN BEGIN
                     OPLOT, [1, 1] * FIRST_CLICK_COORDS[0], [Y_LO, Y_HI], $
                         COLOR = color, _EXTRA = EXTRA
                     OPLOT, [1, 1] * SECOND_CLICK_COORDS[0], [Y_LO, Y_HI], $
                         COLOR = color, _EXTRA = EXTRA
                  ENDIF

                  IF (doGetYrange) THEN BEGIN
                     OPLOT, [X_LO, X_HI], [1, 1] * FIRST_CLICK_COORDS[1], $
                         COLOR = color, _EXTRA = EXTRA
                     OPLOT, [X_LO, X_HI], [1, 1] * SECOND_CLICK_COORDS[1], $
                         COLOR = color, _EXTRA = EXTRA
                  ENDIF

               ENDIF

               IF ((HAVE_ONE_CLICK) AND (HAVE_TWO_CLICK)) THEN BEGIN

                  DONE_SELECTING = 1

                  COORDINATES = FLTARR(2, 2)

                  COORDINATES(0, *) = $
                     [FIRST_CLICK_COORDS[0], SECOND_CLICK_COORDS[0]]
                  COORDINATES(0, *) = COORDINATES(0, SORT(COORDINATES(0, *)))

                  COORDINATES(1, *) = $
                     [FIRST_CLICK_COORDS[1], SECOND_CLICK_COORDS[1]]
                  COORDINATES(1, *) = COORDINATES(1, SORT(COORDINATES(1, *)))

                  IF (NOT doWait) THEN BEGIN

                     REGION = 0

                     XYOUTS, 10, 10, PREV_STRING, /DEVICE, COLOR = background

                     IF (doGetXrange) THEN BEGIN
                        IF ((HAVE_ONE_CLICK) OR (DONE_SELECTING)) THEN $
                           OPLOT, [1, 1] * FIRST_CLICK_COORDS[0], $
                               [Y_LO, Y_HI], COLOR = background, $
                               _EXTRA = EXTRA
                        IF ((HAVE_TWO_CLICK)  OR (DONE_SELECTING)) THEN $
                           OPLOT, [1, 1] * SECOND_CLICK_COORDS[0], $
                           [Y_LO, Y_HI], COLOR = background, $
                            _EXTRA = EXTRA
                     ENDIF

                     IF (doGetYrange) THEN BEGIN
                        IF ((HAVE_ONE_CLICK) OR (DONE_SELECTING)) THEN $
                           OPLOT, [X_LO, X_HI], $
                               [1, 1] * FIRST_CLICK_COORDS[1], $
                               COLOR = background, _EXTRA = EXTRA
                        IF ((HAVE_TWO_CLICK) OR (DONE_SELECTING)) THEN $
                           OPLOT, [X_LO, X_HI], $
                               [1, 1] * SECOND_CLICK_COORDS[1], $
                               COLOR = background, _EXTRA = EXTRA
                     ENDIF

                     IF (doGetBox) THEN BEGIN
                        IF ((HAVE_ONE_CLICK) OR (DONE_SELECTING)) THEN $
                           PLOTS, [START_XY[0], START_XY[0], PREV_XY[0],  $
                               PREV_XY[0], START_XY[0]],  $
                              [START_XY[1], PREV_XY[1], PREV_XY[1], $
                               START_XY[1], START_XY[1]], $
                              COLOR = background, LINESTYLE = 1, $
                              CLIP = CLIPRECT, NOCLIP = 0
                     ENDIF

                     ; Restore DRAW widget event handler state
                     ;
                     IF (NOT haveButtonEvents) THEN $
                        WIDGET_CONTROL, drawWidgetID, DRAW_BUTTON_EVENTS = 0
                     IF (NOT haveMotionEvents) THEN $
                        WIDGET_CONTROL, drawWidgetID, DRAW_MOTION_EVENTS = 0

                     ; Call a user-defined procedure?
                     ;
                     IF (N_ELEMENTS(call_procedure) NE 0) THEN $
                        CALL_PROCEDURE, call_procedure, COORDINATES, $
                        _EXTRA = extra

                     IF (N_ELEMENTS(call_method) NE 0) THEN $
                        CALL_METHOD, call_method, obj, COORDINATES, $
                        _EXTRA = extra

                     RETURN, 1

                  ENDIF ; NOT doWait

               ENDIF ; HAVE_CLICKS

               END ; PRESS

           ELSE: ; Ignore all other events

      ENDCASE

ENDWHILE


END
